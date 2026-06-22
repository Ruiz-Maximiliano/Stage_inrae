# ============================================================
# SCRIPT 1 — Initialisation (depuis la BD + backup CSV)
#
# (Ré)initialise la table météo sans repasser par l'API pour tout l'historique
# (évite les 429 liés aux longues périodes, ex. 10 ans) — version adaptée du
# script 01_initialisation_proposition.R du projet principal (Stage_inrae/scripts/),
# seule version d'initialisation utilisée dans ce dossier pipeline_test/.
#
# CE QUE FAIT CE CODE :
#   1. Vérifie d'abord l'ÉTAT DE LA BD elle-même (pas seulement le CSV) :
#        - Table absente/vide  → premier lancement, tout est à télécharger.
#        - Table déjà présente → compte les dates uniques par site sur les
#          n_days_local derniers jours pour repérer les sites absents/incomplets.
#          Si la BD a DÉJÀ plus de données que n_days_local (ex. les 10 ans
#          rechargés manuellement), RIEN n'est touché — ce script n'écrase
#          jamais la BD (toujours en append, jamais en overwrite).
#   2. Pour les sites identifiés comme manquants/incomplets, essaie d'abord de les
#      compléter depuis le backup local (data/meteo_history_backup.csv, généré par
#      le script 01_initialisation.R du projet principal) — rapide, aucun appel API.
#   3. Pour ce qui reste manquant après le CSV (ou si le CSV n'existe pas), va
#      chercher via l'API Open-Meteo, mais SEULEMENT pour ces sites-là.
#   4. Termine par le téléchargement du forecast (avec la même vérification de
#      fraîcheur que dans 02_hebdomadaire.R).
#
# Prérequis :
#   - Table BD db_table_admin accessible (limites administratives, voir config.R)
#   - modèles .rds (00_train_models.R)
#   - data/meteo_history_backup.csv recommandé mais pas obligatoire (accélère le
#     remplissage si présent, sinon tout passe par l'API)
#
# PARAMÈTRES D'ENTRÉE (à fournir ou changer si besoin) :
#   - n_days_local (ci-dessous) : combien de jours d'historique on veut vérifier/recharger.
#   - Le reste vient de config.R (db_table_admin, admin_dep, admin_level, roi_bbox,
#     n_days_forecast, openmeteo_model, identifiants BD, db_table_meteo).
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   coords (grid de points météo), table_exists, sites_absents, sites_incomplets,
#   sites_a_telecharger, meteo_to_write, meteo_future (forecast).
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   - config.R : tous les paramètres listés ci-dessus.
#   - 00_functions.R : get_weather_history_batch(), get_weather_forecast_batch().
#   - data/meteo_history_backup.csv : généré par le script 01_initialisation.R du
#     projet principal (optionnel).
# ============================================================

library(here)
library(sf)
library(dplyr)
library(purrr)
library(data.table)
library(DBI)
library(RPostgres)

source(here("scripts", "00_functions.R"))
source(here("config.R"))

# new (proposition CSV init) ====
# Nombre de jours d'historique à recharger depuis le backup CSV
# (indépendant de n_days_history, qui sert au téléchargement complet via API)
n_days_local <- 365
# ==============

path_coords_grid <- here("data", "coords_grid.csv")
path_meteo_csv   <- here("data", "meteo_history_backup.csv")
path_models      <- here("models")
grid_res         <- 0.05

dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
dir.create(path_models, showWarnings = FALSE)

# ============================================================
# Connexion BD
# ============================================================
con <- dbConnect(
  RPostgres::Postgres(),
  host     = db_host,
  dbname   = db_name,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# new (colonne is_forecast — fraîcheur du remplacement historique) ====
# Garantit que la table météo a la colonne is_forecast avant toute écriture
# (voir ensure_is_forecast_column() dans 00_functions.R pour le détail).
ensure_is_forecast_column(con, db_table_meteo)
# ==============

# ============================================================
# 1. ROI et création du grid (identique à 01_initialisation.R)
# ============================================================
cat("Chargement du ROI depuis la BD...\n")
# new (ROI depuis BD — idée différée du punteo, maintenant validée) ====
roi <- sf::st_read(con, db_table_admin) %>%
  dplyr::filter(dep == admin_dep, level == admin_level)
roi <- st_transform(roi, 4326)
# ==============

sf::sf_use_s2(FALSE)
geopolygon <- st_union(st_make_valid(roi))
sf::sf_use_s2(TRUE)

bbox <- if (!is.null(roi_bbox)) {
  st_bbox(roi_bbox, crs = 4326)
} else {
  st_bbox(geopolygon)
}

cat("Création du grid (résolution", grid_res, "°)...\n")
grid      <- st_make_grid(st_as_sfc(bbox), cellsize = grid_res,
                           square = TRUE, what = "polygons")
grid_sf   <- st_sf(geometry = grid)
centroids <- st_centroid(grid_sf)

sf::sf_use_s2(FALSE)
centroids <- st_intersection(centroids, geopolygon) %>% dplyr::select(geometry)
sf::sf_use_s2(TRUE)

coords      <- st_coordinates(centroids)
coords      <- round(as.data.frame(coords), 3)
coords$site <- seq_len(nrow(coords))

write.csv(coords, path_coords_grid, row.names = FALSE)
cat("Grid créé :", nrow(coords), "points.\n")

# Reprend la même logique de batching que 01_initialisation.R
make_meteo_prep <- function(coords_df, n_days) {
  coords_per_batch <- max(1, min(100, floor(20000 / n_days)))
  coords_df %>%
    group_by(row_number() %/% coords_per_batch) %>%
    group_map(~.x) %>%
    map(., ~group_split(., site))
}

# ============================================================
# 2. Historique météo — vérifie l'état RÉEL de la BD, complète seulement ce qui
#    manque (jamais d'overwrite — ne touche pas aux données déjà en BD, par
#    exemple si elle contient déjà plusieurs années chargées manuellement)
# ============================================================
cutoff_date     <- Sys.Date() - n_days_local
expected_dates  <- n_days_local * 0.80
all_sites_chr   <- as.character(coords$site)

# new (vérification BD avant tout — création si absente, complément si incomplète) ====
# table_exists : QUOI = booléen. FAIT = teste si db_table_meteo existe ET contient
#   au moins une ligne. Si FALSE → premier lancement, rien en BD, tout est à
#   télécharger. Si TRUE → on regarde le détail par site (sites_absents/incomplets)
#   au lieu de tout retélécharger.
table_exists <- dbExistsTable(con, db_table_meteo) &&
  dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo))[[1]] > 0

if (!table_exists) {
  cat("Table BD '", db_table_meteo, "' absente ou vide — création initiale ",
      "(", n_days_local, " jours, tous les sites)...\n", sep = "")
  sites_absents    <- all_sites_chr
  sites_incomplets <- character(0)

} else {
  cat("Table BD '", db_table_meteo, "' déjà présente — vérification des données ",
      "manquantes (par site, sur les", n_days_local, "derniers jours)...\n", sep = "")

  # site_check : QUOI = nb de dates uniques par site, SUR LA FENÊTRE [cutoff_date,
  #   aujourd'hui-1] uniquement — une table contenant 10 ans de données passera ce
  #   test sans problème (largement au-dessus du seuil), donc rien ne sera modifié.
  site_check <- dbGetQuery(con, sprintf(
    "SELECT site::text, COUNT(DISTINCT date) AS n_dates FROM %s WHERE date >= '%s' GROUP BY site",
    db_table_meteo, as.character(cutoff_date)
  ))

  sites_in_db      <- site_check$site
  sites_absents    <- setdiff(all_sites_chr, sites_in_db)
  sites_incomplets <- as.character(site_check$site[site_check$n_dates < expected_dates])

  cat("  → Sites complets (≥", round(expected_dates), "dates uniques) :",
      sum(site_check$n_dates >= expected_dates), "\n")
  cat("  → Sites incomplets :", length(sites_incomplets), "\n")
  cat("  → Sites absents    :", length(sites_absents), "\n")
}

sites_a_telecharger <- union(sites_absents, sites_incomplets)

if (length(sites_a_telecharger) == 0) {

  cat("✓ Tous les sites sont déjà complets en BD — rien à télécharger\n")

} else {

  cat(length(sites_a_telecharger), "site(s) à compléter (",
      as.character(cutoff_date), "→", as.character(Sys.Date() - 1), ")...\n")

  # Sites incomplets : on supprime leurs données partielles sur cette fenêtre avant
  # de réinsérer une version complète (évite de mélanger doublons et lacunes).
  # Les sites simplement absents n'ont rien à supprimer.
  if (table_exists && length(sites_incomplets) > 0) {
    sites_sql <- paste(sites_incomplets, collapse = ",")
    dbExecute(con, sprintf(
      "DELETE FROM %s WHERE site IN (%s) AND date >= '%s'",
      db_table_meteo, sites_sql, as.character(cutoff_date)
    ))
    cat("Données partielles supprimées pour", length(sites_incomplets), "site(s) incomplet(s)\n")
  }

  meteo_to_write <- data.frame()
  sites_restants <- sites_a_telecharger

  # Étape A — combler depuis le backup CSV local si disponible (rapide, pas d'API)
  if (file.exists(path_meteo_csv)) {
    cat("Tentative de complétion depuis le backup CSV :", path_meteo_csv, "...\n")
    meteo_csv <- read.csv(path_meteo_csv) %>% mutate(date = as.Date(date))

    meteo_from_csv <- meteo_csv %>%
      filter(date >= cutoff_date, as.character(site) %in% sites_a_telecharger)

    if (nrow(meteo_from_csv) > 0) {
      csv_check <- meteo_from_csv %>%
        group_by(site) %>%
        summarise(n_dates = n_distinct(date), .groups = "drop")

      sites_combles_csv <- as.character(csv_check$site[csv_check$n_dates >= expected_dates])
      meteo_from_csv     <- meteo_from_csv %>% filter(as.character(site) %in% sites_combles_csv)
      sites_restants     <- setdiff(sites_a_telecharger, sites_combles_csv)

      meteo_to_write <- bind_rows(meteo_to_write, meteo_from_csv)
      cat("✓", length(sites_combles_csv), "site(s) complété(s) depuis le CSV (",
          nrow(meteo_from_csv), "lignes)\n")
    }
  }

  # Étape B — pour ce qui reste (absent du CSV, ou CSV introuvable/insuffisant),
  # télécharger via l'API Open-Meteo, UNIQUEMENT pour ces sites-là.
  if (length(sites_restants) > 0) {
    cat(length(sites_restants), "site(s) restant(s) à télécharger via l'API...\n")

    coords_restants     <- coords %>% filter(as.character(site) %in% sites_restants)
    meteo_prep_restants <- make_meteo_prep(coords_restants, n_days_local)
    meteo_api           <- data.frame()

    for (i in seq_along(meteo_prep_restants)) {
      cat("Sites restants — paquet", i, "sur", length(meteo_prep_restants), "\n")

      batch_df   <- dplyr::bind_rows(meteo_prep_restants[[i]])
      th_res_api <- get_weather_history_batch(
        latitudes  = batch_df$Y,
        longitudes = batch_df$X,
        start_date = cutoff_date,
        end_date   = Sys.Date() - 1,
        model      = openmeteo_model
      )

      th_res <- th_res_api %>%
        dplyr::left_join(unique(batch_df[, c("X", "Y", "site")]),
                         by = c("longitude" = "X", "latitude" = "Y")) %>%
        dplyr::rename(date = time) %>%
        dplyr::mutate(date = as.Date(as.character(date)), X = longitude, Y = latitude)

      meteo_api <- rbind(meteo_api, th_res)
      Sys.sleep(60)
    }

    meteo_to_write <- bind_rows(meteo_to_write, meteo_api)
    cat("✓", nrow(meteo_api), "lignes téléchargées via l'API\n")
  }

  # Écriture en BD — TOUJOURS en append, JAMAIS en overwrite : on ne touche qu'aux
  # sites identifiés comme manquants/incomplets, le reste de la table (ex. les
  # 10 ans déjà chargés pour les autres sites) reste intact.
  if (nrow(meteo_to_write) > 0) {
    # is_forecast = FALSE : ce sont des valeurs HISTORIQUES réelles (CSV backup
    # ou API archive), jamais des prévisions — voir ensure_is_forecast_column().
    meteo_to_write$is_forecast <- FALSE
    dbWriteTable(con, db_table_meteo, as.data.frame(meteo_to_write),
                  append = TRUE, row.names = FALSE)
    cat("✓", nrow(meteo_to_write), "lignes ajoutées à la table BD", db_table_meteo, "\n")
  }
}
# ==============

# ============================================================
# 3. Forecast — téléchargement via API (léger, n_days_forecast jours), seulement
#    si la BD ne couvre pas déjà toute la fenêtre à venir
# ============================================================

# new (vérification fraîcheur forecast — éviter les téléchargements inutiles) ====
forecast_check <- dbGetQuery(con, sprintf(
  "SELECT site::text, COUNT(DISTINCT date) AS n_dates FROM %s WHERE date >= '%s' GROUP BY site",
  db_table_meteo, as.character(Sys.Date())
))
sites_forecast_ok <- forecast_check$site[forecast_check$n_dates >= n_days_forecast]
forecast_needed   <- !all(as.character(coords$site) %in% sites_forecast_ok)

meteo_future <- data.frame()

if (forecast_needed) {
  cat("Téléchargement du forecast...\n")

  meteo_prep_forecast <- make_meteo_prep(coords, n_days_forecast)

  for (i in seq_along(meteo_prep_forecast)) {
    cat("Forecast — paquet", i, "sur", length(meteo_prep_forecast), "\n")

    batch_df   <- dplyr::bind_rows(meteo_prep_forecast[[i]])
    th_res_api <- get_weather_forecast_batch(
      latitudes  = batch_df$Y,
      longitudes = batch_df$X,
      n_days     = n_days_forecast,
      model      = openmeteo_model
    )

    th_res <- th_res_api %>%
      dplyr::left_join(unique(batch_df[, c("X", "Y", "site")]),
                       by = c("longitude" = "X", "latitude" = "Y")) %>%
      dplyr::rename(date = time) %>%
      dplyr::mutate(date = as.Date(as.character(date)), X = longitude, Y = latitude)

    meteo_future <- rbind(meteo_future, th_res)
    Sys.sleep(60)
  }

  # Supprime l'ancien forecast (s'il y en avait un partiel) avant d'insérer le
  # nouveau, pour éviter d'accumuler des doublons sur les dates futures.
  dbExecute(con, sprintf("DELETE FROM %s WHERE date >= '%s'", db_table_meteo, as.character(Sys.Date())))
  # is_forecast = TRUE : prévisions, à remplacer par les vraies valeurs une fois
  # la date passée (voir Étape 1 de 02_hebdomadaire.R).
  meteo_future$is_forecast <- TRUE
  dbWriteTable(con, db_table_meteo, as.data.frame(meteo_future),
                append = TRUE, row.names = FALSE)
} else {
  cat("✓ Forecast déjà à jour en BD pour les", n_days_forecast,
      "jours à venir — téléchargement ignoré\n")
}
# ==============

# new (fix affichage COUNT(*) — integer64) ====
# Voir 01_initialisation.R pour le détail : COUNT(*) revient en integer64, cat()
# l'affiche mal sans as.numeric().
n_total <- as.numeric(dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo))[[1]])
# ==============
cat("✓ Table", db_table_meteo, "—", n_total, "lignes au total\n")

# ============================================================
# 4. Vérification des modèles entraînés
# ============================================================
rds_files <- c(
  "res_presence_LOSO_probabilistic.rds",
  "res_abundance_LOSO_quantile_rf.rds",
  "res_training_data.rds"
)

missing_rds <- rds_files[!file.exists(file.path(path_models, rds_files))]
if (length(missing_rds) > 0) {
  stop("Modèles manquants dans ", path_models, " :\n",
       paste(" -", missing_rds, collapse = "\n"),
       "\nExécuter d'abord : source('scripts/00_train_models.R')")
}
cat("✓ Modèles entraînés détectés :", length(rds_files), "fichiers RDS\n")

dbDisconnect(con)
cat("\n✓ Initialisation (depuis CSV) terminée. Vous pouvez maintenant lancer 02_hebdomadaire.R\n")
