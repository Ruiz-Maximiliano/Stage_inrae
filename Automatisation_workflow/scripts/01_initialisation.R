# ============================================================
# SCRIPT 1 — Initialisation
# À exécuter UNE SEULE FOIS avant de lancer le pipeline hebdomadaire
#
# Prérequis :
#   - Avoir exécuté 00_train_models.R au préalable (modèles .rds)
#   - Table BD db_table_admin accessible (limites administratives, voir config.R)
#
# Génère :
#   - data/meteo_history_backup.csv (backup local brut par point de grille)
#   - Table BD <db_table_meteo_grid> (historique + forecast météo, format grille brut)
#
# CE QUE FAIT CE CODE :
#   1. Charge le ROI (limites administratives) depuis la BD et construit un grid
#      de points météo réguliers à l'intérieur de cette zone.
#   2. Charge l'historique météo, EN FORMAT BRUT (X, Y, date, TM, RR, UM,
#      is_forecast) — sans agrégation par commune à l'écriture :
#      - Si data/meteo_history_backup.csv existe : relit le CSV, écrit en BD
#        année par année (voir aussi scripts/08_seed_meteo_grid.R pour une
#        siembra initiale dédiée, plus rapide, hors de ce script).
#      - Sinon : télécharge via l'API Open-Meteo par semestre, écrit en BD.
#   3. Télécharge un forecast initial (n_days_forecast jours), écrit en BD brut.
#   4. Vérifie que les modèles entraînés (00_train_models.R) existent.
#
#   Nouveau schéma BD (point 2, demande Paul) : (X, Y, date, TM, RR, UM, is_forecast)
#   — retour au format brut par point de grille, dans une table SÉPARÉE
#   (db_table_meteo_grid), agrégée à la volée par aggregate_meteo_to_roi() au
#   moment de la LECTURE dans 02_hebdomadaire.R (plus à l'écriture). L'ancienne
#   table par commune (db_table_meteo, meteo_ruiz) N'EST PLUS ÉCRITE par ce
#   script — elle reste intacte comme archive au format commune.
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Tous viennent de config.R : db_table_admin, admin_dep, admin_level, roi_bbox,
#   n_days_history, n_days_forecast, openmeteo_model, db_host/name/port/user/password,
#   db_table_meteo_grid. Rien à fournir directement dans CE script.
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   roi, geopolygon, coords — voir le commentaire au-dessus de chaque variable.
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   - config.R : tous les paramètres listés ci-dessus.
#   - 00_functions.R : make_grid(), get_weather_history_batch(),
#     get_weather_forecast_batch().
# ============================================================

library(here)
library(terra)
library(sf)
library(purrr)
library(furrr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)
library(DBI)
library(RPostgres)
library(exactextractr)
# new: logs =======
library(logr)
# ==============

source(here("scripts", "00_functions.R"))
source(here("config.R"))

# fix (bug variables globales collées entre sessions) : ce script fixe lui-même
# init_lookback / skip_shap / force_recompute / init_forecast_done pour ses
# deux runs (voir section 5 plus bas) — mais si un script de backfill par
# tranches (06_backfill_shap_por_tramos.R) a été interrompu AVANT son propre
# rm() final, des variables comme backfill_start_date / backfill_end_date
# restent dans la session R. 02_hebdomadaire.R les détecte et leur donne la
# PRIORITÉ sur init_lookback (voir .backfill_plage_fixe dans ce script) —
# sans savoir que ce n'est pas ce script-ci qui les a définies. Résultat vécu
# en pratique : initialisation.R a silencieusement traité une fenêtre de 2 ans
# laissée par une tentative de backfill précédente, au lieu de sa propre
# fenêtre (10 ans sans SHAP + 14 jours avec SHAP). On nettoie donc ici, avant
# de commencer, toute variable "optionnelle" que ce script ne contrôle pas
# lui-même — pour un comportement prévisible à chaque exécution, peu importe
# ce qu'une session R précédente a laissé traîner.
.vars_a_nettoyer <- c("backfill_start_date", "backfill_end_date",
                       "init_lookback", "skip_shap",
                       "force_recompute", "init_forecast_done",
                       "shap_max_background", "shap_batch_size")
rm(list = intersect(.vars_a_nettoyer, ls(envir = .GlobalEnv)), envir = .GlobalEnv)
rm(.vars_a_nettoyer)

# ============================================================
# Paramètres locaux (lus depuis config.R)
# ============================================================
# grid_res, path_models, path_backup, n_days_forecast définis dans config.R

dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
dir.create(path_models, showWarnings = FALSE)
# new: logs =======
dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)
lf <- log_open(
  here("logs", paste0("initialisation_", Sys.Date(), ".log")),
  autolog    = TRUE,
  show_notes = FALSE
)
log_print(paste("=== Run initialisation —", Sys.time(), "==="))
log_print(paste("Table météo (grille) :", db_table_meteo_grid,
                "| n_days_history :", n_days_history,
                "| n_days_forecast :", n_days_forecast))
# ==============

# ============================================================
# Connexion à la base de données
# ============================================================
con <- dbConnect(
  RPostgres::Postgres(),
  host     = db_host,
  dbname   = db_name,
  port     = db_port,
  user     = db_user,
  password = db_password
)
# Désactiver le timeout serveur pour les COPY longues (chargement historique 10 ans)
dbExecute(con, "SET statement_timeout = 0")

# new (colonne is_forecast — fraîcheur du remplacement historique) ====
# new (point 2 — table grille) : db_table_meteo n'est plus écrite par ce script,
# seule db_table_meteo_grid en a besoin désormais.
ensure_is_forecast_column(con, db_table_meteo_grid)
# ==============

# ============================================================
# 1. Chargement du ROI et création du grid
# ============================================================

cat("Chargement du ROI depuis la BD...\n")

# new (ROI depuis BD) ====
roi <- sf::st_read(con, db_table_admin) %>%
  dplyr::filter(dep == admin_dep, level == admin_level)
roi <- st_transform(roi, 4326)
# ==============

# #new (8 - Renommage ROI) ====
# sf_use_s2(FALSE) requis : st_union/st_intersection échouent sur certaines géométries ROI
# avec s2 activé (erreur "format non supporté"). Les messages "Spherical geometry switched
# off/on" et "assumes planar" sont normaux et attendus ici.
sf::sf_use_s2(FALSE)
geopolygon <- st_union(st_make_valid(roi))
sf::sf_use_s2(TRUE)
log_print("sf_use_s2 désactivé temporairement pour st_union/st_make_valid (comportement attendu)")
# ==============

# roi_info : data.frame codgeo/libgeo sans géométrie, pour les jointures
roi_info   <- sf::st_drop_geometry(roi) %>% dplyr::select(codgeo, libgeo)
all_codgeo <- as.character(unique(roi$codgeo))

cat("Création du grid (résolution", grid_res, "°)...\n")

# coords : QUOI = data.frame X/Y/site créé en mémoire par make_grid() (00_functions.R).
coords <- make_grid(geopolygon, roi_bbox, grid_res)
cat("Grid créé :", nrow(coords), "points\n")
# new: logs =======
log_print(paste("Grid créé :", nrow(coords), "points (résolution", grid_res, "°)"))
# ==============

# Fonction utilitaire : crée les batches de coordonnées selon la durée demandée
make_meteo_prep <- function(coords_df, n_days) {
  coords_per_batch <- max(1, min(100, floor(20000 / n_days)))
  cat("Taille des batches :", coords_per_batch, "coords ×", n_days, "jours =",
      coords_per_batch * n_days, "points/requête\n")
  coords_df %>%
    group_by(row_number() %/% coords_per_batch) %>%
    group_map(~.x) %>%
    map(., ~group_split(., site))
}

# ============================================================
# 2. Chargement de l'historique météo → BD (format grille brut, point 2)
# ============================================================

cat("Chargement de l'historique météo (", n_days_history, "jours)...\n")

start_date     <- Sys.Date() - n_days_history
end_date       <- Sys.Date() - 1
expected_dates <- n_days_history * 0.80
phase_needed   <- FALSE

# new (point 2 — fraîcheur vérifiée par point de grille (X, Y), plus par codgeo) ====
# db_table_meteo_grid est censée être créée soit par ce script, soit par
# scripts/08_seed_meteo_grid.R (siembra rapide depuis le backup CSV) — les deux
# écrivent le même schéma (X, Y, date, TM, RR, UM, is_forecast), donc ce test
# fonctionne quelle que soit l'origine des données déjà présentes.
if (dbExistsTable(con, db_table_meteo_grid) &&
    dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo_grid))[[1]] > 0) {

  # fix : "X"/"Y" entre guillemets doubles — Postgres replie les identifiants non
  # quotés en minuscules (x/y), alors que dbWriteTable() a créé les colonnes en
  # conservant la casse R (même sujet que "shap_TM" ailleurs dans le pipeline).
  grid_check <- dbGetQuery(con, sprintf(
    'SELECT "X", "Y", COUNT(DISTINCT date) AS n_dates FROM %s WHERE NOT is_forecast GROUP BY "X", "Y"',
    db_table_meteo_grid
  ))
  grid_ok         <- grid_check[grid_check$n_dates >= expected_dates, ]
  # Points incomplets = présents en BD mais avec trop peu de dates
  grid_incomplets <- grid_check[grid_check$n_dates < expected_dates, ]

  cat("BD actuelle :", nrow(grid_check), "points de grille\n")
  cat("  → Complets (≥", round(expected_dates), "dates) :", nrow(grid_ok), "\n")
  cat("  → Incomplets (présents mais données insuffisantes) :", nrow(grid_incomplets), "\n")
  # new: logs =======
  log_print(paste("BD existante — points complets :", nrow(grid_ok),
                  "| incomplets :", nrow(grid_incomplets)))
  # ==============

  if (nrow(grid_incomplets) > 0) {
    # Supprimer les points incomplets pour re-charger proprement (clé composite X, Y
    # — pas de colonne id unique simple comme codgeo, donc une DELETE par point)
    for (i in seq_len(nrow(grid_incomplets))) {
      dbExecute(con, sprintf(
        'DELETE FROM %s WHERE "X" = %f AND "Y" = %f AND NOT is_forecast',
        db_table_meteo_grid, grid_incomplets$X[i], grid_incomplets$Y[i]
      ))
    }
    phase_needed <- TRUE
  }

} else {
  phase_needed <- TRUE
  cat("BD vide — chargement complet\n")
  # new: logs =======
  log_print("BD vide — chargement complet")
  # ==============
}
# ==============

if (phase_needed) {

  # ---- Chemin 1 : depuis le backup CSV (rapide, pas d'appel API) ----
  if (file.exists(path_backup)) {
    cat("Backup CSV trouvé — chargement (format grille brut, sans agrégation)...\n")

    # Lecture du backup (format brut par point de grille) — écrit TEL QUEL,
    # aggregate_meteo_to_roi() n'est plus appelée ici (point 2 : elle se
    # déplace côté lecture, dans 02_hebdomadaire.R)
    backup <- read.csv(path_backup) %>%
      dplyr::rename(
        TM = temperature_2m_mean,
        RR = precipitation_sum,
        UM = relative_humidity_2m_mean
      ) %>%
      dplyr::mutate(date = as.Date(date), is_forecast = FALSE) %>%
      dplyr::select(X, Y, date, TM, RR, UM, is_forecast) %>%
      dplyr::filter(date >= start_date)

    years <- sort(unique(format(backup$date, "%Y")))
    cat("Années à traiter :", paste(years, collapse = ", "), "\n")

    for (yr in years) {
      cat("Écriture (format grille) —", yr, "...\n")
      yr_data <- backup %>% dplyr::filter(format(date, "%Y") == yr)
      dbWriteTable(con, db_table_meteo_grid, as.data.frame(yr_data),
                   append = TRUE, row.names = FALSE)
      # new: logs =======
      log_print(paste("Année", yr, ":", nrow(yr_data), "lignes écrites"))
      # ==============
    }
    cat("✓ Historique chargé depuis backup CSV (format grille)\n")

  # ---- Chemin 2 : téléchargement API (fallback si pas de backup) ----
  } else {
    cat("Backup CSV absent — téléchargement via l'API Open-Meteo par semestre...\n")

    # Découpe en périodes de 6 mois pour limiter la mémoire par lot
    period_starts <- seq(start_date, end_date, by = "6 months")
    period_ends   <- c(period_starts[-1] - 1, end_date)

    for (p in seq_along(period_starts)) {
      p_start <- period_starts[p]
      p_end   <- period_ends[p]
      n_days_p <- as.numeric(p_end - p_start) + 1

      cat("\n--- Période", as.character(p_start), "→", as.character(p_end),
          "|", p, "/", length(period_starts), "---\n")

      meteo_prep_p <- make_meteo_prep(coords, n_days_p)
      raw_period   <- data.frame()

      for (i in seq_along(meteo_prep_p)) {
        cat("Batch", i, "/", length(meteo_prep_p), "\n")
        batch_df   <- dplyr::bind_rows(meteo_prep_p[[i]])
        th_res_api <- get_weather_history_batch(
          latitudes  = batch_df$Y,
          longitudes = batch_df$X,
          start_date = p_start,
          end_date   = p_end,
          model      = openmeteo_model
        )
        th_res <- th_res_api %>%
          dplyr::left_join(unique(batch_df[, c("X", "Y", "site")]),
                           by = c("longitude" = "X", "latitude" = "Y")) %>%
          dplyr::rename(date = time,
                        TM   = temperature_2m_mean,
                        RR   = precipitation_sum,
                        UM   = relative_humidity_2m_mean) %>%
          dplyr::mutate(date = as.Date(as.character(date)),
                        X = longitude, Y = latitude)
        raw_period <- rbind(raw_period, th_res)
        Sys.sleep(5)
      }

      # new (point 2) : écriture BRUTE (X, Y), plus d'agrégation par commune ici
      raw_period$is_forecast <- FALSE
      grid_data <- raw_period %>% dplyr::select(X, Y, date, TM, RR, UM, is_forecast)
      dbWriteTable(con, db_table_meteo_grid, as.data.frame(grid_data),
                   append = TRUE, row.names = FALSE)
      # new: logs =======
      log_print(paste("Période", as.character(p_start), "→", as.character(p_end),
                      ":", nrow(grid_data), "lignes écrites"))
      # ==============
      Sys.sleep(60)
    }
    cat("✓ Historique téléchargé (format grille)\n")
  }
}

# ---- Vérification de la continuité de l'historique ----
cat("Vérification des lacunes dans l'historique météo...\n")
dates_bd <- as.Date(dbGetQuery(con, sprintf(
  "SELECT DISTINCT date FROM %s WHERE is_forecast = FALSE ORDER BY date",
  db_table_meteo_grid
))$date)

if (length(dates_bd) > 1) {
  dates_attendues  <- seq(min(dates_bd), max(dates_bd), by = "day")
  dates_manquantes <- dates_attendues[!dates_attendues %in% dates_bd]

  if (length(dates_manquantes) == 0) {
    cat("✓ Historique continu — aucune lacune détectée\n")
    log_print("✓ Historique continu — aucune lacune")
  } else {
    # Regrouper en périodes consécutives
    groupes <- split(dates_manquantes, cumsum(c(1, diff(dates_manquantes) > 1)))
    cat("⚠ LACUNES DÉTECTÉES dans l'historique météo :\n")
    for (g in groupes) {
      if (length(g) == 1) {
        cat("  -", format(g), "\n")
      } else {
        cat("  -", format(min(g)), "→", format(max(g)),
            "(", length(g), "jours)\n")
      }
    }
    cat("  Total :", length(dates_manquantes), "jours manquants\n")
    log_print(paste("⚠ Lacunes historique :", length(dates_manquantes),
                    "jours manquants — relancer l'initialisation ou compléter via l'API"))
  }
} else {
  cat("⚠ Historique vide ou insuffisant\n")
}
# ==============

# ============================================================
# 3. Téléchargement du forecast initial
# ============================================================

# new (vérification fraîcheur forecast) ====
# new (point 2 — fraîcheur vérifiée par point de grille, plus par codgeo) : on
# compte combien de points de coords ont au moins n_days_forecast dates de
# forecast déjà en BD — si tous les points téléchargés (coords) sont couverts,
# pas besoin de retélécharger.
forecast_needed <- TRUE

if (dbExistsTable(con, db_table_meteo_grid)) {
  # fix : "X"/"Y" quotés (voir plus haut) — sinon Postgres cherche des colonnes x/y minuscules
  n_points_ok <- dbGetQuery(con, sprintf(
    'SELECT COUNT(*) AS n FROM (SELECT "X", "Y" FROM %s WHERE date >= \'%s\' GROUP BY "X", "Y" HAVING COUNT(DISTINCT date) >= %d) sub',
    db_table_meteo_grid, as.character(Sys.Date()), n_days_forecast
  ))$n
  if (n_points_ok >= nrow(coords)) forecast_needed <- FALSE
}

meteo_future <- data.frame()

if (forecast_needed) {
  cat("Téléchargement du forecast initial...\n")

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
      dplyr::rename(date = time,
                    TM   = temperature_2m_mean,
                    RR   = precipitation_sum,
                    UM   = relative_humidity_2m_mean) %>%
      dplyr::mutate(date = as.Date(as.character(date)),
                    X = longitude, Y = latitude)
    meteo_future <- rbind(meteo_future, th_res)
    Sys.sleep(60)
  }
} else {
  cat("✓ Forecast déjà à jour en BD pour les", n_days_forecast,
      "jours à venir — téléchargement ignoré\n")
}

if (nrow(meteo_future) > 0) {
  # new (point 2) : écriture BRUTE (X, Y), plus d'agrégation par commune ici
  meteo_future$is_forecast <- TRUE
  meteo_future_grid <- meteo_future %>% dplyr::select(X, Y, date, TM, RR, UM, is_forecast)
  dbWriteTable(con, db_table_meteo_grid, as.data.frame(meteo_future_grid),
               append = TRUE, row.names = FALSE)
}
# ==============

n_total <- as.integer(dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo_grid))[[1]])
cat("✓ Météo initiale sauvegardée dans la table BD :", db_table_meteo_grid,
    "(", n_total, "lignes au total)\n")
# new: logs =======
log_print(paste("✓ Table météo BD :", db_table_meteo_grid, "|", n_total, "lignes au total",
                "| forecast téléchargé :", forecast_needed))
# ==============

# new (backup CSV brut — archive des données par point de grille) ====
# Le backup est conservé en format BRUT (site/X/Y/temperature_2m_mean/...)
# — c'est maintenant le MÊME format que db_table_meteo_grid (voir point 2),
# donc ce backup reste utile comme fallback/re-siembra (scripts/08_seed_meteo_grid.R).
if (!file.exists(path_backup) && dbExistsTable(con, db_table_meteo_grid)) {
  cat("Pas de backup CSV — génération ignorée (à régénérer manuellement si besoin,",
      "voir path_backup dans config.R).\n")
}
# ==============

# ============================================================
# 4. Vérification des modèles entraînés
# ============================================================

# #new (3 - Séparer entraînement) ====
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
# new: logs =======
log_print(paste("✓ Modèles détectés :", paste(rds_files, collapse = ", ")))
# ==============

# Récupérer la date min de meteo_ruiz_grid avant de fermer la connexion
# (nécessaire pour calculer init_lookback dans la section 5 ci-dessous)
meteo_min_date <- as.Date(dbGetQuery(con, sprintf(
  "SELECT MIN(date) FROM %s", db_table_meteo_grid))[[1]])

dbDisconnect(con)
cat("\n✓ Météo initialisée. Lancement du pipeline de prédictions...\n")
# new: logs =======
log_print(paste("=== Fin du run initialisation météo —", Sys.time(), "==="))
log_close()
# ==============

# ============================================================
# 5. Génération des prédictions initiales
# ============================================================
# Deux runs de 02_hebdomadaire.R :
#
# Run 1 — historique complet, SHAP désactivé :
#   init_lookback = nbre de jours depuis le début de meteo_ruiz → tous les lundis
#   skip_shap = TRUE → SHAP = NA (trop lent sur des années de données)
#   force_recompute = TRUE → ne pas sauter même si meteo_changed = FALSE
#
# Run 2 — semaines récentes avec SHAP :
#   fenêtre normale (n_days_forecast jours en arrière)
#   SHAP calculé normalement → écrase les semaines récentes avec valeurs SHAP
cat("--- Run 1/2 : prédictions historiques (SHAP = NA) ---\n")
init_lookback  <- as.integer(Sys.Date() - meteo_min_date) + 1
skip_shap      <- TRUE
force_recompute <- TRUE
init_forecast_done <- TRUE
source(here("scripts", "02_hebdomadaire.R"))
rm(init_lookback, skip_shap, force_recompute, init_forecast_done)

cat("--- Run 2/2 : prédictions récentes avec SHAP ---\n")
force_recompute    <- TRUE
init_forecast_done <- TRUE
source(here("scripts", "02_hebdomadaire.R"))
rm(force_recompute, init_forecast_done)
