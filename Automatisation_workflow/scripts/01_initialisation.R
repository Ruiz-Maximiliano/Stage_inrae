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
#   - Table BD <db_table_meteo>    (historique + forecast météo, schéma commune)
#
# CE QUE FAIT CE CODE :
#   1. Charge le ROI (limites administratives) depuis la BD et construit un grid
#      de points météo réguliers à l'intérieur de cette zone.
#   2. Charge l'historique météo :
#      - Si data/meteo_history_backup.csv existe : relit le CSV, agrège par commune
#        (rasterize_to_communes), écrit en BD année par année.
#      - Sinon : télécharge via l'API Open-Meteo par semestre, agrège, écrit en BD.
#   3. Télécharge un forecast initial (n_days_forecast jours), agrège, écrit en BD.
#   4. Vérifie que les modèles entraînés (00_train_models.R) existent.
#
#   Nouveau schéma BD : (codgeo, date, TM, RR, UM, is_forecast)
#   Plus de colonnes site/X/Y — la météo est directement au niveau commune.
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Tous viennent de config.R : db_table_admin, admin_dep, admin_level, roi_bbox,
#   n_days_history, n_days_forecast, openmeteo_model, db_host/name/port/user/password,
#   db_table_meteo. Rien à fournir directement dans CE script.
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   roi, geopolygon, coords — voir le commentaire au-dessus de chaque variable.
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   - config.R : tous les paramètres listés ci-dessus.
#   - 00_functions.R : make_grid(), get_weather_history_batch(),
#     get_weather_forecast_batch(), aggregate_meteo_to_communes().
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

# ============================================================
# Paramètres locaux
# ============================================================
path_models  <- here("models")
path_backup  <- here("data", "meteo_history_backup.csv")
grid_res     <- 0.05

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
log_print(paste("Table météo :", db_table_meteo,
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

# new (colonne is_forecast — fraîcheur du remplacement historique) ====
ensure_is_forecast_column(con, db_table_meteo)
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
# fix warning : "Spherical geometry switched off/on" et "assumes planar" — messages cosmétiques supprimés
suppressMessages({
  sf::sf_use_s2(FALSE)
  geopolygon <- st_union(st_make_valid(roi))
  sf::sf_use_s2(TRUE)
})
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
# 2. Chargement de l'historique météo → BD (schéma commune)
# ============================================================

cat("Chargement de l'historique météo (", n_days_history, "jours)...\n")

start_date     <- Sys.Date() - n_days_history
end_date       <- Sys.Date() - 1
expected_dates <- n_days_history * 0.80
phase_needed   <- FALSE

# Vérifier ce qui est déjà dans la BD (par codgeo)
# Si la table existe mais a l'ancien schéma (site/X/Y), la supprimer
if (dbExistsTable(con, db_table_meteo)) {
  cols_existantes <- dbGetQuery(con, sprintf(
    "SELECT column_name FROM information_schema.columns WHERE table_name = '%s'",
    db_table_meteo
  ))$column_name
  if (!"codgeo" %in% cols_existantes) {
    cat("Table existante avec ancien schéma (site/X/Y) — suppression pour recréation...\n")
    log_print("Ancien schéma détecté — DROP TABLE avant recréation")
    dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", db_table_meteo))
  }
}

if (dbExistsTable(con, db_table_meteo) &&
    dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo))[[1]] > 0) {

  codgeo_check <- dbGetQuery(con, sprintf(
    "SELECT codgeo::text, COUNT(DISTINCT date) AS n_dates FROM %s WHERE NOT is_forecast GROUP BY codgeo",
    db_table_meteo
  ))
  codgeo_ok        <- codgeo_check$codgeo[codgeo_check$n_dates >= expected_dates]
  codgeo_incomplets <- setdiff(all_codgeo, codgeo_ok)

  cat("BD actuelle :", length(codgeo_check$codgeo), "communes\n")
  cat("  → Complètes (≥", round(expected_dates), "dates) :", length(codgeo_ok), "\n")
  cat("  → Incomplètes/absentes :", length(codgeo_incomplets), "\n")
  # new: logs =======
  log_print(paste("BD existante — communes complètes :", length(codgeo_ok),
                  "| incomplètes/absentes :", length(codgeo_incomplets)))
  # ==============

  if (length(codgeo_incomplets) > 0) {
    # Supprimer les communes incomplètes pour re-charger proprement
    codgeo_sql <- paste(paste0("'", codgeo_incomplets, "'"), collapse = ",")
    dbExecute(con, sprintf(
      "DELETE FROM %s WHERE codgeo IN (%s) AND NOT is_forecast",
      db_table_meteo, codgeo_sql
    ))
    phase_needed <- TRUE
  }

} else {
  phase_needed <- TRUE
  cat("BD vide — chargement complet\n")
  # new: logs =======
  log_print("BD vide — chargement complet")
  # ==============
}

if (phase_needed) {

  # ---- Chemin 1 : depuis le backup CSV (rapide, pas d'appel API) ----
  if (file.exists(path_backup)) {
    cat("Backup CSV trouvé — chargement et agrégation par commune...\n")

    # Lecture du backup (format brut par point de grille)
    backup <- read.csv(path_backup) %>%
      dplyr::rename(
        TM = temperature_2m_mean,
        RR = precipitation_sum,
        UM = relative_humidity_2m_mean
      ) %>%
      dplyr::mutate(date = as.Date(date)) %>%
      # fix : le backup brut n'a pas de colonne is_forecast — on filtre uniquement par date
      dplyr::filter(date >= start_date)

    years <- sort(unique(format(backup$date, "%Y")))
    cat("Années à traiter :", paste(years, collapse = ", "), "\n")

    for (yr in years) {
      cat("Agrégation par commune —", yr, "...\n")
      yr_data   <- backup %>% dplyr::filter(format(date, "%Y") == yr)
      comm_data <- aggregate_meteo_to_communes(yr_data, roi, grid_res)
      comm_data$is_forecast <- FALSE
      dbWriteTable(con, db_table_meteo, as.data.frame(comm_data),
                   append = TRUE, row.names = FALSE)
      # new: logs =======
      log_print(paste("Année", yr, ":", nrow(comm_data), "lignes écrites"))
      # ==============
    }
    cat("✓ Historique chargé depuis backup CSV\n")

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

      # Agréger par commune avant écriture BD
      comm_data <- aggregate_meteo_to_communes(raw_period, roi, grid_res)
      comm_data$is_forecast <- FALSE
      dbWriteTable(con, db_table_meteo, as.data.frame(comm_data),
                   append = TRUE, row.names = FALSE)
      # new: logs =======
      log_print(paste("Période", as.character(p_start), "→", as.character(p_end),
                      ":", nrow(comm_data), "lignes écrites"))
      # ==============
      Sys.sleep(60)
    }
    cat("✓ Historique téléchargé et agrégé par commune\n")
  }
}

# ============================================================
# 3. Téléchargement du forecast initial
# ============================================================

# new (vérification fraîcheur forecast) ====
forecast_needed <- TRUE

if (dbExistsTable(con, db_table_meteo)) {
  forecast_check    <- dbGetQuery(con, sprintf(
    "SELECT codgeo::text, COUNT(DISTINCT date) AS n_dates FROM %s WHERE date >= '%s' GROUP BY codgeo",
    db_table_meteo, as.character(Sys.Date())
  ))
  communes_ok      <- forecast_check$codgeo[forecast_check$n_dates >= n_days_forecast]
  if (all(all_codgeo %in% communes_ok)) forecast_needed <- FALSE
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
  # Agréger par commune et écrire en BD
  meteo_future_comm <- aggregate_meteo_to_communes(meteo_future, roi, grid_res)
  meteo_future_comm$is_forecast <- TRUE
  dbWriteTable(con, db_table_meteo, as.data.frame(meteo_future_comm),
               append = TRUE, row.names = FALSE)
}
# ==============

n_total <- as.integer(dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo))[[1]])
cat("✓ Météo initiale sauvegardée dans la table BD :", db_table_meteo,
    "(", n_total, "lignes au total)\n")
# new: logs =======
log_print(paste("✓ Table météo BD :", db_table_meteo, "|", n_total, "lignes au total",
                "| forecast téléchargé :", forecast_needed))
# ==============

# new (backup CSV brut — archive des données par point de grille) ====
# Le backup est conservé en format BRUT (site/X/Y/temperature_2m_mean/...)
# pour pouvoir re-migrer vers un autre schéma sans repasser par l'API.
# Il n'est mis à jour ici que si absent ou si la BD vient d'être rechargée.
if (!file.exists(path_backup) && dbExistsTable(con, db_table_meteo)) {
  cat("Pas de backup CSV — génération ignorée (la BD est en schéma commune,",
      "le backup brut doit être généré depuis les données d'origine).\n")
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

dbDisconnect(con)
cat("\n✓ Météo initialisée. Lancement du pipeline de prédictions...\n")
# new: logs =======
log_print(paste("=== Fin du run initialisation météo —", Sys.time(), "==="))
log_close()
# ==============

# ============================================================
# 5. Génération des prédictions initiales (mosquito)
# ============================================================
# Lancer hebdomadaire pour peupler les tables albopictus dès l'initialisation.
# skip_recompute sera FALSE car db_layer n'existe pas encore.
# init_forecast_done indique à hebdo de ne pas re-télécharger le forecast.
init_forecast_done <- TRUE
cat("--- Lancement de 02_hebdomadaire.R pour les prédictions initiales ---\n")
source(here("scripts", "02_hebdomadaire.R"))
rm(init_forecast_done)  # Nettoyage — ne doit pas persister dans l'environnement
