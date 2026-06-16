# ============================================================
# SCRIPT 1 — Initialisation
# À exécuter UNE SEULE FOIS avant de lancer le pipeline hebdomadaire
#
# Prérequis :
#   - Avoir exécuté 00_train_models.R au préalable (modèles .rds)
#   - data/<roi_file>   (zone d'intérêt, définie dans config.R)
#
# Génère :
#   - data/coords_grid.csv         (points du grid retenus dans le ROI)
#   - Table BD <db_table_meteo>    (historique + forecast météo)
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

source(here("scripts", "00_functions.R"))
source(here("config.R"))

# ============================================================
# Paramètres locaux
# ============================================================
path_coords_grid <- here("data", "coords_grid.csv")
path_models      <- here("models")
grid_res         <- 0.05

dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
dir.create(path_models, showWarnings = FALSE)

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

# ============================================================
# 1. Chargement du ROI et création du grid
# ============================================================

cat("Chargement du ROI...\n")

# #new (2 - Entrée unique ROI) ====
roi <- st_read(path_roi)
roi <- st_transform(roi, 4326)
# ==============

# #new (8 - Renommage ROI) ====
sf::sf_use_s2(FALSE)
geopolygon <- st_union(st_make_valid(roi))
sf::sf_use_s2(TRUE)
# ==============

cat("Création du grid (résolution", grid_res, "°)...\n")

# Bbox : utilise roi_bbox si défini dans config.R, sinon le bbox du ROI
bbox <- if (!is.null(roi_bbox)) {
  st_bbox(roi_bbox, crs = 4326)
} else {
  st_bbox(geopolygon)
}

grid    <- st_make_grid(st_as_sfc(bbox), cellsize = grid_res,
                        square = TRUE, what = "polygons")
grid_sf <- st_sf(geometry = grid)
centroids <- st_centroid(grid_sf)

sf::sf_use_s2(FALSE)
centroids <- st_intersection(centroids, geopolygon) %>% dplyr::select(geometry)
sf::sf_use_s2(TRUE)

coords        <- st_coordinates(centroids)
coords        <- round(as.data.frame(coords), 3)
coords$site   <- seq_len(nrow(coords))

write.csv(coords, path_coords_grid, row.names = FALSE)
cat("Grid créé :", nrow(coords), "points. Sauvegardé dans", path_coords_grid, "\n")

# meteo_prep créé dynamiquement selon le range de dates — voir make_meteo_prep() plus bas

# Fonction utilitaire : crée les batches de coordonnées selon la durée demandée
# Plus la période est longue, moins de coords par batch (volume de données par requête)
make_meteo_prep <- function(coords_df, n_days) {
  # Cible : ~20 000 points de données par requête API (réduit pour éviter les 429
  # sur les longues périodes, ex. historique de 10 ans)
  coords_per_batch <- max(1, min(100, floor(20000 / n_days)))
  cat("Taille des batches :", coords_per_batch, "coords ×", n_days, "jours =",
      coords_per_batch * n_days, "points/requête\n")
  coords_df %>%
    group_by(row_number() %/% coords_per_batch) %>%
    group_map(~.x) %>%
    map(., ~group_split(., site))
}

# Fonction utilitaire : écriture BD robuste avec reconnexion automatique
safe_db_write <- function(data, table_name) {
  tryCatch(dbDisconnect(con), error = function(e) NULL)
  con <<- dbConnect(RPostgres::Postgres(), host = db_host, dbname = db_name,
                    port = db_port, user = db_user, password = db_password)
  dbWriteTable(con, table_name, as.data.frame(data), append = TRUE, row.names = FALSE)
}

# ============================================================
# 2. Téléchargement de l'historique météo → BD
# ============================================================

cat("Téléchargement de l'historique météo (", n_days_history, "jours)...\n")

start_date      <- Sys.Date() - n_days_history
end_date        <- Sys.Date() - 1
all_sites_chr   <- as.character(coords$site)

# ---- Analyser ce qui est déjà dans la BD ----
phase1_needed <- FALSE   # télécharger la période manquante vers le passé
phase2_needed <- FALSE   # télécharger les sites absents de la BD
phase1_start  <- start_date
phase1_end    <- end_date
sites_absents <- all_sites_chr

if (dbExistsTable(con, db_table_meteo) &&
    dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo))[[1]] > 0) {

  # Vérification par dates uniques par site (COUNT DISTINCT évite les doublons)
  # Tolérance 80% — la météo peut avoir des trous légitimes (données API manquantes)
  expected_dates <- n_days_history * 0.80

  site_check <- dbGetQuery(con, sprintf(
    "SELECT site::text, COUNT(DISTINCT date) as n_dates FROM %s GROUP BY site",
    db_table_meteo
  ))

  sites_in_db      <- site_check$site
  sites_absents    <- setdiff(all_sites_chr, sites_in_db)
  sites_incomplete <- site_check$site[site_check$n_dates < expected_dates]

  cat("BD actuelle :", length(sites_in_db), "sites\n")
  cat("  → Complets  (≥", round(expected_dates), "dates uniques) :",
      sum(site_check$n_dates >= expected_dates), "sites\n")
  cat("  → Incomplets (<", round(expected_dates), "dates uniques) :",
      length(sites_incomplete), "sites\n")
  cat("  → Absents                                  :",
      length(sites_absents), "sites\n")

  if (length(sites_incomplete) > 0) {
    # Supprimer les données existantes pour ces sites (évite doublons et lacunes)
    sites_sql <- paste(sites_incomplete, collapse = ",")
    dbExecute(con, sprintf("DELETE FROM %s WHERE site IN (%s)", db_table_meteo, sites_sql))
    cat("Données supprimées pour", length(sites_incomplete),
        "sites incomplets — re-téléchargement complet\n")

    phase1_needed <- TRUE
    phase1_start  <- start_date
    phase1_end    <- end_date
    coords_phase1 <- coords %>% filter(as.character(site) %in% sites_incomplete)
  }

  if (length(sites_absents) > 0) {
    phase2_needed <- TRUE
    cat("Sites absents → téléchargement complet pour", length(sites_absents), "sites\n")
  }

} else {
  # BD vide → tout télécharger
  phase1_needed <- TRUE
  phase1_start  <- start_date
  phase1_end    <- end_date
  coords_phase1 <- coords
  cat("BD vide — téléchargement complet\n")
}

# ---- Phase 1 : Période manquante pour tous les sites ----
meteo_historique <- data.frame()

if (phase1_needed) {
  n_days_phase1  <- as.numeric(phase1_end - phase1_start) + 1
  meteo_prep_p1  <- make_meteo_prep(coords_phase1, n_days_phase1)

  cat("\n--- Phase 1 :", as.character(phase1_start), "→", as.character(phase1_end),
      "|", length(meteo_prep_p1), "batches ---\n")

  for (i in seq_along(meteo_prep_p1)) {
    cat("Historique — paquet", i, "sur", length(meteo_prep_p1), "\n")

    # #new (1 - Choix modèle Open-Meteo) ====
    batch_df   <- dplyr::bind_rows(meteo_prep_p1[[i]])
    th_res_api <- get_weather_history_batch(
      latitudes  = batch_df$Y,
      longitudes = batch_df$X,
      start_date = phase1_start,
      end_date   = phase1_end,
      model      = openmeteo_model
    )
    # ==============

    th_res <- th_res_api %>%
      dplyr::left_join(unique(batch_df[, c("X", "Y", "site")]),
                       by = c("longitude" = "X", "latitude" = "Y")) %>%
      dplyr::rename(date = time) %>%
      dplyr::mutate(date = as.Date(as.character(date)), X = longitude, Y = latitude)

    meteo_historique <- rbind(meteo_historique, th_res)
    safe_db_write(th_res, db_table_meteo)
    Sys.sleep(60)
  }
}

# ---- Phase 2 : Sites absents — période complète ----
if (phase2_needed) {
  n_days_phase2      <- as.numeric(end_date - start_date) + 1
  coords_absents     <- coords %>% filter(as.character(site) %in% sites_absents)
  meteo_prep_absents <- make_meteo_prep(coords_absents, n_days_phase2)

  cat("\n--- Phase 2 : sites absents —", as.character(start_date), "→",
      as.character(end_date), "|", length(meteo_prep_absents), "batches ---\n")

  for (i in seq_along(meteo_prep_absents)) {
    cat("Sites absents — paquet", i, "sur", length(meteo_prep_absents), "\n")

    batch_df   <- dplyr::bind_rows(meteo_prep_absents[[i]])
    th_res_api <- get_weather_history_batch(
      latitudes  = batch_df$Y,
      longitudes = batch_df$X,
      start_date = start_date,
      end_date   = end_date,
      model      = openmeteo_model
    )

    th_res <- th_res_api %>%
      dplyr::left_join(unique(batch_df[, c("X", "Y", "site")]),
                       by = c("longitude" = "X", "latitude" = "Y")) %>%
      dplyr::rename(date = time) %>%
      dplyr::mutate(date = as.Date(as.character(date)), X = longitude, Y = latitude)

    safe_db_write(th_res, db_table_meteo)
    Sys.sleep(60)
  }
}

cat("Téléchargement du forecast initial...\n")

meteo_prep_forecast <- make_meteo_prep(coords, n_days_forecast)
meteo_future        <- data.frame()

for (i in seq_along(meteo_prep_forecast)) {
  cat("Forecast — paquet", i, "sur", length(meteo_prep_forecast), "\n")

  # #new (1 - Choix modèle Open-Meteo) ====
  batch_df   <- dplyr::bind_rows(meteo_prep_forecast[[i]])
  th_res_api <- get_weather_forecast_batch(
    latitudes  = batch_df$Y,
    longitudes = batch_df$X,
    n_days     = n_days_forecast,
    model      = openmeteo_model
  )
  # ==============

  th_res <- th_res_api %>%
    dplyr::left_join(unique(batch_df[, c("X", "Y", "site")]),
                     by = c("longitude" = "X", "latitude" = "Y")) %>%
    dplyr::rename(date = time) %>%
    dplyr::mutate(date = as.Date(as.character(date)),
                  X = longitude, Y = latitude)

  meteo_future <- rbind(meteo_future, th_res)
  Sys.sleep(60)
}

meteo_future_clean <- meteo_future %>%
  mutate(date = as.Date(date))

meteo_init <- bind_rows(
  if (nrow(meteo_historique) > 0) meteo_historique else data.frame(),
  meteo_future_clean
)

# #new (5 - Gestion données BD) ====
# Reconnecter si la connexion a expiré pendant les téléchargements
if (!dbIsValid(con)) {
  cat("Reconnexion à la BD...\n")
  con <- dbConnect(RPostgres::Postgres(),
                   host = db_host, dbname = db_name, port = db_port,
                   user = db_user, password = db_password)
}

# Ajout du forecast dans la BD (l'historique a déjà été écrit en cours de route)
if (nrow(meteo_future_clean) > 0) {
  dbWriteTable(con, db_table_meteo, as.data.frame(meteo_future_clean),
               append = TRUE, row.names = FALSE)
}
n_total <- dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo))[[1]]
cat("✓ Météo initiale sauvegardée dans la table BD :", db_table_meteo,
    "(", n_total, "lignes au total)\n")
# ==============

# new (backup CSV historique) ====
# Sauvegarde locale de toute la table météo (utile si on veut repartir
# d'un fichier local plutôt que retélécharger depuis l'API)
cat("Export de l'historique météo vers CSV...\n")
meteo_full     <- dbReadTable(con, db_table_meteo)
path_meteo_csv <- here("data", "meteo_history_backup.csv")
write.csv(meteo_full, path_meteo_csv, row.names = FALSE)
cat("✓ Backup CSV :", path_meteo_csv, "(", nrow(meteo_full), "lignes)\n")
# ==============

# ============================================================
# 3. Vérification des modèles entraînés
# ============================================================

# #new (3 - Séparer entraînement) ====
# L'entraînement est géré par 00_train_models.R
# Ce script vérifie simplement que les fichiers RDS sont présents
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
# ==============

dbDisconnect(con)
cat("\n✓ Initialisation terminée. Vous pouvez maintenant lancer main.R\n")
