# ============================================================
# SCRIPT 1bis — Initialisation à partir du backup CSV (PROPOSITION)
#
# Alternative à 01_initialisation.R pour (ré)initialiser la table météo
# sans repasser par l'API pour tout l'historique (évite les 429 liés
# aux longues périodes, ex. 10 ans).
#
# Recharge seulement les `n_days_local` derniers jours depuis le backup
# local (data/meteo_history_backup.csv, généré par 01_initialisation.R),
# puis télécharge le forecast (léger) via l'API.
#
# Prérequis :
#   - data/meteo_history_backup.csv existant (généré par 01_initialisation.R)
#   - data/<roi_file> pour recréer le grid (défini dans config.R)
#   - modèles .rds (00_train_models.R)
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

# ============================================================
# 1. ROI et création du grid (identique à 01_initialisation.R)
# ============================================================
cat("Chargement du ROI...\n")
roi <- st_read(path_roi)
roi <- st_transform(roi, 4326)

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
# 2. Historique météo — recharge depuis le backup CSV
# ============================================================
if (!file.exists(path_meteo_csv)) {
  stop("Backup CSV introuvable : ", path_meteo_csv,
       "\nExécuter d'abord 01_initialisation.R (qui le génère).")
}

cat("Lecture du backup CSV :", path_meteo_csv, "...\n")
meteo_csv <- read.csv(path_meteo_csv) %>% mutate(date = as.Date(date))

cutoff_date <- Sys.Date() - n_days_local

meteo_subset <- meteo_csv %>%
  filter(date >= cutoff_date,
         as.character(site) %in% as.character(coords$site))

cat("Sous-ensemble retenu du CSV :", nrow(meteo_subset), "lignes (",
    as.character(cutoff_date), "→", as.character(Sys.Date() - 1), ")\n")

# new (proposition CSV init - sites manquants/incomplets) ====
# Le CSV peut ne pas couvrir tous les sites du grid actuel (ex. grid/ROI
# modifié) ou contenir des sites avec des dates manquantes pour la période
# demandée. Ces sites sont (re)téléchargés via l'API pour `n_days_local` jours.
expected_dates <- n_days_local * 0.80

site_dates <- meteo_subset %>%
  group_by(site) %>%
  summarise(n_dates = n_distinct(date), .groups = "drop")

sites_in_csv     <- as.character(site_dates$site)
sites_absents    <- setdiff(as.character(coords$site), sites_in_csv)
sites_incomplets <- as.character(site_dates$site[site_dates$n_dates < expected_dates])
sites_a_telecharger <- union(sites_absents, sites_incomplets)

cat("  → Sites du CSV couvrant ≥", round(expected_dates), "dates :",
    sum(site_dates$n_dates >= expected_dates), "\n")
cat("  → Sites absents du CSV          :", length(sites_absents), "\n")
cat("  → Sites incomplets dans le CSV  :", length(sites_incomplets), "\n")

if (length(sites_a_telecharger) > 0) {
  cat(length(sites_a_telecharger), "site(s) à compléter via l'API (",
      as.character(cutoff_date), "→", as.character(Sys.Date() - 1), ")...\n")

  # Retire les données partielles de ces sites — seront remplacées au complet
  meteo_subset <- meteo_subset %>%
    filter(!(as.character(site) %in% sites_a_telecharger))

  coords_manquants  <- coords %>% filter(as.character(site) %in% sites_a_telecharger)
  meteo_prep_manq   <- make_meteo_prep(coords_manquants, n_days_local)
  meteo_manquant    <- data.frame()

  for (i in seq_along(meteo_prep_manq)) {
    cat("Sites à compléter — paquet", i, "sur", length(meteo_prep_manq), "\n")

    batch_df   <- dplyr::bind_rows(meteo_prep_manq[[i]])
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

    meteo_manquant <- rbind(meteo_manquant, th_res)
    Sys.sleep(60)
  }

  meteo_subset <- bind_rows(meteo_subset, meteo_manquant)
  cat("✓", nrow(meteo_manquant), "lignes téléchargées pour les sites complétés\n")
} else {
  cat("✓ Tous les sites sont couverts par le backup CSV\n")
}
# ==============

dbWriteTable(con, db_table_meteo, as.data.frame(meteo_subset),
              overwrite = TRUE, row.names = FALSE)
cat("✓ Table BD", db_table_meteo, "réinitialisée avec",
    nrow(meteo_subset), "lignes (", n_days_local, "jours)\n")

# ============================================================
# 3. Forecast — téléchargement via API (léger, n_days_forecast jours)
# ============================================================
cat("Téléchargement du forecast...\n")

meteo_prep_forecast <- make_meteo_prep(coords, n_days_forecast)
meteo_future        <- data.frame()

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

dbWriteTable(con, db_table_meteo, as.data.frame(meteo_future),
              append = TRUE, row.names = FALSE)

n_total <- dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo))[[1]]
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
