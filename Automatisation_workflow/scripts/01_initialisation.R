# ============================================================
# SCRIPT 1 — Initialisation
# À exécuter UNE SEULE FOIS avant de lancer le pipeline hebdomadaire
#
# Prérequis (fichiers à placer dans data/) :
#   - data/df_to_model.csv              (données d'entraînement du modèle)
#   - data/administrative_boundaries.gpkg (limites administratives)
#
# Génère automatiquement :
#   - data/raw/meteofrance_herault.csv  (historique météo)
#   - data/coords_grid.csv              (points du grid retenus)
#   - models/res_presence_LOSO_probabilistic.rds
#   - models/res_abundance_LOSO_quantile_rf.rds
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
library(caret)
library(ranger)
library(CAST)
library(correlation)
library(pROC)

source(here("scripts", "00_functions.R"))
source(here("config.R"))

# Reproducibilité
set.seed(123)

# ============================================================
# Paramètres
# ============================================================
path_meteo_csv    <- here("data", "raw", "meteofrance_herault.csv")
path_admin_bounds <- here("data", "administrative_boundaries.gpkg")
path_df_model     <- here("data", "df_to_model.csv")
path_coords_grid  <- here("data", "coords_grid.csv")
path_models       <- here("models")
n_days_history    <- 120  # minimum = lag_max (84 jours)
n_days_forecast   <- 14
bbox_xmin         <- 2.40
bbox_xmax         <- 4.30
bbox_ymin         <- 43.1
bbox_ymax         <- 44.0

dir.create(here("data", "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(path_models, showWarnings = FALSE)


# ============================================================
# 1. Grid et centroids
# ============================================================

cat("Création du grid...\n")

limites_administratives <- st_read(path_admin_bounds)
limites_administratives <- st_transform(limites_administratives, 4326)

sf::sf_use_s2(FALSE)
geopolygon <- st_union(st_make_valid(limites_administratives))
sf::sf_use_s2(TRUE)

bbox <- st_bbox(c(xmin = bbox_xmin, xmax = bbox_xmax,
                  ymin = bbox_ymin, ymax = bbox_ymax), crs = 4326)

grid <- st_make_grid(st_as_sfc(bbox), cellsize = 0.05, square = TRUE, what = "polygons")
grid_sf <- st_sf(geometry = grid)
centroids <- st_centroid(grid_sf)

sf::sf_use_s2(FALSE)
centroids <- st_intersection(centroids, geopolygon) %>% dplyr::select(geometry)
sf::sf_use_s2(TRUE)

coords <- st_coordinates(centroids)
coords <- round(coords, 3)
coords <- as.data.frame(coords)
coords$site <- seq(1:nrow(coords))

# Sauvegarder les coordonnées du grid pour le pipeline hebdomadaire
write.csv(coords, path_coords_grid, row.names = FALSE)
cat("Grid créé :", nrow(coords), "points retenus. Sauvegardé dans", path_coords_grid, "\n")

meteo_prep <- coords %>%
  group_by(row_number() %/% 20) %>%
  group_map(~.x) %>%
  map(., ~group_split(., site))


# ============================================================
# 2. Téléchargement de l'historique météo
# ============================================================

cat("Téléchargement de l'historique météo (", n_days_history, "jours)...\n")

meteo_historique <- data.frame()
start_date <- Sys.Date() - n_days_history
end_date   <- Sys.Date() - 1

for(i in 1:length(meteo_prep)){
  cat("Paquet", i, "sur", length(meteo_prep), "\n")

  th_meteo <- map(meteo_prep[[i]], ~get_weather_history(
    latitude   = .$Y,
    longitude  = .$X,
    start_date = start_date,
    end_date   = end_date
  ))

  th_res <- map2_dfr(meteo_prep[[i]], th_meteo, ~bind_cols(.x, .y))
  meteo_historique <- rbind(meteo_historique, th_res)

  Sys.sleep(60)
}

cat("Téléchargement du forecast initial...\n")

meteo_future <- data.frame()

for(i in 1:length(meteo_prep)){
  cat("Paquet", i, "sur", length(meteo_prep), "\n")

  th_meteo <- map(meteo_prep[[i]], ~get_weather_forecast(
    latitude  = .$Y,
    longitude = .$X,
    n_days    = n_days_forecast
  ))

  th_res <- map2_dfr(meteo_prep[[i]], th_meteo, ~bind_cols(.x, .y))
  meteo_future <- rbind(meteo_future, th_res)

  Sys.sleep(60)
}

meteo_init <- bind_rows(meteo_historique, meteo_future)
data.table::fwrite(meteo_init, path_meteo_csv)
cat("CSV météo sauvegardé :", path_meteo_csv, "\n")


# ============================================================
# 3. Entraînement du modèle
# ============================================================

cat("Chargement des données d'entraînement...\n")

df_model <- read.csv(path_df_model) %>%
  dplyr::filter(site != "MURET") %>%
  filter(!is.na(Year))

df_model <- df_model %>%
  relocate(effectif_jour, .before = RR_0_0) %>%
  group_by(site, Year, week) %>%
  summarise_at(vars(effectif_jour:photoperiod), mean, na.rm = TRUE) %>%
  ungroup()

df_model <- df_model %>%
  rename(NB_ALBO_TOT = effectif_jour) %>%
  mutate(
    PRES_ALBO = ifelse(NB_ALBO_TOT > 0, "Presence", "Absence"),
    PRES_ALBO = factor(PRES_ALBO, levels = c("Presence", "Absence")),
    PRES_ALBO_NUMERIC = ifelse(PRES_ALBO == "Presence", 1, 0)
  ) %>%
  filter(!is.na(NB_ALBO_TOT)) %>%
  filter(site != "RENNES") %>%
  mutate(row_id = row_number())

predictors_presence  <- c("TM_0_8", "UM_5_11")
predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")

# ---- Modèle de présence ----
cat("Entraînement du modèle de présence...\n")

df_model_presence <- df_model %>%
  dplyr::select(row_id, site, Year, week, NB_ALBO_TOT, PRES_ALBO, all_of(predictors_presence))

indices_cv_presence <- CAST::CreateSpacetimeFolds(
  df_model_presence,
  spacevar = "site",
  k = length(unique(df_model_presence$site))
)

tr_presence <- trainControl(
  method = "cv",
  index = indices_cv_presence$index,
  indexOut = indices_cv_presence$indexOut,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final",
  verboseIter = FALSE
)

mod_presence <- caret::train(
  x = df_model_presence[, predictors_presence],
  y = df_model_presence$PRES_ALBO,
  method = "ranger",
  tuneLength = 10,
  trControl = tr_presence,
  metric = "ROC",
  maximize = TRUE,
  preProcess = c("center", "scale"),
  importance = "permutation"
)

df_cv_presence <- mod_presence$pred %>%
  left_join(df_model_presence, by = c("rowIndex" = "row_id")) %>%
  dplyr::select(rowIndex, pred, Presence, obs, site, week, Year, NB_ALBO_TOT,
                all_of(predictors_presence)) %>%
  mutate(
    obs_num = ifelse(obs == "Presence", 1, 0),
    pred_presence_prob = Presence,
    pred_presence_class = pred,
    pred_presence_var = pred_presence_prob * (1 - pred_presence_prob),
    pred_presence_entropy = -(
      pred_presence_prob * log(pmax(pred_presence_prob, 1e-8)) +
        (1 - pred_presence_prob) * log(pmax(1 - pred_presence_prob, 1e-8))
    )
  )

# ---- Modèle d'abondance ----
cat("Entraînement du modèle d'abondance...\n")

df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT > 0) %>%
  dplyr::select(row_id, site, Year, week, NB_ALBO_TOT, PRES_ALBO, all_of(predictors_abundance)) %>%
  mutate(NB_ALBO_TOT_LOG = log(NB_ALBO_TOT))

cv_quantile_rf <- function(data, predictors, response = "NB_ALBO_TOT_LOG",
                            site_col = "site", quantiles = c(0.05, 0.5, 0.95),
                            num.trees = 500, mtry = NULL, min.node.size = 5, seed = 123) {

  sites <- unique(data[[site_col]])
  if (is.null(mtry)) mtry <- max(1, floor(sqrt(length(predictors))))
  fold_preds <- vector("list", length(sites))

  for (i in seq_along(sites)) {
    test_site <- sites[i]
    train_dat <- data %>% filter(.data[[site_col]] != test_site)
    test_dat  <- data %>% filter(.data[[site_col]] == test_site)
    set.seed(seed + i)

    mod_qrf <- ranger(
      dependent.variable.name = response,
      data = train_dat[, c(response, predictors), drop = FALSE],
      num.trees = num.trees, mtry = mtry, min.node.size = min.node.size,
      importance = "permutation", quantreg = TRUE, keep.inbag = TRUE
    )

    pred_q <- predict(mod_qrf, data = test_dat[, predictors, drop = FALSE],
                      type = "quantiles", quantiles = quantiles)$predictions
    colnames(pred_q) <- paste0("pred_log_q", c("05", "50", "95"))

    fold_preds[[i]] <- bind_cols(test_dat, as.data.frame(pred_q)) %>%
      mutate(
        fold = i,
        pred_abundance_q05 = exp(pred_log_q05),
        pred_abundance_q50 = exp(pred_log_q50),
        pred_abundance_q95 = exp(pred_log_q95),
        pred_abundance_interval_width = pred_abundance_q95 - pred_abundance_q05,
        covered = NB_ALBO_TOT >= pred_abundance_q05 & NB_ALBO_TOT <= pred_abundance_q95
      )
  }
  bind_rows(fold_preds)
}

df_cv_abundance_quantiles <- cv_quantile_rf(
  data = df_model_abundance,
  predictors = predictors_abundance,
  response = "NB_ALBO_TOT_LOG",
  site_col = "site",
  quantiles = c(0.05, 0.5, 0.95),
  num.trees = 500,
  mtry = max(1, floor(sqrt(length(predictors_abundance)))),
  min.node.size = 5,
  seed = 123
)

rf_abundance_q <- ranger(
  dependent.variable.name = "NB_ALBO_TOT_LOG",
  data = df_model_abundance[, c("NB_ALBO_TOT_LOG", predictors_abundance)],
  num.trees = 500,
  mtry = max(1, floor(sqrt(length(predictors_abundance)))),
  min.node.size = 5,
  importance = "permutation",
  quantreg = TRUE,
  keep.inbag = TRUE
)

# ---- Sauvegarder les modèles ----
cat("Sauvegarde des modèles...\n")

saveRDS(
  list(model = mod_presence, df_cv = df_cv_presence, df_mod = df_model_presence),
  file.path(path_models, "res_presence_LOSO_probabilistic.rds")
)

saveRDS(
  list(model_quantile = rf_abundance_q, df_cv_quantiles = df_cv_abundance_quantiles,
       df_mod = df_model_abundance),
  file.path(path_models, "res_abundance_LOSO_quantile_rf.rds")
)

cat("\n✓ Initialisation terminée. Vous pouvez maintenant lancer le pipeline hebdomadaire.\n")
