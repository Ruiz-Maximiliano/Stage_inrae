# ============================================================
# SCRIPT 2 — Pipeline hebdomadaire
# À exécuter chaque semaine sur le serveur (cron job)
# Prérequis : Script 1 (initialisation) déjà exécuté
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
library(exactextractr)
library(DBI)
library(RPostgres)
library(caret)
library(ranger)
library(CAST)
library(treeshap)

source(here("scripts", "00_functions.R"))
source(here("config.R"))

# ============================================================
# Paramètres
# ============================================================
path_meteo_csv    <- here("data", "raw", "meteofrance_herault.csv")
path_admin_bounds <- here("data", "administrative_boundaries.gpkg")
path_coords_grid  <- here("data", "coords_grid.csv")
path_models       <- here("models")
n_days_forecast   <- 14

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
# Chargement des données spatiales
# ============================================================
limites_administratives <- st_read(path_admin_bounds)
limites_administratives <- st_transform(limites_administratives, 4326)

sf::sf_use_s2(FALSE)
limites_administratives <- st_make_valid(limites_administratives)
geopolygon <- st_union(limites_administratives)
sf::sf_use_s2(TRUE)

#============ new
# Charger les coordonnées du grid sauvegardées lors de l'initialisation
# Évite de recréer le grid et de refaire l'intersection à chaque exécution
coords <- read.csv(path_coords_grid)
#===========

meteo_prep <- coords %>%
  group_by(row_number() %/% 20) %>%
  group_map(~.x) %>%
  map(., ~group_split(., site))


######################################################
######### Téléchargement des données météo
######################################################

#============ new
# Charger le CSV existant (déjà téléchargé lors de l'initialisation)
meteo <- fread(path_meteo_csv)
meteo <- meteo %>% rename(date = time)
meteo$date <- as.Date(as.character(meteo$date))

# ---- Étape 1 : Remplacer les données forecast de la semaine passée par le historical ----
dates_a_remplacer <- unique(meteo$date[
  meteo$date >= Sys.Date() - 7 & meteo$date < Sys.Date()
])

if (length(dates_a_remplacer) > 0) {
  cat("Remplacement forecast -> historical pour", length(dates_a_remplacer), "dates\n")

  meteo_updated <- data.frame()

  for(i in 1:length(meteo_prep)){
    cat("Mise à jour historical — paquet", i, "sur", length(meteo_prep), "\n")

    th_meteo <- map(meteo_prep[[i]], ~get_weather_history(
      latitude   = .$Y,
      longitude  = .$X,
      start_date = min(dates_a_remplacer),
      end_date   = max(dates_a_remplacer)
    ))

    th_res <- map2_dfr(meteo_prep[[i]], th_meteo, ~bind_cols(.x, .y))
    meteo_updated <- rbind(meteo_updated, th_res)
    Sys.sleep(60)
  }

  meteo <- meteo %>%
    filter(!(date %in% dates_a_remplacer)) %>%
    bind_rows(meteo_updated)
}

# ---- Étape 2 : Télécharger la nouvelle semaine de forecast ----
cat("Téléchargement du forecast...\n")

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

meteo <- bind_rows(meteo, meteo_future)
data.table::fwrite(meteo, path_meteo_csv)
#===========


######################################################
######### Création des variables indépendantes
######################################################

meteo <- data.table(meteo)

meteo <- meteo %>%
  unique() %>%
  group_by(X, Y) %>%
  mutate(site = cur_group_id()) %>%
  ungroup() %>%
  relocate(site, 1) %>%
  data.table()

unique_coords <- unique(meteo[, c("site", "X", "Y")])
unique_coords_sf <- st_as_sf(unique_coords, coords = c("X", "Y"), crs = 4326)

sf::sf_use_s2(FALSE)
coords_retain <- st_intersection(unique_coords_sf, geopolygon)
sf::sf_use_s2(TRUE)

coords_retain <- cbind(coords_retain, st_coordinates(coords_retain))
coords_retain <- st_drop_geometry(coords_retain)
coords_retain <- coords_retain[, c("site", "X", "Y")]

meteo <- meteo %>%
  filter(site %in% coords_retain$site) %>%
  mutate(date = as.Date(date)) %>%
  rename(TM = temperature_2m_mean, 
         RR = precipitation_sum, 
         UM = relative_humidity_2m_mean) %>%
  dplyr::select(site, date, RR, TM, UM)

lag_max <- 84

meteo2 <- meteo %>%
  dplyr::select(site, date) %>%
  mutate(year = year(date), week = week(date), weekday = wday(date)) %>%
  filter(weekday == 1) %>%
  slice(rep(1:n(), each = lag_max)) %>%
  group_by(site, year, week) %>%
  mutate(lag_n = row_number()) %>%
  ungroup() %>%
  dplyr::select(-weekday) %>%
  rename(th_date = date) %>%
  mutate(date = th_date - lag_n) %>%
  data.table()

meteo3 <- meteo2 %>%
  left_join(meteo, by = c("date", "site")) %>%
  pivot_longer(!(site:date), names_to = "var", values_to = 'val') %>%
  data.table()


#### Functions

fun_summarize_week <- function(meteo3, var_to_summarize, fun_summarize, new_var_name, n_days_agg){

  if(fun_summarize == "sum"){
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = sum(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]

  } else if (fun_summarize == "mean"){
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = mean(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]

  } else if (fun_summarize == "max"){
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = max(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]

  } else if (fun_summarize == "min"){
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = min(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]
  }

  meteo3_summarize <- data.table(meteo3_summarize)
  return(meteo3_summarize)
}

df_meteo_pieges_summ <- fun_summarize_week(meteo3, "RR", "sum", "RR", 7) %>%
  bind_rows(fun_summarize_week(meteo3, "TM", "mean", "TM", 7)) %>%
  bind_rows(fun_summarize_week(meteo3, "UM", "mean", "UM", 7))

df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n < 12)

fun_ccm_df <- function(df_timeseries, varr, function_to_apply){

  df_timeseries_wide <- df_timeseries %>%
    filter(var == varr) %>%
    dplyr::select(-c("date", "var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr, "_"))

  max_col <- ncol(df_timeseries_wide)

  for(i in 3:(max_col - 1)){
    for(j in (i + 1):max_col){
      column_name <- paste0(colnames(df_timeseries_wide[i]), "_", (j - 2))
      if(function_to_apply == "mean"){
        df_timeseries_wide[column_name] <- rowMeans(df_timeseries_wide[, i:j], na.rm = T)
      } else if (function_to_apply == "sum"){
        df_timeseries_wide[column_name] <- rowSums(df_timeseries_wide[, i:j], na.rm = T)
      } else if (function_to_apply == "max"){
        df_timeseries_wide[column_name] <- max(df_timeseries_wide[, i:j], na.rm = T)
      } else if (function_to_apply == "min"){
        df_timeseries_wide[column_name] <- min(df_timeseries_wide[, i:j], na.rm = T)
      }
    }
  }

  for(i in 3:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i], "_", sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }

  return(df_timeseries_wide)
}

df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ, "RR", "sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ, "TM", "mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ, "UM", "mean")

df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide3)

df_meteo_predictions <- df_meteo_pieges_summ_wide_meteofrance %>%
  dplyr::select(site, th_date, TM_0_8, UM_5_11, TM_0_4, UM_0_11, RR_1_5, TM_0_0, RR_0_0, TM_0_5, UM_1_10, RR_1_10) %>%
  rename(date = th_date) %>%
  na.omit(.)


######################################################
######### Génération des prédictions
######################################################

#============ new
# Charger les modèles entraînés depuis les fichiers .rds
res_presence  <- readRDS(file.path(path_models, "res_presence_LOSO_probabilistic.rds"))
res_abundance <- readRDS(file.path(path_models, "res_abundance_LOSO_quantile_rf.rds"))

mod_presence   <- res_presence$model
rf_abundance_q <- res_abundance$model_quantile

predictors_presence  <- c("TM_0_8", "UM_5_11")
predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")

# Fonction de prédiction two-part avec propagation d'incertitude
predict_two_part_uncertainty <- function(newdata, mod_presence, rf_abundance_q,
                                          predictors_presence, predictors_abundance,
                                          n_sim = 2000) {

  pred_presence <- predict(mod_presence,
    newdata = newdata[, predictors_presence, drop = FALSE], type = "prob")
  p <- pred_presence$Presence

  pred_q <- predict(rf_abundance_q,
    data = newdata[, predictors_abundance, drop = FALSE],
    type = "quantiles", quantiles = c(0.05, 0.5, 0.95))$predictions

  out <- newdata %>%
    mutate(
      pred_presence_prob = p,
      pred_presence_var = p * (1 - p),
      pred_presence_entropy = -(p * log(pmax(p, 1e-8)) + (1 - p) * log(pmax(1 - p, 1e-8))),
      pred_log_abundance_q05 = pred_q[, 1],
      pred_log_abundance_q50 = pred_q[, 2],
      pred_log_abundance_q95 = pred_q[, 3],
      pred_abundance_q05 = exp(pred_log_abundance_q05),
      pred_abundance_q50 = exp(pred_log_abundance_q50),
      pred_abundance_q95 = exp(pred_log_abundance_q95),
      pred_expected_abundance = pred_presence_prob * pred_abundance_q50
    )

  sim_res <- purrr::map_dfr(seq_len(nrow(out)), function(i) {
    p_i  <- out$pred_presence_prob[i]
    mu_i <- out$pred_log_abundance_q50[i]
    sd_i <- pmax((out$pred_log_abundance_q95[i] - out$pred_log_abundance_q05[i]) / (2 * 1.645), 1e-6)
    y_sim <- rbinom(n_sim, 1, p_i) * exp(rnorm(n_sim, mu_i, sd_i))
    tibble(row_id = i, pred_combined_mean = mean(y_sim),
           pred_combined_q05 = quantile(y_sim, 0.05),
           pred_combined_q50 = quantile(y_sim, 0.50),
           pred_combined_q95 = quantile(y_sim, 0.95),
           pred_combined_sd = sd(y_sim))
  })

  out %>%
    mutate(row_id = seq_len(n())) %>%
    left_join(sim_res, by = "row_id") %>%
    mutate(pred_thresholded = ifelse(pred_presence_prob > 0.5, pred_abundance_q50, 0))
}

# Récupérer les coordonnées et appliquer le modèle
df_meteo_predictions <- df_meteo_predictions %>%
  left_join(coords_retain, by = "site")

df_meteo_predictions <- predict_two_part_uncertainty(
  newdata              = df_meteo_predictions,
  mod_presence         = mod_presence,
  rf_abundance_q       = rf_abundance_q,
  predictors_presence  = predictors_presence,
  predictors_abundance = predictors_abundance,
  n_sim                = 2000
)
#===========


######################################################
######### Calcul des valeurs SHAP (explicabilité du modèle)
######################################################

# Données d'entrée pour SHAP (prédicteurs d'abondance)
X_abundance <- as.data.frame(df_meteo_predictions[, predictors_abundance])

# Convertir el modelo ranger a formato treeshap
unified_model <- ranger.unify(rf_abundance_q, X_abundance)

# Calcular SHAP values
treeshap_result <- treeshap(unified_model, X_abundance)

# Convertir a data.frame
shap_abundance_df <- as.data.frame(treeshap_result$shaps)
colnames(shap_abundance_df) <- paste0("shap_", colnames(shap_abundance_df))

# Variable dominante
shap_cols_abundance <- colnames(shap_abundance_df)

df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_abundance_df) %>%
  mutate(
    shap_dominant_var = shap_cols_abundance[
      apply(abs(.[, shap_cols_abundance]), 1, which.max)
    ],
    shap_dominant_val = apply(.[, shap_cols_abundance], 1,
                              function(x) x[which.max(abs(x))])
  )




######################################################
######### Rasterisation
######################################################

grid_res <- 0.05

df_meteo_predictions <- df_meteo_predictions %>%
  mutate(
    X_snap = round(X / grid_res) * grid_res,
    Y_snap = round(Y / grid_res) * grid_res
  )

res_x <- min(diff(sort(unique(df_meteo_predictions$X_snap))))
res_y <- min(diff(sort(unique(df_meteo_predictions$Y_snap))))

xmin <- min(df_meteo_predictions$X_snap) - res_x / 2
xmax <- max(df_meteo_predictions$X_snap) + res_x / 2
ymin <- min(df_meteo_predictions$Y_snap) - res_y / 2
ymax <- max(df_meteo_predictions$Y_snap) + res_y / 2

r_template <- rast(
  ext(xmin, xmax, ymin, ymax),
  resolution = c(res_x, res_y),
  crs = "+proj=longlat +datum=WGS84"
)

df_meteo_predictions$date <- as.character(df_meteo_predictions$date)
v <- vect(df_meteo_predictions, geom = c("X_snap", "Y_snap"), crs = "EPSG:4326")
dates <- unique(df_meteo_predictions$date)

rasters <- lapply(dates, function(d) {
  v_d <- v[v$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "pred_combined_mean", fun = "mean")
  names(r) <- d
  r
})
r_stack <- rast(rasters)


######################################################
######### Agrégation par commune — prédictions entomo
######################################################

mean_abundance <- exact_extract(r_stack, limites_administratives, c('mean')) %>%
  bind_cols(limites_administratives) %>%
  pivot_longer(cols = starts_with("mean"), names_to = "date", values_to = "mean_abundance_albopictus") %>%
  mutate(date = gsub("\\.", "-", date)) %>%
  mutate(date = gsub("mean-", "", date)) %>%
  dplyr::select(codgeo, libgeo, date, mean_abundance_albopictus) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_abundance_albopictus))

abundance <- mean_abundance %>%
  mutate(date = as.Date(date)) %>%
  mutate(mean_abundance_albopictus = round(mean_abundance_albopictus, 1))

abundance$date <- abundance$date + 1


######################################################
######### Agrégation par commune — données météo
######################################################

meteo_comm <- df_meteo_pieges_summ %>%
  filter(lag_n == 0) %>%
  dplyr::select(-c("th_date", "lag_n")) %>%
  pivot_wider(names_from = var, values_from = val) %>%
  left_join(coords_retain) %>%
  mutate(
    X_snap = round(X / grid_res) * grid_res,
    Y_snap = round(Y / grid_res) * grid_res
  ) %>%
  mutate(date = as.character(date))

res_x <- min(diff(sort(unique(meteo_comm$X_snap))))
res_y <- min(diff(sort(unique(meteo_comm$Y_snap))))
xmin <- min(meteo_comm$X_snap) - res_x / 2
xmax <- max(meteo_comm$X_snap) + res_x / 2
ymin <- min(meteo_comm$Y_snap) - res_y / 2
ymax <- max(meteo_comm$Y_snap) + res_y / 2

r_template <- rast(ext(xmin, xmax, ymin, ymax),
                   resolution = c(res_x, res_y),
                   crs = "+proj=longlat +datum=WGS84")

v_meteo <- vect(meteo_comm, geom = c("X_snap", "Y_snap"), crs = "EPSG:4326")
dates <- unique(meteo_comm$date)

rasters <- lapply(dates, function(d) {
  v_d <- v_meteo[v_meteo$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "TM", fun = "mean")
  names(r) <- d; r
})
r_meteo_tm <- rast(rasters)

rasters <- lapply(dates, function(d) {
  v_d <- v_meteo[v_meteo$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "UM", fun = "mean")
  names(r) <- d; r
})
r_meteo_um <- rast(rasters)

rasters <- lapply(dates, function(d) {
  v_d <- v_meteo[v_meteo$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "RR", fun = "mean")
  names(r) <- d; r
})
r_meteo_rr <- rast(rasters)

mean_temperature <- exact_extract(r_meteo_tm, limites_administratives, c('mean')) %>%
  bind_cols(limites_administratives) %>%
  pivot_longer(cols = starts_with("mean"), names_to = "date", values_to = "mean_temperature") %>%
  mutate(date = gsub("\\.", "-", date), date = gsub("mean-", "", date)) %>%
  dplyr::select(codgeo, libgeo, date, mean_temperature) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_temperature)) %>%
  mutate(mean_temperature = round(mean_temperature, 1))

mean_rainfall <- exact_extract(r_meteo_rr, limites_administratives, c('mean')) %>%
  bind_cols(limites_administratives) %>%
  pivot_longer(cols = starts_with("mean"), names_to = "date", values_to = "mean_rainfall") %>%
  mutate(date = gsub("\\.", "-", date), date = gsub("mean-", "", date)) %>%
  dplyr::select(codgeo, libgeo, date, mean_rainfall) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_rainfall)) %>%
  mutate(mean_rainfall = round(mean_rainfall, 1))

mean_humidity <- exact_extract(r_meteo_um, limites_administratives, c('mean')) %>%
  bind_cols(limites_administratives) %>%
  pivot_longer(cols = starts_with("mean"), names_to = "date", values_to = "mean_humidity") %>%
  mutate(date = gsub("\\.", "-", date), date = gsub("mean-", "", date)) %>%
  dplyr::select(codgeo, libgeo, date, mean_humidity) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_humidity)) %>%
  mutate(mean_humidity = round(mean_humidity, 1))

meteo_out <- left_join(mean_temperature, mean_rainfall)
meteo_out <- left_join(meteo_out, mean_humidity)
meteo_out$date <- meteo_out$date + 2


######################################################
######### Publication dans PostGIS
######################################################

albopictus_predictions <- left_join(meteo_out, abundance) %>%
  mutate(date_fin = date + 7) %>%
  relocate(date_fin, .after = date) %>%
  mutate(last_update = as.Date(Sys.Date()))

thresh_orange_red <- median(albopictus_predictions$mean_abundance_albopictus[
  which(albopictus_predictions$mean_abundance_albopictus > 0 &
        !is.na(albopictus_predictions$mean_abundance_albopictus > 0))])

albopictus_predictions <- albopictus_predictions %>%
  mutate(level_risk = case_when(
    mean_abundance_albopictus == 0 | is.na(mean_abundance_albopictus) ~ "Faible",
    mean_abundance_albopictus > 0 & mean_abundance_albopictus < thresh_orange_red ~ "Modéré",
    mean_abundance_albopictus >= thresh_orange_red ~ "Élevé"
  ))

albopictus_predictions <- albopictus_predictions %>%
  arrange(codgeo, date) %>%
  group_by(codgeo) %>%
  mutate(
    trend = case_when(
      is.na(mean_abundance_albopictus) ~ NA_real_,
      is.na(lag(mean_abundance_albopictus)) ~ NA_real_,
      lag(mean_abundance_albopictus) == 0 ~ NA_real_,
      TRUE ~ 100 * (mean_abundance_albopictus - lag(mean_abundance_albopictus)) /
        lag(mean_abundance_albopictus)
    )
  ) %>%
  ungroup() %>%
  mutate(trend = ifelse(is.na(trend), 0, round(trend)))

albopictus_predictions <- albopictus_predictions %>%
  mutate(
    class_trend = case_when(
      is.na(trend) ~ "Stable",
      trend > 20   ~ "En hausse",
      trend < -20  ~ "En baisse",
      TRUE         ~ "Stable"
    )
  )

# Publication dans la table définie dans config.R
st_write(albopictus_predictions, dsn = con, layer = db_layer, append = FALSE)
cat("✓ Prédictions publiées dans la table", db_layer, "\n")


######################################################
######### Table test2 — avec valeurs SHAP par commune
######################################################

shap_cols_all <- c("shap_TM_0_4", "shap_UM_0_11", "shap_RR_1_5")

# Agrégation spatiale des SHAP : rasteriser puis extraire par commune
df_shap_spatial <- df_meteo_predictions %>%
  dplyr::select(X_snap, Y_snap, date, all_of(shap_cols_all)) %>%
  mutate(date = as.character(date))

res_x_s <- min(diff(sort(unique(df_shap_spatial$X_snap))))
res_y_s <- min(diff(sort(unique(df_shap_spatial$Y_snap))))
xmin_s  <- min(df_shap_spatial$X_snap) - res_x_s / 2
xmax_s  <- max(df_shap_spatial$X_snap) + res_x_s / 2
ymin_s  <- min(df_shap_spatial$Y_snap) - res_y_s / 2
ymax_s  <- max(df_shap_spatial$Y_snap) + res_y_s / 2

r_template_s <- rast(
  ext(xmin_s, xmax_s, ymin_s, ymax_s),
  resolution = c(res_x_s, res_y_s),
  crs = "+proj=longlat +datum=WGS84"
)

v_shap <- vect(df_shap_spatial, geom = c("X_snap", "Y_snap"), crs = "EPSG:4326")
dates_shap <- unique(df_shap_spatial$date)

extract_shap_commune <- function(shap_col) {
  rasters_s <- lapply(dates_shap, function(d) {
    v_d <- v_shap[v_shap$date == d, ]
    r   <- terra::rasterize(v_d, r_template_s, field = shap_col, fun = "mean")
    names(r) <- d
    r
  })
  r_stack_s <- rast(rasters_s)

  exact_extract(r_stack_s, limites_administratives, c('mean')) %>%
    bind_cols(limites_administratives) %>%
    pivot_longer(cols = starts_with("mean"), names_to = "date", values_to = shap_col) %>%
    mutate(date = as.Date(gsub("mean-", "", gsub("\\.", "-", date)))) %>%
    dplyr::select(codgeo, date, all_of(shap_col)) %>%
    filter(!is.nan(.data[[shap_col]]))
}

shap_comm <- extract_shap_commune("shap_TM_0_4") %>%
  left_join(extract_shap_commune("shap_UM_0_11"), by = c("codgeo", "date")) %>%
  left_join(extract_shap_commune("shap_RR_1_5"),  by = c("codgeo", "date")) %>%
  mutate(date = date + 1)  # même décalage que abundance

# Variable SHAP dominante par commune (basée sur les moyennes)
shap_comm <- shap_comm %>%
  mutate(
    shap_dominant_var = shap_cols_all[
      apply(abs(dplyr::select(., all_of(shap_cols_all))), 1, which.max)
    ],
    shap_dominant_var = gsub("shap_", "", shap_dominant_var),
    shap_dominant_val = apply(dplyr::select(., all_of(shap_cols_all)), 1,
                              function(x) x[which.max(abs(x))])
  ) %>%
  mutate(across(all_of(shap_cols_all), ~round(.x, 4)))

albopictus_predictions_test2 <- albopictus_predictions %>%
  left_join(shap_comm, by = c("codgeo", "date"))

db_layer_test2 <- paste0(db_layer, "2")
st_write(albopictus_predictions_test2, dsn = con, layer = db_layer_test2, append = FALSE)
cat("✓ Prédictions + SHAP publiées dans la table", db_layer_test2, "\n")






#meteo <- fread(path_meteo_csv)
#meteo$date <- as.Date(as.character(meteo$date))

#dates_a_remplacer <- unique(meteo$date[
#  meteo$date >= Sys.Date() - 7 & meteo$date < Sys.Date()
#])

#cat("Dates à remplacer:\n")
#print(dates_a_remplacer)
#cat("Sys.Date():", as.character(Sys.Date()), "\n")


# Ver las primeras fechas del CSV tal como están guardadas
#head(fread(path_meteo_csv)$date)
#class(fread(path_meteo_csv)$date)
exists("predict_two_part_uncertainty")
 names(meteo)
#names(fread(path_meteo_csv))
