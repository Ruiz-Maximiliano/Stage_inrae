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

# Fixer le comportement de week() pour éviter le warning data.table
options(datatable.week = "legacy")

# ============================================================
# Paramètres locaux
# ============================================================
path_coords_grid <- here("data", "coords_grid.csv")
path_models      <- here("models")
grid_res         <- 0.05
n_days_forecast  <- 14
lag_max          <- 84

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
# Chargement du ROI et des coordonnées du grid
# ============================================================

# #new (2 - Entrée unique ROI) ====
roi <- st_read(path_roi)
roi <- st_transform(roi, 4326)
# ==============

# #new (8 - Renommage ROI) ====
sf::sf_use_s2(FALSE)
roi <- st_make_valid(roi)
geopolygon <- st_union(roi)
sf::sf_use_s2(TRUE)
# ==============

coords <- read.csv(path_coords_grid)

meteo_prep <- coords %>%
  group_by(row_number() %/% 20) %>%
  group_map(~.x) %>%
  map(., ~group_split(., site))


######################################################
######### Mise à jour des données météo
######################################################

# #new (5 - Gestion données BD) ====
# Lecture de la météo depuis la BD (remplace fread du CSV local)
cat("Lecture de la météo depuis la BD...\n")
meteo <- dbReadTable(con, db_table_meteo) %>% as.data.table()
meteo$date <- as.Date(meteo$date)
# ==============

# ---- Étape 1 : Remplacer forecast de la semaine passée par historical ----
dates_a_remplacer <- unique(meteo$date[
  meteo$date >= Sys.Date() - 7 & meteo$date < Sys.Date()
])

if (length(dates_a_remplacer) > 0) {
  cat("Remplacement forecast -> historical pour", length(dates_a_remplacer), "dates\n")

  meteo_updated <- data.frame()

  for (i in seq_along(meteo_prep)) {
    cat("Mise à jour historical — paquet", i, "sur", length(meteo_prep), "\n")

    # #new (1 - Choix modèle Open-Meteo) ====
    batch_df   <- dplyr::bind_rows(meteo_prep[[i]])
    th_res_api <- get_weather_history_batch(
      latitudes  = batch_df$Y,
      longitudes = batch_df$X,
      start_date = min(dates_a_remplacer),
      end_date   = max(dates_a_remplacer),
      model      = openmeteo_model
    )
    # ==============

    th_res <- th_res_api %>%
      dplyr::left_join(unique(batch_df[, c("X", "Y", "site")]),
                       by = c("longitude" = "X", "latitude" = "Y")) %>%
      dplyr::rename(date = time) %>%
      dplyr::mutate(date = as.Date(as.character(date)),
                    X = longitude, Y = latitude)

    meteo_updated <- rbind(meteo_updated, th_res)
    Sys.sleep(1)
  }

  meteo_updated <- meteo_updated %>%
    mutate(date = as.Date(date))

  # #new (5 - Gestion données BD) ====
  # Supprimer les lignes forecast à remplacer, insérer l'historical
  dates_sql <- paste(paste0("'", dates_a_remplacer, "'"), collapse = ",")
  dbExecute(con, sprintf("DELETE FROM %s WHERE date IN (%s)", db_table_meteo, dates_sql))
  dbWriteTable(con, db_table_meteo, as.data.frame(meteo_updated), append = TRUE, row.names = FALSE)
  # ==============

  meteo <- meteo %>%
    filter(!(date %in% dates_a_remplacer)) %>%
    bind_rows(meteo_updated)
}

# ---- Étape 2 : Télécharger la nouvelle semaine de forecast ----
cat("Téléchargement du forecast...\n")

meteo_future <- data.frame()

for (i in seq_along(meteo_prep)) {
  cat("Forecast — paquet", i, "sur", length(meteo_prep), "\n")

  # #new (1 - Choix modèle Open-Meteo) ====
  batch_df   <- dplyr::bind_rows(meteo_prep[[i]])
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
  Sys.sleep(1)
}

meteo_future <- meteo_future %>% mutate(date = as.Date(date))

# #new (5 - Gestion données BD) ====
# Supprimer l'ancien forecast futur, insérer le nouveau
dbExecute(con, sprintf("DELETE FROM %s WHERE date >= '%s'", db_table_meteo, Sys.Date()))
dbWriteTable(con, db_table_meteo, as.data.frame(meteo_future), append = TRUE, row.names = FALSE)

# Relire la météo complète et à jour depuis la BD
meteo <- dbReadTable(con, db_table_meteo) %>% as.data.table()
meteo$date <- as.Date(meteo$date)
# ==============


######################################################
######### Création des variables indépendantes
######################################################

meteo <- meteo %>%
  unique() %>%
  group_by(X, Y) %>%
  mutate(site = cur_group_id()) %>%
  ungroup() %>%
  relocate(site, 1) %>%
  data.table()

unique_coords    <- unique(meteo[, c("site", "X", "Y")])
unique_coords_sf <- st_as_sf(unique_coords, coords = c("X", "Y"), crs = 4326)

sf::sf_use_s2(FALSE)
coords_retain <- st_intersection(unique_coords_sf, geopolygon)
sf::sf_use_s2(TRUE)

coords_retain <- cbind(coords_retain, st_coordinates(coords_retain))
coords_retain <- st_drop_geometry(coords_retain)[, c("site", "X", "Y")]

meteo <- meteo %>%
  filter(site %in% coords_retain$site) %>%
  mutate(date = as.Date(date)) %>%
  rename(TM = temperature_2m_mean,
         RR = precipitation_sum,
         UM = relative_humidity_2m_mean) %>%
  dplyr::select(site, date, RR, TM, UM)

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
  pivot_longer(!(site:date), names_to = "var", values_to = "val") %>%
  data.table()

fun_summarize_week <- function(meteo3, var_to_summarize, fun_summarize,
                                new_var_name, n_days_agg) {

  if (fun_summarize == "sum") {
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = sum(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][, year := NULL]

  } else if (fun_summarize == "mean") {
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = mean(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][, year := NULL]

  } else if (fun_summarize == "max") {
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = max(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][, year := NULL]

  } else if (fun_summarize == "min") {
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = min(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][, year := NULL]
  }

  data.table(meteo3_summarize)
}

df_meteo_pieges_summ <- fun_summarize_week(meteo3, "RR", "sum",  "RR", 7) %>%
  bind_rows(fun_summarize_week(meteo3, "TM", "mean", "TM", 7)) %>%
  bind_rows(fun_summarize_week(meteo3, "UM", "mean", "UM", 7))

df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n < 12)

fun_ccm_df <- function(df_timeseries, varr, function_to_apply) {

  df_timeseries_wide <- df_timeseries %>%
    filter(var == varr) %>%
    dplyr::select(-c("date", "var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n,
                names_prefix = paste0(varr, "_"))

  max_col <- ncol(df_timeseries_wide)

  for (i in 3:(max_col - 1)) {
    for (j in (i + 1):max_col) {
      column_name <- paste0(colnames(df_timeseries_wide[i]), "_", (j - 2))
      if (function_to_apply == "mean") {
        df_timeseries_wide[column_name] <- rowMeans(df_timeseries_wide[, i:j], na.rm = TRUE)
      } else if (function_to_apply == "sum") {
        df_timeseries_wide[column_name] <- rowSums(df_timeseries_wide[, i:j], na.rm = TRUE)
      }
    }
  }

  for (i in 3:max_col) {
    colnames(df_timeseries_wide)[i] <- paste0(
      colnames(df_timeseries_wide)[i], "_",
      sub(".*\\_", "", colnames(df_timeseries_wide)[i])
    )
  }

  df_timeseries_wide
}

df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ, "RR", "sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ, "TM", "mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ, "UM", "mean")

df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide3)

df_meteo_predictions <- df_meteo_pieges_summ_wide_meteofrance %>%
  dplyr::select(site, th_date,
                TM_0_8, UM_5_11,          # prédicteurs présence
                TM_0_4, UM_0_11, RR_1_5,  # prédicteurs abondance
                TM_0_0, RR_0_0, TM_0_5, UM_1_10, RR_1_10) %>%
  rename(date = th_date) %>%
  na.omit()


######################################################
######### Chargement des modèles
######################################################

res_presence  <- readRDS(file.path(path_models, "res_presence_LOSO_probabilistic.rds"))
res_abundance <- readRDS(file.path(path_models, "res_abundance_LOSO_quantile_rf.rds"))
res_train     <- readRDS(file.path(path_models, "res_training_data.rds"))

mod_presence   <- res_presence$model
rf_abundance_q <- res_abundance$model_quantile

predictors_presence  <- c("TM_0_8", "UM_5_11")
predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")


######################################################
######### Génération des prédictions
######################################################

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
      pred_presence_prob    = p,
      pred_presence_var     = p * (1 - p),
      pred_presence_entropy = -(p * log(pmax(p, 1e-8)) + (1 - p) * log(pmax(1 - p, 1e-8))),
      pred_log_abundance_q05 = pred_q[, 1],
      pred_log_abundance_q50 = pred_q[, 2],
      pred_log_abundance_q95 = pred_q[, 3],
      pred_abundance_q05     = exp(pred_log_abundance_q05),
      pred_abundance_q50     = exp(pred_log_abundance_q50),
      pred_abundance_q95     = exp(pred_log_abundance_q95),
      pred_expected_abundance = pred_presence_prob * pred_abundance_q50
    )

  sim_res <- purrr::map_dfr(seq_len(nrow(out)), function(i) {
    p_i  <- out$pred_presence_prob[i]
    mu_i <- out$pred_log_abundance_q50[i]
    sd_i <- pmax((out$pred_log_abundance_q95[i] - out$pred_log_abundance_q05[i]) / (2 * 1.645), 1e-6)
    y_sim <- rbinom(n_sim, 1, p_i) * exp(rnorm(n_sim, mu_i, sd_i))
    tibble(row_id = i,
           pred_combined_mean = mean(y_sim),
           pred_combined_q05  = quantile(y_sim, 0.05),
           pred_combined_q50  = quantile(y_sim, 0.50),
           pred_combined_q95  = quantile(y_sim, 0.95),
           pred_combined_sd   = sd(y_sim))
  })

  out %>%
    mutate(row_id = seq_len(n())) %>%
    left_join(sim_res, by = "row_id") %>%
    mutate(pred_thresholded = ifelse(pred_presence_prob > 0.5, pred_abundance_q50, 0))
}

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


######################################################
######### Calcul des valeurs SHAP
######################################################

# #new (7 - SHAP toutes variables) ====
# SHAP calculé sur les données de prédiction courantes pour les deux modèles
# Les données d'entraînement (res_train) servent de référence pour le baseline

# SHAP — modèle d'abondance (ranger direct)
X_abundance_pred <- as.data.frame(df_meteo_predictions[, predictors_abundance])
shap_abundance   <- compute_shap(rf_abundance_q, X_abundance_pred, model_type = "ranger")
if (!is.null(shap_abundance)) {
  colnames(shap_abundance) <- gsub("^shap_", "shap_abund_", colnames(shap_abundance))
  df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_abundance)
} else {
  cat("⚠ SHAP abondance non disponible\n")
}

# SHAP — modèle de présence (caret/ranger)
# Note : les forêts de classification par probabilité (caret) ne sont pas toujours
# compatibles avec ranger.unify — le pipeline continue sans SHAP présence si ça échoue
X_presence_pred <- as.data.frame(df_meteo_predictions[, predictors_presence])
shap_presence   <- compute_shap(mod_presence, X_presence_pred, model_type = "caret_ranger")
if (!is.null(shap_presence)) {
  colnames(shap_presence) <- gsub("^shap_", "shap_pres_", colnames(shap_presence))
  df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_presence)
} else {
  cat("⚠ SHAP présence non disponible (modèle caret/classification non supporté par treeshap)\n")
}
# ==============


######################################################
######### Snapping des coordonnées pour rasterisation
######################################################

df_meteo_predictions <- df_meteo_predictions %>%
  mutate(
    X_snap = round(X / grid_res) * grid_res,
    Y_snap = round(Y / grid_res) * grid_res
  )


######################################################
######### Agrégation par commune — prédictions entomo
######################################################

# #new (6 - Factorisation rasterisation) ====
df_meteo_predictions$date <- as.character(df_meteo_predictions$date)

abundance <- rasterize_to_communes(df_meteo_predictions, "pred_combined_mean", roi) %>%
  rename(mean_abundance_albopictus = pred_combined_mean) %>%
  dplyr::select(codgeo, libgeo, date, mean_abundance_albopictus) %>%
  mutate(
    mean_abundance_albopictus = round(mean_abundance_albopictus, 1),
    date = date + 1   # décalage d'un jour
  )
# ==============


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
    Y_snap = round(Y / grid_res) * grid_res,
    date   = as.character(date)
  )

# #new (6 - Factorisation rasterisation) ====
mean_temperature <- rasterize_to_communes(meteo_comm, "TM", roi) %>%
  rename(mean_temperature = TM) %>%
  dplyr::select(codgeo, libgeo, date, mean_temperature) %>%
  mutate(mean_temperature = round(mean_temperature, 1))

mean_rainfall <- rasterize_to_communes(meteo_comm, "RR", roi) %>%
  rename(mean_rainfall = RR) %>%
  dplyr::select(codgeo, libgeo, date, mean_rainfall) %>%
  mutate(mean_rainfall = round(mean_rainfall, 1))

mean_humidity <- rasterize_to_communes(meteo_comm, "UM", roi) %>%
  rename(mean_humidity = UM) %>%
  dplyr::select(codgeo, libgeo, date, mean_humidity) %>%
  mutate(mean_humidity = round(mean_humidity, 1))
# ==============

meteo_out <- left_join(mean_temperature, mean_rainfall) %>%
  left_join(mean_humidity) %>%
  mutate(date = date + 2)   # décalage de deux jours


######################################################
######### Construction de la table de prédictions
######################################################

albopictus_predictions <- left_join(meteo_out, abundance) %>%
  mutate(
    date_fin    = date + 7,
    last_update = as.Date(Sys.Date())
  ) %>%
  relocate(date_fin, .after = date)

thresh_orange_red <- median(
  albopictus_predictions$mean_abundance_albopictus[
    which(albopictus_predictions$mean_abundance_albopictus > 0 &
          !is.na(albopictus_predictions$mean_abundance_albopictus))
  ]
)

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
      is.na(mean_abundance_albopictus)        ~ NA_real_,
      is.na(lag(mean_abundance_albopictus))   ~ NA_real_,
      lag(mean_abundance_albopictus) == 0     ~ NA_real_,
      TRUE ~ 100 * (mean_abundance_albopictus - lag(mean_abundance_albopictus)) /
        lag(mean_abundance_albopictus)
    )
  ) %>%
  ungroup() %>%
  mutate(trend = ifelse(is.na(trend), 0, round(trend))) %>%
  mutate(class_trend = case_when(
    is.na(trend) ~ "Stable",
    trend > 20   ~ "En hausse",
    trend < -20  ~ "En baisse",
    TRUE         ~ "Stable"
  ))

# Publication table principale
st_write(albopictus_predictions, dsn = con, layer = db_layer, append = FALSE)
cat("✓ Prédictions publiées dans la table", db_layer, "\n")


######################################################
######### Table SHAP — agrégation SHAP par commune
######################################################

# #new (7 - SHAP toutes variables) ====
# Agrégation spatiale de toutes les colonnes SHAP par commune
# Abondance : shap_abund_<var> | Présence : shap_pres_<var>

shap_cols_abund <- grep("^shap_abund_TM|^shap_abund_UM|^shap_abund_RR",
                         colnames(df_meteo_predictions), value = TRUE)
shap_cols_pres  <- grep("^shap_pres_TM|^shap_pres_UM",
                         colnames(df_meteo_predictions), value = TRUE)
all_shap_cols   <- c(shap_cols_abund, shap_cols_pres)

if (length(all_shap_cols) > 0) {

  # Rasteriser chaque colonne SHAP et extraire par commune
  shap_comm <- purrr::reduce(
    all_shap_cols[-1],
    function(acc, col) {
      comm_col <- rasterize_to_communes(df_meteo_predictions, col, roi) %>%
        dplyr::select(codgeo, date, dplyr::all_of(col)) %>%
        mutate(dplyr::across(dplyr::all_of(col), ~round(.x, 4)))
      left_join(acc, comm_col, by = c("codgeo", "date"))
    },
    .init = rasterize_to_communes(df_meteo_predictions, all_shap_cols[1], roi) %>%
      dplyr::select(codgeo, date, dplyr::all_of(all_shap_cols[1])) %>%
      mutate(dplyr::across(dplyr::all_of(all_shap_cols[1]), ~round(.x, 4)))
  )

  # Variable dominante par commune — abondance (si disponible)
  if (length(shap_cols_abund) > 0) {
    shap_comm <- shap_comm %>%
      mutate(
        shap_abund_dominant_var = gsub("shap_abund_", "", shap_cols_abund[
          apply(abs(dplyr::select(., dplyr::all_of(shap_cols_abund))), 1, which.max)
        ]),
        shap_abund_dominant_val = apply(
          dplyr::select(., dplyr::all_of(shap_cols_abund)), 1,
          function(x) x[which.max(abs(x))]
        )
      )
  }

  # Variable dominante par commune — présence (si disponible)
  if (length(shap_cols_pres) > 0) {
    shap_comm <- shap_comm %>%
      mutate(
        shap_pres_dominant_var = gsub("shap_pres_", "", shap_cols_pres[
          apply(abs(dplyr::select(., dplyr::all_of(shap_cols_pres))), 1, which.max)
        ]),
        shap_pres_dominant_val = apply(
          dplyr::select(., dplyr::all_of(shap_cols_pres)), 1,
          function(x) x[which.max(abs(x))]
        )
      )
  }

  shap_comm <- shap_comm %>% mutate(date = date + 1)

  albopictus_predictions_shap <- albopictus_predictions %>%
    left_join(shap_comm, by = c("codgeo", "date"))

} else {
  cat("⚠ Aucune colonne SHAP disponible — table SHAP identique à la table principale\n")
  albopictus_predictions_shap <- albopictus_predictions
}

st_write(albopictus_predictions_shap, dsn = con, layer = db_layer_shap, append = FALSE)
cat("✓ Prédictions + SHAP publiées dans la table", db_layer_shap, "\n")
# ==============

dbDisconnect(con)
cat("\n✓ Pipeline hebdomadaire terminé.\n")
