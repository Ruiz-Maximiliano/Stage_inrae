# ============================================================
# SCRIPT 2 — Pipeline hebdomadaire
# À exécuter chaque semaine sur le serveur (cron job)
# Prérequis : Script 1 (initialisation) déjà exécuté
#
# CE QUE FAIT CE CODE (dans l'ordre) :
#   1. Met à jour la météo en BD : remplace le forecast de la semaine passée par
#      les vraies données historiques, télécharge le forecast de la semaine à venir.
#      → Agrégation par commune AVANT écriture (nouveau schéma).
#   2. Lit uniquement les lag_max derniers jours de météo (optim. Paul) — au lieu
#      de toute la table — et construit les variables retardées (lags TM/RR/UM).
#   3. Charge les modèles entraînés et génère les prédictions two-part.
#   4. Calcule le SHAP pour les 4 modèles.
#   5. Publie 1 table en BD (db_layer) avec prédictions + SHAP.
#      Plus de rasterize_to_communes pour météo/prédictions — tout est déjà au
#      niveau commune depuis la BD.
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Tous viennent de config.R. force_recompute (défaut FALSE) force le recalcul.
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   - config.R, 00_functions.R, models/*.rds
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
# new: logs =======
library(logr)
# ==============

source(here("scripts", "00_functions.R"))
source(here("config.R"))

# new: logs =======
dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)
lf <- log_open(
  here("logs", paste0("hebdomadaire_", Sys.Date(), ".log")),
  autolog    = TRUE,
  show_notes = FALSE
)
log_print(paste("=== Run hebdomadaire —", Sys.time(), "==="))
# ==============

options(datatable.week = "legacy")

# ============================================================
# Paramètres locaux
# ============================================================
path_models     <- here("models")
grid_res        <- 0.05
n_days_forecast <- 14
lag_max         <- 84

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
# Chargement du ROI et du grid
# ============================================================

roi <- sf::st_read(con, db_table_admin) %>%
  dplyr::filter(dep == admin_dep, level == admin_level)
roi <- st_transform(roi, 4326)

# fix warning : "Spherical geometry switched off/on" et "assumes planar" — messages cosmétiques supprimés
suppressMessages({
  sf::sf_use_s2(FALSE)
  roi <- st_make_valid(roi)
  geopolygon <- st_union(roi)
  sf::sf_use_s2(TRUE)
})

# roi_info : codgeo/libgeo sans géométrie — pour les jointures sur les tables publiées
roi_info   <- sf::st_drop_geometry(roi) %>% dplyr::select(codgeo, libgeo)
all_codgeo <- as.character(unique(roi$codgeo))

# coords : grid de points météo (toujours nécessaire pour télécharger les données brutes)
coords <- make_grid(geopolygon, roi_bbox, grid_res)

# meteo_prep : batches de coordonnées pour les appels API
meteo_prep <- coords %>%
  group_by(row_number() %/% 20) %>%
  group_map(~.x) %>%
  map(., ~group_split(., site))


######################################################
######### Mise à jour des données météo
######################################################

# new (colonne is_forecast) ====
ensure_is_forecast_column(con, db_table_meteo)
# ==============

# Lecture légère : seulement les 7 derniers jours pour détecter ce qui est encore forecast
# (optim. Paul — pas besoin de lire 10 ans pour ce check)
meteo_recent <- dbGetQuery(con, sprintf(
  "SELECT date, is_forecast FROM %s WHERE date >= '%s' AND date < '%s'",
  db_table_meteo,
  as.character(Sys.Date() - 7),
  as.character(Sys.Date())
)) %>% as.data.table()
meteo_recent$date <- as.Date(meteo_recent$date)

# ---- Étape 1 : Remplacer forecast de la semaine passée par historical ----
# new (fix — ne retélécharger que ce qui est ENCORE marqué forecast) ====
dates_a_remplacer <- unique(meteo_recent$date[meteo_recent$is_forecast %in% TRUE])
# ==============

# new: logs =======
log_print(paste("Dates à remplacer (forecast → historical) :", length(dates_a_remplacer)))
# ==============

if (length(dates_a_remplacer) > 0) {
  cat("Remplacement forecast -> historical pour", length(dates_a_remplacer), "dates\n")

  meteo_updated <- data.frame()

  for (i in seq_along(meteo_prep)) {
    cat("Mise à jour historical — paquet", i, "sur", length(meteo_prep), "\n")

    batch_df   <- dplyr::bind_rows(meteo_prep[[i]])
    th_res_api <- get_weather_history_batch(
      latitudes  = batch_df$Y,
      longitudes = batch_df$X,
      start_date = min(dates_a_remplacer),
      end_date   = max(dates_a_remplacer),
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
    meteo_updated <- rbind(meteo_updated, th_res)
    Sys.sleep(1)
  }

  # Agréger par commune avant écriture
  comm_updated <- aggregate_meteo_to_communes(meteo_updated, roi, grid_res)
  comm_updated$is_forecast <- FALSE

  dates_sql <- paste(paste0("'", dates_a_remplacer, "'"), collapse = ",")
  dbExecute(con, sprintf("DELETE FROM %s WHERE date IN (%s)", db_table_meteo, dates_sql))
  dbWriteTable(con, db_table_meteo, as.data.frame(comm_updated), append = TRUE, row.names = FALSE)

  cat("✓ Remplacement historique écrit en BD (", nrow(comm_updated), "lignes)\n")
} else {
  cat("✓ Historique déjà à jour — aucune date encore marquée forecast dans les 7 derniers jours\n")
}

# ---- Étape 2 : Télécharger la nouvelle semaine de forecast ----

# new (vérification fraîcheur forecast) ====
# Si appelé depuis 01_initialisation.R, le forecast vient d'être téléchargé —
# on saute la re-vérification pour éviter une double écriture en BD.
if (exists("init_forecast_done") && isTRUE(init_forecast_done)) {
  forecast_needed <- FALSE
  cat("✓ Forecast déjà téléchargé par l'initialisation — téléchargement ignoré\n")
} else {
  forecast_check    <- dbGetQuery(con, sprintf(
    "SELECT codgeo::text, COUNT(DISTINCT date) AS n_dates FROM %s WHERE date >= '%s' GROUP BY codgeo",
    db_table_meteo, as.character(Sys.Date())
  ))
  communes_ok     <- forecast_check$codgeo[forecast_check$n_dates >= n_days_forecast]
  forecast_needed <- !all(all_codgeo %in% communes_ok)
}

meteo_future <- data.frame()

if (forecast_needed) {
  cat("Téléchargement du forecast...\n")

  for (i in seq_along(meteo_prep)) {
    cat("Forecast — paquet", i, "sur", length(meteo_prep), "\n")

    batch_df   <- dplyr::bind_rows(meteo_prep[[i]])
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
    Sys.sleep(1)
  }

  # Agréger par commune avant écriture
  comm_future <- aggregate_meteo_to_communes(meteo_future, roi, grid_res)
  comm_future$is_forecast <- TRUE

  dbExecute(con, sprintf("DELETE FROM %s WHERE date >= '%s'", db_table_meteo, Sys.Date()))
  dbWriteTable(con, db_table_meteo, as.data.frame(comm_future), append = TRUE, row.names = FALSE)
  cat("✓ Forecast écrit en BD (", nrow(comm_future), "lignes)\n")
} else {
  cat("✓ Forecast déjà à jour en BD pour les", n_days_forecast,
      "jours à venir — téléchargement ignoré\n")
}
# ==============

# new: logs =======
log_print(paste("Forecast nécessaire :", forecast_needed))
# ==============

######################################################
######### Création des variables indépendantes
######################################################

# Optim. Paul : lire seulement les lag_max derniers jours au lieu de toute la table.
# Les lags vont au maximum à (forecast_monday - lag_max) ≈ Sys.Date() - lag_max,
# donc on n'a besoin de rien de plus ancien.
cat("Lecture de la météo (derniers", lag_max, "jours) depuis la BD...\n")
meteo <- dbGetQuery(con, sprintf(
  "SELECT * FROM %s WHERE date >= '%s'",
  db_table_meteo, as.character(Sys.Date() - lag_max)
)) %>% as.data.table()
meteo$date <- as.Date(meteo$date)

# meteo2 : lundis du forecast uniquement (filtre date >= Sys.Date() — tâche 2).
# Données déjà au niveau commune (codgeo) — pas besoin de site/X/Y.
meteo2 <- meteo %>%
  dplyr::select(codgeo, date) %>%
  mutate(year = year(date), week = week(date), weekday = wday(date)) %>%
  filter(weekday == 1, date >= Sys.Date()) %>%
  slice(rep(1:n(), each = lag_max)) %>%
  group_by(codgeo, year, week) %>%
  mutate(lag_n = row_number()) %>%
  ungroup() %>%
  dplyr::select(-weekday) %>%
  rename(th_date = date) %>%
  mutate(date = th_date - lag_n) %>%
  data.table()

# meteo3 : jointure avec les valeurs météo historiques pour chaque lag
meteo3 <- meteo2 %>%
  left_join(meteo %>% dplyr::select(codgeo, date, TM, RR, UM),
            by = c("date", "codgeo")) %>%
  pivot_longer(c(TM, RR, UM), names_to = "var", values_to = "val") %>%
  data.table()

fun_summarize_week <- function(meteo3, var_to_summarize, fun_summarize,
                                new_var_name, n_days_agg) {

  if (fun_summarize == "sum") {
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = sum(val, na.rm = TRUE), date = max(date)),
          by = .(codgeo, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(codgeo, th_date)][
              , var := new_var_name][, year := NULL]

  } else if (fun_summarize == "mean") {
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = mean(val, na.rm = TRUE), date = max(date)),
          by = .(codgeo, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(codgeo, th_date)][
              , var := new_var_name][, year := NULL]

  } else if (fun_summarize == "max") {
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = max(val, na.rm = TRUE), date = max(date)),
          by = .(codgeo, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(codgeo, th_date)][
              , var := new_var_name][, year := NULL]

  } else if (fun_summarize == "min") {
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][
          , .(val = min(val, na.rm = TRUE), date = max(date)),
          by = .(codgeo, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(codgeo, th_date)][
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

# df_meteo_predictions : prédicteurs par commune x semaine
df_meteo_predictions <- df_meteo_pieges_summ_wide_meteofrance %>%
  dplyr::select(codgeo, th_date,
                TM_0_8, UM_5_11,          # prédicteurs présence
                TM_0_4, UM_0_11, RR_1_5,  # prédicteurs abondance
                TM_0_0, RR_0_0, TM_0_5, UM_1_10, RR_1_10) %>%
  rename(date = th_date) %>%
  na.omit()


# new (skip recalcul/republication si rien n'a changé) ====
force_recompute <- FALSE
meteo_changed   <- (length(dates_a_remplacer) > 0) || forecast_needed
db_layer_exists <- dbExistsTable(con, db_layer)
skip_recompute  <- !force_recompute && !meteo_changed && db_layer_exists

# new: logs =======
log_print(paste("meteo_changed :", meteo_changed,
                "| db_layer_exists :", db_layer_exists,
                "| skip_recompute :", skip_recompute))
# ==============

if (!skip_recompute) {
######################################################
######### Chargement des modèles
######################################################

res_presence  <- readRDS(file.path(path_models, "res_presence_LOSO_probabilistic.rds"))
res_abundance <- readRDS(file.path(path_models, "res_abundance_LOSO_quantile_rf.rds"))
res_train     <- readRDS(file.path(path_models, "res_training_data.rds"))

mod_presence   <- res_presence$model
rf_abundance_q <- res_abundance$model_quantile
mod_abundance_cv <- res_abundance$model_cv

predictors_presence  <- c("TM_0_8", "UM_5_11")
predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")


######################################################
######### Génération des prédictions
######################################################

# predict_two_part_uncertainty() définie dans 00_functions.R
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

# SHAP — modèle d'abondance (ranger direct)
# fix warning : "keep.inbag = TRUE" — modèles entraînés sans cette option, SHAP approché mais fonctionnel
X_abundance_pred <- as.data.frame(df_meteo_predictions[, predictors_abundance])
shap_abundance   <- suppressWarnings(compute_shap(rf_abundance_q, X_abundance_pred, model_type = "ranger"))
if (!is.null(shap_abundance)) {
  colnames(shap_abundance) <- gsub("^shap_", "shap_abund_", colnames(shap_abundance))
  df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_abundance)
} else {
  cat("⚠ SHAP abondance non disponible\n")
}

# SHAP — modèle de présence (forêt de probabilité — calcul exact maison)
X_presence_pred <- as.data.frame(df_meteo_predictions[, predictors_presence])
shap_presence   <- suppressWarnings(compute_shap(mod_presence, X_presence_pred, model_type = "caret_ranger",
                                 X_background = res_train$X_presence))
if (!is.null(shap_presence)) {
  colnames(shap_presence) <- gsub("^shap_", "shap_pres_", colnames(shap_presence))
  df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_presence)
} else {
  cat("⚠ SHAP présence non disponible\n")
}

# SHAP combined (approximation par règle du produit)
if (!is.null(shap_abundance)) {
  shap_combined_abund <- shap_abundance %>%
    dplyr::select(dplyr::starts_with("shap_abund_") &
                  !dplyr::ends_with(c("dominant_var", "dominant_val"))) %>%
    `*`(df_meteo_predictions$pred_presence_prob)
  colnames(shap_combined_abund) <- gsub("^shap_abund_", "shap_combined_", colnames(shap_combined_abund))
  df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_combined_abund)
}

if (!is.null(shap_presence)) {
  shap_combined_pres <- shap_presence %>%
    dplyr::select(dplyr::starts_with("shap_pres_") &
                  !dplyr::ends_with(c("dominant_var", "dominant_val"))) %>%
    `*`(df_meteo_predictions$pred_abundance_q50)
  colnames(shap_combined_pres) <- gsub("^shap_pres_", "shap_combined_", colnames(shap_combined_pres))
  df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_combined_pres)
}

if (is.null(shap_abundance) && is.null(shap_presence)) {
  cat("⚠ SHAP combined non disponible\n")
}

# SHAP — modèle d'abondance caret (comparaison)
shap_abundance_cv <- suppressWarnings(compute_shap(mod_abundance_cv, X_abundance_pred, model_type = "caret_ranger"))
if (!is.null(shap_abundance_cv)) {
  colnames(shap_abundance_cv) <- gsub("^shap_", "shap_abundcv_", colnames(shap_abundance_cv))
  df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_abundance_cv)
} else {
  cat("⚠ SHAP abondance (modèle caret) non disponible\n")
}


######################################################
######### Agrégation par commune — prédictions entomo
######################################################

# Nouveau schéma : df_meteo_predictions est déjà par commune (codgeo).
# Plus besoin de rasterize_to_communes() — on sélectionne directement.

abundance <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_combined_mean) %>%
  dplyr::rename(mean_abundance_albopictus = pred_combined_mean) %>%
  dplyr::mutate(
    mean_abundance_albopictus = round(mean_abundance_albopictus, 1),
    date = date + 1   # décalage d'un jour
  ) %>%
  dplyr::left_join(roi_info, by = "codgeo")

combined_q05 <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_combined_q05) %>%
  dplyr::rename(combined_abundance_q05 = pred_combined_q05) %>%
  dplyr::mutate(combined_abundance_q05 = round(combined_abundance_q05, 1), date = date + 1)

combined_q95 <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_combined_q95) %>%
  dplyr::rename(combined_abundance_q95 = pred_combined_q95) %>%
  dplyr::mutate(combined_abundance_q95 = round(combined_abundance_q95, 1), date = date + 1)

combined_sd <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_combined_sd) %>%
  dplyr::rename(combined_abundance_sd = pred_combined_sd) %>%
  dplyr::mutate(combined_abundance_sd = round(combined_abundance_sd, 2), date = date + 1)

abundance <- abundance %>%
  left_join(combined_q05, by = c("codgeo", "date")) %>%
  left_join(combined_q95, by = c("codgeo", "date")) %>%
  left_join(combined_sd,  by = c("codgeo", "date"))


######################################################
######### Agrégation par commune — données météo
######################################################

# df_meteo_pieges_summ est déjà par codgeo — lag_n == 0 = semaine courante
meteo_out <- df_meteo_pieges_summ %>%
  filter(lag_n == 0) %>%
  dplyr::select(-c("th_date", "lag_n")) %>%
  pivot_wider(names_from = var, values_from = val) %>%
  dplyr::rename(mean_temperature = TM,
                mean_rainfall    = RR,
                mean_humidity    = UM) %>%
  dplyr::mutate(
    mean_temperature = round(mean_temperature, 1),
    mean_rainfall    = round(mean_rainfall,    1),
    mean_humidity    = round(mean_humidity,    1),
    date             = date + 2   # décalage de deux jours
  ) %>%
  dplyr::left_join(roi_info, by = "codgeo")


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



######################################################
######### Table SHAP — directement par commune
######################################################

# Nouveau schéma : SHAP déjà calculé par commune dans df_meteo_predictions.
# Plus de rasterize_to_communes() — on sélectionne et on arrondit directement.

shap_cols_abund    <- grep("^shap_abund_TM|^shap_abund_UM|^shap_abund_RR",
                            colnames(df_meteo_predictions), value = TRUE)
shap_cols_pres     <- grep("^shap_pres_TM|^shap_pres_UM",
                            colnames(df_meteo_predictions), value = TRUE)
shap_cols_combined <- grep("^shap_combined_TM|^shap_combined_UM|^shap_combined_RR",
                            colnames(df_meteo_predictions), value = TRUE)
shap_cols_abundcv  <- grep("^shap_abundcv_TM|^shap_abundcv_UM|^shap_abundcv_RR",
                            colnames(df_meteo_predictions), value = TRUE)
all_shap_cols      <- c(shap_cols_abund, shap_cols_pres, shap_cols_combined, shap_cols_abundcv)

if (length(all_shap_cols) > 0) {

  shap_comm <- df_meteo_predictions %>%
    dplyr::select(codgeo, date, dplyr::all_of(all_shap_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(all_shap_cols), ~round(.x, 4))) %>%
    dplyr::mutate(date = date + 1)

  # Variable dominante — abondance
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

  # Variable dominante — présence
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

  # Variable dominante — combined
  if (length(shap_cols_combined) > 0) {
    shap_comm <- shap_comm %>%
      mutate(
        shap_combined_dominant_var = gsub("shap_combined_", "", shap_cols_combined[
          apply(abs(dplyr::select(., dplyr::all_of(shap_cols_combined))), 1, which.max)
        ]),
        shap_combined_dominant_val = apply(
          dplyr::select(., dplyr::all_of(shap_cols_combined)), 1,
          function(x) x[which.max(abs(x))]
        )
      )
  }

  # Variable dominante — abondance caret
  if (length(shap_cols_abundcv) > 0) {
    shap_comm <- shap_comm %>%
      mutate(
        shap_abundcv_dominant_var = gsub("shap_abundcv_", "", shap_cols_abundcv[
          apply(abs(dplyr::select(., dplyr::all_of(shap_cols_abundcv))), 1, which.max)
        ]),
        shap_abundcv_dominant_val = apply(
          dplyr::select(., dplyr::all_of(shap_cols_abundcv)), 1,
          function(x) x[which.max(abs(x))]
        )
      )
  }

  albopictus_predictions_shap <- albopictus_predictions %>%
    left_join(shap_comm, by = c("codgeo", "date"))

} else {
  cat("⚠ Aucune colonne SHAP disponible — table SHAP identique à la table principale\n")
  albopictus_predictions_shap <- albopictus_predictions
}

st_write(albopictus_predictions_shap, dsn = con, layer = db_layer, append = FALSE)
cat("✓ Prédictions + SHAP publiées dans la table", db_layer, "\n")
# new: logs =======
log_print(paste("✓ Prédictions + SHAP publiées →", db_layer,
                "| communes :", length(unique(albopictus_predictions_shap$codgeo)),
                "| colonnes SHAP :", length(all_shap_cols)))
# ==============

} else {
  cat("✓ Aucune nouvelle donnée météo — prédictions/SHAP non recalculés, table",
      db_layer, "inchangée.\n",
      "  (Mettre force_recompute <- TRUE pour forcer.)\n", sep = "")
}

dbDisconnect(con)
cat("\n✓ Pipeline hebdomadaire terminé.\n")
# new: logs =======
log_print(paste("=== Fin du run hebdomadaire —", Sys.time(), "==="))
log_close()
# ==============
