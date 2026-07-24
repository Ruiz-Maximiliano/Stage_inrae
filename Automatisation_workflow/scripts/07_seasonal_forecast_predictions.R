# ============================================================
# SCRIPT 7 — Prédictions à partir du forecast SAISONNIER (exploratoire / test)
# À exécuter MANUELLEMENT. Ne fait JAMAIS d'écriture dans meteo_ruiz ni
# albopictus_ruiz_test (tables de production) — publie uniquement dans
# db_layer_seasonal ("test_seasonal_ruiz" par défaut).
#
# CE QUE FAIT CE CODE :
#   1. Télécharge le forecast saisonnier (moyenne d'ensemble, ~6 mois par
#      défaut) via get_weather_seasonal_forecast_batch() (00_functions_api.R),
#      pour tous les points du grid — même découpage par paquets de 20 que
#      le forecast classique dans 02_hebdomadaire.R.
#   2. Agrège par commune (aggregate_meteo_to_roi(), identique au reste
#      du pipeline).
#   3. Lit UNIQUEMENT en lecture la météo réelle récente de meteo_ruiz (les
#      lag_max=84 derniers jours) — sert de "buffer" pour calculer les lags
#      des toutes premières semaines du forecast saisonnier. meteo_ruiz n'est
#      jamais modifiée.
#   4. Concatène météo réelle récente + forecast saisonnier en une seule série
#      continue par commune, puis construit les lags (TM_0_8, UM_5_11, TM_0_4,
#      UM_0_11, RR_1_5) pour TOUS les lundis du forecast saisonnier (~26
#      semaines) via fun_summarize_week()/fun_ccm_df() (00_functions_formats.R,
#      partagées avec 02_hebdomadaire.R).
#   5. Prédit avec le modèle COMBINÉ habituel (predict_two_part_uncertainty())
#      — présence x abondance, exactement comme partout ailleurs dans le
#      pipeline. Pas de SHAP dans cette version (coûterait ~1.5-2h de plus
#      pour une table exploratoire — voir 05_backfill_shap.R pour le détail
#      de ce coût).
#   6. Publie dans db_layer_seasonal (table de TEST, overwrite = TRUE à chaque
#      run — un nouveau run remplace entièrement l'ancien forecast saisonnier,
#      qui n'a de sens que "vu depuis aujourd'hui").
#
# LIMITE ASSUMÉE : pour les semaines à 4-6 mois, les lags (jusqu'à 84 jours en
# arrière) portent sur des données qui sont ELLES-MÊMES du forecast
# saisonnier, pas de la météo réelle — assumé comme acceptable pour cette
# première version (prévision sur prévision, incertitude croissante avec
# l'horizon, cohérent avec la nature même d'un forecast saisonnier).
#
# PARAMÈTRES À AJUSTER :
#   n_months_seasonal → horizon en mois (1-9, limite de l'API), défaut 6.
#   seasonal_model    → NULL (défaut API, cfs_v2 4 membres) ou "ecmwf_ifs04"
#                        (51 membres, jusqu'à 7 mois) — voir
#                        get_weather_seasonal_forecast_batch() pour le détail.
#   db_layer_seasonal → nom de la table de sortie (test, pas de production).
# ============================================================

library(here)
library(terra)
library(sf)
library(purrr)
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

source(here("scripts", "00_functions_api.R"))
source(here("scripts", "00_functions_formats.R"))
source(here("scripts", "00_functions_models.R"))
source(here("config.R"))

options(datatable.week = "legacy")

# ============================================================
# Paramètres de ce script
# ============================================================
n_months_seasonal <- 6                      # <-- AJUSTER ICI : horizon en mois (1-9)
seasonal_model    <- NULL                   # <-- AJUSTER ICI : NULL = défaut API (cfs_v2)
db_layer_seasonal <- "test_seasonal_ruiz"   # <-- table de TEST, jamais albopictus_ruiz_test
lag_max           <- 84

cat("=== Prédictions saisonnières —", as.character(Sys.time()), "===\n")
cat("Horizon :", n_months_seasonal, "mois | Table de sortie :", db_layer_seasonal, "\n")

# ============================================================
# Connexion BD
# ============================================================
con <- dbConnect(
  RPostgres::Postgres(),
  host            = db_host,
  dbname          = db_name,
  port            = db_port,
  user            = db_user,
  password        = db_password,
  keepalives      = 1L,
  keepalives_idle = 60L
)
dbExecute(con, "SET statement_timeout = 0")

# ============================================================
# ROI (déjà chargé par config.R) + grid
# ============================================================
# roi vient de config.R (lu une seule fois là-bas, connexion temporaire —
# voir config.R) — plus besoin de le recharger ici.

sf::sf_use_s2(FALSE)
roi <- st_make_valid(roi)
geopolygon <- st_union(roi)
sf::sf_use_s2(TRUE)

roi_info <- sf::st_drop_geometry(roi) %>% dplyr::select(codgeo, libgeo)
coords   <- make_grid(geopolygon, roi_bbox, grid_res)

meteo_prep <- coords %>%
  group_by(row_number() %/% 20) %>%
  group_map(~.x) %>%
  map(., ~group_split(., site))

######################################################
######### 1. Téléchargement du forecast saisonnier
######################################################

cat("Téléchargement du forecast saisonnier (", length(meteo_prep), "paquets )...\n")
meteo_seasonal <- data.frame()

for (i in seq_along(meteo_prep)) {
  cat("Forecast saisonnier — paquet", i, "sur", length(meteo_prep), "\n")

  batch_df   <- dplyr::bind_rows(meteo_prep[[i]])
  th_res_api <- get_weather_seasonal_forecast_batch(
    latitudes  = batch_df$Y,
    longitudes = batch_df$X,
    n_months   = n_months_seasonal,
    model      = seasonal_model
  )

  th_res <- th_res_api %>%
    dplyr::left_join(unique(batch_df[, c("X", "Y", "site")]),
                     by = c("longitude" = "X", "latitude" = "Y")) %>%
    dplyr::rename(date = time,
                  TM   = temperature_2m_mean,
                  RR   = precipitation_sum) %>%
    dplyr::mutate(date = as.Date(as.character(date)),
                  X = longitude, Y = latitude)

  # UM peut être absente selon le modèle saisonnier (voir doc de
  # get_weather_seasonal_forecast_batch() dans 00_functions_api.R)
  if ("relative_humidity_2m_mean" %in% colnames(th_res)) {
    th_res <- th_res %>% dplyr::rename(UM = relative_humidity_2m_mean)
  } else {
    th_res$UM <- NA_real_
  }

  meteo_seasonal <- rbind(meteo_seasonal, th_res)
  Sys.sleep(1)
}

if (all(is.na(meteo_seasonal$UM))) {
  stop("UM (humidité) indisponible pour tous les points avec ce modèle saisonnier — ",
       "le modèle de présence a besoin de UM_5_11, impossible de continuer. ",
       "Essayer un autre modèle via seasonal_model (voir get_weather_seasonal_forecast_batch()).")
}

cat("✓ Forecast saisonnier téléchargé (", nrow(meteo_seasonal), "lignes brutes par point )\n")

# Agrégation par commune — identique à la météo régulière du pipeline
comm_seasonal <- aggregate_meteo_to_roi(meteo_seasonal, roi, grid_res)
cat("✓ Agrégé par commune (", nrow(comm_seasonal), "lignes commune x date )\n")

######################################################
######### 2. Météo réelle récente (buffer de lags) — LECTURE SEULE
######################################################

read_from <- Sys.Date() - lag_max
cat("Lecture météo réelle récente depuis", format(read_from),
    "(buffer de", lag_max, "jours pour les lags — meteo_ruiz en lecture seule)...\n")

meteo_recent <- dbGetQuery(con, sprintf(
  "SELECT codgeo, date, \"TM\", \"RR\", \"UM\" FROM %s WHERE date >= '%s' AND date < '%s'",
  db_table_meteo, as.character(read_from), as.character(Sys.Date())
)) %>% as.data.table()
meteo_recent$date <- as.Date(meteo_recent$date)
meteo_recent <- meteo_recent %>% dplyr::distinct(codgeo, date, .keep_all = TRUE)

######################################################
######### 3. Série continue : météo réelle récente + forecast saisonnier
######################################################

meteo <- rbind(
  meteo_recent,
  comm_seasonal %>% dplyr::select(codgeo, date, TM, RR, UM)
) %>%
  dplyr::distinct(codgeo, date, .keep_all = TRUE) %>%
  as.data.table()

cat("✓ Série continue construite —", nrow(meteo), "lignes (",
    format(min(meteo$date)), "à", format(max(meteo$date)), ")\n")

######################################################
######### 4. Construction des lags — même logique que 02_hebdomadaire.R
######################################################
# fun_summarize_week()/fun_ccm_df() définies dans 00_functions_formats.R
# (partagées avec 02_hebdomadaire.R).

meteo2 <- meteo %>%
  dplyr::select(codgeo, date) %>%
  mutate(year = year(date), week = week(date), weekday = wday(date)) %>%
  filter(weekday == 1,
         date >= Sys.Date(),
         date <= Sys.Date() + n_months_seasonal * 31) %>%
  slice(rep(1:n(), each = lag_max)) %>%
  group_by(codgeo, year, week) %>%
  mutate(lag_n = row_number()) %>%
  ungroup() %>%
  dplyr::select(-weekday) %>%
  rename(th_date = date) %>%
  mutate(date = th_date - lag_n) %>%
  data.table()

meteo3 <- meteo2 %>%
  left_join(meteo %>% dplyr::select(codgeo, date, TM, RR, UM),
            by = c("date", "codgeo")) %>%
  pivot_longer(c(TM, RR, UM), names_to = "var", values_to = "val") %>%
  data.table()

df_meteo_pieges_summ <- fun_summarize_week(meteo3, "RR", "sum",  "RR", 7) %>%
  bind_rows(fun_summarize_week(meteo3, "TM", "mean", "TM", 7)) %>%
  bind_rows(fun_summarize_week(meteo3, "UM", "mean", "UM", 7))

df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n < 12)

df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ, "RR", "sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ, "TM", "mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ, "UM", "mean")

df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2, by = c("codgeo", "th_date")) %>%
  left_join(df_meteo_pieges_summ_wide3, by = c("codgeo", "th_date"))

df_meteo_predictions <- df_meteo_pieges_summ_wide_meteofrance %>%
  dplyr::select(codgeo, th_date,
                TM_0_8, UM_5_11,          # prédicteurs présence
                TM_0_4, UM_0_11, RR_1_5,  # prédicteurs abondance
                TM_0_0, RR_0_0, TM_0_5, UM_1_10, RR_1_10) %>%
  rename(date = th_date) %>%
  na.omit()

cat("✓ Prédicteurs construits pour", nrow(df_meteo_predictions), "lignes (commune x semaine)\n")

######################################################
######### 5. Chargement des modèles + prédiction (modèle COMBINÉ, pas de SHAP)
######################################################

res_presence  <- readRDS(file.path(path_models, "res_presence_LOSO_probabilistic.rds"))
res_abundance <- readRDS(file.path(path_models, "res_abundance_LOSO_quantile_rf.rds"))

mod_presence     <- res_presence$model
rf_abundance_q   <- res_abundance$model_quantile

predictors_presence  <- c("TM_0_8", "UM_5_11")
predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")

df_meteo_predictions <- predict_two_part_uncertainty(
  newdata              = df_meteo_predictions,
  mod_presence         = mod_presence,
  rf_abundance_q       = rf_abundance_q,
  predictors_presence  = predictors_presence,
  predictors_abundance = predictors_abundance,
  n_sim                = 2000
)

cat("✓ Prédictions calculées (modèle combiné présence x abondance)\n")

######################################################
######### 6. Construction de la table finale
######################################################

abundance <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_combined_mean, pred_combined_q05,
                pred_combined_q95, pred_combined_sd) %>%
  dplyr::rename(
    combined_abundance_q50 = pred_combined_mean,
    combined_abundance_q05 = pred_combined_q05,
    combined_abundance_q95 = pred_combined_q95,
    combined_abundance_sd  = pred_combined_sd
  ) %>%
  dplyr::mutate(
    combined_abundance_q50 = round(combined_abundance_q50, 1),
    combined_abundance_q05 = round(combined_abundance_q05, 1),
    combined_abundance_q95 = round(combined_abundance_q95, 1),
    combined_abundance_sd  = round(combined_abundance_sd,  2),
    date = date + 1   # même décalage que 02_hebdomadaire.R
  )

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
    date             = date + 2
  ) %>%
  dplyr::left_join(roi_info, by = "codgeo")

seasonal_predictions <- left_join(meteo_out, abundance, by = c("codgeo", "date")) %>%
  mutate(
    date_fin       = date + 7,
    last_update    = as.Date(Sys.Date()),
    horizon_jours  = as.integer(date - Sys.Date())   # utile pour savoir "combien de temps à l'avance"
  ) %>%
  relocate(date_fin, .after = date)

thresh_orange_red <- median(
  seasonal_predictions$combined_abundance_q50[
    which(seasonal_predictions$combined_abundance_q50 > 0 &
          !is.na(seasonal_predictions$combined_abundance_q50))
  ]
)

seasonal_predictions <- seasonal_predictions %>%
  mutate(level_risk = case_when(
    combined_abundance_q50 == 0 | is.na(combined_abundance_q50) ~ "Faible",
    combined_abundance_q50 > 0 & combined_abundance_q50 < thresh_orange_red ~ "Modéré",
    combined_abundance_q50 >= thresh_orange_red ~ "Élevé"
  )) %>%
  arrange(codgeo, date) %>%
  group_by(codgeo) %>%
  mutate(
    trend = case_when(
      is.na(combined_abundance_q50)      ~ NA_real_,
      is.na(lag(combined_abundance_q50)) ~ NA_real_,
      lag(combined_abundance_q50) == 0   ~ NA_real_,
      TRUE ~ 100 * (combined_abundance_q50 - lag(combined_abundance_q50)) /
        lag(combined_abundance_q50)
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
######### 7. Écriture BD — table de TEST (jamais albopictus_ruiz_test)
######################################################

cat("Écriture dans", db_layer_seasonal,
    "(table de test — overwrite complet à chaque run, ce n'est PAS albopictus_ruiz_test)\n")

# delete_layer = TRUE (plutôt que overwrite = TRUE, déprécié dans les versions
# récentes de sf/st_write()) : supprime la table existante puis la recrée.
# Sans risque ici : db_layer_seasonal est une table de TEST (test_seasonal_ruiz),
# jamais meteo_ruiz ni albopictus_ruiz_test.
st_write(seasonal_predictions, dsn = con, layer = db_layer_seasonal, delete_layer = TRUE)

cat("✓ Prévisions saisonnières publiées dans", db_layer_seasonal,
    "|", length(unique(seasonal_predictions$codgeo)), "communes |",
    length(unique(seasonal_predictions$date)), "semaines |",
    "horizon", n_months_seasonal, "mois\n")

dbDisconnect(con)
cat("\n✓ Script saisonnier terminé —", as.character(Sys.time()), "\n")
