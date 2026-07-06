# ============================================================
# SCRIPT 2 — Pipeline hebdomadaire
# À exécuter chaque semaine sur le serveur (cron job)
# Prérequis : Script 1 (initialisation) déjà exécuté
#
# CE QUE FAIT CE CODE (dans l'ordre) :
#   1. Met à jour la météo en BD : remplace le forecast de la semaine passée par
#      les vraies données historiques, télécharge le forecast de la semaine à venir.
#      → Agrégation par commune AVANT écriture (nouveau schéma).
#   2. Lit uniquement les lag_max derniers jours de météo — au lieu
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
# Paramètres locaux (lus depuis config.R)
# ============================================================
# grid_res, path_models, n_days_forecast définis dans config.R
lag_max <- 84

# ============================================================
# Connexion à la base de données
# ============================================================
con <- dbConnect(
  RPostgres::Postgres(),
  host     = db_host,
  dbname   = db_name,
  port     = db_port,
  user     = db_user,
  password = db_password,
  # Keepalives TCP : envoie un paquet toutes les 60 s pour éviter que la connexion
  # soit coupée par le serveur pendant les longs calculs R (init historique, SHAP...)
  keepalives      = 1L,
  keepalives_idle = 60L
)
# Désactiver le timeout serveur pour les opérations longues
dbExecute(con, "SET statement_timeout = 0")

# ============================================================
# Chargement du ROI et du grid
# ============================================================

roi <- sf::st_read(con, db_table_admin) %>%
  dplyr::filter(dep == admin_dep, level == admin_level)
roi <- st_transform(roi, 4326)

# sf_use_s2(FALSE) requis : st_union/st_intersection échouent sur certaines géométries ROI
# avec s2 activé (erreur "format non supporté"). Les messages "Spherical geometry switched
# off/on" et "assumes planar" sont normaux et attendus ici.
sf::sf_use_s2(FALSE)
roi <- st_make_valid(roi)
geopolygon <- st_union(roi)
sf::sf_use_s2(TRUE)
log_print("sf_use_s2 désactivé temporairement pour st_union/st_make_valid (comportement attendu)")

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
# (optimisation — pas besoin de lire 10 ans pour ce check)
meteo_recent <- dbGetQuery(con, sprintf(
  "SELECT date, is_forecast FROM %s WHERE date >= '%s' AND date < '%s'",
  db_table_meteo,
  as.character(Sys.Date() - n_days_forecast),  # fenêtre = horizon forecast (config.R)
  as.character(Sys.Date())
)) %>% as.data.table()
meteo_recent$date <- as.Date(meteo_recent$date)

# ---- Étape 1 : Remplacer forecast de la semaine passée par historical ----
# new (fix — ne retélécharger que ce qui est ENCORE marqué forecast) ====
dates_a_remplacer <- unique(meteo_recent$date[meteo_recent$is_forecast %in% TRUE])
# ==============

# new: logs =======
cat("Dates à remplacer (forecast → historical) :", length(dates_a_remplacer), "\n")
if (length(dates_a_remplacer) > 0) {
  log_print(paste("Dates à remplacer (forecast → historical) :",
                  paste(sort(dates_a_remplacer), collapse = ", ")))
} else {
  log_print("Dates à remplacer (forecast → historical) : aucune")
}
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
  # re-télécharger toujours le forecast futur à chaque run hebdomadaire :
  # Le forecast téléchargé la semaine dernière est périmé (modèle météo mis à jour chaque jour) —
  # on supprime toujours les lignes is_forecast = TRUE >= Sys.Date() et on re-télécharge.
  forecast_needed <- TRUE
  cat("Forecast futur à re-télécharger (données fraîches)\n")
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

# new (fenêtre calendaire fixe pour backfill par tranches) ====
# backfill_start_date / backfill_end_date : QUOI = Date, Date — si les DEUX sont
# définies (depuis un script appelant, ex. 06_backfill_shap_par_tranches.R),
# remplacent complètement la logique init_lookback/n_days_forecast ci-dessous
# pour cibler une plage calendaire FIXE (ex. "2016-01-01" à "2017-12-31"), au
# lieu d'une fenêtre toujours ancrée sur Sys.Date(). Permet de backfiller le
# SHAP historique par tranches (ex. 2 ans à la fois) sans re-traiter à chaque
# fois les années déjà faites — chaque tranche s'écrit en BD indépendamment.
# Si l'une des deux (ou les deux) n'est pas définie, comportement INCHANGÉ
# (run hebdomadaire normal ou init_lookback classique depuis Sys.Date()).
#
# ⚠ RISQUE VÉCU EN PRATIQUE — variables "collantes" entre sessions R : si
# 06_backfill_shap_por_tramos.R est interrompu (Échap, crash) PENDANT le
# traitement d'une tranche, backfill_start_date/backfill_end_date restent
# définies dans la session R (le rm() de fin de boucle n'est jamais atteint).
# Toute exécution suivante de 01_initialisation.R ou de ce script tout seul,
# DANS LA MÊME SESSION R, hérite alors silencieusement de cette vieille plage
# au lieu de sa propre fenêtre normale — déjà arrivé (initialisation.R a
# traité 2 ans au lieu de son fonctionnement habituel). 01_initialisation.R
# nettoie maintenant ces variables au démarrage par précaution (voir son
# début de fichier) — mais si vous lancez CE script seul après avoir
# interrompu un backfill, pensez à redémarrer la session R avant, ou à faire
# rm(backfill_start_date, backfill_end_date) à la main.
.backfill_plage_fixe <- exists("backfill_start_date") && exists("backfill_end_date")
# ==============

# Optimisation : lire seulement les lag_max derniers jours au lieu de toute la table.
# Exception : si init_lookback est défini (depuis 01_initialisation.R), on lit aussi
# tout l'historique nécessaire pour couvrir tous les lundis historiques + leurs lags.
# .read_from = Sys.Date() - init_lookback - lag_max pour couvrir le lundi le plus ancien
# et ses lag_max jours de lags en arrière.
.read_from <- if (.backfill_plage_fixe) {
  as.Date(backfill_start_date) - lag_max
} else if (exists("init_lookback")) {
  Sys.Date() - init_lookback - lag_max
} else {
  Sys.Date() - lag_max
}
# new (plage fixe — borne haute explicite) : en mode backfill_start_date/end_date,
# on ajoute une borne haute à la lecture BD pour ne PAS lire jusqu'à aujourd'hui
# à chaque tranche — c'est tout l'intérêt de découper le backfill en tranches
# calendaires (moins de données lues/en mémoire par run, pas juste moins de
# lundis à prédire).
.read_to_clause <- if (.backfill_plage_fixe) {
  sprintf(" AND date <= '%s'", as.character(as.Date(backfill_end_date)))
} else {
  ""
}
cat("Lecture de la météo depuis", format(.read_from), "depuis la BD...\n")
meteo <- dbGetQuery(con, sprintf(
  "SELECT * FROM %s WHERE date >= '%s'%s",
  db_table_meteo, as.character(.read_from), .read_to_clause
)) %>% as.data.table()
meteo$date <- as.Date(meteo$date)
# fix : dédupliquer après lecture BD pour éviter le warning many-to-many
# (des doublons (codgeo, date) peuvent apparaître si la table a été écrite plusieurs fois)
meteo <- meteo %>% dplyr::distinct(codgeo, date, .keep_all = TRUE)

# meteo2 : tous les lundis disponibles dans la fenêtre météo (passés ET futurs).
# On inclut les lundis passés pour prédire aussi sur la météo réelle archivée —
# les lags seront calculés avec les valeurs réelles (archive) et non plus les
# valeurs de forecast de l'époque. na.omit() en aval élimine automatiquement les
# lundis trop anciens dont les lags dépassent la fenêtre lue (lag_max jours).
# Avant : filter(weekday == 1, date >= Sys.Date()) → lundis futurs seulement.
# .lookback : fenêtre pour les lundis à prédire (ignorée en mode plage fixe,
# où le filtre ci-dessous utilise directement backfill_start_date/end_date).
# Hebdo normal = n_days_forecast (14 j → 1-2 lundis passés + 2 futurs).
# Init historique = init_lookback (tous les lundis depuis début de meteo_ruiz).
.lookback <- if (exists("init_lookback")) init_lookback else n_days_forecast
meteo2 <- meteo %>%
  dplyr::select(codgeo, date) %>%
  mutate(year = year(date), week = week(date), weekday = wday(date)) %>%
  {
    if (.backfill_plage_fixe) {
      dplyr::filter(., weekday == 1,
                    date >= as.Date(backfill_start_date),
                    date <= as.Date(backfill_end_date))
    } else {
      dplyr::filter(., weekday == 1, date >= Sys.Date() - .lookback)
    }
  } %>%
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

# fix : by= explicite pour éviter le warning many-to-many (dplyr 1.1+)
df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2, by = c("codgeo", "th_date")) %>%
  left_join(df_meteo_pieges_summ_wide3, by = c("codgeo", "th_date"))

# df_meteo_predictions : prédicteurs par commune x semaine
df_meteo_predictions <- df_meteo_pieges_summ_wide_meteofrance %>%
  dplyr::select(codgeo, th_date,
                TM_0_8, UM_5_11,          # prédicteurs présence
                TM_0_4, UM_0_11, RR_1_5,  # prédicteurs abondance
                TM_0_0, RR_0_0, TM_0_5, UM_1_10, RR_1_10) %>%
  rename(date = th_date) %>%
  na.omit()


# new (skip recalcul/republication si rien n'a changé) ====
# Peut être défini depuis la console avant source() pour forcer le recalcul :
#   force_recompute <- TRUE ; source("scripts/02_hebdomadaire.R")
if (!exists("force_recompute")) force_recompute <- FALSE
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
######### Calcul SHAP exact du modèle combiné → 3 colonnes
######################################################

# CONTEXTE — pourquoi un calcul exact plutôt que la règle du produit :
#
#   Le modèle combiné est le produit de deux modèles distincts :
#     f(x) = p(TM_0_8, UM_5_11)  ×  exp(a_log(TM_0_4, UM_0_11, RR_1_5))
#   où p = P(présence) et exp(a_log) = abondance médiane en échelle originale.
#
#   L'ancienne approche "règle du produit" calculait :
#     shap_combined_TM_0_8 = shap_pres_TM_0_8  × pred_abundance_q50   ← amplifié par abondance (0–500+)
#     shap_combined_TM_0_4 = shap_abund_TM_0_4 × pred_presence_prob   ← amorti par p ∈ [0,1]
#   Résultat : en haute abondance, les variables de PRÉSENCE dominaient toujours,
#   même si ce sont les variables d'ABONDANCE qui expliquaient vraiment la variation.
#   C'est un artefact de l'asymétrie des multiplicateurs, pas un signal biologique.
#
# SOLUTION — Shapley exact sur f(x) traité comme une fonction UNIQUE de 5 variables :
#   On énumère les 2^5 = 32 coalitions possibles de {TM_0_8, UM_5_11, TM_0_4, UM_0_11, RR_1_5}
#   et on applique la définition exacte de Shapley :
#
#     φ_j = Σ_{S ⊆ {1..5}\{j}} [|S|!(4−|S|)! / 5!] × [v(S∪{j}) − v(S)]
#
#   où v(S) = E_bg[f(x) | variables de S fixées à leur valeur observée,
#                          autres variables tirées du background]
#
#   Même méthode que .shapley_exact() utilisée pour le modèle de présence seul
#   (qui avait 2^2 = 4 coalitions) — on l'applique ici sur les 5 prédicteurs combinés.
#
# SORTIE — 3 colonnes (une par type de variable météo) :
#   On somme les contributions des lags de présence ET d'abondance pour le même
#   type de variable car ils représentent le même signal physique à différentes
#   échelles temporelles :
#     shap_TM = φ(TM_0_8) + φ(TM_0_4)    # temp. lags 0-8 sem. (présence) + 0-4 sem. (abondance)
#     shap_UM = φ(UM_5_11) + φ(UM_0_11)  # humidité lags 5-11 sem. + 0-11 sem.
#     shap_RR = φ(RR_1_5)                 # pluie lags 1-5 sem. (abondance uniquement)

# skip_shap : défini par 01_initialisation.R lors du run historique complet pour éviter
# de lancer SHAP sur des milliers de semaines (trop lent). En run normal, non défini → SHAP calculé.
if (exists("skip_shap") && isTRUE(skip_shap)) {
  df_meteo_predictions <- df_meteo_predictions %>%
    dplyr::mutate(shap_TM = NA_real_, shap_UM = NA_real_, shap_RR = NA_real_,
                  shap_TM_abs = NA_real_, shap_UM_abs = NA_real_, shap_RR_abs = NA_real_)
  cat("ℹ SHAP désactivé (chargement historique initial) — colonnes NA\n")
} else {

# shap_max_background : QUOI = taille de l'échantillon background utilisé dans
# .shapley_exact() (paramètre max_background). FAIT = chaque coalition construit
# un dataframe de n_lignes_à_prédire × shap_max_background et y appelle predict()
# — le coût de TOUT le calcul SHAP (32 coalitions × 2 backgrounds × 2 predict())
# est linéaire en cette valeur. Défaut 50 (comportement historique, inchangé pour
# le run hebdomadaire normal). Peut être réduit (ex. 20) depuis un script appelant
# (05_backfill_shap.R) pour accélérer un backfill sur beaucoup de lignes, au prix
# d'une estimation background un peu plus bruitée (c'est déjà un sous-échantillon,
# pas le jeu complet — réduire sa taille ne change pas le principe, juste la variance).
if (!exists("shap_max_background")) shap_max_background <- 50

# fix (bug "vector memory limit of 16.0 Gb reached") : shap_batch_size borne
# le nombre de lignes à expliquer traitées EN UNE FOIS par .shapley_exact()
# (voir 00_functions.R) — sans ça, un backfill de plusieurs dizaines de
# milliers de lignes fait planter le predict() du modèle d'abondance
# (ranger quantile regression) en dépassant la limite mémoire par vecteur de
# R sur Mac. Défaut 2000 (comportement inchangé pour le run hebdomadaire
# normal, où n est de toute façon petit — quelques centaines de lignes).
if (!exists("shap_batch_size")) shap_batch_size <- 2000

all_predictors <- c(predictors_presence, predictors_abundance)
# = c("TM_0_8", "UM_5_11", "TM_0_4", "UM_0_11", "RR_1_5")

X_combined <- as.data.frame(df_meteo_predictions[, all_predictors])

# Encapsulation des deux modèles dans une liste — pred_wrapper reçoit cet objet
combined_model_obj <- list(
  mod_presence         = mod_presence,
  rf_abundance_q       = rf_abundance_q,
  predictors_presence  = predictors_presence,
  predictors_abundance = predictors_abundance
)

# pred_wrapper : implémente f(x) = p(x_pres) × exp(a_log_q50(x_abund))
# exp() pour revenir de l'échelle log (dans laquelle le modèle est entraîné) à l'originale
pred_wrapper_combined <- function(object, newdata) {
  X_pres  <- newdata[, object$predictors_presence,  drop = FALSE]
  X_abund <- newdata[, object$predictors_abundance, drop = FALSE]
  p       <- predict(object$mod_presence, newdata = X_pres, type = "prob")$Presence
  a_log   <- predict(object$rf_abundance_q, data = X_abund,
                     type = "quantiles", quantiles = 0.5)$predictions[, 1]
  p * exp(a_log)
}

# Background = données courantes de prédiction (distribution marginale des 5 prédicteurs
# sur toutes les communes et semaines du forecast — référence raisonnable en l'absence
# du jeu d'entraînement complet avec les 5 prédicteurs combinés)
shap_exact <- tryCatch(
  .shapley_exact(
    model          = combined_model_obj,
    X_df           = X_combined,
    background     = X_combined,
    feature_names  = all_predictors,
    pred_wrapper   = pred_wrapper_combined,
    max_background = shap_max_background,  # lignes background × 32 coalitions × n_obs prédictions
    batch_size     = shap_batch_size       # borne mémoire — voir commentaire plus haut
  ),
  error = function(e) {
    # fix (bug SHAP silencieux) : l'ancien suppressWarnings() qui entourait ce
    # tryCatch avalait le warning() ci-dessous — un échec de calcul passait
    # totalement inaperçu (aucune trace en log ni en console) et TOUTES les
    # lignes de la table recevaient shap_TM/UM/RR = NA sans diagnostic possible.
    # On logue désormais explicitement le message d'erreur réel (log_print,
    # capturé dans logs/log/) en plus du warning() console.
    msg <- paste("SHAP combiné exact (spatial) : calcul échoué —", conditionMessage(e))
    warning(msg, call. = FALSE)
    log_print(msg)
    NULL
  }
)

# Agrégation en 3 colonnes (même signal physique, lags différents → on somme)
if (!is.null(shap_exact)) {
  df_meteo_predictions <- df_meteo_predictions %>%
    dplyr::mutate(
      shap_TM = shap_exact[["TM_0_8"]] + shap_exact[["TM_0_4"]],
      shap_UM = shap_exact[["UM_5_11"]] + shap_exact[["UM_0_11"]],
      shap_RR = shap_exact[["RR_1_5"]]
    )
  cat("✓ SHAP spatial calculé (3 colonnes : shap_TM, shap_UM, shap_RR)\n")
  # fix (visibilité log — cron n'a pas de console) : log_print() en plus du cat()
  log_print("✓ SHAP spatial calculé (3 colonnes : shap_TM, shap_UM, shap_RR)")
} else {
  df_meteo_predictions <- df_meteo_predictions %>%
    dplyr::mutate(shap_TM = NA_real_, shap_UM = NA_real_, shap_RR = NA_real_)
  cat("⚠ SHAP spatial non disponible — colonnes NA\n")
  log_print("⚠ SHAP spatial non disponible — colonnes NA")
}


######################################################
######### SHAP absolu — background = données d'entraînement
######################################################

# DIFFÉRENCE AVEC LE SHAP SPATIAL CI-DESSUS :
#
#   shap_TM / shap_UM / shap_RR (background = communes du forecast courant) :
#     → mesurent la VARIATION INTER-COMMUNES cette semaine
#     → une variable est "dominante" si elle différencie les communes entre elles
#     → en été quand toutes les communes ont ~même TM, shap_TM ≈ 0 même si TM
#        est biologiquement important
#
#   shap_TM_abs / shap_UM_abs / shap_RR_abs (background = données d'entraînement) :
#     → mesurent l'ÉCART PAR RAPPORT À LA NORMALE HISTORIQUE
#     → une variable est "dominante" si elle s'éloigne de ce qu'a vu le modèle
#        lors de son entraînement, indépendamment des autres communes
#     → nécessite res_train$X_combined (disponible si 00_train_models.R a été relancé)

if (!is.null(res_train$X_combined)) {
  shap_exact_abs <- tryCatch(
    .shapley_exact(
      model          = combined_model_obj,
      X_df           = X_combined,
      background     = res_train$X_combined,
      feature_names  = all_predictors,
      pred_wrapper   = pred_wrapper_combined,
      max_background = shap_max_background,
      batch_size     = shap_batch_size
    ),
    error = function(e) {
      # fix (bug SHAP silencieux) : voir commentaire équivalent sur le bloc SHAP
      # spatial ci-dessus — suppressWarnings() cachait totalement les échecs.
      msg <- paste("SHAP absolu : calcul échoué —", conditionMessage(e))
      warning(msg, call. = FALSE)
      log_print(msg)
      NULL
    }
  )

  if (!is.null(shap_exact_abs)) {
    df_meteo_predictions <- df_meteo_predictions %>%
      dplyr::mutate(
        shap_TM_abs = shap_exact_abs[["TM_0_8"]] + shap_exact_abs[["TM_0_4"]],
        shap_UM_abs = shap_exact_abs[["UM_5_11"]] + shap_exact_abs[["UM_0_11"]],
        shap_RR_abs = shap_exact_abs[["RR_1_5"]]
      )
    cat("✓ SHAP absolu calculé (3 colonnes : shap_TM_abs, shap_UM_abs, shap_RR_abs)\n")
    log_print("✓ SHAP absolu calculé (3 colonnes : shap_TM_abs, shap_UM_abs, shap_RR_abs)")
  } else {
    df_meteo_predictions <- df_meteo_predictions %>%
      dplyr::mutate(shap_TM_abs = NA_real_, shap_UM_abs = NA_real_, shap_RR_abs = NA_real_)
    cat("⚠ SHAP absolu non disponible — colonnes NA\n")
    log_print("⚠ SHAP absolu non disponible — colonnes NA")
  }
} else {
  # X_combined absent = 00_train_models.R n'a pas encore été relancé avec le nouveau code
  df_meteo_predictions <- df_meteo_predictions %>%
    dplyr::mutate(shap_TM_abs = NA_real_, shap_UM_abs = NA_real_, shap_RR_abs = NA_real_)
  cat("⚠ res_train$X_combined absent — relancer 00_train_models.R pour activer SHAP absolu\n")
}

} # fin du bloc else (SHAP calculé — pas de skip_shap)


######################################################
######### Agrégation par commune — prédictions entomo
######################################################

# Nouveau schéma : df_meteo_predictions est déjà par commune (codgeo).
# Plus besoin de rasterize_to_communes() — on sélectionne directement.

abundance <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_combined_mean) %>%
  dplyr::rename(combined_abundance_q50 = pred_combined_mean) %>%
  dplyr::mutate(
    combined_abundance_q50 = round(combined_abundance_q50, 1),
    date = date + 1   # décalage d'un jour
  )
# fix : pas de left_join(roi_info) ici — libgeo vient déjà de meteo_out
# (évite les colonnes communes implicites qui génèrent le warning many-to-many)

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

# fix : by= explicite — libgeo vient de meteo_out uniquement
albopictus_predictions <- left_join(meteo_out, abundance, by = c("codgeo", "date")) %>%
  mutate(
    date_fin    = date + 7,
    last_update = as.Date(Sys.Date())
  ) %>%
  relocate(date_fin, .after = date)

thresh_orange_red <- median(
  albopictus_predictions$combined_abundance_q50[
    which(albopictus_predictions$combined_abundance_q50 > 0 &
          !is.na(albopictus_predictions$combined_abundance_q50))
  ]
)

albopictus_predictions <- albopictus_predictions %>%
  mutate(level_risk = case_when(
    combined_abundance_q50 == 0 | is.na(combined_abundance_q50) ~ "Faible",
    combined_abundance_q50 > 0 & combined_abundance_q50 < thresh_orange_red ~ "Modéré",
    combined_abundance_q50 >= thresh_orange_red ~ "Élevé"
  ))

albopictus_predictions <- albopictus_predictions %>%
  arrange(codgeo, date) %>%
  group_by(codgeo) %>%
  mutate(
    trend = case_when(
      is.na(combined_abundance_q50)        ~ NA_real_,
      is.na(lag(combined_abundance_q50))   ~ NA_real_,
      lag(combined_abundance_q50) == 0     ~ NA_real_,
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
######### Construction de la table finale avec SHAP
######################################################

# Colonnes SHAP spatial (background = communes du forecast)
shap_cols     <- c("shap_TM", "shap_UM", "shap_RR")
# Colonnes SHAP absolu (background = données d'entraînement)
shap_cols_abs <- c("shap_TM_abs", "shap_UM_abs", "shap_RR_abs")
all_shap_cols <- c(shap_cols, shap_cols_abs)

if (all(shap_cols %in% colnames(df_meteo_predictions))) {

  # Colonnes à sélectionner : spatial (obligatoires) + absolu (si disponibles)
  cols_shap_dispo <- intersect(all_shap_cols, colnames(df_meteo_predictions))

  shap_comm <- df_meteo_predictions %>%
    dplyr::select(codgeo, date, dplyr::all_of(cols_shap_dispo)) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(all_shap_cols), ~round(.x, 4)),
      date = date + 1
    )

  albopictus_predictions_shap <- albopictus_predictions %>%
    left_join(shap_comm, by = c("codgeo", "date"))

} else {
  cat("⚠ Colonnes SHAP absentes — table identique à la table principale\n")
  albopictus_predictions_shap <- albopictus_predictions
}

# fix (bug perte de calcul après coupure réseau) : dbIsValid(con) NE SUFFIT PAS
# pour détecter une connexion coupée côté serveur (timeout alwaysdata, NAT, etc.)
# après un calcul long (backfill de plusieurs heures) — il vérifie seulement que
# l'objet R n'a pas été fermé explicitement, pas que le lien réseau fonctionne
# encore. Vu en pratique : dbIsValid(con) répondait TRUE juste avant que
# dbExistsTable() plante avec "Operation timed out" — 3h de calcul SHAP perdues
# car rien n'avait encore été écrit en BD. On teste donc activement avec une
# requête légère, et en plus on entoure l'écriture elle-même d'un retry avec
# reconnexion — pour ne plus jamais perdre un calcul long sur un simple aléa réseau.
.reconnecter_bd <- function() {
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
  con
}

connexion_ok <- isTRUE(dbIsValid(con)) && !inherits(
  tryCatch(dbGetQuery(con, "SELECT 1"), error = function(e) e), "error"
)
if (!connexion_ok) {
  cat("ℹ Reconnexion BD (connexion expirée ou coupée pendant le calcul)...\n")
  log_print("ℹ Reconnexion BD (connexion expirée ou coupée pendant le calcul)")
  try(dbDisconnect(con), silent = TRUE)
  con <- .reconnecter_bd()
}

# Supprimer uniquement les dates qu'on va écrire (évite les doublons si hebdo
# tourne deux fois dans la même semaine, et conserve l'historique des semaines passées)
dates_a_ecrire <- unique(albopictus_predictions_shap$date)
dates_sql      <- paste(paste0("'", dates_a_ecrire, "'"), collapse = ",")

.ecrire_predictions <- function(con) {
  if (dbExistsTable(con, db_layer)) {
    dbExecute(con, sprintf("DELETE FROM %s WHERE date IN (%s)", db_layer, dates_sql))
  }
  st_write(albopictus_predictions_shap, dsn = con, layer = db_layer, append = TRUE)
}

# 1re tentative — si elle échoue (coupure réseau juste après le test ci-dessus,
# ou pendant l'écriture elle-même), on reconnecte et on retente UNE fois avant
# d'abandonner (au lieu de tout perdre après un calcul potentiellement long).
ecriture_ok <- tryCatch({ .ecrire_predictions(con); TRUE }, error = function(e) {
  msg <- paste("Écriture BD échouée (1re tentative) —", conditionMessage(e),
               "— reconnexion et nouvelle tentative...")
  cat("⚠", msg, "\n")
  log_print(msg)
  FALSE
})

if (!ecriture_ok) {
  try(dbDisconnect(con), silent = TRUE)
  con <- .reconnecter_bd()
  .ecrire_predictions(con)   # si ça replante ici, on laisse l'erreur remonter (problème réseau plus profond)
  cat("✓ Écriture BD réussie après reconnexion (2e tentative)\n")
  log_print("✓ Écriture BD réussie après reconnexion (2e tentative)")
}

cat("✓ Prédictions + SHAP publiées dans la table", db_layer, "\n")
# new: logs =======
log_print(paste("✓ Prédictions + SHAP publiées →", db_layer,
                "| communes :", length(unique(albopictus_predictions_shap$codgeo)),
                "| colonnes SHAP :", length(all_shap_cols)))
# ==============

} else {
  cat("✓ Aucune nouvelle donnée météo — prédictions/SHAP non recalculés, table",
      db_layer, "inchangée.\n  (Mettre force_recompute <- TRUE pour forcer.)\n")
}

dbDisconnect(con)
cat("\n✓ Pipeline hebdomadaire terminé.\n")
# new: logs =======
log_print(paste("=== Fin du run hebdomadaire —", Sys.time(), "==="))
log_close()
# ==============
