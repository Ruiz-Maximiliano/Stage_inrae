# ============================================================
# SCRIPT 2 — Pipeline hebdomadaire
# À exécuter chaque semaine sur le serveur (cron job)
# Prérequis : Script 1 (initialisation) déjà exécuté
#
# CE QUE FAIT CE CODE (dans l'ordre) :
#   1. Met à jour la météo en BD : remplace le forecast de la semaine passée par
#      les vraies données historiques, télécharge le forecast de la semaine à venir.
#   2. Construit les variables retardées (lags TM/RR/UM sur plusieurs semaines).
#   3. Charge les modèles entraînés (00_train_models.R) et génère les prédictions
#      two-part (présence + abondance + combined avec incertitude).
#   4. Calcule le SHAP pour les 4 modèles (abondance, présence, combined, abondance
#      de comparaison) — voir 00_functions.R::compute_shap().
#   5. Agrège tout par commune (rasterisation) et publie 2 tables en BD :
#      db_layer (prédictions) et db_layer_shap (prédictions + SHAP).
#   Les étapes 3-5 (modèles/prédictions/SHAP/publication) sont SAUTÉES si
#   aucune donnée météo nouvelle n'est arrivée à l'étape 1 ET qu'une
#   publication précédente existe déjà (voir skip_recompute/force_recompute).
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Tous viennent de config.R (db_table_admin, admin_dep, admin_level, openmeteo_model,
#   n_days_forecast, db_host/name/port/user/password, db_table_meteo, db_layer,
#   db_layer_shap). force_recompute (par défaut FALSE, voir avant "Chargement des
#   modèles") force le recalcul même si la météo n'a pas changé.
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   roi, geopolygon, coords, meteo, df_meteo_pieges_summ, df_meteo_predictions,
#   mod_presence/rf_abundance_q/mod_abundance_cv, albopictus_predictions,
#   albopictus_predictions_shap — voir le commentaire au-dessus de chaque variable.
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   - config.R : tous les paramètres listés ci-dessus.
#   - 00_functions.R : get_weather_history_batch(), get_weather_forecast_batch(),
#     rasterize_to_communes(), compute_shap(), predict_two_part_uncertainty().
#   - data/coords_grid.csv : généré par 01_initialisation.R (ou sa proposition).
#   - models/*.rds : générés par 00_train_models.R.
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

# new (ROI depuis BD — idée différée du punteo, maintenant validée) ====
roi <- sf::st_read(con, db_table_admin) %>%
  dplyr::filter(dep == admin_dep, level == admin_level)
roi <- st_transform(roi, 4326)
# ==============

# #new (8 - Renommage ROI) ====
sf::sf_use_s2(FALSE)
roi <- st_make_valid(roi)
geopolygon <- st_union(roi)
sf::sf_use_s2(TRUE)
# ==============

# coords : QUOI = data.frame X/Y/site, LE grid de points météo de la zone d'étude.
#   VIENT DE = data/coords_grid.csv, écrit par 01_initialisation.R (ou sa proposition)
#   — ce script ne recrée PAS le grid, il réutilise celui déjà calculé.
coords <- read.csv(path_coords_grid)

# meteo_prep : QUOI = liste de batches de coordonnées (groupes de 20 points), pour
#   limiter la taille de chaque appel à l'API Open-Meteo (1 requête par batch au lieu
#   d'1 requête par point). Utilisé pour les 2 téléchargements ci-dessous (historical
#   de remplacement + forecast).
meteo_prep <- coords %>%
  group_by(row_number() %/% 20) %>%
  group_map(~.x) %>%
  map(., ~group_split(., site))


######################################################
######### Mise à jour des données météo
######################################################

# new (colonne is_forecast — fraîcheur du remplacement historique) ====
# Garantit que la table météo a la colonne is_forecast avant la lecture ci-dessous
# (voir ensure_is_forecast_column() dans 00_functions.R pour le détail).
ensure_is_forecast_column(con, db_table_meteo)
# ==============

# #new (5 - Gestion données BD) ====
# meteo : QUOI = data.table, TOUTE la table météo (historique + forecast déjà en
#   BD). FAIT = dbReadTable() lit la table SQL telle quelle, as.data.table() la
#   convertit pour les manipulations rapides qui suivent (data.table est plus
#   efficace que dplyr sur de gros volumes). REMPLACE = l'ancien fread() du CSV
#   local d'avant le refactor BD.
cat("Lecture de la météo depuis la BD...\n")
meteo <- dbReadTable(con, db_table_meteo) %>% as.data.table()
meteo$date <- as.Date(meteo$date)
# ==============

# ---- Étape 1 : Remplacer forecast de la semaine passée par historical ----
# new (fix — ne retélécharger que ce qui est ENCORE marqué forecast) ====
# dates_a_remplacer : QUOI = dates des 7 derniers jours dont les lignes en BD
#   sont ENCORE des prévisions (is_forecast == TRUE), pas simplement "présentes".
#   POURQUOI ce fix = avant, on testait juste si la date existait en BD — donc
#   si on relançait le script plusieurs fois le même jour (tests), ces 7 jours
#   étaient retéléchargés et réécrits à CHAQUE fois, même déjà remplacés par les
#   vraies valeurs. Maintenant, une fois remplacées, is_forecast passe à FALSE
#   et cette date ne sera plus retouchée tant qu'elle reste dans cette fenêtre.
dates_a_remplacer <- unique(meteo$date[
  meteo$date >= Sys.Date() - 7 & meteo$date < Sys.Date() & meteo$is_forecast %in% TRUE
])
# ==============

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
  # Supprimer les lignes forecast à remplacer, insérer l'historical.
  # is_forecast = FALSE : ce sont maintenant des valeurs réelles observées —
  # ce flag est ce qui permettra de NE PAS retoucher ces dates au prochain run.
  dates_sql <- paste(paste0("'", dates_a_remplacer, "'"), collapse = ",")
  dbExecute(con, sprintf("DELETE FROM %s WHERE date IN (%s)", db_table_meteo, dates_sql))
  meteo_updated$is_forecast <- FALSE
  dbWriteTable(con, db_table_meteo, as.data.frame(meteo_updated), append = TRUE, row.names = FALSE)
  # ==============

  meteo <- meteo %>%
    filter(!(date %in% dates_a_remplacer)) %>%
    bind_rows(meteo_updated)
} else {
  cat("✓ Historique déjà à jour — aucune date encore marquée forecast dans les 7 derniers jours, rien à remplacer\n")
}

# ---- Étape 2 : Télécharger la nouvelle semaine de forecast (si nécessaire) ----

# new (vérification fraîcheur forecast — éviter les téléchargements inutiles) ====
# forecast_needed : QUOI = booléen, FALSE si la BD couvre déjà TOUS les sites pour
#   TOUTE la fenêtre [aujourd'hui, aujourd'hui + n_days_forecast - 1]. POURQUOI =
#   si ce script est relancé plusieurs fois le même jour (ex. tests, debug), inutile
#   de repayer des appels API pour un forecast déjà à jour.
forecast_check <- dbGetQuery(con, sprintf(
  "SELECT site::text, COUNT(DISTINCT date) AS n_dates FROM %s WHERE date >= '%s' GROUP BY site",
  db_table_meteo, as.character(Sys.Date())
))
sites_forecast_ok <- forecast_check$site[forecast_check$n_dates >= n_days_forecast]
forecast_needed   <- !all(as.character(coords$site) %in% sites_forecast_ok)

meteo_future <- data.frame()

if (forecast_needed) {
  cat("Téléchargement du forecast...\n")

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
  # Supprimer l'ancien forecast futur, insérer le nouveau.
  # is_forecast = TRUE : prévisions, à remplacer par les vraies valeurs une fois
  # la date passée (Étape 1, au prochain run).
  dbExecute(con, sprintf("DELETE FROM %s WHERE date >= '%s'", db_table_meteo, Sys.Date()))
  meteo_future$is_forecast <- TRUE
  dbWriteTable(con, db_table_meteo, as.data.frame(meteo_future), append = TRUE, row.names = FALSE)
  # ==============
} else {
  cat("✓ Forecast déjà à jour en BD pour les", n_days_forecast,
      "jours à venir — téléchargement ignoré\n")
}

# Relire la météo complète et à jour depuis la BD
meteo <- dbReadTable(con, db_table_meteo) %>% as.data.table()
meteo$date <- as.Date(meteo$date)
# ==============


######################################################
######### Création des variables indépendantes
######################################################

# Recalcule un identifiant "site" propre (1, 2, 3...) à partir des coordonnées
# uniques X/Y — la BD peut contenir des "site" non contigus après les phases
# d'initialisation, ceci les renumérote de façon cohérente pour ce run.
meteo <- meteo %>%
  unique() %>%
  group_by(X, Y) %>%
  mutate(site = cur_group_id()) %>%
  ungroup() %>%
  relocate(site, 1) %>%
  data.table()

unique_coords    <- unique(meteo[, c("site", "X", "Y")])
unique_coords_sf <- st_as_sf(unique_coords, coords = c("X", "Y"), crs = 4326)

# coords_retain : QUOI = data.frame site/X/Y, les points météo qui tombent
#   VRAIMENT à l'intérieur de geopolygon (le ROI exact, pas juste son bbox).
#   FAIT = st_intersection() filtre géométriquement. RÉUTILISÉ plus loin pour
#   rattacher X/Y aux prédictions (jointure par site) avant rasterisation.
sf::sf_use_s2(FALSE)
coords_retain <- st_intersection(unique_coords_sf, geopolygon)
sf::sf_use_s2(TRUE)

coords_retain <- cbind(coords_retain, st_coordinates(coords_retain))
coords_retain <- st_drop_geometry(coords_retain)[, c("site", "X", "Y")]

# Filtre meteo aux seuls sites retenus, renomme les colonnes Open-Meteo (noms
# techniques) vers les noms courts utilisés partout dans le pipeline :
#   TM = température moyenne, RR = précipitations, UM = humidité relative
meteo <- meteo %>%
  filter(site %in% coords_retain$site) %>%
  mutate(date = as.Date(date)) %>%
  rename(TM = temperature_2m_mean,
         RR = precipitation_sum,
         UM = relative_humidity_2m_mean) %>%
  dplyr::select(site, date, RR, TM, UM)

# meteo2 : QUOI = grille site x date_lundi x lag_n (1 à lag_max=84 jours en arrière).
#   FAIT = pour chaque LUNDI (weekday==1, début de semaine ISO) de chaque site,
#   génère 84 lignes représentant chacun des 84 jours précédents (lag_n=1..84).
#   th_date = la date du lundi de référence ; date = th_date - lag_n (le jour
#   réel correspondant à ce lag). REPRÉSENTE = le squelette utilisé pour rattacher
#   ensuite les valeurs météo journalières à chaque décalage temporel.
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

# meteo3 : QUOI = meteo2 + les valeurs météo (TM/RR/UM) du jour "date" correspondant
#   à chaque lag, mises au format long (1 ligne par site x th_date x lag_n x variable).
#   REPRÉSENTE = la table pivot à partir de laquelle on calcule les agrégats
#   hebdomadaires (fun_summarize_week ci-dessous).
meteo3 <- meteo2 %>%
  left_join(meteo, by = c("date", "site")) %>%
  pivot_longer(!(site:date), names_to = "var", values_to = "val") %>%
  data.table()

# fun_summarize_week() : QUOI = fonction qui agrège meteo3 par paquets de
#   n_days_agg jours (ex. 7 = 1 semaine) avec la fonction fun_summarize
#   (sum/mean/max/min). FAIT = transforme les 84 lags journaliers en ~12 lags
#   hebdomadaires (lag_n=0 = semaine la plus récente, lag_n=1 = semaine précédente...).
#   REPRÉSENTE = le passage de "jour" à "semaine", l'échelle de temps des prédicteurs
#   du modèle (ex. TM_0_4 = température moyenne des semaines 0 à 4).
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

# df_meteo_pieges_summ : QUOI = agrégats hebdomadaires par site x semaine x variable
#   (lag_n = 0..11, soit jusqu'à 12 semaines en arrière). RR = somme hebdomadaire des
#   précipitations, TM/UM = moyenne hebdomadaire de température/humidité.
#   filter(lag_n < 12) : ne garde que 12 semaines (au-delà, pas utile aux modèles).
df_meteo_pieges_summ <- fun_summarize_week(meteo3, "RR", "sum",  "RR", 7) %>%
  bind_rows(fun_summarize_week(meteo3, "TM", "mean", "TM", 7)) %>%
  bind_rows(fun_summarize_week(meteo3, "UM", "mean", "UM", 7))

df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n < 12)

# fun_ccm_df() : QUOI = fonction qui transforme le format long (1 ligne par lag) en
#   format large (1 colonne par lag, ex. TM_0, TM_1, TM_2...) PUIS ajoute toutes les
#   combinaisons de moyennes/sommes GLISSANTES entre deux lags (ex. TM_0_4 = moyenne
#   des lags 0 à 4, c'est-à-dire des semaines 0 à 4). REPRÉSENTE = la génération de
#   TOUS les prédicteurs météo à fenêtre variable utilisés par les modèles
#   (TM_0_4, UM_0_11, RR_1_5, etc. — noms repris du CCM fait par Paul/l'équipe).
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

# df_meteo_pieges_summ_wide_meteofrance : QUOI = fusion des 3 tables larges
#   (RR/TM/UM) en une seule, par site x th_date. REPRÉSENTE = la table complète de
#   tous les prédicteurs météo possibles (toutes fenêtres temporelles confondues).
df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide3)

# df_meteo_predictions : QUOI = LA table d'entrée des modèles — 1 ligne par site x
#   semaine, ne garde QUE les prédicteurs effectivement utilisés par les modèles
#   (TM_0_8/UM_5_11 = présence ; TM_0_4/UM_0_11/RR_1_5 = abondance ; les autres
#   colonnes TM_0_0/RR_0_0/TM_0_5/UM_1_10/RR_1_10 sont gardées pour les visualisations
#   SHAP/diagnostics même si pas utilisées par les modèles publiés).
#   na.omit() retire les lignes incomplètes (ex. sites trop récents sans assez
#   d'historique pour calculer toutes les fenêtres).
df_meteo_predictions <- df_meteo_pieges_summ_wide_meteofrance %>%
  dplyr::select(site, th_date,
                TM_0_8, UM_5_11,          # prédicteurs présence
                TM_0_4, UM_0_11, RR_1_5,  # prédicteurs abondance
                TM_0_0, RR_0_0, TM_0_5, UM_1_10, RR_1_10) %>%
  rename(date = th_date) %>%
  na.omit()


# new (skip recalcul/republication si rien n'a changé) ====
# force_recompute : QUOI = mettre à TRUE pour forcer le recalcul même si rien
#   n'a été téléchargé (ex. après un nouvel entraînement de modèles avec
#   00_train_models.R — les données météo n'ont pas changé mais les modèles
#   oui, donc les prédictions doivent être refaites).                  [ENTRÉE]
force_recompute <- FALSE

# meteo_changed : QUOI = TRUE si l'Étape 1 (remplacement historique) OU
#   l'Étape 2 (forecast) a réellement téléchargé/écrit quelque chose dans
#   cette exécution. Si FALSE, la table météo en BD est identique à ce
#   qu'elle était lors de la dernière exécution.
meteo_changed <- (length(dates_a_remplacer) > 0) || forecast_needed

# db_layer_exists : QUOI = la table de publication existe déjà (donc une
#   précédente exécution a déjà calculé et publié des prédictions).
db_layer_exists <- dbExistsTable(con, db_layer)

# skip_recompute : QUOI = TRUE seulement si AUCUNE donnée météo nouvelle n'est
#   arrivée ET qu'une publication précédente existe déjà ET qu'on ne force pas
#   le recalcul. POURQUOI = calculer prédictions + SHAP + republier db_layer/
#   db_layer_shap est de la pure computation locale (pas d'appel API), mais ça
#   prend du temps pour rien si l'entrée (météo) n'a pas bougé depuis le
#   dernier run — le résultat serait identique.
skip_recompute <- !force_recompute && !meteo_changed && db_layer_exists

if (!skip_recompute) {
######################################################
######### Chargement des modèles
######################################################

# res_presence/res_abundance/res_train : QUOI = listes R relues depuis les fichiers
#   .rds générés par 00_train_models.R (chaque .rds contient le(s) modèle(s) entraîné(s)
#   + leurs données de validation croisée — voir le détail dans 00_train_models.R).
#   VIENT DE = models/*.rds, donc OBLIGATOIRE d'avoir lancé 00_train_models.R avant.
res_presence  <- readRDS(file.path(path_models, "res_presence_LOSO_probabilistic.rds"))
res_abundance <- readRDS(file.path(path_models, "res_abundance_LOSO_quantile_rf.rds"))
res_train     <- readRDS(file.path(path_models, "res_training_data.rds"))

# mod_presence   : modèle de présence (caret/ranger) — utilisé pour predict_two_part_uncertainty()
# rf_abundance_q : modèle d'abondance quantile (ranger) — LE modèle déployé pour les prédictions
mod_presence   <- res_presence$model
rf_abundance_q <- res_abundance$model_quantile

# new (SHAP sur tous les modèles) ====
# mod_abundance_cv : modèle caret/ranger d'abondance (comparaison/tuning, entraîné
# dans 00_train_models.R) — non utilisé pour les prédictions publiées (qui utilisent
# rf_abundance_q), mais on calcule aussi son SHAP pour avoir une explication complète
# de TOUS les modèles entraînés, pas seulement ceux utilisés en prédiction finale.
mod_abundance_cv <- res_abundance$model_cv
# ==============

predictors_presence  <- c("TM_0_8", "UM_5_11")
predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")


######################################################
######### Génération des prédictions
######################################################

# new (restauration modèle two-part complet) ====
# predict_two_part_uncertainty() est maintenant centralisée dans 00_functions.R
# (utilisée aussi par 00_train_models.R pour le diagnostic sur les données
# d'entraînement complètes) — voir scripts/00_functions.R
# ==============

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

# SHAP — modèle de présence (caret/ranger, forêt de probabilité)
# new (fix SHAP présence — calcul exact maison, sans dépendance externe) ====
# Les forêts de probabilité ne sont PAS supportées par treeshap::ranger.unify()
# (limitation réelle du package, pas un problème de format) — compute_shap()
# bascule automatiquement sur un calcul de Shapley EXACT par énumération des
# coalitions (.shapley_exact() dans 00_functions.R, aucun package externe requis,
# exact car predictors_presence n'a que 2 variables) dans ce cas, en utilisant
# X_presence (données d'entraînement, res_train) comme référence de background.
X_presence_pred <- as.data.frame(df_meteo_predictions[, predictors_presence])
shap_presence   <- compute_shap(mod_presence, X_presence_pred, model_type = "caret_ranger",
                                 X_background = res_train$X_presence)
if (!is.null(shap_presence)) {
  colnames(shap_presence) <- gsub("^shap_", "shap_pres_", colnames(shap_presence))
  df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_presence)
} else {
  cat("⚠ SHAP présence non disponible — voir warning ci-dessus\n")
}
# ==============

# new (SHAP sur le modèle combined — le plus important) ====
# Le modèle "combined" (two-part) n'est pas un objet d'arbre unique que treeshap
# peut expliquer directement : pred_combined_mean = pred_presence_prob * pred_abundance_q50
# (approximativement, avant simulation Monte Carlo). On approxime donc la contribution
# de chaque variable à la prédiction COMBINÉE par la règle du produit :
#   - variables d'abondance : poids = pred_presence_prob (la présence est tenue fixe)
#   - variables de présence : poids = pred_abundance_q50  (l'abondance est tenue fixe)
# C'est cette version "combined" qui doit être considérée comme la référence,
# pas le SHAP abondance seul.
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
  cat("⚠ SHAP combined non disponible (ni abondance ni présence n'ont pu être calculés)\n")
}
# ==============

# new (SHAP sur tous les modèles) ====
# SHAP — modèle d'abondance caret/ranger (mod_abundance_cv, comparaison/tuning)
# Même jeu de prédicteurs que l'abondance quantile (predictors_abundance), mais ce
# modèle est une régression caret classique (pas de quantiles) — utile pour comparer
# son explication à celle du modèle quantile final (shap_abund_*).
shap_abundance_cv <- compute_shap(mod_abundance_cv, X_abundance_pred, model_type = "caret_ranger")
if (!is.null(shap_abundance_cv)) {
  colnames(shap_abundance_cv) <- gsub("^shap_", "shap_abundcv_", colnames(shap_abundance_cv))
  df_meteo_predictions <- bind_cols(df_meteo_predictions, shap_abundance_cv)
} else {
  cat("⚠ SHAP abondance (modèle caret de comparaison) non disponible\n")
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

# new (modèle combined — exposer l'intervalle d'incertitude publié) ====
# mean_abundance_albopictus vient déjà de pred_combined_mean (le modèle two-part
# combined), mais on expose aussi l'intervalle de confiance combiné pour que la
# table publiée reflète explicitement le modèle combined, pas juste un point estimé.
combined_q05 <- rasterize_to_communes(df_meteo_predictions, "pred_combined_q05", roi) %>%
  rename(combined_abundance_q05 = pred_combined_q05) %>%
  dplyr::select(codgeo, date, combined_abundance_q05) %>%
  mutate(combined_abundance_q05 = round(combined_abundance_q05, 1), date = date + 1)

combined_q95 <- rasterize_to_communes(df_meteo_predictions, "pred_combined_q95", roi) %>%
  rename(combined_abundance_q95 = pred_combined_q95) %>%
  dplyr::select(codgeo, date, combined_abundance_q95) %>%
  mutate(combined_abundance_q95 = round(combined_abundance_q95, 1), date = date + 1)

combined_sd <- rasterize_to_communes(df_meteo_predictions, "pred_combined_sd", roi) %>%
  rename(combined_abundance_sd = pred_combined_sd) %>%
  dplyr::select(codgeo, date, combined_abundance_sd) %>%
  mutate(combined_abundance_sd = round(combined_abundance_sd, 2), date = date + 1)

abundance <- abundance %>%
  left_join(combined_q05, by = c("codgeo", "date")) %>%
  left_join(combined_q95, by = c("codgeo", "date")) %>%
  left_join(combined_sd,  by = c("codgeo", "date"))
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

# thresh_orange_red : QUOI = un seul nombre, la médiane de mean_abundance_albopictus
#   parmi les communes/semaines où l'abondance prédite est STRICTEMENT positive
#   (on ignore les zéros pour ne pas tirer la médiane vers le bas).
#   FAIT = sert de seuil pour classer le niveau de risque ci-dessous : c'est un
#   seuil RELATIF (recalculé à chaque run), pas une valeur absolue fixée d'avance.
thresh_orange_red <- median(
  albopictus_predictions$mean_abundance_albopictus[
    which(albopictus_predictions$mean_abundance_albopictus > 0 &
          !is.na(albopictus_predictions$mean_abundance_albopictus))
  ]
)

# level_risk : QUOI = catégorie texte (Faible/Modéré/Élevé) par commune x semaine,
#   dérivée de mean_abundance_albopictus comparée à thresh_orange_red ci-dessus.
albopictus_predictions <- albopictus_predictions %>%
  mutate(level_risk = case_when(
    mean_abundance_albopictus == 0 | is.na(mean_abundance_albopictus) ~ "Faible",
    mean_abundance_albopictus > 0 & mean_abundance_albopictus < thresh_orange_red ~ "Modéré",
    mean_abundance_albopictus >= thresh_orange_red ~ "Élevé"
  ))

# trend/class_trend : QUOI = variation en % de l'abondance par rapport à LA SEMAINE
#   PRÉCÉDENTE pour la même commune (lag(mean_abundance_albopictus) dans le temps,
#   groupé par codgeo). class_trend catégorise cette variation : "En hausse" si
#   > +20%, "En baisse" si < -20%, "Stable" sinon (ou si pas de comparaison possible).
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
# Abondance : shap_abund_<var> | Présence : shap_pres_<var> | Combined : shap_combined_<var>

shap_cols_abund <- grep("^shap_abund_TM|^shap_abund_UM|^shap_abund_RR",
                         colnames(df_meteo_predictions), value = TRUE)
shap_cols_pres  <- grep("^shap_pres_TM|^shap_pres_UM",
                         colnames(df_meteo_predictions), value = TRUE)
# new (SHAP sur le modèle combined — le plus important) ====
shap_cols_combined <- grep("^shap_combined_TM|^shap_combined_UM|^shap_combined_RR",
                            colnames(df_meteo_predictions), value = TRUE)
# ==============
# new (SHAP sur tous les modèles) ====
shap_cols_abundcv <- grep("^shap_abundcv_TM|^shap_abundcv_UM|^shap_abundcv_RR",
                           colnames(df_meteo_predictions), value = TRUE)
# ==============
all_shap_cols   <- c(shap_cols_abund, shap_cols_pres, shap_cols_combined, shap_cols_abundcv)

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

  # new (SHAP sur le modèle combined — le plus important) ====
  # Variable dominante par commune pour la prédiction COMBINÉE (référence principale,
  # à privilégier sur shap_abund_dominant_var qui n'explique que la sous-composante abondance)
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
  # ==============

  # new (SHAP sur tous les modèles) ====
  # Variable dominante par commune — modèle caret de comparaison (mod_abundance_cv)
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
  # ==============

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
} else {
  cat("✓ Aucune nouvelle donnée météo (historique et forecast déjà à jour) — ",
      "prédictions/SHAP non recalculés, tables", db_layer, "et", db_layer_shap,
      "inchangées.\n  (Mettre force_recompute <- TRUE en haut de cette section ",
      "pour forcer le recalcul, ex. après un nouvel entraînement de modèles.)\n", sep = "")
}
# ==============

dbDisconnect(con)
cat("\n✓ Pipeline hebdomadaire terminé.\n")
