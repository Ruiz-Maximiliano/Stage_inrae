# ============================================================
# SCRIPT 2 — Pipeline de prédiction (remplacement forecast + calcul + publication)
# À exécuter régulièrement (cron job) — indépendant de la fréquence réelle
# d'exécution (voir section "Création des variables indépendantes" : la
# fenêtre à prédire se recalcule depuis la dernière prédiction publiée,
# quel que soit l'écart depuis la dernière exécution).
# Prérequis : Script 1 (initialisation) déjà exécuté
#
# CE QUE FAIT CE CODE (dans l'ordre) :
#   1. Met à jour la météo en BD (db_table_meteo_grid, format brut X/Y/date/TM/RR/UM) :
#      remplace le forecast déjà périmé par les vraies données historiques
#      (depuis la dernière date réelle en BD jusqu'à hier), télécharge le
#      forecast à venir (n_days_forecast jours). Écriture brute — aucune
#      agrégation par commune à l'écriture.
#   2. Lit uniquement les lag_max derniers jours de météo (format grille brut),
#      puis AGRÈGE PAR COMMUNE ICI, À LA LECTURE, via aggregate_meteo_to_roi()
#      — au lieu de toute la table — et construit les variables retardées
#      (lags TM/RR/UM) sur le résultat agrégé.
#   3. Charge les modèles entraînés et génère les prédictions two-part.
#   4. Calcule les explications LIME (lime_TM/lime_UM/lime_RR).
#   5. Publie 1 table en BD (db_layer) avec prédictions + LIME.
#   6. Rafraîchit les vues matérielles mean_10y/mean_2y (comparatif page web).
#
# NOTE : db_table_meteo (meteo_ruiz, par commune) n'est plus lue ni écrite par
#   ce script — remplacée par db_table_meteo_grid (format brut par point de
#   grille). meteo_ruiz reste disponible comme archive au format commune.
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Tous viennent de config.R. force_recompute (défaut FALSE) force le recalcul.
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   - config.R, 00_functions_api.R, 00_functions_formats.R, 00_functions_models.R, models/*.rds
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
library(lime)
library(logr)

source(here("scripts", "00_functions_api.R"))
source(here("scripts", "00_functions_formats.R"))
source(here("scripts", "00_functions_models.R"))
source(here("config.R"))

dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)
lf <- log_open(
  here("logs", paste0("hebdomadaire_", Sys.Date(), ".log")),
  autolog    = TRUE,
  show_notes = FALSE
)
log_print(paste("=== Run hebdomadaire —", Sys.time(), "==="))

options(datatable.week = "legacy")

# ============================================================
# Parallélisme (calcul LIME, via furrr) — reproductible en cron
# ============================================================
# n_workers défini dans config.R. Réglé ICI (au lieu de dépendre d'un
# future::plan() tapé à la main en console avant de sourcer ce script) pour
# que les runs cron/backfill soient parallélisés de façon identique à chaque
# fois. Si ce script est resourcé plusieurs fois dans la même session (ex.
# boucle de backfill par tranches), plan() est idempotent — pas de coût
# significatif à le rappeler.
future::plan(future::multisession, workers = n_workers)
log_print(paste("Parallélisme LIME : multisession,", n_workers, "workers"))

# ============================================================
# Paramètres locaux (lus depuis config.R)
# ============================================================
# grid_res, path_models, n_days_forecast, lag_max définis dans config.R —
# lag_max est aussi utilisé par 01_initialisation.R (marge de recul avant la
# 1ère semaine publiée) : centralisé pour que les deux restent synchronisés.

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
  # soit coupée par le serveur pendant les longs calculs R (init historique, LIME...)
  keepalives      = 1L,
  keepalives_idle = 60L
)
# Désactiver le timeout serveur pour les opérations longues
dbExecute(con, "SET statement_timeout = 0")

# ============================================================
# ROI (déjà chargé par config.R) et grid
# ============================================================

# roi, geopolygon, roi_info, all_codgeo viennent tous de config.R (calculés
# une seule fois là-bas — voir config.R) — plus besoin de les recalculer ici.

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

ensure_is_forecast_column(con, db_table_meteo_grid)

# ---- Étape 1 : Remplacer forecast (et combler les trous) par historical ----
# On regarde la DERNIÈRE date réellement historique (is_forecast = FALSE) en
# BD, et on télécharge tout ce qui manque depuis cette date jusqu'à hier —
# peu importe la taille du trou (pas de fenêtre fixe : si le pipeline ne
# tourne pas pendant longtemps, tout le retard est comblé en un run).
derniere_date_reelle <- as.Date(dbGetQuery(con, sprintf(
  "SELECT MAX(date) FROM %s WHERE NOT is_forecast", db_table_meteo_grid
))[[1]])

dates_a_remplacer <- if (is.na(derniere_date_reelle)) {
  # Table vide ou jamais alimentée en historique réel — rien à faire ici
  # (01_initialisation.R s'occupe du premier chargement complet).
  as.Date(character(0))
} else if (derniere_date_reelle >= Sys.Date() - 1) {
  # Déjà à jour jusqu'à hier — rien à remplacer.
  as.Date(character(0))
} else {
  seq(derniere_date_reelle + 1, Sys.Date() - 1, by = "day")
}

log_print(paste("Dernière date historique réelle :", if (is.na(derniere_date_reelle)) "aucune" else format(derniere_date_reelle),
                "| dates à remplacer :", length(dates_a_remplacer)))

if (length(dates_a_remplacer) > 0) {
  log_print(paste("Remplacement forecast/trous -> historical pour", length(dates_a_remplacer), "dates (",
      format(min(dates_a_remplacer)), "→", format(max(dates_a_remplacer)), ")"))

  meteo_updated <- data.frame()

  for (i in seq_along(meteo_prep)) {
    log_print(paste("Mise à jour historical — paquet", i, "sur", length(meteo_prep)))

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

  # Écriture BRUTE (X, Y) — aggregate_meteo_to_roi() est appelée plus bas, à
  # la LECTURE, avant les lags.
  meteo_updated$is_forecast <- FALSE
  grid_updated <- meteo_updated %>% dplyr::select(X, Y, date, TM, RR, UM, is_forecast)

  # BETWEEN plutôt qu'un IN(...) — dates_a_remplacer est une plage continue,
  # une borne min/max suffit et reste lisible même sur 2 mois de trous.
  dbExecute(con, sprintf(
    "DELETE FROM %s WHERE date >= '%s' AND date <= '%s'",
    db_table_meteo_grid, as.character(min(dates_a_remplacer)), as.character(max(dates_a_remplacer))
  ))
  dbWriteTable(con, db_table_meteo_grid, as.data.frame(grid_updated), append = TRUE, row.names = FALSE)

  log_print(paste("✓ Remplacement historique écrit en BD (", nrow(grid_updated), "lignes)"))
} else {
  log_print("✓ Historique déjà à jour jusqu'à hier — rien à remplacer")
}

# ---- Étape 2 : Télécharger le forecast à venir ----

# Si appelé depuis 01_initialisation.R, le forecast vient d'être téléchargé —
# on saute la re-vérification pour éviter une double écriture en BD.
if (exists("init_forecast_done") && isTRUE(init_forecast_done)) {
  forecast_needed <- FALSE
  log_print("✓ Forecast déjà téléchargé par l'initialisation — téléchargement ignoré")
} else {
  # Le forecast déjà en BD est périmé (modèle météo mis à jour chaque jour) —
  # on re-télécharge systématiquement le forecast futur.
  forecast_needed <- TRUE
  log_print("Forecast futur à re-télécharger (données fraîches)")
}

meteo_future <- data.frame()

if (forecast_needed) {
  log_print("Téléchargement du forecast...")

  for (i in seq_along(meteo_prep)) {
    log_print(paste("Forecast — paquet", i, "sur", length(meteo_prep)))

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

  # Écriture BRUTE (X, Y) — aucune agrégation par commune ici.
  meteo_future$is_forecast <- TRUE
  grid_future <- meteo_future %>% dplyr::select(X, Y, date, TM, RR, UM, is_forecast)

  dbExecute(con, sprintf("DELETE FROM %s WHERE date >= '%s'", db_table_meteo_grid, Sys.Date()))
  dbWriteTable(con, db_table_meteo_grid, as.data.frame(grid_future), append = TRUE, row.names = FALSE)
  log_print(paste("✓ Forecast écrit en BD (", nrow(grid_future), "lignes)"))
} else {
  log_print(paste("✓ Forecast déjà à jour en BD pour les", n_days_forecast,
      "jours à venir — téléchargement ignoré"))
}

log_print(paste("Forecast nécessaire :", forecast_needed))

# ---- Test de fraîcheur : BD vs backup CSV ----
# Compare la date historique la plus récente en BD (db_table_meteo_grid) avec
# celle du backup CSV local (path_backup). Le backup ne se régénère jamais
# automatiquement après la siembra initiale (scripts/08_seed_meteo_grid.R) —
# il se désynchronise donc naturellement de la BD au fil des runs
# hebdomadaires. Ce test se contente de le signaler (log + console), il ne
# corrige rien tout seul.
log_print("--- Test de fraîcheur : BD vs backup CSV ---")

max_date_bd <- as.Date(dbGetQuery(con, sprintf(
  "SELECT MAX(date) FROM %s WHERE NOT is_forecast", db_table_meteo_grid
))[[1]])
retard_bd <- as.integer(Sys.Date() - max_date_bd)
log_print(paste("BD (", db_table_meteo_grid, ") — dernière date historique :", format(max_date_bd),
    "(", retard_bd, "jour(s) avant aujourd'hui)"))
if (retard_bd > 7) {
  log_print(paste("⚠ BD météo en retard —", retard_bd, "jours sans donnée historique récente",
                  "— vérifier que le run hebdomadaire tourne bien régulièrement"))
}

if (file.exists(path_backup)) {
  max_date_backup <- suppressWarnings(max(
    as.Date(data.table::fread(path_backup, select = "date")$date), na.rm = TRUE
  ))
  ecart_backup <- as.integer(max_date_bd - max_date_backup)
  log_print(paste("Backup CSV (", path_backup, ") — dernière date :", format(max_date_backup),
      "| écart avec la BD :", ecart_backup, "jour(s)"))
  if (ecart_backup > 30) {
    log_print(paste("⚠ Backup CSV désynchronisé —", ecart_backup, "jours de retard sur la BD",
                    "— envisager de le régénérer (re-source 01_initialisation.R, ou exporter",
                    "manuellement depuis", db_table_meteo_grid, ")"))
  }
} else {
  max_date_backup <- NA
  log_print(paste("⚠ Backup CSV introuvable (", path_backup, ") — pas de comparaison possible."))
}

log_print(paste("Fraîcheur météo — BD :", format(max_date_bd),
                "| backup CSV :", if (is.na(max_date_backup)) "absent" else format(max_date_backup)))

######################################################
######### Création des variables indépendantes
######################################################

# backfill_start_date / backfill_end_date : si les DEUX sont définies (depuis
# un script appelant, ex. un backfill par tranches), remplacent complètement
# la logique init_lookback/n_days_forecast ci-dessous pour cibler une plage
# calendaire FIXE, au lieu d'une fenêtre ancrée sur Sys.Date(). Permet de
# backfiller l'explicabilité (LIME) historique par tranches sans re-traiter
# à chaque fois les années déjà faites.
#
# ⚠ Ces variables peuvent "coller" entre sessions R si un script de backfill
# est interrompu avant son rm() de fin de boucle — 01_initialisation.R les
# nettoie au démarrage par précaution (voir son début de fichier). Si vous
# lancez ce script seul après avoir interrompu un backfill, redémarrez la
# session R avant, ou faites rm(backfill_start_date, backfill_end_date).
.backfill_plage_fixe <- exists("backfill_start_date") && exists("backfill_end_date")

# .mode_normal : TRUE seulement pour le run hebdomadaire "normal" (ni
# backfill par tranches, ni Run 1 historique de 01_initialisation.R). Dans ce
# mode, .fenetre_a_predire est l'UNION de deux fenêtres JOURNALIÈRES (pas
# encore filtrées par jour de semaine — le filtre weekday == 1 est appliqué
# plus bas, sur meteo2, comme pour les 2 autres modes) :
#   - fenetre_fixe       : toujours [Sys.Date() - n_days_forecast, Sys.Date() +
#                           n_days_forecast] — TOUJOURS incluse. Nécessaire
#                           pour le Run 2 de 01_initialisation.R (LIME réel)
#                           juste après le Run 1 (historique, LIME NA) : le
#                           Run 1 publie jusqu'à l'horizon de forecast, donc
#                           sans fenetre_fixe le Run 2 croirait n'avoir rien
#                           à refaire et ne recalculerait jamais le LIME réel
#                           des prédictions récentes.
#   - fenetre_rattrapage : tout ce qui manque depuis la dernière prédiction
#                          publiée dans db_layer — pour ne rien sauter si le
#                          run a été espacé plus que d'habitude.
# Les modes backfill/init_lookback restent inchangés (toujours par lundis).
.mode_normal <- !.backfill_plage_fixe && !exists("init_lookback")

if (.mode_normal) {
  derniere_prediction <- if (dbExistsTable(con, db_layer)) {
    as.Date(dbGetQuery(con, sprintf("SELECT MAX(date) FROM %s", db_layer))[[1]])
  } else {
    NA
  }

  fenetre_fixe <- seq(Sys.Date() - n_days_forecast, Sys.Date() + n_days_forecast, by = "day")

  fenetre_rattrapage <- if (is.na(derniere_prediction) ||
                            derniere_prediction + 1 > Sys.Date() + n_days_forecast) {
    as.Date(character(0))
  } else {
    seq(derniere_prediction + 1, Sys.Date() + n_days_forecast, by = "day")
  }

  .fenetre_a_predire <- sort(union(fenetre_fixe, fenetre_rattrapage))

  log_print(paste("Dernière prédiction publiée :", if (is.na(derniere_prediction)) "aucune" else format(derniere_prediction),
                  "| jours de rattrapage :", length(fenetre_rattrapage),
                  "| jours candidats (avant filtre lundi) :", length(.fenetre_a_predire)))
}

# Optimisation : lire seulement les lag_max derniers jours au lieu de toute la table.
# Exception : si init_lookback est défini (depuis 01_initialisation.R), on lit aussi
# tout l'historique nécessaire pour couvrir tous les lundis historiques + leurs lags.
# .read_from = Sys.Date() - init_lookback - lag_max pour couvrir le lundi le plus ancien
# et ses lag_max jours de lags en arrière. En mode normal, on part du jour
# le plus ancien à prédire (pas forcément aujourd'hui) moins lag_max.
.read_from <- if (.backfill_plage_fixe) {
  as.Date(backfill_start_date) - lag_max
} else if (exists("init_lookback")) {
  Sys.Date() - init_lookback - lag_max
} else if (length(.fenetre_a_predire) > 0) {
  min(.fenetre_a_predire) - lag_max
} else {
  Sys.Date() - lag_max
}
# En mode backfill_start_date/end_date, on ajoute une borne haute à la
# lecture BD pour ne pas lire jusqu'à aujourd'hui à chaque tranche — c'est
# tout l'intérêt de découper le backfill en tranches calendaires.
.read_to_clause <- if (.backfill_plage_fixe) {
  sprintf(" AND date <= '%s'", as.character(as.Date(backfill_end_date)))
} else {
  ""
}
log_print(paste("Lecture de la météo depuis", format(.read_from), "depuis la BD (format grille)..."))

# Lecture BRUTE (X, Y, date, TM, RR, UM, is_forecast) depuis
# db_table_meteo_grid. aggregate_meteo_to_roi() reconstruit ensuite le
# niveau commune ICI, à la lecture (voir 00_functions_formats.R).
meteo_grid_brut <- dbGetQuery(con, sprintf(
  "SELECT * FROM %s WHERE date >= '%s'%s",
  db_table_meteo_grid, as.character(.read_from), .read_to_clause
))
meteo_grid_brut$date <- as.Date(meteo_grid_brut$date)
# Dédupliquer après lecture BD pour éviter le warning many-to-many (des
# doublons (X, Y, date) peuvent apparaître si la table a été écrite plusieurs fois)
meteo_grid_brut <- meteo_grid_brut %>% dplyr::distinct(X, Y, date, .keep_all = TRUE)

log_print(paste("Agrégation par commune (aggregate_meteo_to_roi) —", nrow(meteo_grid_brut), "lignes brutes..."))
meteo <- aggregate_meteo_to_roi(meteo_grid_brut, roi, grid_res) %>% as.data.table()
# Dédupliquer (codgeo, date) après agrégation, avant les lags.
meteo <- meteo %>% dplyr::distinct(codgeo, date, .keep_all = TRUE)

# meteo2 : dates à prédire dans la fenêtre météo (passées ET futures).
# On inclut les dates passées pour prédire aussi sur la météo réelle archivée —
# les lags seront calculés avec les valeurs réelles (archive) et non plus les
# valeurs de forecast de l'époque. na.omit() en aval élimine automatiquement les
# dates trop anciennes dont les lags dépassent la fenêtre lue (lag_max jours).
#
# Les 3 modes restent TOUJOURS par LUNDIS uniquement (weekday == 1) — le modèle
# est entraîné sur des semaines calendaires lundi-dimanche, une prédiction par
# jour n'aurait pas de sens. .fenetre_a_predire (mode normal) peut contenir des
# jours de semaine quelconques (c'est une plage journalière large — voir plus
# haut), mais seuls les lundis qu'elle contient donnent lieu à une prédiction ;
# les autres jours de la fenêtre (ex. le reliquat d'une semaine incomplète si
# on tombe un mercredi) sont ignorés ici, pas téléchargés/publiés en trop.
#
# group_by(codgeo, date) (plutôt que group_by(codgeo, year, week)) reste
# correct dans TOUS les cas (1 ou plusieurs lundis par commune).
meteo2 <- meteo %>%
  dplyr::select(codgeo, date) %>%
  # lubridate::wday() EXPLICITE (pas wday() nu) : lubridate ET data.table
  # exportent tous les deux une fonction wday() — comme library(data.table)
  # est chargé APRÈS library(lubridate) plus haut dans ce script,
  # data.table::wday() masque celle de lubridate pour tout appel non préfixé,
  # et options(datatable.week = "legacy") change en plus son comportement.
  # Résultat, déjà observé en pratique : weekday == 1 signifie LUNDI ou
  # DIMANCHE selon l'état exact de la session R (ordre de chargement des
  # packages, detach/reload...) — un bug non-reproductible, silencieux, qui
  # peut faire sauter des semaines entières lors d'un backfill.
  # week_start = 1 rend le sens de "1" explicite et indépendant de la session.
  mutate(weekday = lubridate::wday(date, week_start = 1)) %>%
  {
    if (.backfill_plage_fixe) {
      dplyr::filter(., weekday == 1,
                    date >= as.Date(backfill_start_date),
                    date <= as.Date(backfill_end_date))
    } else if (exists("init_lookback")) {
      dplyr::filter(., weekday == 1, date >= Sys.Date() - init_lookback)
    } else {
      dplyr::filter(., weekday == 1, date %in% .fenetre_a_predire)
    }
  } %>%
  slice(rep(1:n(), each = lag_max)) %>%
  group_by(codgeo, date) %>%
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

# fun_summarize_week()/fun_ccm_df() définies dans 00_functions_formats.R
# (partagées avec 07_seasonal_forecast_predictions.R)
df_meteo_pieges_summ <- fun_summarize_week(meteo3, "RR", "sum",  "RR", 7) %>%
  bind_rows(fun_summarize_week(meteo3, "TM", "mean", "TM", 7)) %>%
  bind_rows(fun_summarize_week(meteo3, "UM", "mean", "UM", 7))

df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n < 12)

df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ, "RR", "sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ, "TM", "mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ, "UM", "mean")

# by= explicite pour éviter le warning many-to-many (dplyr 1.1+)
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


# Skip recalcul/republication si rien n'a changé. Peut être forcé depuis la
# console avant source() : force_recompute <- TRUE ; source("scripts/02_hebdomadaire.R")
if (!exists("force_recompute")) force_recompute <- FALSE
meteo_changed   <- (length(dates_a_remplacer) > 0) || forecast_needed
db_layer_exists <- dbExistsTable(con, db_layer)
skip_recompute  <- !force_recompute && !meteo_changed && db_layer_exists

log_print(paste("meteo_changed :", meteo_changed,
                "| db_layer_exists :", db_layer_exists,
                "| skip_recompute :", skip_recompute))

if (!skip_recompute) {
######################################################
######### Chargement des modèles
######################################################

res_presence  <- readRDS(file.path(path_models, "res_presence_LOSO_probabilistic.rds"))
res_abundance <- readRDS(file.path(path_models, "res_abundance_LOSO_quantile_rf.rds"))

mod_presence   <- res_presence$model
rf_abundance_q <- res_abundance$model_quantile
mod_abundance_cv <- res_abundance$model_cv

predictors_presence  <- c("TM_0_8", "UM_5_11")
predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")

# explainer_presence/explainer_abundance : utilisés par add_lime_explanations()
# (00_functions_models.R) plus bas. Construits UNE SEULE FOIS à l'entraînement
# (00_train_models.R, section 9b) — les reconstruire ici à chaque run serait
# du travail redondant (lime::lime() ne dépend que des données
# d'entraînement, jamais des nouvelles prédictions).
# SÉPARÉS des RDS modèle : fichiers propres
# explainer_presence.rds/explainer_abundance.rds, plutôt qu'un élément
# $explainer dans res_presence/res_abundance. ATTENTION reproductibilité :
# si un modèle est réentraîné, ces 2 fichiers doivent l'être aussi (toujours
# le cas si on relance 00_train_models.R en entier — voir sa section 10) —
# sinon LIME expliquerait un modèle différent de celui réellement chargé.
explainer_presence  <- readRDS(file.path(path_models, "explainer_presence.rds"))
explainer_abundance <- readRDS(file.path(path_models, "explainer_abundance.rds"))


######################################################
######### Génération des prédictions
######################################################

# predict_two_part_uncertainty() définie dans 00_functions_models.R
df_meteo_predictions <- predict_two_part_uncertainty(
  newdata              = df_meteo_predictions,
  mod_presence         = mod_presence,
  rf_abundance_q       = rf_abundance_q,
  predictors_presence  = predictors_presence,
  predictors_abundance = predictors_abundance,
  n_sim                = 2000
)


######################################################
######### Calcul LIME
######################################################

# LIME explique séparément le modèle qui a effectivement produit la
# prédiction affichée pour cette ligne, selon le seuil pred_presence_prob :
#   - pred_presence_prob <  0.5 -> expliquée par le modèle de PRÉSENCE
#     (TM_0_8, UM_5_11 -> lime_TM, lime_UM ; lime_RR = NA, la présence
#     n'utilise pas RR)
#   - pred_presence_prob >= 0.5 -> expliquée par le modèle d'ABONDANCE
#     (TM_0_4, UM_0_11, RR_1_5 -> lime_TM, lime_UM, lime_RR)
# add_lime_explanations() (00_functions_models.R) fait ce partitionnement et
# renvoie les 3 colonnes lime_TM/lime_UM/lime_RR jointes sur (codgeo, date).

# skip_lime : utilisé pour désactiver LIME sur des milliers de semaines d'un
# coup (trop lent) lors du chargement historique. En run normal, non défini
# → LIME calculé. Les anciens scripts scripts/legacy/05_backfill_shap.R et
# scripts/legacy/06_backfill_shap_por_tramos.R (abandonnés, archivés, ne pas
# relancer) référencent encore skip_shap, le nom historique de cette
# variable (avant la migration SHAP → LIME) — volontairement laissés tels
# quels, ce sont des artefacts figés.
if (exists("skip_lime") && isTRUE(skip_lime)) {
  df_meteo_predictions <- df_meteo_predictions %>%
    dplyr::mutate(lime_TM = NA_real_, lime_UM = NA_real_, lime_RR = NA_real_)
  log_print("ℹ LIME désactivé (chargement historique initial) — colonnes NA")
} else {

  # lime_n_permutations/lime_n_features/lime_batch_size : valeurs par défaut
  # du calcul LIME — surchargeables depuis un script appelant (ex. un futur
  # backfill par tranches, sur le modèle des backfills existants).
  if (!exists("lime_n_permutations")) lime_n_permutations <- 200
  if (!exists("lime_n_features"))     lime_n_features     <- 6
  if (!exists("lime_batch_size"))     lime_batch_size     <- 1000

  df_meteo_predictions <- add_lime_explanations(
    df_meteo_predictions,
    explainer_presence   = explainer_presence,
    explainer_abundance  = explainer_abundance,
    predictors_presence  = predictors_presence,
    predictors_abundance = predictors_abundance,
    n_permutations       = lime_n_permutations,
    n_features           = lime_n_features,
    batch_size            = lime_batch_size
  )
  log_print("✓ LIME calculé (3 colonnes : lime_TM, lime_UM, lime_RR)")
}


######################################################
######### Agrégation par commune — prédictions entomo
######################################################

# df_meteo_predictions est déjà par commune (codgeo) — plus besoin de
# rasterize_to_roi() ici, on sélectionne directement.

abundance <- df_meteo_predictions %>%
  # TM_0_8/TM_0_4 : prédicteurs de température déjà calculés plus haut
  # (utilisés par les modèles de présence/abondance) — ajoutés ici tels
  # quels à la table publiée, pour traçabilité/lecture directe des valeurs
  # d'entrée des modèles.
  dplyr::select(codgeo, date, pred_combined_mean, TM_0_8, TM_0_4) %>%
  dplyr::rename(combined_abundance_q50 = pred_combined_mean) %>%
  dplyr::mutate(
    combined_abundance_q50 = round(combined_abundance_q50, 1),
    TM_0_8 = round(TM_0_8, 1),
    TM_0_4 = round(TM_0_4, 1)
    # ATTENTION : NE PAS ajouter "date = date + 1" ici — voir la note
    # détaillée sur meteo_out plus bas (même logique, décalage inverse).
    # "date" ici est déjà th_date (le vrai lundi), sans décalage à appliquer.
  )
# Pas de left_join(roi_info) ici — libgeo vient déjà de meteo_out (évite les
# colonnes communes implicites qui génèrent le warning many-to-many)

combined_q05 <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_combined_q05) %>%
  dplyr::rename(combined_abundance_q05 = pred_combined_q05) %>%
  dplyr::mutate(combined_abundance_q05 = round(combined_abundance_q05, 1))

combined_q95 <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_combined_q95) %>%
  dplyr::rename(combined_abundance_q95 = pred_combined_q95) %>%
  dplyr::mutate(combined_abundance_q95 = round(combined_abundance_q95, 1))

combined_sd <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_combined_sd) %>%
  dplyr::rename(combined_abundance_sd = pred_combined_sd) %>%
  dplyr::mutate(combined_abundance_sd = round(combined_abundance_sd, 2))

presence_prob <- df_meteo_predictions %>%
  dplyr::select(codgeo, date, pred_presence_prob) %>%
  dplyr::mutate(pred_presence_prob = round(pred_presence_prob, 3))

abundance <- abundance %>%
  left_join(combined_q05,   by = c("codgeo", "date")) %>%
  left_join(combined_q95,   by = c("codgeo", "date")) %>%
  left_join(combined_sd,    by = c("codgeo", "date")) %>%
  left_join(presence_prob,  by = c("codgeo", "date"))


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
    # ATTENTION : "date" ici vaut th_date - 1 (pas
    # th_date), car fun_summarize_week() construit la "semaine 0" à partir
    # des lags JOURNALIERS 1 à 6 seulement (6 jours, pas 7 — lag_n démarre à
    # 1, pas 0, donc floor(7/7)=1 fait basculer le jour lag=7 dans la semaine
    # 1) et prend "date = max(date)" de ce groupe = th_date - 1. Le code
    # faisait "date + 2" ici ET "date + 1" sur abundance/lime (qui partent,
    # eux, de th_date directement) — les deux se rejoignaient bien entre eux
    # (d'où l'absence d'erreur/de ligne perdue), mais le résultat publié était
    # th_date + 1 (ex. un mardi) au lieu de th_date (le vrai lundi) —
    # confirmé en BD : la date la plus récente publiée tombait un mardi
    # (EXTRACT(DOW)=2). Fix : +1 ici (pour repasser de th_date-1 à th_date),
    # et suppression du "+1" sur abundance/combined_q05/q95/sd/presence_prob/
    # lime_comm plus bas (qui partent déjà de th_date, sans décalage à
    # appliquer).
    date             = date + 1
  ) %>%
  dplyr::left_join(roi_info, by = "codgeo")


######################################################
######### Construction de la table de prédictions
######################################################

# by= explicite — libgeo vient de meteo_out uniquement
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
######### Construction de la table finale avec LIME
######################################################

lime_cols <- c("lime_TM", "lime_UM", "lime_RR")

if (all(lime_cols %in% colnames(df_meteo_predictions))) {

  lime_comm <- df_meteo_predictions %>%
    dplyr::select(codgeo, date, dplyr::all_of(lime_cols)) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(lime_cols), ~round(.x, 4))
      # ATTENTION : NE PAS ajouter "date = date + 1" ici — voir la note sur
      # meteo_out plus haut. "date" est déjà th_date, sans décalage.
    )

  albopictus_predictions_final <- albopictus_predictions %>%
    left_join(lime_comm, by = c("codgeo", "date"))

} else {
  log_print("⚠ Colonnes LIME absentes — table identique à la table principale")
  albopictus_predictions_final <- albopictus_predictions
}

# dbIsValid(con) ne suffit pas pour détecter une connexion coupée côté
# serveur (timeout alwaysdata, NAT, etc.) après un calcul long — il vérifie
# seulement que l'objet R n'a pas été fermé explicitement, pas que le lien
# réseau fonctionne encore. On teste donc activement avec une requête légère,
# et l'écriture elle-même est entourée d'un retry avec reconnexion — pour ne
# pas perdre un calcul long sur un simple aléa réseau.
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
  log_print("ℹ Reconnexion BD (connexion expirée ou coupée pendant le calcul)")
  try(dbDisconnect(con), silent = TRUE)
  con <- .reconnecter_bd()
}

# pred_presence_prob / lime_TM / lime_UM / lime_RR ajoutées au data.frame en
# amont, mais la table db_layer peut déjà exister en BD SANS ces colonnes
# (ex. créée avant leur ajout au pipeline) : st_write(..., append = TRUE)
# échouerait sinon avec "column ... does not exist". ALTER TABLE ... ADD
# COLUMN IF NOT EXISTS est idempotent — ne fait rien si la colonne existe déjà.
#
# col ENTRE GUILLEMETS DOUBLES dans le ALTER TABLE — sinon Postgres replie
# l'identifiant non quoté en minuscules (ex. lime_TM -> lime_tm), alors que
# dbWriteTable()/st_write() écrit ensuite avec la casse R exacte (lime_TM) :
# la colonne minuscule créée sans guillemets ne matcherait jamais, d'où
# "column lime_TM does not exist" au moment de l'écriture (même piège que
# "X"/"Y" dans 01_initialisation.R).
if (dbExistsTable(con, db_layer)) {
  champs_existants <- dbListFields(con, db_layer)
  colonnes_a_verifier <- c(
    pred_presence_prob = "DOUBLE PRECISION",
    lime_TM             = "DOUBLE PRECISION",
    lime_UM              = "DOUBLE PRECISION",
    lime_RR              = "DOUBLE PRECISION",
    # TM_0_8/TM_0_4 : prédicteurs de température ajoutés à la table publiée —
    # voir "abundance" plus haut.
    TM_0_8               = "DOUBLE PRECISION",
    TM_0_4               = "DOUBLE PRECISION",
    # level : "commune" ou "departement" — vient de roi_info (config.R,
    # admin_levels), rejoint via meteo_out plus haut (left_join(roi_info,
    # by = "codgeo")), aucun calcul ici.
    level                = "TEXT"
  )
  for (col in names(colonnes_a_verifier)) {
    if (!(col %in% champs_existants)) {
      dbExecute(con, sprintf(
        'ALTER TABLE %s ADD COLUMN "%s" %s', db_layer, col, colonnes_a_verifier[[col]]
      ))
      log_print(paste("✓ Colonne", col, "ajoutée à la table", db_layer))
    }
  }
}

# Supprimer uniquement les dates qu'on va écrire (évite les doublons si ce
# script tourne plusieurs fois sur la même fenêtre de dates, et conserve
# l'historique des dates déjà publiées)
dates_a_ecrire <- unique(albopictus_predictions_final$date)
dates_sql      <- paste(paste0("'", dates_a_ecrire, "'"), collapse = ",")

.ecrire_predictions <- function(con) {
  if (dbExistsTable(con, db_layer)) {
    dbExecute(con, sprintf("DELETE FROM %s WHERE date IN (%s)", db_layer, dates_sql))
  }
  st_write(albopictus_predictions_final, dsn = con, layer = db_layer, append = TRUE)
}

# 1re tentative — si elle échoue (coupure réseau juste après le test ci-dessus,
# ou pendant l'écriture elle-même), on reconnecte et on retente UNE fois avant
# d'abandonner (au lieu de tout perdre après un calcul potentiellement long).
ecriture_ok <- tryCatch({ .ecrire_predictions(con); TRUE }, error = function(e) {
  msg <- paste("Écriture BD échouée (1re tentative) —", conditionMessage(e),
               "— reconnexion et nouvelle tentative...")
  warning(msg, call. = FALSE)
  log_print(msg)
  FALSE
})

if (!ecriture_ok) {
  try(dbDisconnect(con), silent = TRUE)
  con <- .reconnecter_bd()
  .ecrire_predictions(con)   # si ça replante ici, on laisse l'erreur remonter (problème réseau plus profond)
  log_print("✓ Écriture BD réussie après reconnexion (2e tentative)")
}

log_print(paste("✓ Prédictions + LIME publiées →", db_layer,
                "| communes :", length(unique(albopictus_predictions_final$codgeo)),
                "| colonnes LIME :", length(lime_cols)))

} else {
  log_print(paste("✓ Aucune nouvelle donnée météo — prédictions/LIME non recalculés, table",
      db_layer, "inchangée. (Mettre force_recompute <- TRUE pour forcer.)"))
}

# Rafraîchit mean_10y/mean_2y à chaque exécution (même si le bloc ci-dessus
# n'a rien recalculé) — coût négligeable, garantit que les vues restent à
# jour sans script séparé à relancer manuellement. Voir refresh_mean_views()
# dans 00_functions_formats.R.
refresh_mean_views(con, db_layer, db_table_mean_10y, db_table_mean_2y)
log_print(paste("✓", db_table_mean_10y, "/", db_table_mean_2y, "rafraîchies"))

dbDisconnect(con)
log_print(paste("=== Fin du run hebdomadaire —", Sys.time(), "==="))
log_close()
