# ============================================================
# SCRIPT 1 — Initialisation
# À exécuter UNE SEULE FOIS avant de lancer le pipeline hebdomadaire
#
# Prérequis :
#   - Avoir exécuté 00_train_models.R au préalable (modèles .rds)
#   - Table BD db_table_admin accessible (limites administratives, voir config.R)
#
# Génère :
#   - data/meteo_history_backup.csv (backup local brut par point de grille)
#   - Table BD <db_table_meteo_grid> (historique + forecast météo, format grille brut)
#
# CE QUE FAIT CE CODE :
#   1. Construit un grid de points météo réguliers à l'intérieur du ROI (le
#      ROI lui-même est chargé une seule fois dans config.R, pas ici).
#   2. Charge l'historique météo, EN FORMAT BRUT (X, Y, date, TM, RR, UM,
#      is_forecast) — sans agrégation par commune à l'écriture :
#      - Si data/meteo_history_backup.csv existe : relit le CSV, écrit en BD
#        année par année (voir aussi scripts/08_seed_meteo_grid.R pour une
#        siembra initiale dédiée, plus rapide, hors de ce script).
#      - Sinon : télécharge via l'API Open-Meteo par semestre, écrit en BD.
#   3. Télécharge un forecast initial (n_days_forecast jours), écrit en BD brut.
#   4. Vérifie que les modèles entraînés (00_train_models.R) existent.
#
#   Schéma BD : (X, Y, date, TM, RR, UM, is_forecast) — format brut par point
#   de grille, dans une table SÉPARÉE (db_table_meteo_grid), agrégée à la
#   volée par aggregate_meteo_to_roi() au moment de la LECTURE dans
#   02_hebdomadaire.R. L'ancienne table par commune (db_table_meteo,
#   meteo_ruiz) N'EST PLUS ÉCRITE par ce script — elle reste disponible
#   comme archive au format commune.
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Tous viennent de config.R : db_table_admin, admin_dep, admin_levels, roi_bbox,
#   n_days_history, n_days_forecast, openmeteo_model, db_host/name/port/user/password,
#   db_table_meteo_grid. Rien à fournir directement dans CE script.
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   coords — voir le commentaire au-dessus de la variable.
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   - config.R : tous les paramètres listés ci-dessus, plus roi, geopolygon,
#     roi_info, all_codgeo (déjà chargés/calculés dans config.R).
#   - 00_functions_api.R : get_weather_history_batch(), get_weather_forecast_batch().
#   - 00_functions_formats.R : make_grid(), ensure_is_forecast_column().
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
library(exactextractr)
library(logr)

source(here("scripts", "00_functions_api.R"))
source(here("scripts", "00_functions_formats.R"))
source(here("config.R"))

# Nettoyage préventif : ce script fixe lui-même init_lookback / skip_lime /
# force_recompute / init_forecast_done pour ses deux runs (section 5) — mais
# des variables laissées par un backfill par tranches interrompu (ex.
# backfill_start_date/end_date) peuvent rester "collées" dans la session R si
# son rm() de fin de boucle n'a jamais été atteint. 02_hebdomadaire.R leur
# donne la PRIORITÉ sur init_lookback (voir .backfill_plage_fixe dans ce
# script) sans savoir qu'elles viennent d'ailleurs — on nettoie donc ici,
# avant de commencer, toute variable "optionnelle" que ce script ne contrôle
# pas lui-même.
# shap_max_background/shap_batch_size restent dans la liste ci-dessous — pas
# des variables actives, juste un filet de sécurité pour nettoyer ces noms
# s'ils traînent encore dans une session très ancienne (pré-migration LIME),
# aucun code actuel ne les définit ni ne les lit.
.vars_a_nettoyer <- c("backfill_start_date", "backfill_end_date",
                       "init_lookback", "skip_lime",
                       "force_recompute", "init_forecast_done",
                       "shap_max_background", "shap_batch_size",
                       "lime_n_permutations", "lime_n_features", "lime_batch_size")
rm(list = intersect(.vars_a_nettoyer, ls(envir = .GlobalEnv)), envir = .GlobalEnv)
rm(.vars_a_nettoyer)

# ============================================================
# Paramètres locaux (lus depuis config.R)
# ============================================================
# grid_res, path_models, path_backup, n_days_forecast définis dans config.R

dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
dir.create(path_models, showWarnings = FALSE)
dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)
lf <- log_open(
  here("logs", paste0("initialisation_", Sys.Date(), ".log")),
  autolog    = TRUE,
  show_notes = FALSE
)
log_print(paste("=== Run initialisation —", Sys.time(), "==="))
log_print(paste("Table météo (grille) :", db_table_meteo_grid,
                "| n_days_history :", n_days_history,
                "| n_days_forecast :", n_days_forecast))

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
# Désactiver le timeout serveur pour les COPY longues (chargement historique 10 ans)
dbExecute(con, "SET statement_timeout = 0")

ensure_is_forecast_column(con, db_table_meteo_grid)

# ============================================================
# 1. ROI (déjà chargé par config.R) et création du grid
# ============================================================

# roi, geopolygon, roi_info, all_codgeo viennent tous de config.R (calculés
# une seule fois là-bas — voir config.R) — plus besoin de les recalculer ici.

log_print(paste("Création du grid (résolution", grid_res, "°)..."))

# coords : data.frame X/Y/site créé en mémoire par make_grid() (00_functions_formats.R).
coords <- make_grid(geopolygon, roi_bbox, grid_res)
log_print(paste("Grid créé :", nrow(coords), "points (résolution", grid_res, "°)"))

# Fonction utilitaire : crée les batches de coordonnées selon la durée demandée
make_meteo_prep <- function(coords_df, n_days) {
  coords_per_batch <- max(1, min(100, floor(20000 / n_days)))
  log_print(paste("Taille des batches :", coords_per_batch, "coords ×", n_days, "jours =",
      coords_per_batch * n_days, "points/requête"))
  coords_df %>%
    group_by(row_number() %/% coords_per_batch) %>%
    group_map(~.x) %>%
    map(., ~group_split(., site))
}

# ============================================================
# 2. Chargement de l'historique météo → BD (format grille brut)
# ============================================================

log_print(paste("Chargement de l'historique météo (", n_days_history, "jours, +", lag_max,
                "jours de marge lags)..."))

# start_date recule de lag_max jours SUPPLÉMENTAIRES par rapport à
# n_days_history — cette marge n'est JAMAIS publiée comme semaine prédite
# (voir init_lookback, section 5 plus bas, qui la retranche explicitement),
# elle sert uniquement à donner aux TOUTES premières semaines publiées un
# recul météo complet pour leurs lags (jusqu'à 11 semaines = 77 jours réels,
# voir fun_summarize_week()). Sans cette marge, les ~6 premières semaines de
# tout run d'initialisation complet manquent de lags, sont éliminées par
# na.omit() dans 02_hebdomadaire.R, et publient NULL pour toutes les communes.
start_date     <- Sys.Date() - n_days_history - lag_max
end_date       <- Sys.Date() - 1
expected_dates <- (n_days_history + lag_max) * 0.80
phase_needed   <- FALSE

# db_table_meteo_grid est censée être créée soit par ce script, soit par
# scripts/08_seed_meteo_grid.R (siembra rapide depuis le backup CSV) — les deux
# écrivent le même schéma (X, Y, date, TM, RR, UM, is_forecast), donc ce test
# fonctionne quelle que soit l'origine des données déjà présentes. Fraîcheur
# vérifiée par point de grille (X, Y), pas par codgeo.
if (dbExistsTable(con, db_table_meteo_grid) &&
    dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo_grid))[[1]] > 0) {

  # "X"/"Y" entre guillemets doubles — Postgres replie les identifiants non
  # quotés en minuscules (x/y), alors que dbWriteTable() a créé les colonnes en
  # conservant la casse R.
  grid_check <- dbGetQuery(con, sprintf(
    'SELECT "X", "Y", COUNT(DISTINCT date) AS n_dates FROM %s WHERE NOT is_forecast GROUP BY "X", "Y"',
    db_table_meteo_grid
  ))
  grid_ok         <- grid_check[grid_check$n_dates >= expected_dates, ]
  # Points incomplets = présents en BD mais avec trop peu de dates
  grid_incomplets <- grid_check[grid_check$n_dates < expected_dates, ]

  log_print(paste("BD existante —", nrow(grid_check), "points de grille",
                  "| complets (≥", round(expected_dates), "dates) :", nrow(grid_ok),
                  "| incomplets :", nrow(grid_incomplets)))

  if (nrow(grid_incomplets) > 0) {
    # Supprimer les points incomplets pour re-charger proprement (clé composite X, Y
    # — pas de colonne id unique simple comme codgeo, donc une DELETE par point)
    for (i in seq_len(nrow(grid_incomplets))) {
      dbExecute(con, sprintf(
        'DELETE FROM %s WHERE "X" = %f AND "Y" = %f AND NOT is_forecast',
        db_table_meteo_grid, grid_incomplets$X[i], grid_incomplets$Y[i]
      ))
    }
    phase_needed <- TRUE
  }

} else {
  phase_needed <- TRUE
  log_print("BD vide — chargement complet")
}

if (phase_needed) {

  # ---- Chemin 1 : depuis le backup CSV (rapide, pas d'appel API) ----
  if (file.exists(path_backup)) {
    log_print("Backup CSV trouvé — chargement (format grille brut, sans agrégation)...")

    # Lecture du backup (format brut par point de grille) — écrit TEL QUEL,
    # sans passer par aggregate_meteo_to_roi() (l'agrégation se fait côté
    # lecture, dans 02_hebdomadaire.R)
    backup <- read.csv(path_backup) %>%
      dplyr::rename(
        TM = temperature_2m_mean,
        RR = precipitation_sum,
        UM = relative_humidity_2m_mean
      ) %>%
      dplyr::mutate(date = as.Date(date), is_forecast = FALSE) %>%
      dplyr::select(X, Y, date, TM, RR, UM, is_forecast) %>%
      dplyr::filter(date >= start_date)

    years <- sort(unique(format(backup$date, "%Y")))
    log_print(paste("Années à traiter :", paste(years, collapse = ", ")))

    for (yr in years) {
      yr_data <- backup %>% dplyr::filter(format(date, "%Y") == yr)
      dbWriteTable(con, db_table_meteo_grid, as.data.frame(yr_data),
                   append = TRUE, row.names = FALSE)
      log_print(paste("Année", yr, ":", nrow(yr_data), "lignes écrites"))
    }
    log_print("✓ Historique chargé depuis backup CSV (format grille)")

  # ---- Chemin 2 : téléchargement API (fallback si pas de backup) ----
  } else {
    log_print("Backup CSV absent — téléchargement via l'API Open-Meteo par semestre...")

    # Découpe en périodes de 6 mois pour limiter la mémoire par lot
    period_starts <- seq(start_date, end_date, by = "6 months")
    period_ends   <- c(period_starts[-1] - 1, end_date)

    for (p in seq_along(period_starts)) {
      p_start <- period_starts[p]
      p_end   <- period_ends[p]
      n_days_p <- as.numeric(p_end - p_start) + 1

      log_print(paste("--- Période", as.character(p_start), "→", as.character(p_end),
          "|", p, "/", length(period_starts), "---"))

      meteo_prep_p <- make_meteo_prep(coords, n_days_p)
      raw_period   <- data.frame()

      for (i in seq_along(meteo_prep_p)) {
        log_print(paste("Batch", i, "/", length(meteo_prep_p)))
        batch_df   <- dplyr::bind_rows(meteo_prep_p[[i]])
        th_res_api <- get_weather_history_batch(
          latitudes  = batch_df$Y,
          longitudes = batch_df$X,
          start_date = p_start,
          end_date   = p_end,
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
        raw_period <- rbind(raw_period, th_res)
        Sys.sleep(5)
      }

      # Écriture BRUTE (X, Y), sans agrégation par commune ici.
      raw_period$is_forecast <- FALSE
      grid_data <- raw_period %>% dplyr::select(X, Y, date, TM, RR, UM, is_forecast)
      dbWriteTable(con, db_table_meteo_grid, as.data.frame(grid_data),
                   append = TRUE, row.names = FALSE)
      log_print(paste("Période", as.character(p_start), "→", as.character(p_end),
                      ":", nrow(grid_data), "lignes écrites"))
      Sys.sleep(60)
    }
    log_print("✓ Historique téléchargé (format grille)")
  }
}

# ---- Vérification de la continuité de l'historique ----
log_print("Vérification des lacunes dans l'historique météo...")
dates_bd <- as.Date(dbGetQuery(con, sprintf(
  "SELECT DISTINCT date FROM %s WHERE is_forecast = FALSE ORDER BY date",
  db_table_meteo_grid
))$date)

if (length(dates_bd) > 1) {
  dates_attendues  <- seq(min(dates_bd), max(dates_bd), by = "day")
  dates_manquantes <- dates_attendues[!dates_attendues %in% dates_bd]

  if (length(dates_manquantes) == 0) {
    log_print("✓ Historique continu — aucune lacune")
  } else {
    # Regrouper en périodes consécutives
    groupes <- split(dates_manquantes, cumsum(c(1, diff(dates_manquantes) > 1)))
    log_print("⚠ LACUNES DÉTECTÉES dans l'historique météo :")
    for (g in groupes) {
      if (length(g) == 1) {
        log_print(paste("  -", format(g)))
      } else {
        log_print(paste("  -", format(min(g)), "→", format(max(g)),
            "(", length(g), "jours)"))
      }
    }
    log_print(paste("⚠ Lacunes historique :", length(dates_manquantes),
                    "jours manquants — relancer l'initialisation ou compléter via l'API"))
  }
} else {
  log_print("⚠ Historique vide ou insuffisant")
}

# ============================================================
# 3. Téléchargement du forecast initial
# ============================================================

# Compte combien de points de coords ont au moins n_days_forecast dates de
# forecast déjà en BD — si tous les points téléchargés (coords) sont
# couverts, pas besoin de retélécharger.
forecast_needed <- TRUE

if (dbExistsTable(con, db_table_meteo_grid)) {
  # "X"/"Y" quotés (voir plus haut) — sinon Postgres cherche des colonnes x/y minuscules
  n_points_ok <- dbGetQuery(con, sprintf(
    'SELECT COUNT(*) AS n FROM (SELECT "X", "Y" FROM %s WHERE date >= \'%s\' GROUP BY "X", "Y" HAVING COUNT(DISTINCT date) >= %d) sub',
    db_table_meteo_grid, as.character(Sys.Date()), n_days_forecast
  ))$n
  if (n_points_ok >= nrow(coords)) forecast_needed <- FALSE
}

meteo_future <- data.frame()

if (forecast_needed) {
  log_print("Téléchargement du forecast initial...")

  meteo_prep_forecast <- make_meteo_prep(coords, n_days_forecast)

  for (i in seq_along(meteo_prep_forecast)) {
    log_print(paste("Forecast — paquet", i, "sur", length(meteo_prep_forecast)))

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
      dplyr::rename(date = time,
                    TM   = temperature_2m_mean,
                    RR   = precipitation_sum,
                    UM   = relative_humidity_2m_mean) %>%
      dplyr::mutate(date = as.Date(as.character(date)),
                    X = longitude, Y = latitude)
    meteo_future <- rbind(meteo_future, th_res)
    Sys.sleep(60)
  }
} else {
  log_print(paste("✓ Forecast déjà à jour en BD pour les", n_days_forecast,
      "jours à venir — téléchargement ignoré"))
}

if (nrow(meteo_future) > 0) {
  # Écriture BRUTE (X, Y), sans agrégation par commune ici.
  meteo_future$is_forecast <- TRUE
  meteo_future_grid <- meteo_future %>% dplyr::select(X, Y, date, TM, RR, UM, is_forecast)
  dbWriteTable(con, db_table_meteo_grid, as.data.frame(meteo_future_grid),
               append = TRUE, row.names = FALSE)
}

n_total <- as.integer(dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", db_table_meteo_grid))[[1]])
log_print(paste("✓ Table météo BD :", db_table_meteo_grid, "|", n_total, "lignes au total",
                "| forecast téléchargé :", forecast_needed))

# Le backup CSV est conservé au format BRUT (site/X/Y/temperature_2m_mean/...)
# — même format que db_table_meteo_grid — donc utile comme fallback/re-siembra
# (scripts/08_seed_meteo_grid.R). Il n'est pas régénéré automatiquement ici.
if (!file.exists(path_backup) && dbExistsTable(con, db_table_meteo_grid)) {
  log_print("Pas de backup CSV — génération ignorée (à régénérer manuellement si besoin, voir path_backup dans config.R).")
}

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
log_print(paste("✓ Modèles détectés :", paste(rds_files, collapse = ", ")))

# Récupérer la date min de db_table_meteo_grid avant de fermer la connexion
# (nécessaire pour calculer init_lookback dans la section 5 ci-dessous)
meteo_min_date <- as.Date(dbGetQuery(con, sprintf(
  "SELECT MIN(date) FROM %s", db_table_meteo_grid))[[1]])

dbDisconnect(con)

# Annonce des 2 runs qui suivent, consignée ici (dans le log de CE script)
# avant de le fermer — logr ne supporte qu'un seul log actif à la fois, donc
# ce log doit être fermé avant d'appeler 02_hebdomadaire.R, qui ouvre (et
# ferme) le sien, une fois par run (logs/hebdomadaire_<date>.log).
log_print("--- Run 1/2 à suivre : prédictions historiques (LIME = NA) ---")
log_print("--- Run 2/2 à suivre : prédictions récentes avec LIME ---")
log_print(paste("=== Fin du run initialisation météo —", Sys.time(), "==="))
log_close()

# ============================================================
# 5. Génération des prédictions initiales
# ============================================================
# Deux runs de 02_hebdomadaire.R (chacun avec son propre log, voir ci-dessus) :
#
# Run 1 — historique complet, LIME désactivé :
#   init_lookback = nbre de jours depuis le début de db_table_meteo_grid → tous les lundis
#   skip_lime = TRUE → LIME = NA (trop lent sur des années de données)
#   force_recompute = TRUE → ne pas sauter même si meteo_changed = FALSE
#
# Run 2 — prédictions récentes avec LIME :
#   fenêtre normale (n_days_forecast jours en arrière)
#   LIME calculé normalement → écrase les prédictions récentes avec valeurs LIME
#
# "- lag_max" : la 1ère semaine publiée doit rester à AU MOINS lag_max jours
# de meteo_min_date (voir start_date, section 2 plus haut, qui télécharge
# exactement cette marge en plus) — sinon .read_from dans 02_hebdomadaire.R
# demande des jours antérieurs à meteo_min_date, qui n'existent pas, et
# na.omit() élimine ces semaines pour toutes les communes (NULL en BD).
# max(..., 7) : garde-fou pour ne jamais tomber à 0/négatif si l'historique
# réellement en BD est plus court que lag_max (backup CSV partiel, etc.).
init_lookback  <- max(as.integer(Sys.Date() - meteo_min_date) + 1 - lag_max, 7)
skip_lime      <- TRUE
force_recompute <- TRUE
init_forecast_done <- TRUE
source(here("scripts", "02_hebdomadaire.R"))
rm(init_lookback, skip_lime, force_recompute, init_forecast_done)

force_recompute    <- TRUE
init_forecast_done <- TRUE
source(here("scripts", "02_hebdomadaire.R"))
rm(force_recompute, init_forecast_done)

# ============================================================
# 6. Backfill complet de LIME sur tout l'historique
# ============================================================
# Après les Runs 1/2 ci-dessus, l'historique complet a des prédictions
# (présence/abondance) mais LIME reste en NA sur les années anciennes — Run 1
# l'a désactivé exprès (skip_lime = TRUE) pour aller vite. Ce bloc évite
# d'avoir à relancer ce backfill à la main après coup : un seul
# source("01_initialisation.R") suffit à tout avoir, LIME inclus.
#
# PAR ANNÉE CIVILE (plutôt qu'un seul backfill de tout l'historique d'un
# coup) : plus gérable en mémoire/temps, et permet de reprendre facilement
# si la session R plante à mi-chemin (relancer ce for() en ajustant
# annee_min ci-dessous à l'année où ça s'est arrêté).
#
# Idempotent avec Run 2 : les dernières semaines seront recalculées une
# 2e fois ici (déjà avec LIME réel depuis Run 2) — résultat identique,
# juste un peu de calcul redondant, pas de risque.
# NOTE : pas de log_print() ici — le log de CE script (lf) a déjà été fermé
# plus haut (section 4, avant les Runs 1/2), et logr ne supporte qu'un log
# actif à la fois. cat() suffit pour voir la progression en console ; le
# détail de chaque année est de toute façon dans son propre
# logs/hebdomadaire_<date>.log (ouvert/fermé par 02_hebdomadaire.R).
cat("--- Backfill LIME sur tout l'historique (par année) ---\n")
annee_min <- lubridate::year(meteo_min_date)
annee_max <- lubridate::year(Sys.Date())

for (.y in annee_min:annee_max) {
  backfill_start_date <- max(as.Date(paste0(.y, "-01-01")), meteo_min_date)
  backfill_end_date   <- min(as.Date(paste0(.y, "-12-31")), Sys.Date())
  force_recompute     <- TRUE
  # skip_lime NE DOIT PAS être défini ici — on veut LIME calculé pour de vrai
  cat("=== Backfill LIME — année", .y, ":", format(backfill_start_date), "->",
      format(backfill_end_date), "===\n")
  source(here("scripts", "02_hebdomadaire.R"))
  rm(backfill_start_date, backfill_end_date, force_recompute)
}
rm(.y, annee_min, annee_max)
