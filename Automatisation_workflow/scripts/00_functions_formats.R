# ============================================================
# 00_functions_formats.R — Fonctions de format et transformation des données
#
# CE QUE FAIT CE CODE :
#   Définit les fonctions qui construisent/transforment les structures de
#   données du pipeline : grid de points météo, rasterisation/agrégation
#   spatiale, résumé hebdomadaire et mise en forme large des séries météo,
#   colonne is_forecast, vues matérielles de moyenne historique. Ce fichier
#   ne fait RIEN tout seul — il ne fait que DÉFINIR des fonctions, appelées
#   depuis 01_initialisation.R, 02_hebdomadaire.R,
#   07_seasonal_forecast_predictions.R, 10_creer_vues_moyennes_hebdo.R, etc.
#   après un source(here("scripts", "00_functions_formats.R")).
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Aucun au niveau fichier — chaque fonction reçoit ses propres paramètres en
#   argument (voir le tag [ENTRÉE] dans la documentation de chaque fonction).
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   make_grid(), rasterize_to_roi(), aggregate_meteo_to_roi(),
#   ensure_is_forecast_column(), refresh_mean_views(), fun_summarize_week(),
#   fun_ccm_df().
# ============================================================

#' Crée le grid de points météo à l'intérieur du ROI
#'
#' @description
#' Génère un quadrillage régulier de points (centroïdes de carrés de
#' grid_res degrés) et ne garde que ceux qui tombent à l'intérieur de
#' geopolygon.
#'
#' @param geopolygon  Polygone sf représentant le ROI (union des communes)  [ENTRÉE]
#' @param roi_bbox    Bbox sf optionnel depuis config.R (NULL = bbox du ROI) [ENTRÉE]
#' @param grid_res    Résolution du grid en degrés (défaut 0.05)            [ENTRÉE]
#' @return data.frame avec colonnes X (longitude), Y (latitude), site (id)  [SORTIE]
make_grid <- function(geopolygon, roi_bbox = NULL, grid_res = 0.05) {

  bbox <- if (!is.null(roi_bbox)) {
    sf::st_bbox(roi_bbox, crs = 4326)
  } else {
    sf::st_bbox(geopolygon)
  }

  grid      <- sf::st_make_grid(sf::st_as_sfc(bbox), cellsize = grid_res,
                                square = TRUE, what = "polygons")
  grid_sf   <- sf::st_sf(geometry = grid)
  centroids <- sf::st_centroid(grid_sf)

  # Garder uniquement les centroïdes qui tombent dans geopolygon
  # sf_use_s2(FALSE) requis : st_intersection échoue sur certaines géométries ROI avec s2 activé.
  # Les messages "Spherical geometry switched off/on" et "assumes planar" sont attendus ici.
  sf::sf_use_s2(FALSE)
  centroids <- sf::st_intersection(centroids, geopolygon) %>%
    dplyr::select(geometry)
  sf::sf_use_s2(TRUE)

  coords      <- sf::st_coordinates(centroids)
  coords      <- round(as.data.frame(coords), 3)
  coords$site <- seq_len(nrow(coords))
  coords
}

#' Rasterise une variable et agrège par entité spatiale du ROI
#'
#' @description
#' Fonction générique de rasterisation + extraction spatiale, utilisée par
#' aggregate_meteo_to_roi() (agrégation météo) et par le calcul d'agrégats
#' spatiaux de colonnes SHAP. Gère automatiquement la résolution à partir
#' des données.
#'
#' La construction du raster multi-dates est VECTORISÉE (terra::cellFromXY()
#' une seule fois sur tous les points, puis remplissage direct de la matrice
#' de valeurs) plutôt que rasterisée date par date en boucle — sur un backfill
#' historique (des millions de lignes brutes, des milliers de dates), la
#' version en boucle refiltrait le vecteur de points complet à chaque date
#' (un terra::rasterize() par date), ce qui pouvait prendre des heures. Le
#' résultat est identique (même moyenne par cellule pour les points en double
#' après snapping, même exact_extract() en sortie), juste construit en une
#' poignée d'opérations vectorisées au lieu de milliers d'appels terra.
#'
#' @param df       data.frame avec colonnes x_col, y_col, date_col, var_col  [ENTRÉE]
#' @param var_col  Nom de la colonne à rasteriser (caractère)                [ENTRÉE]
#' @param roi      Objet sf — unités d'agrégation spatiale                   [ENTRÉE]
#' @param x_col    Nom de la colonne longitude snappée (défaut : "X_snap")   [ENTRÉE]
#' @param y_col    Nom de la colonne latitude snappée  (défaut : "Y_snap")   [ENTRÉE]
#' @param date_col Nom de la colonne date (défaut : "date")                  [ENTRÉE]
#' @return data.frame : toutes les colonnes du roi (sans géométrie) + date + var_col [SORTIE]
#'
#' @examples
#' mean_tm <- rasterize_to_roi(meteo_comm, "TM", roi)
#' mean_shap <- rasterize_to_roi(df_pred, "shap_TM_0_4", roi)
rasterize_to_roi <- function(df, var_col, roi,
                                   x_col    = "X_snap",
                                   y_col    = "Y_snap",
                                   date_col = "date") {

  df_sub <- df %>%
    dplyr::select(dplyr::all_of(c(x_col, y_col, date_col, var_col))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(date_col), as.character))

  xs    <- sort(unique(df_sub[[x_col]]))
  ys    <- sort(unique(df_sub[[y_col]]))
  res_x <- if (length(xs) > 1) min(diff(xs)) else 0.05
  res_y <- if (length(ys) > 1) min(diff(ys)) else 0.05

  # fix warning : "Cannot find proj.db" / "Cannot set SRS to vector: empty srs" — PROJ système absent, opérations WGS84 non affectées
  r_template <- suppressWarnings(terra::rast(
    terra::ext(min(xs) - res_x / 2, max(xs) + res_x / 2,
               min(ys) - res_y / 2, max(ys) + res_y / 2),
    resolution = c(res_x, res_y),
    crs = "+proj=longlat +datum=WGS84"
  ))

  dates <- sort(unique(df_sub[[date_col]]))

  # cell_id : indice de cellule du template pour CHAQUE point brut, calculé en
  # UN SEUL appel vectorisé (toutes les dates partagent la même grille X/Y,
  # donc ce mapping (X,Y) -> cellule est valable pour toutes les dates).
  # cbind(df_sub[[x_col]], df_sub[[y_col]]) plutôt que df_sub[, c(x_col, y_col)] :
  # ce dernier a une signification différente selon que df_sub est un
  # data.frame/tibble (sélectionne les 2 colonnes) ou un data.table (évalue
  # c(x_col, y_col) comme une expression, PAS une sélection de colonnes) —
  # [[ ]] extrait toujours le vecteur de la colonne, quel que soit le type.
  cell_id <- terra::cellFromXY(r_template, cbind(df_sub[[x_col]], df_sub[[y_col]]))

  dt <- data.table::data.table(
    cell     = cell_id,
    date_idx = match(df_sub[[date_col]], dates),
    val      = df_sub[[var_col]]
  )
  # Équivalent de fun = "mean" dans l'ancien terra::rasterize() : plusieurs
  # points bruts peuvent tomber sur la même cellule après snapping
  # (X_snap/Y_snap) — on moyenne les doublons (cell, date_idx).
  dt <- dt[, .(val = mean(val, na.rm = TRUE)), by = .(cell, date_idx)]

  vals_mat <- matrix(NA_real_, nrow = terra::ncell(r_template), ncol = length(dates))
  vals_mat[cbind(dt$cell, dt$date_idx)] <- dt$val

  r_stack <- suppressWarnings(terra::rast(r_template, nlyrs = length(dates)))
  terra::values(r_stack) <- vals_mat
  names(r_stack) <- dates

  result_ex <- exactextractr::exact_extract(r_stack, roi, c("mean"))
  # fix : exact_extract retourne un vecteur (ou data.frame à 1 col sans suffixe de date)
  # pour les rasters mono-date — on normalise en data.frame avec le bon nom de colonne
  if (length(dates) == 1) {
    if (!is.data.frame(result_ex)) result_ex <- data.frame(V1 = result_ex)
    names(result_ex) <- paste0("mean.", gsub("-", ".", as.character(dates[1])))
  }

  result <- result_ex %>%
    dplyr::bind_cols(sf::st_drop_geometry(roi)) %>%
    tidyr::pivot_longer(
      cols      = dplyr::starts_with("mean"),
      names_to  = date_col,
      values_to = var_col
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(date_col),
                    ~as.Date(gsub("mean-", "", gsub("\\.", "-", .x))))
    ) %>%
    dplyr::filter(!is.nan(.data[[var_col]]))

  return(result)
}

#' Agrège des données météo brutes (par point de grille) à l'échelle de la commune
#'
#' @description
#' Convertit les données brutes Open-Meteo (X/Y/date/TM/RR/UM) en données
#' agrégées par commune via rasterize_to_roi(). Appelée à la LECTURE dans
#' 02_hebdomadaire.R (et 07_seasonal_forecast_predictions.R) — la BD stocke
#' la météo au format brut par point de grille (db_table_meteo_grid),
#' l'agrégation par commune se fait au moment de l'utilisation plutôt qu'à
#' l'écriture.
#'
#' Avant le calcul, les coordonnées X/Y sont "snappées" sur la grille
#' (arrondies à la résolution grid_res) avec un petit epsilon avant round().
#' Sans cet epsilon, R arrondit les .5 exacts vers l'entier PAIR le plus
#' proche ("banker's rounding" — round(78.5) = 78, pas 79) : une coordonnée
#' tombant pile sur une frontière de grille (X / grid_res == un .5 exact)
#' se décale alors d'une cellule entière, ce qui peut faire disparaître
#' silencieusement une commune de la sortie agrégée (cellule NA à
#' l'extraction, filtrée par le !is.nan() de rasterize_to_roi()).
#'
#' @param raw_df   data.frame avec colonnes X, Y, date, TM, RR, UM             [ENTRÉE]
#' @param roi      Objet sf — communes du ROI                                    [ENTRÉE]
#' @param grid_res Résolution du grid en degrés (défaut 0.05)                   [ENTRÉE]
#' @return data.frame (codgeo, date, TM, RR, UM) — une ligne par commune x date [SORTIE]
aggregate_meteo_to_roi <- function(raw_df, roi, grid_res = 0.05) {
  raw_df <- raw_df %>%
    dplyr::mutate(
      date   = as.character(as.Date(date)),
      X_snap = round(X / grid_res + 1e-6) * grid_res,
      Y_snap = round(Y / grid_res + 1e-6) * grid_res
    )

  tm <- rasterize_to_roi(raw_df, "TM", roi) %>% dplyr::select(codgeo, date, TM)
  rr <- rasterize_to_roi(raw_df, "RR", roi) %>% dplyr::select(codgeo, date, RR)
  um <- rasterize_to_roi(raw_df, "UM", roi) %>% dplyr::select(codgeo, date, UM)

  tm %>%
    dplyr::left_join(rr, by = c("codgeo", "date")) %>%
    dplyr::left_join(um, by = c("codgeo", "date")) %>%
    dplyr::mutate(date = as.Date(date))
}

#' Garantit que la table météo possède la colonne is_forecast (BOOLEAN)
#'
#' @description
#' is_forecast distingue, pour chaque ligne de la table météo, si la valeur
#' est une PRÉVISION (forecast, encore incertaine) ou une vraie valeur
#' HISTORIQUE observée (réelle). Sert à savoir si une plage de dates a déjà
#' été remplacée par les vraies valeurs (Étape 1 de 02_hebdomadaire.R) sans
#' avoir à retélécharger/réécraser à chaque exécution.
#' ALTER TABLE ... ADD COLUMN est idempotent (sans effet si la colonne existe
#' déjà) et DEFAULT FALSE donne automatiquement is_forecast = FALSE à toutes
#' les lignes déjà existantes (considérées comme historiques).
#'
#' @param con        Connexion DBI active                              [ENTRÉE]
#' @param table_name Nom de la table météo (db_table_meteo_grid de config.R) [ENTRÉE]
#' @return invisible(NULL) — effet de bord : ALTER TABLE si la table existe  [SORTIE]
ensure_is_forecast_column <- function(con, table_name) {
  if (DBI::dbExistsTable(con, table_name)) {
    # fix warning : "NOTICE: column already exists, skipping" — on vérifie en R avant ALTER pour éviter le NOTICE PostgreSQL
    col_exists <- "is_forecast" %in% DBI::dbListFields(con, table_name)
    if (!col_exists) {
      DBI::dbExecute(con, sprintf(
        "ALTER TABLE %s ADD COLUMN is_forecast BOOLEAN DEFAULT FALSE",
        table_name
      ))
    }
  }
  invisible(NULL)
}

#' Crée/rafraîchit les vues matérielles de moyenne hebdomadaire par commune
#'
#' @description
#' (Re)crée deux vues matérielles PostgreSQL à partir de db_layer : moyenne,
#' par commune (codgeo) et semaine de l'année (même définition que
#' lubridate::week(), pas la semaine ISO), de toutes les métriques numériques
#' de prédiction (combined_abundance_q50/q05/q95/sd, pred_presence_prob) —
#' une sur les 10 années civiles précédant l'année la plus récente présente
#' dans db_layer, une sur les 2 années civiles précédentes. L'année en cours
#' est TOUJOURS exclue (elle sert de comparaison, pas de référence). Sert au
#' comparatif "abondance de l'année en cours vs moyenne historique" de
#' l'interface web. Appelée en fin de 02_hebdomadaire.R (donc aussi via
#' 01_initialisation.R, qui source hebdomadaire.R) — les vues restent à jour
#' automatiquement, sans script séparé à relancer manuellement.
#'
#' @param con              Connexion DBI active                              [ENTRÉE]
#' @param db_layer         Table source des prédictions (config.R)           [ENTRÉE]
#' @param db_table_mean_10y Nom de la vue matérielle 10 ans (config.R)        [ENTRÉE]
#' @param db_table_mean_2y  Nom de la vue matérielle 2 ans (config.R)         [ENTRÉE]
#' @return invisible(NULL) — effet de bord : DROP/CREATE des 2 vues en BD    [SORTIE]
refresh_mean_views <- function(con, db_layer, db_table_mean_10y, db_table_mean_2y) {
  if (!DBI::dbExistsTable(con, db_layer)) return(invisible(NULL))

  annee_max <- as.integer(DBI::dbGetQuery(con, sprintf(
    "SELECT EXTRACT(YEAR FROM MAX(date))::int FROM %s", db_layer
  ))[[1]])
  if (is.na(annee_max)) return(invisible(NULL))

  .creer_vue <- function(nom_vue, n_years) {
    annee_min <- annee_max - n_years
    sql <- sprintf("
      CREATE MATERIALIZED VIEW %s AS
      SELECT
        codgeo,
        (FLOOR((EXTRACT(DOY FROM date)::int - 1) / 7) + 1)::int AS week,
        AVG(combined_abundance_q50) AS combined_abundance_q50,
        AVG(combined_abundance_q05) AS combined_abundance_q05,
        AVG(combined_abundance_q95) AS combined_abundance_q95,
        AVG(combined_abundance_sd)  AS combined_abundance_sd,
        AVG(pred_presence_prob)     AS pred_presence_prob,
        COUNT(*) AS n_obs
      FROM %s
      WHERE EXTRACT(YEAR FROM date) >= %d
        AND EXTRACT(YEAR FROM date) <  %d
      GROUP BY codgeo, week
      ORDER BY codgeo, week
    ", nom_vue, db_layer, annee_min, annee_max)

    DBI::dbExecute(con, sprintf("DROP MATERIALIZED VIEW IF EXISTS %s", nom_vue))
    DBI::dbExecute(con, sql)
  }

  .creer_vue(db_table_mean_10y, 10)
  .creer_vue(db_table_mean_2y, 2)
  invisible(NULL)
}

#' Résume une série météo par pas de temps (ex. semaine) via une fonction d'agrégation
#'
#' @description
#' Agrège meteo3 (1 ligne par codgeo x th_date x lag_n x variable) en blocs de
#' n_days_agg jours (ex. 7 = semaine), en appliquant fun_summarize (sum/mean/
#' max/min) à var_to_summarize. Utilisée pour construire les prédicteurs
#' météo hebdomadaires (TM/RR/UM résumées, puis mises en forme large par
#' fun_ccm_df()) dans 02_hebdomadaire.R et 07_seasonal_forecast_predictions.R.
#'
#' @param meteo3          data.table avec colonnes codgeo, th_date, date, lag_n, var, val [ENTRÉE]
#' @param var_to_summarize Variable à résumer ("TM", "RR" ou "UM")                        [ENTRÉE]
#' @param fun_summarize    Fonction d'agrégation : "sum", "mean", "max" ou "min"          [ENTRÉE]
#' @param new_var_name     Nom à donner à la variable résumée en sortie                   [ENTRÉE]
#' @param n_days_agg       Taille du bloc temporel en jours (7 = semaine)                 [ENTRÉE]
#' @return data.table (codgeo, th_date, lag_n, date, var, val) résumé par bloc de n_days_agg [SORTIE]
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

#' Met en forme large une série résumée par lag, avec fenêtres glissantes cumulées
#'
#' @description
#' Pivote df_timeseries (filtré sur une variable) au format large (1 colonne
#' par lag_n), puis ajoute pour chaque paire de lags (i, j) une colonne
#' cumulée (moyenne ou somme des lags i à j) — c'est de là que viennent des
#' prédicteurs comme TM_0_8 (moyenne des lags 0 à 8) ou RR_1_5 (somme des
#' lags 1 à 5). Utilisée dans 02_hebdomadaire.R et
#' 07_seasonal_forecast_predictions.R après fun_summarize_week().
#'
#' @param df_timeseries     data.frame avec colonnes codgeo, th_date, var, lag_n, val [ENTRÉE]
#' @param varr              Variable à traiter ("TM", "RR" ou "UM")                   [ENTRÉE]
#' @param function_to_apply Agrégation des fenêtres cumulées : "mean" ou "sum"        [ENTRÉE]
#' @return data.frame large (codgeo, th_date, <varr>_<lag>, <varr>_<i>_<j>, ...)      [SORTIE]
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
