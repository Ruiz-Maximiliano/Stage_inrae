# ============================================================
# 00_functions.R — Fonctions utilitaires du pipeline
# ============================================================

library(httr)
library(jsonlite)

#' Téléchargement des données météo historiques via l'API Open-Meteo
#'
#' @description
#' Télécharge des données météorologiques journalières historiques.
#' Deux modes : n_days jours vers le passé, ou dates explicites.
#'
#' @param latitude  Latitude du point (numérique)              [ENTRÉE]
#' @param longitude Longitude du point (numérique)             [ENTRÉE]
#' @param n_days    Nombre de jours vers le passé (optionnel)  [ENTRÉE]
#' @param start_date Date de début "YYYY-MM-DD" (optionnel)    [ENTRÉE]
#' @param end_date   Date de fin "YYYY-MM-DD" (optionnel)      [ENTRÉE]
#' @param daily     Vecteur de variables météo à télécharger   [ENTRÉE]
#' @param model     Modèle Open-Meteo (NULL = best match)      [ENTRÉE]
#' @return data.frame avec les variables météo + latitude + longitude [SORTIE]
#'
#' @examples
#' get_weather_history(43.6, 3.9, n_days = 90)
#' get_weather_history(43.6, 3.9, start_date = "2024-01-01", end_date = "2024-12-31")
#' get_weather_history(43.6, 3.9, n_days = 90, model = "meteofrance_seamless")
get_weather_history <- function(latitude, longitude, n_days = NULL,
                                 start_date = NULL, end_date = NULL,
                                 daily = c("temperature_2m_mean",
                                           "relative_humidity_2m_mean",
                                           "precipitation_sum"),
                                 model = NULL) {

  if (!is.null(n_days) & is.null(start_date) & is.null(end_date)) {
    start_date <- as.character(Sys.Date() - n_days)
    end_date   <- as.character(Sys.Date() - 1)

  } else if (!is.null(start_date) & !is.null(end_date)) {
    start_date <- as.character(start_date)
    end_date   <- as.character(end_date)

  } else {
    stop("Fournir soit n_days, soit start_date ET end_date")
  }

  query <- list(
    latitude   = latitude,
    longitude  = longitude,
    daily      = paste(daily, collapse = ","),
    start_date = start_date,
    end_date   = end_date
  )

  if (!is.null(model)) query$models <- model

  max_retries <- 7
  response    <- NULL
  for (attempt in seq_len(max_retries)) {
    wait_sec <- min(10 * 2^(attempt - 1), 300)  # backoff exponentiel : 10, 20, 40, 80, 160, 300s
    response <- tryCatch(
      GET("https://archive-api.open-meteo.com/v1/archive", query = query,
          httr::timeout(60)),
      error = function(e) {
        cat("  [history] Erreur réseau tentative", attempt, "/", max_retries,
            ":", conditionMessage(e), "— attente", wait_sec, "s\n")
        NULL
      }
    )
    if (!is.null(response) && status_code(response) == 200) break
    if (!is.null(response)) {
      cat("  [history] Status", status_code(response), "tentative", attempt,
          "/", max_retries, "— attente", wait_sec, "s\n")
    }
    if (attempt < max_retries) Sys.sleep(wait_sec)
  }
  if (is.null(response) || status_code(response) != 200)
    stop(paste("Erreur API après", max_retries, "tentatives"))

  df <- as.data.frame(fromJSON(content(response, as = "text", encoding = "UTF-8"))$daily)
  df$latitude  <- latitude
  df$longitude <- longitude
  return(df)
}


#' Téléchargement des prévisions météo via l'API Open-Meteo
#'
#' @description
#' Télécharge des prévisions météorologiques journalières.
#' Deux modes : n_days jours à partir d'aujourd'hui, ou dates explicites.
#'
#' @param latitude  Latitude du point (numérique)              [ENTRÉE]
#' @param longitude Longitude du point (numérique)             [ENTRÉE]
#' @param n_days    Nombre de jours de prévision (optionnel)   [ENTRÉE]
#' @param start_date Date de début "YYYY-MM-DD" (optionnel)    [ENTRÉE]
#' @param end_date   Date de fin "YYYY-MM-DD" (optionnel)      [ENTRÉE]
#' @param daily     Vecteur de variables météo à télécharger   [ENTRÉE]
#' @param model     Modèle Open-Meteo (NULL = best match)      [ENTRÉE]
#'   Note : maximum 16 jours de prévision (limite API)
#' @return data.frame avec les variables météo + latitude + longitude [SORTIE]
#'
#' @examples
#' get_weather_forecast(43.6, 3.9, n_days = 15)
#' get_weather_forecast(43.6, 3.9, start_date = "2026-05-18", end_date = "2026-06-01")
#' get_weather_forecast(43.6, 3.9, n_days = 15, model = "meteofrance_seamless")
get_weather_forecast <- function(latitude, longitude, n_days = NULL,
                                  start_date = NULL, end_date = NULL,
                                  daily = c("temperature_2m_mean",
                                            "relative_humidity_2m_mean",
                                            "precipitation_sum"),
                                  model = NULL) {

  if (!is.null(n_days) & is.null(start_date) & is.null(end_date)) {
    start_date <- as.character(Sys.Date())
    end_date   <- as.character(Sys.Date() + n_days)

  } else if (!is.null(start_date) & !is.null(end_date)) {
    start_date <- as.character(start_date)
    end_date   <- as.character(end_date)

  } else {
    stop("Fournir soit n_days, soit start_date ET end_date")
  }

  query <- list(
    latitude   = latitude,
    longitude  = longitude,
    daily      = paste(daily, collapse = ","),
    start_date = start_date,
    end_date   = end_date
  )

  if (!is.null(model)) query$models <- model

  max_retries <- 7
  response    <- NULL
  for (attempt in seq_len(max_retries)) {
    wait_sec <- min(10 * 2^(attempt - 1), 300)  # backoff exponentiel : 10, 20, 40, 80, 160, 300s
    response <- tryCatch(
      GET("https://api.open-meteo.com/v1/forecast", query = query,
          httr::timeout(60)),
      error = function(e) {
        cat("  [forecast] Erreur réseau tentative", attempt, "/", max_retries,
            ":", conditionMessage(e), "— attente", wait_sec, "s\n")
        NULL
      }
    )
    if (!is.null(response) && status_code(response) == 200) break
    if (!is.null(response)) {
      cat("  [forecast] Status", status_code(response), "tentative", attempt,
          "/", max_retries, "— attente", wait_sec, "s\n")
    }
    if (attempt < max_retries) Sys.sleep(wait_sec)
  }
  if (is.null(response) || status_code(response) != 200)
    stop(paste("Erreur API après", max_retries, "tentatives"))

  df <- as.data.frame(fromJSON(content(response, as = "text", encoding = "UTF-8"))$daily)
  df$latitude  <- latitude
  df$longitude <- longitude
  return(df)
}


# #new (1 - Choix modèle Open-Meteo) ====
#' Téléchargement batch historique — plusieurs coordonnées en 1 seul appel API
#'
#' @param latitudes  Vecteur de latitudes                                [ENTRÉE]
#' @param longitudes Vecteur de longitudes (même longueur que latitudes) [ENTRÉE]
#' @param start_date Date de début "YYYY-MM-DD"                          [ENTRÉE]
#' @param end_date   Date de fin "YYYY-MM-DD"                            [ENTRÉE]
#' @param daily      Variables météo à télécharger                       [ENTRÉE]
#' @param model      Modèle Open-Meteo (NULL = best match)               [ENTRÉE]
#' @return data.frame avec toutes les variables + latitude + longitude    [SORTIE]
get_weather_history_batch <- function(latitudes, longitudes, start_date, end_date,
                                       daily = c("temperature_2m_mean",
                                                 "relative_humidity_2m_mean",
                                                 "precipitation_sum"),
                                       model = NULL) {
  query <- list(
    latitude   = paste(round(latitudes,  6), collapse = ","),
    longitude  = paste(round(longitudes, 6), collapse = ","),
    daily      = paste(daily, collapse = ","),
    start_date = as.character(start_date),
    end_date   = as.character(end_date)
  )
  if (!is.null(model)) query$models <- model

  max_retries <- 7
  response    <- NULL
  for (attempt in seq_len(max_retries)) {
    wait_sec <- min(10 * 2^(attempt - 1), 300)
    response <- tryCatch(
      GET("https://archive-api.open-meteo.com/v1/archive", query = query,
          httr::timeout(120)),
      error = function(e) {
        cat("  [history batch] Erreur réseau tentative", attempt, "/", max_retries,
            ":", conditionMessage(e), "— attente", wait_sec, "s\n")
        NULL
      }
    )
    if (!is.null(response) && status_code(response) == 200) break
    if (!is.null(response))
      cat("  [history batch] Status", status_code(response), "tentative", attempt,
          "/", max_retries, "— attente", wait_sec, "s\n")
    if (attempt < max_retries) Sys.sleep(wait_sec)
  }
  if (is.null(response) || status_code(response) != 200)
    stop(paste("Erreur API batch après", max_retries, "tentatives"))

  .parse_openmeteo_batch(response, latitudes, longitudes)
}

#' Téléchargement batch forecast — plusieurs coordonnées en 1 seul appel API
#'
#' @param latitudes  Vecteur de latitudes                                [ENTRÉE]
#' @param longitudes Vecteur de longitudes (même longueur que latitudes) [ENTRÉE]
#' @param n_days     Nombre de jours de prévision                        [ENTRÉE]
#' @param daily      Variables météo à télécharger                       [ENTRÉE]
#' @param model      Modèle Open-Meteo (NULL = best match)               [ENTRÉE]
#' @return data.frame avec toutes les variables + latitude + longitude    [SORTIE]
get_weather_forecast_batch <- function(latitudes, longitudes, n_days = 14,
                                        daily = c("temperature_2m_mean",
                                                  "relative_humidity_2m_mean",
                                                  "precipitation_sum"),
                                        model = NULL) {
  query <- list(
    latitude   = paste(round(latitudes,  6), collapse = ","),
    longitude  = paste(round(longitudes, 6), collapse = ","),
    daily      = paste(daily, collapse = ","),
    start_date = as.character(Sys.Date()),
    end_date   = as.character(Sys.Date() + n_days)
  )
  if (!is.null(model)) query$models <- model

  max_retries <- 7
  response    <- NULL
  for (attempt in seq_len(max_retries)) {
    wait_sec <- min(10 * 2^(attempt - 1), 300)
    response <- tryCatch(
      GET("https://api.open-meteo.com/v1/forecast", query = query,
          httr::timeout(120)),
      error = function(e) {
        cat("  [forecast batch] Erreur réseau tentative", attempt, "/", max_retries,
            ":", conditionMessage(e), "— attente", wait_sec, "s\n")
        NULL
      }
    )
    if (!is.null(response) && status_code(response) == 200) break
    if (!is.null(response))
      cat("  [forecast batch] Status", status_code(response), "tentative", attempt,
          "/", max_retries, "— attente", wait_sec, "s\n")
    if (attempt < max_retries) Sys.sleep(wait_sec)
  }
  if (is.null(response) || status_code(response) != 200)
    stop(paste("Erreur API batch après", max_retries, "tentatives"))

  .parse_openmeteo_batch(response, latitudes, longitudes)
}

# Parseur interne — gère réponse unique ou tableau (batch)
.parse_openmeteo_batch <- function(response, latitudes, longitudes) {
  raw    <- fromJSON(content(response, as = "text", encoding = "UTF-8"),
                     simplifyDataFrame = FALSE)

  # Réponse unique (1 point) → liste simple ; multiple → liste de listes
  if (!is.null(raw$daily)) raw <- list(raw)

  results <- lapply(seq_along(raw), function(i) {
    daily_list <- raw[[i]]$daily
    if (is.null(daily_list)) return(NULL)
    df <- as.data.frame(daily_list, stringsAsFactors = FALSE)
    df$latitude  <- latitudes[i]
    df$longitude <- longitudes[i]
    df
  })

  dplyr::bind_rows(Filter(Negate(is.null), results))
}
# ==============


# #new (6 - Factorisation rasterisation) ====
#' Rasterise une variable et agrège par entité spatiale du ROI
#'
#' @description
#' Fonction générique qui remplace les blocs répétés de rasterisation +
#' extraction spatiale dans le pipeline. Gère automatiquement la résolution
#' à partir des données.
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
#' mean_tm <- rasterize_to_communes(meteo_comm, "TM", roi)
#' mean_shap <- rasterize_to_communes(df_pred, "shap_TM_0_4", roi)
rasterize_to_communes <- function(df, var_col, roi,
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

  r_template <- terra::rast(
    terra::ext(min(xs) - res_x / 2, max(xs) + res_x / 2,
               min(ys) - res_y / 2, max(ys) + res_y / 2),
    resolution = c(res_x, res_y),
    crs = "+proj=longlat +datum=WGS84"
  )

  v     <- terra::vect(df_sub, geom = c(x_col, y_col), crs = "EPSG:4326")
  dates <- unique(df_sub[[date_col]])

  rasters <- lapply(dates, function(d) {
    v_d <- v[v[[date_col]] == d, ]
    r   <- terra::rasterize(v_d, r_template, field = var_col, fun = "mean")
    names(r) <- d
    r
  })
  r_stack <- terra::rast(rasters)

  result <- exactextractr::exact_extract(r_stack, roi, c("mean")) %>%
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
# ==============


# #new (7 - SHAP toutes variables) ====
#' Calcule les valeurs SHAP pour un modèle ranger ou caret/ranger
#'
#' @description
#' Fonction générique SHAP utilisable avec n'importe quel modèle ranger
#' ou caret entraîné avec méthode ranger. Gère les deux formats et vérifie
#' la compatibilité (keep.inbag requis par treeshap).
#'
#' @param model      Modèle ranger ou caret (méthode ranger)        [ENTRÉE]
#' @param X_data     data.frame ou matrix des prédicteurs           [ENTRÉE]
#' @param model_type "ranger" (défaut) ou "caret_ranger"            [ENTRÉE]
#' @return data.frame avec colonnes shap_<var>, shap_dominant_var,
#'         shap_dominant_val pour chaque observation                [SORTIE]
#'
#' @examples
#' shap_abund <- compute_shap(rf_abundance_q, X_abundance, model_type = "ranger")
#' shap_pres  <- compute_shap(mod_presence,   X_presence,  model_type = "caret_ranger")
compute_shap <- function(model, X_data, model_type = c("ranger", "caret_ranger")) {

  model_type <- match.arg(model_type)
  X_df       <- as.data.frame(X_data)

  # Extraire le modèle ranger sous-jacent si caret
  ranger_mod <- if (model_type == "caret_ranger") model$finalModel else model

  if (!inherits(ranger_mod, "ranger")) {
    stop("Le modèle doit être un objet ranger (ou caret avec méthode ranger)")
  }

  # treeshap requiert keep.inbag = TRUE à l'entraînement
  if (!isTRUE(ranger_mod$inbag.counts)) {
    # keep.inbag présent sous forme de liste — vérification souple
    has_inbag <- !is.null(ranger_mod$inbag.counts)
    if (!has_inbag) {
      warning("Le modèle ranger n'a pas été entraîné avec keep.inbag = TRUE. ",
              "Les valeurs SHAP pourraient être incorrectes.")
    }
  }

  result <- tryCatch({

    # ranger.unify attend "pred.1" pour la régression, mais les forêts de
    # classification (probability = TRUE) utilisent des noms différents.
    # On tente une correction automatique si nécessaire.
    unified <- tryCatch(
      treeshap::ranger.unify(ranger_mod, X_df),
      error = function(e) {
        # Récupérer les arbres bruts et renommer la colonne de prédiction
        tree_data <- ranger::treeInfo(ranger_mod, tree = 1)
        pred_cols <- grep("^pred\\.", names(ranger_mod$forest$terminal.class.counts %||%
                                              ranger_mod$forest$split.values), value = TRUE)
        if (length(pred_cols) == 0) stop(conditionMessage(e))

        # Patch : renommer la première colonne pred.* en pred.1
        env <- environment(treeshap::ranger.unify)
        # Fallback : retenter avec le modèle tel quel mais skip_absent
        data.table::setnames(
          data.table::as.data.table(ranger_mod$forest),
          old = pred_cols[1], new = "pred.1", skip_absent = TRUE
        )
        treeshap::ranger.unify(ranger_mod, X_df)
      }
    )

    ts_result <- treeshap::treeshap(unified, X_df)
    shap_df   <- as.data.frame(ts_result$shaps)
    colnames(shap_df) <- paste0("shap_", colnames(shap_df))
    shap_cols <- colnames(shap_df)

    shap_df %>%
      dplyr::mutate(
        shap_dominant_var = gsub("shap_", "", shap_cols[
          apply(abs(.[, shap_cols, drop = FALSE]), 1, which.max)
        ]),
        shap_dominant_val = apply(.[, shap_cols, drop = FALSE], 1,
                                  function(x) x[which.max(abs(x))])
      )

  }, error = function(e) {
    warning("compute_shap : impossible de calculer les valeurs SHAP — ",
            conditionMessage(e), ". Colonnes SHAP absentes pour ce modèle.")
    NULL
  })

  return(result)
}
# ==============
