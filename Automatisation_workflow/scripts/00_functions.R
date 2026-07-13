# ============================================================
# 00_functions.R — Fonctions utilitaires du pipeline
#
# CE QUE FAIT CE CODE :
#   Définit toutes les fonctions partagées par les autres scripts (téléchargement
#   météo Open-Meteo, rasterisation/agrégation spatiale, calcul SHAP, prédiction
#   combinée two-part). Ce fichier ne fait RIEN tout seul — il ne fait que DÉFINIR
#   des fonctions, qui sont appelées depuis 00_train_models.R, 01_initialisation.R,
#   01_initialisation_proposition.R et 02_hebdomadaire.R après un
#   source(here("scripts", "00_functions.R")).
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Aucun au niveau fichier — chaque fonction reçoit ses propres paramètres en
#   argument (voir le tag [ENTRÉE] dans la documentation de chaque fonction ci-dessous).
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   Les fonctions elles-mêmes : get_weather_history(), get_weather_forecast(),
#   get_weather_history_batch(), get_weather_forecast_batch(),
#   get_weather_seasonal_forecast_batch(), .parse_openmeteo_batch() (interne),
#   rasterize_to_roi(), compute_shap(), predict_two_part_uncertainty().
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   Aucun à la lecture du fichier — mais au moment de l'EXÉCUTION, les fonctions
#   utilisent des objets créés ailleurs (ex. predict_two_part_uncertainty() reçoit
#   mod_presence et rf_abundance_q, chargés depuis les RDS dans 02_hebdomadaire.R).
# ============================================================

library(httr)    # GET(), status_code(), content() — appels à l'API Open-Meteo
library(jsonlite) # fromJSON() — décodage des réponses JSON de l'API

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

# #new (prévision saisonnière Open-Meteo) ====
#' Téléchargement batch prévision saisonnière — jusqu'à 9 mois, plusieurs points
#'
#' @description
#' Utilise l'API seasonal d'Open-Meteo (https://seasonal-api.open-meteo.com/v1/seasonal)
#' pour télécharger des prévisions d'ensemble jusqu'à ~9 mois à l'avance.
#' Retourne la moyenne des membres d'ensemble pour TM, RR, UM — même format que
#' get_weather_forecast_batch(), compatible avec le pipeline.
#'
#' PARTICULARITÉS DE L'API SEASONAL (différences avec l'API forecast standard) :
#'   - "temperature_2m_mean" N'EXISTE PAS → l'API ne fournit que max et min
#'     par membre. Cette fonction calcule la moyenne (max+min)/2 en R.
#'   - Les variables ont un suffixe "_memberXX" (ex. precipitation_sum_member01).
#'   - Le nombre de membres dépend du modèle :
#'       cfs_v2       → 4  membres (member01..member04) | jusqu'à 9 mois
#'       ecmwf_ifs04  → 51 membres (member01..member51) | jusqu'à 7 mois
#'       bom_access_s2 → 11 membres                     | jusqu'à 6 mois
#'
#' @param latitudes  Vecteur de latitudes                                         [ENTRÉE]
#' @param longitudes Vecteur de longitudes (même longueur que latitudes)          [ENTRÉE]
#' @param n_months   Nombre de mois de prévision (1–9, défaut 6)                 [ENTRÉE]
#' @param model      Modèle saisonnier. Défaut "cfs_v2" (4 membres, 9 mois).    [ENTRÉE]
#'                   Alternatives : "ecmwf_ifs04" (51 membres), "bom_access_s2" (11).
#' @return data.frame avec colonnes time, temperature_2m_mean, precipitation_sum,
#'         relative_humidity_2m_mean, latitude, longitude — même format que
#'         get_weather_forecast_batch()                                           [SORTIE]
#'
#' @examples
#' df_saison <- get_weather_seasonal_forecast_batch(
#'   latitudes  = c(43.6, 43.7),
#'   longitudes = c(3.9, 3.8),
#'   n_months   = 6
#' )
get_weather_seasonal_forecast_batch <- function(latitudes, longitudes, n_months = 6,
                                                 model = NULL,
                                                 n_members = 4) {

  if (n_months < 1 || n_months > 9)
    stop("n_months doit être compris entre 1 et 9 (limite de l'API saisonnière)")

  start_date <- as.character(Sys.Date())
  end_date   <- as.character(Sys.Date() + n_months * 31)

  # Nombre de membres par défaut = 4 (correspond à CFS v2, le modèle par défaut de l'API).
  # Passer n_members = 51 si on utilise un modèle ECMWF (51 membres).
  members_sfx <- sprintf("%02d", seq_len(n_members))

  # COMPORTEMENT DE L'API SEASONAL :
  #   On demande les variables de BASE sans suffixe membre.
  #   Ex : "temperature_2m_max" → l'API retourne la moyenne d'ensemble dans
  #   la colonne "temperature_2m_max" PLUS les membres individuels "_memberXX".
  #   On utilise les colonnes de base (déjà agrégées) — pas besoin de moyenner.
  #   temperature_2m_mean N'EXISTE PAS → on calcule (Tmax + Tmin) / 2.
  #   relative_humidity_2m_mean peut être absent selon le modèle → fallback.
  vars_api_base <- c("temperature_2m_max", "temperature_2m_min",
                     "precipitation_sum", "relative_humidity_2m_mean")

  .make_query <- function(vars) {
    q <- list(
      latitude   = paste(round(latitudes,  6), collapse = ","),
      longitude  = paste(round(longitudes, 6), collapse = ","),
      daily      = paste(vars, collapse = ","),
      start_date = start_date,
      end_date   = end_date
    )
    if (!is.null(model)) q$models <- model
    q
  }

  # Tentative avec toutes les variables, puis fallback sans humidité si 400
  vars_to_use <- vars_api_base
  max_retries <- 7
  response    <- NULL

  for (attempt in seq_len(max_retries)) {
    wait_sec <- min(10 * 2^(attempt - 1), 300)
    response <- tryCatch(
      GET("https://seasonal-api.open-meteo.com/v1/seasonal",
          query   = .make_query(vars_to_use),
          httr::timeout(120)),
      error = function(e) {
        cat("  [seasonal batch] Erreur réseau tentative", attempt, "/", max_retries,
            ":", conditionMessage(e), "— attente", wait_sec, "s\n")
        NULL
      }
    )
    if (!is.null(response) && status_code(response) == 200) break

    # Fallback : supprimer l'humidité si absent du modèle (erreur 400)
    if (!is.null(response) && status_code(response) == 400 &&
        "relative_humidity_2m_mean" %in% vars_to_use) {
      cat("  [seasonal batch] relative_humidity_2m_mean indisponible — retry sans UM\n")
      vars_to_use <- setdiff(vars_to_use, "relative_humidity_2m_mean")
      next
    }
    if (!is.null(response))
      cat("  [seasonal batch] Status", status_code(response), "tentative", attempt,
          "/", max_retries, "— attente", wait_sec, "s\n")
    if (attempt < max_retries) Sys.sleep(wait_sec)
  }
  if (is.null(response) || status_code(response) != 200)
    stop(paste("Erreur API saisonnière après", max_retries, "tentatives"))

  # Parseur — utilise les colonnes de base (moyenne d'ensemble déjà calculée par l'API)
  raw <- fromJSON(content(response, as = "text", encoding = "UTF-8"),
                  simplifyDataFrame = FALSE)
  if (!is.null(raw$daily)) raw <- list(raw)  # 1 seul point → liste simple

  results <- lapply(seq_along(raw), function(i) {
    daily_list <- raw[[i]]$daily
    if (is.null(daily_list)) return(NULL)
    df <- as.data.frame(daily_list, stringsAsFactors = FALSE)

    # Supprimer les colonnes _memberXX (on utilise uniquement la moyenne d'ensemble)
    cols_membres <- grep("_member[0-9]", colnames(df), value = TRUE)
    df <- df[, !colnames(df) %in% cols_membres, drop = FALSE]

    # TM = (Tmax + Tmin) / 2 depuis les colonnes de base (moyennes d'ensemble)
    if (all(c("temperature_2m_max", "temperature_2m_min") %in% colnames(df))) {
      df$temperature_2m_mean <- (as.numeric(df$temperature_2m_max) +
                                 as.numeric(df$temperature_2m_min)) / 2
      df$temperature_2m_max  <- NULL
      df$temperature_2m_min  <- NULL
    }

    # RR : déjà dans precipitation_sum (colonne de base)
    if ("precipitation_sum" %in% colnames(df))
      df$precipitation_sum <- as.numeric(df$precipitation_sum)

    # UM : déjà dans relative_humidity_2m_mean si disponible
    if ("relative_humidity_2m_mean" %in% colnames(df))
      df$relative_humidity_2m_mean <- as.numeric(df$relative_humidity_2m_mean)

    df$latitude  <- latitudes[i]
    df$longitude <- longitudes[i]
    df
  })

  dplyr::bind_rows(Filter(Negate(is.null), results))
}
# ==============


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

  v     <- suppressWarnings(terra::vect(df_sub, geom = c(x_col, y_col), crs = "EPSG:4326"))
  dates <- unique(df_sub[[date_col]])

  rasters <- lapply(dates, function(d) {
    v_d <- v[v[[date_col]] == d, ]
    r   <- suppressWarnings(terra::rasterize(v_d, r_template, field = var_col, fun = "mean"))
    names(r) <- d
    r
  })
  r_stack <- terra::rast(rasters)

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
# ==============


# #new (7bis - calcul Shapley exact, sans dépendance externe) ====
#' Calcule les valeurs de Shapley EXACTES par énumération de toutes les
#' coalitions de prédicteurs (utilisé uniquement pour les forêts de
#' probabilité, où treeshap ne fonctionne pas — voir compute_shap()).
#'
#' @description
#' QUOI = implémentation maison, sans aucun package externe (pas de fastshap),
#' de la définition mathématique exacte de la valeur de Shapley :
#'   phi_j = somme sur toutes les coalitions S de {1..p}\\{j} de
#'           [ |S|! (p-|S|-1)! / p! ] * ( v(S U {j}) - v(S) )
#' où v(S) = prédiction moyenne du modèle quand seules les variables de S
#' prennent la vraie valeur de l'observation, et les autres variables sont
#' tirées d'un échantillon de référence (background). C'est EXACT (pas une
#' approximation Monte Carlo) car on énumère les 2^p coalitions complètes —
#' viable ici car p est petit (le modèle de présence n'a que 2 prédicteurs :
#' TM_0_8, UM_5_11 ; pour p plus grand, ce calcul deviendrait trop coûteux
#' et il faudrait revenir à une méthode approchée).
#'
#' @param model          Modèle (caret ou ranger) sur lequel pred_wrapper
#'                       sait faire des prédictions                    [ENTRÉE]
#' @param X_df           data.frame des observations à expliquer        [ENTRÉE]
#' @param background     data.frame de référence (valeurs "absentes")   [ENTRÉE]
#' @param feature_names   noms des colonnes/prédicteurs (longueur p)     [ENTRÉE]
#' @param pred_wrapper    fonction(object, newdata) -> vecteur numérique
#'                        de prédictions (une valeur par ligne)          [ENTRÉE]
#' @param max_background Taille max. de l'échantillon de référence tiré
#'                       au hasard dans background (limite le coût calcul,
#'                       défaut 100)                                    [ENTRÉE]
#' @return data.frame p colonnes (une par variable de feature_names),
#'         une ligne par observation de X_df — valeurs de Shapley exactes [SORTIE]
# new (indicateur de progression pour les calculs longs) ====
#' Formate une durée en secondes en texte lisible (ex. "1 h 12 min", "45 s")
#'
#' @description
#' QUOI = petite fonction utilitaire, purement cosmétique, utilisée par
#' .shapley_exact() et predict_two_part_uncertainty() pour afficher le temps
#' écoulé / temps restant estimé (ETA) pendant les calculs longs (backfill
#' historique de plusieurs heures) — sans ça, impossible de savoir si un run
#' de plusieurs heures avance normalement ou est bloqué.
#'
#' @param secondes Durée en secondes (numérique, peut être NA/Inf)  [ENTRÉE]
#' @return Texte formaté (heures/minutes/secondes selon la grandeur) [SORTIE]
.format_duree <- function(secondes) {
  if (!is.finite(secondes) || secondes < 0) return("? ")
  if (secondes < 60) return(sprintf("%.0f s", secondes))
  if (secondes < 3600) return(sprintf("%.0f min %.0f s", secondes %/% 60, secondes %% 60))
  sprintf("%.0f h %.0f min", secondes %/% 3600, (secondes %% 3600) %/% 60)
}

#' Affiche un message de progression — log_print() si disponible, sinon cat()
#'
#' @description
#' QUOI = évite de dupliquer le if(exists("log_print")) partout. Utilise
#' log_print() (package logr, ouvert dans 02_hebdomadaire.R) quand disponible,
#' pour que la progression reste dans logs/log/ — sinon retombe sur cat().
#'
#' @param msg Message texte à afficher                              [ENTRÉE]
.log_progression <- function(msg) {
  # fix ("Log is not open.") : logr::log_print() plante si le package logr est
  # chargé (ex. resté attaché d'un script précédent dans la même session R,
  # comme 02_hebdomadaire.R) mais qu'aucun log_open() n'a été appelé dans CETTE
  # session (ex. 07_seasonal_forecast_predictions.R, qui n'utilise pas logr).
  # tryCatch() permet de retomber proprement sur cat() dans ce cas, au lieu de
  # spammer la console avec "Log is not open." à chaque appel.
  ok <- FALSE
  if (exists("log_print", mode = "function")) {
    ok <- isTRUE(tryCatch({ log_print(msg); TRUE }, error = function(e) FALSE))
  }
  if (!ok) cat(msg, "\n")
}
# ==============

.shapley_exact <- function(model, X_df, background, feature_names, pred_wrapper,
                            max_background = 100, batch_size = 2000) {

  p <- length(feature_names)
  n <- nrow(X_df)

  # n_bg : QUOI = échantillon de référence limité à max_background lignes
  #   POURQUOI = chaque coalition S nécessite n * n_bg prédictions ; limiter
  #   n_bg évite l'explosion du coût quand le jeu d'entraînement est grand.
  if (nrow(background) > max_background) {
    set.seed(123)
    background <- background[sample(nrow(background), max_background), , drop = FALSE]
  }
  n_bg <- nrow(background)

  # Toutes les coalitions possibles de {1..p}, codées en bits (0 = absent, 1 = présent)
  # ex. pour p=2 : {}, {1}, {2}, {1,2}  →  4 coalitions = 2^p
  all_subsets <- lapply(0:(2^p - 1), function(mask) {
    which(bitwAnd(mask, 2^(0:(p - 1))) > 0)
  })

  # v_S : QUOI = cache des valeurs v(S) pour chaque coalition S, calculées une
  #   seule fois pour TOUTES les observations (vectorisé PAR LOT) plutôt que
  #   ligne par ligne, pour la performance.
  v_S <- vector("list", length(all_subsets))
  n_subsets <- length(all_subsets)

  # new (indicateur de progression) : chaque itération traite un data.frame de
  # n * n_bg lignes — pour un backfill historique (n = milliers de lignes),
  # une seule itération peut prendre plusieurs minutes. On affiche l'avancement
  # (coalition i/n_subsets) + temps écoulé + ETA (estimé à partir du temps moyen
  # des itérations déjà faites) pour pouvoir suivre un run long sans deviner.
  t_debut_shap <- Sys.time()

  # fix (bug "vector memory limit of 16.0 Gb reached") : sur un backfill de
  # plusieurs milliers de lignes (n grand), l'ancienne version construisait
  # bg_rep = TOUT n * n_bg d'un coup (ex. 33 600 lignes à expliquer x 50
  # background = 1,68 million de lignes) et passait ça en un seul appel à
  # pred_wrapper() — pour le modèle d'abondance (ranger quantile regression),
  # cet unique appel sur >1M lignes dépasse la limite mémoire par vecteur de R
  # sur Mac (16 Go), et FAIT PLANTER TOUT LE CALCUL SHAP (capturé par le
  # tryCatch dans 02_hebdomadaire.R → colonnes NA pour TOUTE la tranche, sans
  # que la taille du problème soit visible avant le crash). On découpe donc
  # les n observations à expliquer en lots de `batch_size` : chaque appel à
  # pred_wrapper() ne traite jamais plus de batch_size * n_bg lignes à la
  # fois, quel que soit n — résultat mathématiquement identique (mêmes
  # moyennes par observation), juste calculé en morceaux bornés en mémoire.
  batch_starts <- seq(1, n, by = batch_size)

  for (i in seq_along(all_subsets)) {
    S <- all_subsets[[i]]
    v_i <- numeric(n)

    for (b_start in batch_starts) {
      b_end   <- min(b_start + batch_size - 1, n)
      idx_obs <- b_start:b_end
      n_batch <- length(idx_obs)

      # Construit un data.frame de n_bg * n_batch lignes (borné, jamais tout
      # n d'un coup) : pour chaque observation du lot, on répète le
      # background n_bg fois et on remplace les colonnes de S par la vraie
      # valeur de l'observation (les autres colonnes restent celles du
      # background = "valeur absente/marginalisée").
      bg_rep <- background[rep(seq_len(n_bg), times = n_batch), , drop = FALSE]

      if (length(S) > 0) {
        obs_rep <- X_df[rep(idx_obs, each = n_bg), feature_names[S], drop = FALSE]
        bg_rep[, feature_names[S]] <- obs_rep
      }

      preds <- pred_wrapper(model, bg_rep)
      # Moyenne par observation (bloc de n_bg lignes consécutives = 1 observation)
      v_i[idx_obs] <- vapply(seq_len(n_batch), function(k) {
        mean(preds[((k - 1) * n_bg + 1):(k * n_bg)])
      }, numeric(1))
    }

    v_S[[i]] <- v_i

    # new (indicateur de progression) — voir commentaire plus haut
    ecoule <- as.numeric(difftime(Sys.time(), t_debut_shap, units = "secs"))
    eta    <- (ecoule / i) * (n_subsets - i)
    .log_progression(sprintf(
      "  SHAP coalition %d/%d (n=%d obs x n_bg=%d, %d lot(s) de %d) — écoulé %s — restant ~%s",
      i, n_subsets, n, n_bg, length(batch_starts), batch_size,
      .format_duree(ecoule), .format_duree(eta)
    ))
  }

  # Combine les v(S) en valeurs de Shapley via la formule de pondération exacte
  shap_mat <- matrix(0, nrow = n, ncol = p)
  colnames(shap_mat) <- feature_names

  for (j in seq_len(p)) {
    subsets_without_j <- which(vapply(all_subsets, function(S) !(j %in% S), logical(1)))

    for (idx in subsets_without_j) {
      S      <- all_subsets[[idx]]
      s_size <- length(S)

      # Trouve l'indice de la coalition S U {j}
      mask_with_j <- 0
      if (s_size > 0) mask_with_j <- sum(2^(S - 1))
      mask_with_j <- mask_with_j + 2^(j - 1)
      idx_with_j  <- mask_with_j + 1  # +1 car all_subsets est indexé à partir de mask=0

      weight <- factorial(s_size) * factorial(p - s_size - 1) / factorial(p)
      shap_mat[, j] <- shap_mat[, j] + weight * (v_S[[idx_with_j]] - v_S[[idx]])
    }
  }

  as.data.frame(shap_mat)
}
# ==============


# new (suppression coords_grid.csv — grid recréé en mémoire) ====
#' Crée le grid de points météo à l'intérieur du ROI
#'
#' @description
#' Remplace la lecture de data/coords_grid.csv. Génère un quadrillage régulier
#' de points (centroïdes de carrés de grid_res degrés) et ne garde que ceux
#' qui tombent à l'intérieur de geopolygon.
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
# ==============


# new (point 3 — agrégation météo par commune avant stockage BD) ====
#' Agrège des données météo brutes (par point de grille) à l'échelle de la commune
#'
#' @description
#' Convertit les données brutes Open-Meteo (site/X/Y) en données agrégées par
#' commune via rasterize_to_roi(). Appelée dans 01_initialisation.R et
#' 02_hebdomadaire.R avant toute écriture en BD — permet de stocker directement
#' (codgeo, date, TM, RR, UM) au lieu des données brutes par point de grille.
#'
#' @param raw_df   data.frame avec colonnes X, Y, date, TM, RR, UM             [ENTRÉE]
#' @param roi      Objet sf — communes du ROI                                    [ENTRÉE]
#' @param grid_res Résolution du grid en degrés (défaut 0.05)                   [ENTRÉE]
#' @return data.frame (codgeo, date, TM, RR, UM) — une ligne par commune x date [SORTIE]
aggregate_meteo_to_roi <- function(raw_df, roi, grid_res = 0.05) {
  # fix (bug trouvé en pratique avec debug_missing_communes.R — Pérols et 21 autres
  # communes du ROI disparaissaient silencieusement de meteo_ruiz) : round() en R
  # arrondit les .5 exacts vers l'entier PAIR le plus proche ("banker's rounding" —
  # round(78.5) = 78, pas 79, contrairement à l'arrondi "classique"). Quand une
  # coordonnée tombe PILE sur une frontière de grille (X / grid_res == un .5 exact,
  # ex. 3.925 / 0.05 = 78.5), elle se fait décaler d'une cellule entière au lieu de
  # rester sur place — le point atterrit dans la mauvaise cellule du raster, et la
  # commune qui comptait sur lui pour être recouverte se retrouve avec des cellules
  # NA tout autour (NaN à l'extraction dans rasterize_to_roi(), filtré
  # silencieusement par son !is.nan() final — voir 00_functions.R). Confirmé pas à
  # pas avec scripts/debug_missing_communes.R sur Pérols (codgeo 34198) : le point
  # de grille le plus proche (0.68 km) existe bien dans les données brutes tous les
  # jours, mais se snappait sur la mauvaise cellule à cause de ce .5 exact.
  # Un petit epsilon avant round() lève l'ambiguïté de façon stable (toujours vers
  # le haut sur une frontière exacte, jamais au hasard pair/impair).
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
# ==============


# #new (9 - colonne is_forecast, fraîcheur historique) ====
#' Garantit que la table météo possède la colonne is_forecast (BOOLEAN).
#'
#' @description
#' QUOI = is_forecast distingue, pour chaque ligne de la table météo, si la
#' valeur est une PRÉVISION (forecast, encore incertaine) ou une vraie valeur
#' HISTORIQUE observée (réelle). POURQUOI = sans cette colonne, on ne peut pas
#' savoir si les 7 derniers jours ont déjà été remplacés par les vraies valeurs
#' (Étape 1 de 02_hebdomadaire.R) ou si ce sont encore les estimations de
#' prévision de la semaine dernière — du coup l'ancien code retéléchargeait et
#' réécrasait ces 7 jours À CHAQUE EXÉCUTION, même si c'était déjà à jour.
#' Avec cette colonne, ce remplacement ne se fait que si nécessaire (lignes
#' encore marquées is_forecast = TRUE dans la fenêtre concernée).
#' ALTER TABLE ... ADD COLUMN IF NOT EXISTS est idempotent (sans effet si la
#' colonne existe déjà) et DEFAULT FALSE donne automatiquement is_forecast =
#' FALSE à toutes les lignes déjà existantes (considérées comme historiques).
#'
#' @param con        Connexion DBI active                              [ENTRÉE]
#' @param table_name Nom de la table météo (db_table_meteo de config.R) [ENTRÉE]
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
# ==============


# #new (7 - SHAP toutes variables) ====
#' Calcule les valeurs SHAP pour un modèle ranger ou caret/ranger
#'
#' @description
#' Fonction générique SHAP utilisable avec n'importe quel modèle ranger
#' ou caret entraîné avec méthode ranger. Gère les deux formats et vérifie
#' la compatibilité (keep.inbag requis par treeshap).
#'
#' @param model        Modèle ranger ou caret (méthode ranger)        [ENTRÉE]
#' @param X_data       data.frame ou matrix des prédicteurs           [ENTRÉE]
#' @param model_type   "ranger" (défaut) ou "caret_ranger"            [ENTRÉE]
#' @param X_background data.frame de référence pour le calcul exact (forêts
#'                     de probabilité uniquement) — idéalement les
#'                     données d'entraînement. NULL = utilise X_data  [ENTRÉE]
#' @param max_background Taille max. de l'échantillon de référence
#'                     (forêts de probabilité uniquement, défaut 100) [ENTRÉE]
#' @return data.frame avec colonnes shap_<var>, shap_dominant_var,
#'         shap_dominant_val pour chaque observation                [SORTIE]
#'
#' @examples
#' shap_abund <- compute_shap(rf_abundance_q, X_abundance, model_type = "ranger")
#' shap_pres  <- compute_shap(mod_presence, X_presence, model_type = "caret_ranger",
#'                             X_background = res_train$X_presence)
compute_shap <- function(model, X_data, model_type = c("ranger", "caret_ranger"),
                          X_background = NULL, max_background = 100) {

  model_type <- match.arg(model_type)
  X_df       <- as.data.frame(X_data)

  # Extraire le modèle ranger sous-jacent si caret
  ranger_mod <- if (model_type == "caret_ranger") model$finalModel else model

  if (!inherits(ranger_mod, "ranger")) {
    stop("Le modèle doit être un objet ranger (ou caret avec méthode ranger)")
  }

  # treeshap requiert keep.inbag = TRUE à l'entraînement
  if (is.null(ranger_mod$inbag.counts)) {
    warning("Le modèle ranger n'a pas été entraîné avec keep.inbag = TRUE. ",
            "Les valeurs SHAP pourraient être incorrectes.")
  }

  # new (fix SHAP présence — calcul exact maison, sans dépendance externe) ====
  # is_proba_forest : QUOI = booléen, TRUE si le ranger sous-jacent est une forêt
  #   DE PROBABILITÉ (chaque feuille stocke un vecteur de probabilités par classe,
  #   pas une seule valeur numérique). POURQUOI ça compte = treeshap::ranger.unify()
  #   ne supporte PAS ce format — ce n'est pas un problème de nommage de colonnes
  #   (l'ancien "patch" ici ne faisait rien de réel). Le package 'fastshap' aurait
  #   pu résoudre ça, mais n'étant pas installable facilement (binaire CRAN pas
  #   disponible pour toutes les versions de R), on calcule directement les valeurs
  #   de Shapley EXACTES par énumération de toutes les coalitions de prédicteurs
  #   (.shapley_exact ci-dessous) — aucune dépendance externe, et exact (pas une
  #   approximation Monte Carlo) puisque predictors_presence n'a que 2 variables.
  is_proba_forest <- isTRUE(ranger_mod$treetype == "Probability estimation")

  if (is_proba_forest) {

    feature_names <- colnames(X_df)
    background    <- if (!is.null(X_background)) as.data.frame(X_background) else X_df
    background    <- background[, feature_names, drop = FALSE]

    pred_wrapper <- function(object, newdata) {
      predict(object, newdata = newdata, type = "prob")$Presence
    }

    result <- tryCatch({
      shap_df <- .shapley_exact(
        model = model, X_df = X_df, background = background,
        feature_names = feature_names, pred_wrapper = pred_wrapper,
        max_background = max_background
      )
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
      warning("compute_shap (calcul exact) : impossible de calculer les valeurs ",
              "SHAP — ", conditionMessage(e), ". Colonnes SHAP absentes pour ce modèle.")
      NULL
    })

    return(result)
  }
  # ==============

  # Chemin normal (treeshap) — régression ou classification par vote (non probabiliste)
  result <- tryCatch({

    unified   <- treeshap::ranger.unify(ranger_mod, X_df)
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


# new (restauration modèle two-part complet) ====
#' Prédiction combinée présence + abondance avec propagation d'incertitude
#'
#' @description
#' Combine un modèle de présence (classification probabiliste) et un modèle
#' d'abondance (quantile RF) pour produire une prédiction d'abondance combinée
#' avec incertitude propagée par simulation Monte Carlo. Utilisée à la fois
#' à l'entraînement (00_train_models.R, sur les données complètes) et lors
#' du pipeline hebdomadaire (02_hebdomadaire.R, sur les nouvelles données).
#'
#' @param newdata              data.frame contenant les prédicteurs            [ENTRÉE]
#' @param mod_presence         Modèle caret de présence (classification)       [ENTRÉE]
#' @param rf_abundance_q       Modèle ranger quantile d'abondance              [ENTRÉE]
#' @param predictors_presence  Vecteur des noms de prédicteurs (présence)      [ENTRÉE]
#' @param predictors_abundance Vecteur des noms de prédicteurs (abondance)     [ENTRÉE]
#' @param n_sim                Nombre de simulations Monte Carlo (défaut 2000) [ENTRÉE]
#' @return newdata enrichi des colonnes de prédiction présence/abondance/incertitude [SORTIE]
predict_two_part_uncertainty <- function(newdata, mod_presence, rf_abundance_q,
                                          predictors_presence, predictors_abundance,
                                          n_sim = 2000) {

  # pred_presence : QUOI = data.frame retourné par predict(..., type="prob") avec une
  #   colonne par classe (Presence/Absence). FAIT = predict_two_part_uncertainty()
  #   l'utilise pour extraire la probabilité de présence ci-dessous.
  #   VIENT DE = mod_presence, le modèle caret/ranger chargé depuis
  #   res_presence_LOSO_probabilistic.rds.
  pred_presence <- predict(mod_presence,
    newdata = newdata[, predictors_presence, drop = FALSE], type = "prob")

  # p : QUOI = vecteur numérique, probabilité de présence (entre 0 et 1) par ligne.
  #   REPRÉSENTE = P(Aedes albopictus présent) selon le modèle de présence.
  p <- pred_presence$Presence

  # pred_q : QUOI = matrice à 3 colonnes (quantiles 0.05/0.5/0.95) de l'abondance
  #   prédite, EN ÉCHELLE LOG (le modèle a été entraîné sur NB_ALBO_TOT_LOG).
  #   VIENT DE = rf_abundance_q, le ranger quantile chargé depuis
  #   res_abundance_LOSO_quantile_rf.rds.
  pred_q <- predict(rf_abundance_q,
    data = newdata[, predictors_abundance, drop = FALSE],
    type = "quantiles", quantiles = c(0.05, 0.5, 0.95))$predictions

  out <- newdata %>%
    dplyr::mutate(
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

  # sim_res : QUOI = data.frame avec 1 ligne par observation, résumant n_sim tirages
  #   Monte Carlo. FAIT (par ligne i) =
  #     1. tire z_sim ~ Bernoulli(p_i)               (présence ou non, n_sim fois)
  #     2. tire a_log_sim ~ Normale(mu_i, sd_i)       (abondance log, n_sim fois)
  #     3. y_sim = z_sim * exp(a_log_sim)             (abondance finale si présent, 0 sinon)
  #   REPRÉSENTE = la distribution complète de la prédiction COMBINÉE — c'est de là
  #   que viennent pred_combined_mean/q05/q50/q95/sd, LA prédiction la plus importante
  #   du pipeline (publiée comme mean_abundance_albopictus dans 02_hebdomadaire.R).
  #   sd_i est approximé à partir de l'intervalle 90% [q05,q95] en supposant une loi
  #   normale (1.645 = quantile normal à 95%, donc q95-q05 couvre 2*1.645 écarts-types).
  # new (indicateur de progression) : pour un backfill historique (milliers de
  # lignes x n_sim tirages chacune), cette boucle peut prendre plusieurs minutes.
  # Affichage périodique (tous les 5% ou au moins toutes les 2000 lignes) plutôt
  # qu'à chaque ligne, pour ne pas ralentir le calcul avec trop d'I/O de log.
  n_total_sim  <- nrow(out)
  pas_affichage <- max(1, min(2000L, ceiling(n_total_sim / 20)))
  t_debut_sim  <- Sys.time()

  sim_res <- purrr::map_dfr(seq_len(nrow(out)), function(i) {
    p_i  <- out$pred_presence_prob[i]
    mu_i <- out$pred_log_abundance_q50[i]
    sd_i <- pmax((out$pred_log_abundance_q95[i] - out$pred_log_abundance_q05[i]) / (2 * 1.645), 1e-6)
    y_sim <- rbinom(n_sim, 1, p_i) * exp(rnorm(n_sim, mu_i, sd_i))

    # new (indicateur de progression) — voir commentaire ci-dessus
    if (i %% pas_affichage == 0 || i == n_total_sim) {
      ecoule <- as.numeric(difftime(Sys.time(), t_debut_sim, units = "secs"))
      eta    <- (ecoule / i) * (n_total_sim - i)
      .log_progression(sprintf(
        "  Simulation Monte Carlo : ligne %d/%d (%.0f%%) — écoulé %s — restant ~%s",
        i, n_total_sim, 100 * i / n_total_sim, .format_duree(ecoule), .format_duree(eta)
      ))
    }

    tibble::tibble(row_id = i,
           pred_combined_mean = mean(y_sim),
           pred_combined_q05  = quantile(y_sim, 0.05),
           pred_combined_q50  = quantile(y_sim, 0.50),
           pred_combined_q95  = quantile(y_sim, 0.95),
           pred_combined_sd   = sd(y_sim))
  })

  out %>%
    dplyr::mutate(row_id = seq_len(dplyr::n())) %>%
    dplyr::left_join(sim_res, by = "row_id") %>%
    dplyr::mutate(pred_thresholded = ifelse(pred_presence_prob > 0.5, pred_abundance_q50, 0))
}
# ==============
