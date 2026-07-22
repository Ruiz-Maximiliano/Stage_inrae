# ============================================================
# 00_functions_api.R — Fonctions d'accès à l'API Open-Meteo
#
# CE QUE FAIT CE CODE :
#   Définit toutes les fonctions de téléchargement météo (historique, forecast,
#   forecast saisonnier), en version point unique ou batch multi-points. Ce
#   fichier ne fait RIEN tout seul — il ne fait que DÉFINIR des fonctions,
#   appelées depuis 00_train_models.R, 01_initialisation.R, 02_hebdomadaire.R,
#   07_seasonal_forecast_predictions.R, etc. après un
#   source(here("scripts", "00_functions_api.R")).
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Aucun au niveau fichier — chaque fonction reçoit ses propres paramètres en
#   argument (voir le tag [ENTRÉE] dans la documentation de chaque fonction).
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   get_weather_history(), get_weather_forecast(), get_weather_history_batch(),
#   get_weather_forecast_batch(), get_weather_seasonal_forecast_batch(),
#   .parse_openmeteo_batch() (interne).
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
