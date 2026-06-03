# ============================================================
# 00_functions.R — Fonctions de téléchargement météo
# Remplace la librairie openmeteo (plus disponible sur CRAN)
# ============================================================

library(httr)
library(jsonlite)

#' Téléchargement des données météo historiques via l'API Open-Meteo
#'
#' @description
#' Télécharge des données météorologiques journalières historiques.
#' Deux modes : n_days jours vers le passé, ou dates explicites.
#'
#' @param latitude Latitude du point (numérique)
#' @param longitude Longitude du point (numérique)
#' @param n_days Nombre de jours vers le passé à partir d'aujourd'hui (optionnel)
#' @param start_date Date de début "YYYY-MM-DD" (optionnel)
#' @param end_date Date de fin "YYYY-MM-DD" (optionnel)
#' @param daily Vecteur de variables météo à télécharger
#' @param model Modèle météo à utiliser (optionnel, NULL = best match)
#'   Exemples : "meteofrance_seamless", "ecmwf_ifs_analysis_long_window"
#'   Liste complète : https://open-meteo.com/en/docs
#'
#' @return Un data.frame avec les variables météo journalières + latitude + longitude
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

  # Ajouter le modèle seulement si spécifié — sinon best match par défaut
  if (!is.null(model)) query$models <- model

  max_retries <- 5
  wait_sec    <- 30
  response    <- NULL
  for (attempt in seq_len(max_retries)) {
    response <- GET("https://archive-api.open-meteo.com/v1/archive", query = query)
    if (status_code(response) == 200) break
    cat("Erreur API", status_code(response), "— tentative", attempt, "/", max_retries,
        "— attente", wait_sec, "s\n")
    if (attempt < max_retries) Sys.sleep(wait_sec)
  }
  if (status_code(response) != 200) stop(paste("Erreur API:", status_code(response)))

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
#' @param latitude Latitude du point (numérique)
#' @param longitude Longitude du point (numérique)
#' @param n_days Nombre de jours de prévision à partir d'aujourd'hui (optionnel)
#' @param start_date Date de début "YYYY-MM-DD" (optionnel)
#' @param end_date Date de fin "YYYY-MM-DD" (optionnel)
#' @param daily Vecteur de variables météo à télécharger
#' @param model Modèle météo à utiliser (optionnel, NULL = best match)
#'   Exemples : "meteofrance_seamless", "meteofrance_arome_france_hd"
#'   Liste complète : https://open-meteo.com/en/docs
#'   Note : maximum 16 jours de prévision (limite API)
#'
#' @return Un data.frame avec les variables météo journalières + latitude + longitude
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

  # Ajouter le modèle seulement si spécifié — sinon best match par défaut
  if (!is.null(model)) query$models <- model

  max_retries <- 5
  wait_sec    <- 30
  response    <- NULL
  for (attempt in seq_len(max_retries)) {
    response <- GET("https://api.open-meteo.com/v1/forecast", query = query)
    if (status_code(response) == 200) break
    cat("Erreur API", status_code(response), "— tentative", attempt, "/", max_retries,
        "— attente", wait_sec, "s\n")
    if (attempt < max_retries) Sys.sleep(wait_sec)
  }
  if (status_code(response) != 200) stop(paste("Erreur API:", status_code(response)))

  df <- as.data.frame(fromJSON(content(response, as = "text", encoding = "UTF-8"))$daily)
  df$latitude  <- latitude
  df$longitude <- longitude
  return(df)
}
