# ============================================================
# config_template.R — Template de configuration du pipeline
# Copier ce fichier, renommer en config.R et remplir les valeurs
# CE FICHIER peut être commité — il ne contient pas de credentials réels
# ============================================================

# ============================================================
# PARAMÈTRES D'ENTRÉE
# ============================================================

# Zone d'intérêt — fichier sf/gpkg contenant les limites administratives
path_roi <- here::here("data", "administrative_boundaries.gpkg")

# Bounding box optionnelle pour limiter le grid (NULL = utilise le bbox du ROI)
# Exemple Hérault : roi_bbox <- c(xmin=2.40, xmax=4.30, ymin=43.1, ymax=44.0)
roi_bbox <- NULL

# Modèle météo Open-Meteo (NULL = best match automatique, recommandé)
# Alternatives pour la France : "meteofrance_seamless", "meteofrance_arome_france_hd"
# Liste complète : https://open-meteo.com/en/docs
openmeteo_model <- NULL

# Durée de l'historique météo téléchargé lors de l'initialisation (jours)
# Minimum 84 jours (= lag_max utilisé dans la construction des variables)
n_days_history  <- 365

# Durée du forecast téléchargé chaque semaine (jours, maximum 16 — limite API)
n_days_forecast <- 14

# ============================================================
# PARAMÈTRES DE SORTIE
# ============================================================

# Connexion à la base de données PostgreSQL
db_host     <- "postgresql-VOTRE_COMPTE.alwaysdata.net"
db_name     <- "VOTRE_COMPTE_albopictus"
db_port     <- 5432
db_user     <- "VOTRE_UTILISATEUR"
db_password <- "VOTRE_MOT_DE_PASSE"

# Table météo — stockage de l'historique + forecast
db_table_meteo <- "meteo"

# Tables de prédictions publiées chaque semaine
db_layer      <- "albopictus_predictions"       # prédictions principales
db_layer_shap <- paste0(db_layer, "_shap")      # prédictions + valeurs SHAP
