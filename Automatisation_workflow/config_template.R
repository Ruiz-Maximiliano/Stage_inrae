# ============================================================
# config_template.R — Template de configuration du pipeline
#
# CE QUE FAIT CE CODE : définit les mêmes paramètres que config.R (voir ce fichier
# pour le détail QUOI/FAIT/REPRÉSENTE/VIENT DE de chaque variable), mais avec des
# valeurs d'exemple/placeholder au lieu des vrais identifiants.
#
# Copier ce fichier, renommer en config.R et remplir les valeurs.
# CE FICHIER peut être commité — il ne contient pas de credentials réels.
# ============================================================

# ============================================================
# PARAMÈTRES D'ENTRÉE
# ============================================================

# Zone d'intérêt — lue depuis une table BD contenant les limites administratives
db_table_admin <- "administrative_boundaries"
admin_dep      <- 34          # code département à retenir
admin_level    <- "commune"   # niveau administratif à retenir

# Bounding box optionnelle pour limiter le grid météo (NULL = utilise le bbox du ROI)
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

# fix (variables manquantes dans ce template — présentes dans config.R réel,
# nécessaires à 01_initialisation.R/02_hebdomadaire.R, absence = erreur "objet
# introuvable" lors d'une copie fraîche de ce template) :

# grid_res : résolution de la grille météo en degrés (numérique). Cellsize
# passé à sf::st_make_grid() dans make_grid(), utilisé aussi pour le snapping
# des coordonnées dans aggregate_meteo_to_roi().
grid_res <- 0.05

# Dossier contenant les modèles entraînés (.rds, voir 00_train_models.R)
path_models <- here::here("models")

# CSV de backup de l'historique météo brut (utilisé par 01_initialisation.R
# si la BD est vide/incomplète, à défaut de re-télécharger via l'API)
path_backup <- here::here("data", "meteo_history_backup.csv")

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

# Table météo — format BRUT par point de grille (X, Y, date, TM, RR, UM,
# is_forecast), voir config.R pour le détail. Table séparée de db_table_meteo.
db_table_meteo_grid <- "meteo_grid"

# Table de prédictions publiée chaque semaine (prédictions + valeurs SHAP, fusionnées)
db_layer <- "albopictus_predictions"

# Vues matérielles — moyenne par commune/semaine sur 10 ans et 2 ans
# (comparatif page web, voir config.R pour le détail)
db_table_mean_10y <- "mean_10y"
db_table_mean_2y  <- "mean_2y"

# ============================================================
# ROI — chargé ici (demande Paul, revue de code) pour ne pas dupliquer ce
# bloc dans chaque script. Connexion BD TEMPORAIRE, ouverte et refermée
# uniquement pour cette lecture — config.R ne garde aucune connexion ouverte.
# ============================================================

# roi : QUOI = objet sf (polygones des communes du département admin_dep),
#   projetés en EPSG:4326. FAIT = lu une seule fois ici, disponible dans
#   tous les scripts qui font source(here("config.R")) — 01_initialisation.R,
#   02_hebdomadaire.R, 07_seasonal_forecast_predictions.R, etc. n'ont plus
#   besoin de le recharger chacun de leur côté.
.con_roi <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = db_host, dbname = db_name, port = db_port,
  user = db_user, password = db_password
)
roi <- sf::st_read(.con_roi, db_table_admin) |>
  dplyr::filter(dep == admin_dep, level == admin_level)
roi <- sf::st_transform(roi, 4326)
DBI::dbDisconnect(.con_roi)
rm(.con_roi)
