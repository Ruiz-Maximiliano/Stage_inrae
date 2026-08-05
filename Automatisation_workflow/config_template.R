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
admin_dep      <- 34                          # code département à retenir
admin_levels   <- c("commune", "departement") # niveaux administratifs à retenir

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

# lag_max : nombre de jours de recul météo nécessaires pour les prédicteurs
# les plus longs (jusqu'à 11 SEMAINES = 77 jours réels, voir
# fun_summarize_week() dans 00_functions_formats.R — 84 laisse 1 semaine de
# marge). Utilisé par 02_hebdomadaire.R (.read_from) ET 01_initialisation.R
# (marge réservée avant la 1ère semaine publiée) — centralisé ici pour que
# les deux restent synchronisés (sinon : NULL sur toutes les communes des
# premières semaines de l'historique, bug déjà rencontré).
lag_max <- 84

# n_workers : nombre de workers pour le calcul parallèle (LIME, via furrr).
# Utilisé par 02_hebdomadaire.R pour future::plan(multisession, workers =
# n_workers), directement dans le script (reproductible en cron, sans dépendre
# d'un plan() réglé à la main en console). max(1, availableCores() - 1)
# laisse 1 coeur libre pour l'OS.
n_workers <- max(1, future::availableCores() - 1)

# run_seasonal_forecast : si TRUE, main.R lance
# scripts/07_seasonal_forecast_predictions.R juste après 02_hebdomadaire.R.
# Par défaut FALSE (script exploratoire, manuel). 07 n'écrit jamais dans une
# table de production (uniquement une table de TEST) — l'activer ne change
# aucune donnée publiée, juste la fréquence de rafraîchissement de ce test.
run_seasonal_forecast <- FALSE

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

# Table de prédictions publiée chaque semaine (prédictions + valeurs LIME, fusionnées)
db_layer <- "albopictus_predictions"

# Vues matérielles — moyenne par commune/semaine sur 10 ans et 2 ans
# (comparatif page web, voir config.R pour le détail)
db_table_mean_10y <- "mean_10y"
db_table_mean_2y  <- "mean_2y"

# ============================================================
# ROI — chargé ici pour ne pas dupliquer ce
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
  dplyr::filter(dep == admin_dep, level %in% admin_levels)
roi <- sf::st_transform(roi, 4326)
DBI::dbDisconnect(.con_roi)
rm(.con_roi)

# geopolygon / roi_info / all_codgeo : dérivés de roi, calculés ici une seule
# fois (au lieu d'être recalculés dans chaque script). sf_use_s2(FALSE) requis : st_union/st_make_valid
# échouent sur certaines géométries ROI avec s2 activé (erreur "format non
# supporté"). Les messages "Spherical geometry switched off/on" et "assumes
# planar" sont normaux et attendus ici.
sf::sf_use_s2(FALSE)
roi        <- sf::st_make_valid(roi)
geopolygon <- sf::st_union(roi)
sf::sf_use_s2(TRUE)

# roi_info : QUOI = data.frame codgeo/libgeo/level sans géométrie. FAIT =
#   utilisé pour les jointures sur les tables de prédictions publiées.
#   "level" distingue les lignes "commune" des lignes "departement".
roi_info <- sf::st_drop_geometry(roi) |> dplyr::select(codgeo, libgeo, level)

# all_codgeo : liste (texte) des codgeo du ROI.
all_codgeo <- as.character(unique(roi$codgeo))
