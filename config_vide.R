# ============================================================
# config.R — Configuration du pipeline
#
# CE QUE FAIT CE CODE :
#   Ne fait aucun calcul — définit uniquement les paramètres (constantes) lus par
#   tous les autres scripts via source(here("config.R")). C'est le SEUL endroit où
#   on doit changer une valeur (zone d'étude, identifiants BD, noms de tables...).
#
# PARAMÈTRES D'ENTRÉE (à fournir/ajuster manuellement) :
#   Toutes les variables de ce fichier sont des paramètres d'entrée — voir le
#   commentaire au-dessus de chaque variable ci-dessous pour son rôle précis.
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   Aucun — uniquement des affectations directes (pas de calcul).
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   Aucun — config.R est toujours le premier fichier sourcé, il ne dépend de rien.
#
# CE FICHIER NE DOIT PAS ÊTRE COMMITÉ (voir .gitignore) — il contient le mot de
# passe de la base de données en clair.
# Copier config_template.R, renommer en config.R et remplir les valeurs.
#
# new (2 variantes pour ce dossier de test) ====
# Ce dossier pipeline_test/ contient 2 versions de config.R, à copier-coller
# par-dessus CE fichier selon le test voulu (seul config.R, sans suffixe, est
# réellement lu par les scripts via source(here("config.R"))) :
#   - config_vide.R    : tables "meteo_validation"/"albopictus_validation"
#                        (n'existent pas encore en BD) — force tout le pipeline
#                        à tourner depuis zéro (création + téléchargement complet),
#                        aucun raccourci. C'est la version actuellement active ici.
#   - config_charge.R  : tables "meteo_ruiz"/"albopictus_ruiz_test" (déjà
#                        chargées en BD, celles du pipeline principal) — test
#                        rapide, la plupart des étapes seront sautées car déjà
#                        à jour.
# ==============
# ============================================================

# ============================================================
# PARAMÈTRES D'ENTRÉE
# ============================================================

# new (ROI depuis BD — idée différée du punteo, maintenant validée) ====
# db_table_admin : QUOI = nom (texte) de la table PostgreSQL contenant les limites
#   administratives (communes, départements...). FAIT = sert d'argument à
#   sf::st_read(con, db_table_admin) dans 01_initialisation.R / 02_hebdomadaire.R.
#   REPRÉSENTE = la source du ROI (zone d'intérêt) — remplace l'ancien fichier
#   local administrative_boundaries.gpkg. VIENT DE = la BD "taconet_albopictus".
db_table_admin <- "administrative_boundaries"

# admin_dep : QUOI = code département (entier) à filtrer dans la table ci-dessus.
#   FAIT = restreint le ROI à un seul département (34 = Hérault).
#   VIENT DE = nomenclature INSEE des départements français.
admin_dep      <- 34

# admin_level : QUOI = niveau administratif (texte) à filtrer dans la table ci-dessus.
#   FAIT = restreint le ROI au niveau "commune" (et pas "departement"/"region").
#   VIENT DE = valeurs possibles de la colonne "level" de la table db_table_admin.
admin_level    <- "commune"

# roi_bbox : QUOI = bounding box optionnelle (vecteur xmin/xmax/ymin/ymax, degrés WGS84).
#   FAIT = si non NULL, restreint la grille météo téléchargée à cette zone au lieu
#   du bbox complet du ROI (utile pour éviter de payer en appels API une zone trop
#   large). NULL = utilise le bbox calculé automatiquement à partir du ROI.
#   Exemple Hérault : roi_bbox <- c(xmin=2.40, xmax=4.30, ymin=43.1, ymax=44.0)
roi_bbox <- c(xmin = 2.40, xmax = 4.30, ymin = 43.1, ymax = 44.0)
# ==============

# #new (1 - Choix modèle Open-Meteo) ====
# openmeteo_model : QUOI = nom (texte) du modèle météorologique Open-Meteo, ou NULL.
#   FAIT = transmis en paramètre "models=" à l'API Open-Meteo (historique ET prévisions).
#   NULL = best match automatique (recommandé par défaut — Open-Meteo choisit le
#   modèle le plus précis disponible pour chaque point).
#   Options : "meteofrance_seamless", "ecmwf_ifs_analysis_long_window", etc.
#   Liste complète : https://open-meteo.com/en/docs
openmeteo_model <- NULL
# ==============

# n_days_history : QUOI = nombre de jours (entier) d'historique météo à télécharger
#   lors de l'initialisation. FAIT = définit la fenêtre [aujourd'hui - n_days_history,
#   aujourd'hui - 1] passée à l'API archive d'Open-Meteo dans 01_initialisation.R.
#   Minimum 84 jours (= lag_max, le nombre de jours en arrière utilisés pour construire
#   les variables retardées dans 02_hebdomadaire.R).
n_days_history  <- 365   # réduit à 1 an pour le test (config_vide.R) — la production utilise 3655

# n_days_forecast : QUOI = nombre de jours (entier) de prévision à télécharger chaque
#   semaine. FAIT = définit la fenêtre [aujourd'hui, aujourd'hui + n_days_forecast]
#   passée à l'API forecast d'Open-Meteo. Maximum 16 (limite imposée par l'API).
n_days_forecast <- 14

# grid_res : résolution de la grille météo en degrés
grid_res <- 0.05

# path_models : dossier des modèles entraînés
path_models <- here::here("models")

# path_backup : backup CSV brut des données météo historiques
path_backup <- here::here("data", "meteo_history_backup.csv")

# ============================================================
# PARAMÈTRES DE SORTIE
# ============================================================

# db_host/db_name/db_port/db_user/db_password : QUOI = identifiants de connexion
#   PostgreSQL (texte/entier). FAIT = transmis à DBI::dbConnect(RPostgres::Postgres(), ...)
#   dans chaque script. REPRÉSENTE = la BD "taconet_albopictus" hébergée sur
#   alwaysdata.net, qui contient à la fois les données d'entrée (ROI) et de sortie
#   (météo, prédictions).
db_host     <- "postgresql-taconet.alwaysdata.net"
db_name     <- "taconet_albopictus"
db_port     <- 5432
db_user     <- "taconet"
db_password <- "HHKcue51"

# #new (5 - Gestion données BD) ====
# db_table_meteo : QUOI = nom (texte) de la table BD où sont stockées les données
#   météo (historique + prévision). FAIT = lue/écrite par dbReadTable/dbWriteTable
#   dans 01_initialisation.R et 02_hebdomadaire.R — remplace le CSV local utilisé
#   avant le refactor.
# new (config dédiée pipeline_test — tables de validation, vides au départ) ====
# Ces tables sont VOLONTAIREMENT différentes de celles du pipeline principal
# (meteo_ruiz / albopictus_ruiz_test) : elles n'existent pas encore dans la BD,
# donc tous les scripts vont passer par leur chemin "table absente" (création +
# téléchargement complet, aucun raccourci) — c'est exactement le but de ce
# dossier de test (pour vérifier que tout tourne bien à partir de
# rien, sans dépendre des données déjà chargées dans la BD de production).
db_table_meteo <- "meteo_validation"
# ==============

# new (point 2 — table météo brute par point de grille, séparée de meteo_validation) ====
db_table_meteo_grid <- "meteo_validation_grid"
# ==============

# db_layer : QUOI = nom (texte) de la table BD où sont publiées les prédictions
#   hebdomadaires + valeurs SHAP (table unique, fusionnée).
#   FAIT = argument "layer" de st_write(..., dsn = con) dans 02_hebdomadaire.R.
db_layer <- "albopictus_validation"
