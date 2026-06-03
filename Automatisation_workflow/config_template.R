# ============================================================
# config_template.R — Template credentials base de données
# Copier ce fichier, renommer en config.R et remplir les valeurs
# ============================================================

# Base de données
db_host     <- "postgresql-VOTRE_COMPTE.alwaysdata.net"
db_name     <- "VOTRE_COMPTE_albopictus"
db_port     <- 5432
db_user     <- "VOTRE_UTILISATEUR"
db_password <- "VOTRE_MOT_DE_PASSE"
db_layer    <- "albopictus_predictions"  # nom de la table à créer

# Modèle météo OpenMeteo
# NULL = best match automatique (recommandé)
# Alternatives pour la France : "meteofrance_seamless", "meteofrance_arome_france_hd"
# Liste complète des modèles : https://open-meteo.com/en/docs
weather_model <- NULL
