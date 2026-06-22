# ============================================================
# SCRIPT — Vider les tables de test/validation (utilitaire, hors pipeline normal)
#
# CE QUE FAIT CE CODE :
#   Supprime (DROP TABLE) les 3 tables définies par db_table_meteo, db_layer et
#   db_layer_shap dans config.R. Sert à remettre la BD à zéro APRÈS avoir vérifié
#   soi-même que le pipeline tourne bien (config_vide.R), pour ensuite envoyer
#   le dossier pipeline_test à quelqu'un d'autre (ex. Paul) qui doit pouvoir
#   tester que tout fonctionne en partant de rien, sans données préchargées.
#
#   SÉCURITÉ : ce script REFUSE de s'exécuter si config.R pointe vers une des
#   tables de PRODUCTION (meteo_ruiz, albopictus_ruiz_test, albopictus_ruiz_test2)
#   — il ne doit jamais pouvoir effacer les vraies données. Vérifiez quand même
#   que config.R est bien la version "vide" (config_vide.R) avant de lancer ce
#   script — cette vérification automatique est un filet de sécurité, pas une
#   excuse pour ne pas relire ce qu'on va supprimer.
#   En session interactive (RStudio), une confirmation manuelle est aussi demandée.
#
# Usage : source("scripts/vider_bd_validation.R")  — PAS appelé par main.R.
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Aucun directement — tout vient de config.R (db_table_meteo, db_layer,
#   db_layer_shap, identifiants BD).
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   tables_a_supprimer, tables_production_interdites.
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS :
#   - config.R : tous les paramètres listés ci-dessus.
# ============================================================

library(here)
library(DBI)
library(RPostgres)

source(here("config.R"))

con <- dbConnect(
  RPostgres::Postgres(),
  host     = db_host,
  dbname   = db_name,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# tables_a_supprimer : QUOI = les 3 tables que ce pipeline de test écrit
#   (lues depuis config.R — donc ce qui sera supprimé dépend de QUEL config.R
#   est actif au moment de lancer ce script).
tables_a_supprimer <- c(db_table_meteo, db_layer, db_layer_shap)

# new (sécurité — ne jamais toucher à la production) ====
# tables_production_interdites : QUOI = liste blanche des noms RÉSERVÉS au
#   pipeline principal — si config.R pointe vers l'une d'elles (ex. on a oublié
#   de remettre config_vide.R), on arrête tout AVANT toute suppression. C'est
#   la dernière barrière avant un DROP TABLE irréversible.
tables_production_interdites <- c("meteo_ruiz", "albopictus_ruiz_test", "albopictus_ruiz_test2")

if (any(tables_a_supprimer %in% tables_production_interdites)) {
  dbDisconnect(con)
  stop(
    "STOP : config.R actif pointe vers une table de PRODUCTION (",
    paste(intersect(tables_a_supprimer, tables_production_interdites), collapse = ", "),
    "). Ce script ne doit s'exécuter QUE sur les tables de test/validation ",
    "(config_vide.R). Annulé sans rien supprimer."
  )
}
# ==============

cat("Tables qui vont être supprimées (si elles existent) :\n")
cat(" -", paste(tables_a_supprimer, collapse = "\n - "), "\n\n")

# new (confirmation manuelle — dernier garde-fou avant suppression) ====
if (interactive()) {
  reponse <- readline("Confirmer la suppression de ces tables ? (oui/non) : ")
  if (!tolower(trimws(reponse)) %in% c("oui", "o", "yes", "y")) {
    dbDisconnect(con)
    stop("Annulé par l'utilisateur — aucune table supprimée.")
  }
}
# ==============

for (t in tables_a_supprimer) {
  if (dbExistsTable(con, t)) {
    dbExecute(con, sprintf("DROP TABLE %s", t))
    cat("✓ Table supprimée :", t, "\n")
  } else {
    cat("- Table déjà absente :", t, "\n")
  }
}

dbDisconnect(con)
cat("\n✓ BD de validation vidée. Prête à être envoyée pour un test \"à partir de rien\".\n")
