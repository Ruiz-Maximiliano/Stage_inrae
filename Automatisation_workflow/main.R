# ============================================================
# main.R — Point d'entrée du pipeline hebdomadaire
# Usage : Rscript main.R  ou  source("main.R")
#
# Ordre d'exécution complet (première fois) :
#   1. source("scripts/00_train_models.R")   ← UNE SEULE FOIS
#   2. source("main.R")                      ← initialisation + hebdomadaire
#
# Ensuite (chaque semaine) :
#   source("main.R")                         ← hebdomadaire uniquement
# ============================================================

library(here)

cat("============================================================\n")
cat("ÉTAPE 1 — Initialisation\n")
cat("============================================================\n")
source(here("scripts", "01_initialisation.R"))

cat("\n============================================================\n")
cat("ÉTAPE 2 — Pipeline hebdomadaire\n")
cat("============================================================\n")
source(here("scripts", "02_hebdomadaire.R"))

cat("\n✓ Pipeline terminé.\n")
