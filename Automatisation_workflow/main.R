# ============================================================
# main.R — Point d'entrée du pipeline
# Usage : Rscript main.R
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
