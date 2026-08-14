# ============================================================
# ⚠ SCRIPT ABANDONNÉ — CONSERVÉ POUR RÉFÉRENCE, NE PAS RELANCER SANS RAISON ⚠
#
# Ce script (et 05_backfill_shap.R) a été utilisé pour tenter de backfiller
# le SHAP réel sur tout l'historique (10 ans). Trois bugs réels ont été
# trouvés et corrigés au passage dans 02_hebdomadaire.R / 00_functions_models.R
# (échec SHAP silencieux via suppressWarnings, connexion BD morte non
# détectée, "vector memory limit of 16.0 Gb" sur les grosses tranches) — ces
# corrections restent en place, elles profitent aussi au run hebdomadaire
# normal. Mais même avec tout ça corrigé et en tranches d'1 an (~4h30/tranche
# mesuré), le run a tourné deux nuits de suite sans arriver à couvrir tout
# l'historique dans un temps praticable sur ce Mac. Voir le détail complet du
# raisonnement et de l'abandon en tête de 05_backfill_shap.R.
#
# Décision : on revient à la conception d'origine du pipeline — SHAP = NULL
# pour tout l'historique (skip_shap = TRUE dans 01_initialisation.R,
# inchangé), SHAP réel calculé uniquement pour les semaines récentes via le
# run hebdomadaire normal (02_hebdomadaire.R — ça, ça marche très bien,
# quelques minutes par semaine, continue de tourner).
# ============================================================
#
# SCRIPT 6 — Backfill SHAP historique PAR TRANCHES calendaires
# À exécuter MANUELLEMENT. Complète 05_backfill_shap.R : au lieu d'UN run
# monolithique ancré sur Sys.Date() (qui re-traite toujours les jours les
# plus récents à chaque fois qu'on rallonge la fenêtre), découpe tout
# l'historique de meteo_ruiz en tranches calendaires FIXES (ex. 2 ans à la
# fois, "2016-2017", "2018-2019", ...) et lance 02_hebdomadaire.R une fois
# par tranche, avec écriture BD indépendante à la fin de CHAQUE tranche.
#
# POURQUOI DÉCOUPER (ça ne va PAS plus vite au total) :
#   Un seul run de 10 ans (05_backfill_shap.R) et ce script par tranches font
#   le MÊME calcul total, donc ~le même temps cumulé (~40-45h, voir
#   05_backfill_shap.R pour le détail du calcul). Ce script ne réduit PAS ce
#   total. Ce qu'il change :
#     - si une tranche plante (crash R, coupure réseau profonde, Mac qui
#       s'éteint...), seule CETTE tranche est perdue — celles déjà écrites
#       en BD avant restent acquises (contrairement à un run monolithique où
#       tout est perdu si ça plante avant l'écriture finale) ;
#     - possibilité de faire une pause entre deux tranches (fermer R, revenir
#       le lendemain) sans tout reprendre à zéro ;
#     - chaque tranche (~9h pour 2 ans, voir estimation ci-dessous) tient
#       plus facilement dans une session/nuit qu'un bloc de 40-45h d'affilée.
#
# CE QUE FAIT CE CODE :
#   1. Connexion BD légère (fermée immédiatement après) pour lire MIN(date)
#      de meteo_ruiz — sert de point de départ du découpage.
#   2. Découpe [MIN(date), aujourd'hui] en tranches de `taille_tranche_jours`.
#   3. Pour chaque tranche (à partir de `tranche_debut_idx`, voir ci-dessous) :
#      source(02_hebdomadaire.R) avec backfill_start_date / backfill_end_date
#      fixés sur cette tranche, force_recompute = TRUE, init_forecast_done =
#      TRUE (pas de nouveau téléchargement de forecast pendant un backfill
#      historique — sans rapport avec les dates passées qu'on backfille).
#
# REPRISE APRÈS UN CRASH : la console/log affiche "TRANCHE i/N : <début> à
# <fin>" avant chaque tranche. Si ça plante à la tranche 3 sur 5, noter le
# numéro, ajuster tranche_debut_idx <- 3 ci-dessous, puis relancer ce script —
# les tranches 1 et 2 (déjà en BD) ne seront pas refaites.
#
# PARAMÈTRES À AJUSTER :
#   taille_tranche_jours → 730 (2 ans, ~9h/tranche extrapolé) par défaut.
#   shap_max_background  → 50 (défaut — voir 05_backfill_shap.R : 10 serait
#                           trop peu pour le SHAP absolu, dont le background
#                           n'a que 188 lignes au total).
#   tranche_debut_idx    → 1 par défaut (toutes les tranches). Mettre à jour
#                           uniquement pour reprendre après un crash.
#
# LIMITE CONNUE : si deux tranches tournent le même jour calendaire, elles
# partagent le même fichier de log (logs/log/hebdomadaire_<date>.log, nommé
# par Sys.Date() dans 02_hebdomadaire.R) — peu probable vu qu'une tranche
# prend ~9h, mais à savoir si vous testez avec une taille_tranche_jours petite.
# ============================================================

shap_max_background  <- 50    # <-- voir 05_backfill_shap.R : gardé au défaut, pas 10
taille_tranche_jours <- 365   # <-- 1 an par tranche (~4h30/tranche mesuré — dernière valeur utilisée avant abandon)
tranche_debut_idx    <- 1     # <-- reprise après crash : numéro de la tranche à partir de laquelle relancer

# --- 1. Connexion légère pour lire la date la plus ancienne de meteo_ruiz ---
source(here::here("config.R"))
con_tmp <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = db_host, dbname = db_name, port = db_port,
  user = db_user, password = db_password
)
date_min <- DBI::dbGetQuery(con_tmp, sprintf("SELECT MIN(date) AS d FROM %s", db_table_meteo))$d[1]
DBI::dbDisconnect(con_tmp)
date_min <- as.Date(date_min)
cat("Date la plus ancienne dans", db_table_meteo, ":", format(date_min), "\n")

# --- 2. Construction des tranches [début, fin] ---
bornes   <- seq(date_min, Sys.Date(), by = taille_tranche_jours)
tranches <- lapply(seq_along(bornes), function(i) {
  debut <- bornes[i]
  fin   <- if (i < length(bornes)) bornes[i + 1] - 1 else Sys.Date()
  list(debut = debut, fin = fin)
})

cat("=== Plan de backfill —", length(tranches), "tranche(s) de", taille_tranche_jours, "jours ===\n")
for (i in seq_along(tranches)) {
  cat(sprintf(" %d. %s à %s\n", i, format(tranches[[i]]$debut), format(tranches[[i]]$fin)))
}
cat("Démarrage à la tranche", tranche_debut_idx, "\n")

# --- 3. Boucle : une tranche = un run complet de 02_hebdomadaire.R ---
for (i in seq_along(tranches)) {
  if (i < tranche_debut_idx) {
    cat(sprintf("Tranche %d/%d ignorée (déjà faite, tranche_debut_idx = %d)\n",
                i, length(tranches), tranche_debut_idx))
    next
  }

  t <- tranches[[i]]
  cat("\n\n########################################\n")
  cat(sprintf("### TRANCHE %d/%d : %s à %s\n", i, length(tranches), format(t$debut), format(t$fin)))
  cat("########################################\n\n")

  backfill_start_date <- t$debut
  backfill_end_date   <- t$fin
  force_recompute     <- TRUE
  init_forecast_done  <- TRUE
  # shap_max_background reste défini au-dessus, réutilisé pour chaque tranche

  source(here::here("scripts", "02_hebdomadaire.R"))

  rm(backfill_start_date, backfill_end_date, force_recompute, init_forecast_done)
}

cat("\n✓ Backfill par tranches terminé —", length(tranches) - tranche_debut_idx + 1, "tranche(s) traitée(s).\n")
rm(shap_max_background, taille_tranche_jours, tranche_debut_idx, bornes, tranches, date_min)
