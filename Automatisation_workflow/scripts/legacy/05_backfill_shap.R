# ============================================================
# ⚠ SCRIPT ABANDONNÉ — CONSERVÉ POUR RÉFÉRENCE, NE PAS RELANCER SANS RAISON ⚠
#
# QUOI : tentative de calculer le SHAP réel (spatial + absolu) sur tout
# l'historique de meteo_ruiz (10 ans), au lieu de le laisser à NULL comme le
# fait 01_initialisation.R (skip_shap = TRUE — décision de conception
# ORIGINALE du pipeline, pas quelque chose introduit ici).
#
# CE QUI A ÉTÉ FAIT PENDANT CETTE TENTATIVE (bugs réels trouvés et corrigés
# dans 02_hebdomadaire.R / 00_functions.R — CEUX-LÀ RESTENT, ils profitent
# aussi au run hebdomadaire normal) :
#   1. Un suppressWarnings() cachait silencieusement tout échec de
#      .shapley_exact() → colonnes SHAP à NA sans AUCUNE trace, ni en log ni
#      en console. Corrigé (l'erreur réelle est maintenant loguée).
#   2. dbIsValid(con) ne détectait pas une connexion coupée côté serveur
#      après un calcul long → un run de 3h a été perdu (jamais écrit en BD).
#      Corrigé (test actif de la connexion + retry avec reconnexion).
#   3. .shapley_exact() construisait tout le batch (n_lignes × background)
#      d'un coup → "vector memory limit of 16.0 Gb reached" sur les grandes
#      fenêtres (crash silencieux à nouveau caché par le bug n°1, avant qu'il
#      soit corrigé). Corrigé (traitement par lots, shap_batch_size).
#
# POURQUOI ABANDONNÉ malgré ces corrections : le coût de calcul est réel et
# incompressible avec la méthode actuelle (SHAP exact = 32 coalitions × 2
# backgrounds × 50 lignes de référence, pour CHAQUE ligne à expliquer — voir
# 06_backfill_shap_por_tramos.R pour le calcul détaillé). Même en tranches
# d'1 an (~4h30 chacune, mesuré), le run a été laissé tourner deux nuits de
# suite sans réussir à couvrir tout l'historique dans un temps praticable sur
# ce Mac (veille, coupures, sessions R qui ne tiennent pas la distance).
# Décision : revenir à la conception d'origine — SHAP = NULL pour tout
# l'historique (skip_shap = TRUE dans 01_initialisation.R, inchangé), SHAP
# réel calculé uniquement pour les semaines récentes via le run hebdomadaire
# normal (02_hebdomadaire.R, ~600 lignes/semaine, quelques minutes — ÇA ce
# n'est PAS abandonné, ça marche et continue de tourner chaque semaine).
#
# Si un jour on veut reprendre ça : la seule vraie option pour que ce soit
# praticable serait une méthode SHAP approchée (échantillonnage de
# permutations au lieu de l'énumération exacte des 32 coalitions) — pas fait
# ici, changement d'algorithme plus profond.
# ============================================================
#
# SCRIPT 5 — Backfill SHAP historique
# À exécuter MANUELLEMENT (pas dans le cron) pour calculer le SHAP réel
# sur une fenêtre historique donnée, au lieu de le laisser à NULL par défaut
# (comme le fait 01_initialisation.R avec skip_shap = TRUE).
#
# CE QUE FAIT CE CODE :
#   Réutilise 02_hebdomadaire.R tel quel (mêmes modèles, même fonction
#   .shapley_exact(), même écriture BD par DELETE + append sur les dates
#   concernées) mais avec init_lookback = backfill_days et SANS définir
#   skip_shap — donc le bloc SHAP réel (pas le raccourci NA) tourne sur
#   toute la fenêtre demandée, pas seulement les n_days_forecast derniers
#   jours du run hebdomadaire normal.
#
# LIMITE IMPORTANTE — fenêtre toujours ancrée sur AUJOURD'HUI :
#   init_lookback filtre meteo2 par date >= Sys.Date() - init_lookback —
#   ça veut dire que backfill_days = 730 RE-traite aussi les 365 derniers
#   jours déjà couverts par un run à 365 (pas de découpage par année propre,
#   ex. "juste 2016" tout seul). Pour l'instant on ne peut que rallonger la
#   fenêtre depuis aujourd'hui, pas viser une plage [début, fin] arbitraire.
#   Si besoin d'un vrai découpage année par année sans recalcul redondant,
#   il faut faire évoluer 02_hebdomadaire.R pour accepter une fenêtre
#   [start_date, end_date] — pas fait ici, à discuter si nécessaire.
#
# COÛT MESURÉ EN PRATIQUE (donnée réelle, pas une estimation théorique) :
#   backfill_days = 365 (~17 268 lignes) avec shap_max_background = 50 (défaut
#   historique) → ETA mesuré ~4h30 (2 x ~2h15 pour spatial + absolu). Extrapolé
#   à 3655 jours (~168 000 lignes, ~9.7x plus de lignes) → ~40-45 HEURES. Le
#   coût de .shapley_exact() est ~linéaire en (n_lignes × shap_max_background) :
#   32 coalitions × 2 backgrounds (spatial + absolu) × 2 predict() (présence +
#   abondance quantile, ce dernier étant le plus lent) sur des data.frames de
#   n_lignes × background.
#
# OPTIMISATION ENVISAGÉE ET ÉCARTÉE : appeler directement le ranger sous-jacent
# de mod_presence (object$mod_presence$finalModel) au lieu de passer par
# caret::predict.train() aurait évité l'overhead de caret. ÉCARTÉ car
# 00_train_models.R (script de Paul — ne pas toucher) entraîne ce modèle avec
# preProcess = c("center", "scale") : caret::predict.train() recentre/réduit
# les variables avant de les passer au ranger interne. Appeler le ranger
# directement avec des variables non transformées donnerait des prédictions
# FAUSSES silencieusement. Le modèle d'abondance (rf_abundance_q) est lui déjà
# un ranger direct (pas de caret) — rien à optimiser de ce côté.
#
# shap_max_background GARDÉ À 50 (pas réduit) : pour le SHAP absolu, le
# background vient de res_train$X_combined — seulement 188 lignes au total.
# Un échantillon de 10 dessus (5% du jeu) serait trop petit/instable. 50 reste
# le bon compromis. Conséquence acceptée : ~40-45h de calcul pour les 10 ans,
# mais ce backfill n'est pas censé tourner souvent (ex. une fois tous les ~2
# ans) donc la durée n'est pas le facteur limitant — la précision du SHAP si.
#
# PARAMÈTRES À AJUSTER :
#   backfill_days       → fenêtre en jours depuis aujourd'hui (voir limite ci-dessus)
#   shap_max_background → taille de l'échantillon background dans .shapley_exact()
#                          (défaut 50 dans 02_hebdomadaire.R si non défini ici)
#
# PROGRESSION : 02_hebdomadaire.R / .shapley_exact() / predict_two_part_uncertainty()
#   affichent désormais un avancement (coalition i/32, % lignes traitées, temps
#   écoulé, ETA estimé) — voir logs/log/hebdomadaire_<date>.log ou la console
#   pendant que ça tourne, pour suivre un run long sans deviner.
#
# Changer les deux valeurs ci-dessous et relancer le script pour chaque fenêtre.
# ============================================================

backfill_days       <- 365   # <-- CIBLE FINALE : 10 ans complets
shap_max_background <- 50     # <-- gardé au défaut historique (188 lignes d'entraînement pour le SHAP absolu — 10 serait trop peu)

init_lookback      <- backfill_days
force_recompute    <- TRUE
init_forecast_done <- TRUE
# skip_shap n'est PAS défini ici → 02_hebdomadaire.R prend la branche SHAP
# réel (voir "if (exists('skip_shap') && isTRUE(skip_shap))" dans ce script).

source(here::here("scripts", "02_hebdomadaire.R"))

rm(init_lookback, force_recompute, init_forecast_done, backfill_days, shap_max_background)
