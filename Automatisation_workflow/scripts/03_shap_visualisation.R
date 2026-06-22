# ============================================================
# SCRIPT 3 — Visualisation des valeurs SHAP
#
# CE QUE FAIT CE CODE :
#   Construit des graphiques (importance globale, distribution, effet partiel,
#   waterfall, variable dominante) à partir des valeurs SHAP déjà calculées par
#   02_hebdomadaire.R. Ne recalcule RIEN — il ne fait que lire et visualiser des
#   colonnes qui existent déjà dans df_meteo_predictions.
#
#   IMPORTANT : ce script doit être exécuté DANS LA MÊME SESSION R, juste après
#   avoir lancé 02_hebdomadaire.R (ou main.R). Il a besoin que l'objet
#   `df_meteo_predictions` soit encore présent dans l'environnement, car les
#   colonnes shap_* n'existent qu'en mémoire (elles ne sont pas relues depuis la BD
#   ici — pour ça, voir la table `db_layer_shap` publiée par 02_hebdomadaire.R).
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   - Aucun à modifier ici. Tout est piloté par predictors_abundance/predictors_presence
#     déjà définis dans 02_hebdomadaire.R.
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   - shap_importance_df, shap_long, feat_long, waterfall_df : data.frames intermédiaires
#     uniquement utilisés pour construire les graphiques ci-dessous.
#
# PARAMÈTRES PRIS D'AUTRES SCRIPTS (variables directes, déjà en mémoire) :
#   - df_meteo_predictions      : table de prédictions + SHAP, créée dans 02_hebdomadaire.R
#   - predictors_abundance      : vecteur de prédicteurs abondance (02_hebdomadaire.R)
#   - predictors_presence       : vecteur de prédicteurs présence  (02_hebdomadaire.R)
#   - colonnes shap_abund_*, shap_pres_*, shap_combined_*, shap_abundcv_* : valeurs
#     SHAP par variable et par modèle, calculées dans le bloc "Calcul des valeurs SHAP"
#     de 02_hebdomadaire.R (voir aussi compute_shap() dans 00_functions.R)
# ============================================================

library(here)
library(ggplot2)
library(dplyr)
library(tidyr)

# Garde-fou : ce script ne sert à rien sans les données de 02_hebdomadaire.R en mémoire
if (!exists("df_meteo_predictions")) {
  stop("df_meteo_predictions introuvable. Exécutez d'abord 02_hebdomadaire.R ",
       "(ou main.R) dans cette même session R avant de lancer ce script.")
}

# new (SHAP sur tous les modèles) ====
# Les 4 familles de SHAP disponibles, chacune associée à un modèle entraîné :
#   abund    = rf_abundance_q     (ranger quantile, modèle d'abondance final/déployé)
#   pres     = mod_presence       (caret/ranger classification, modèle de présence)
#   combined = approximation par règle du produit (presence_prob x abundance) — la
#              prédiction la PLUS IMPORTANTE car c'est elle qui est publiée comme
#              mean_abundance_albopictus
#   abundcv  = mod_abundance_cv   (caret/ranger régression, modèle de comparaison/tuning,
#              non utilisé pour les prédictions finales mais utile en diagnostic)
shap_families <- list(
  abund    = list(prefix = "shap_abund_",    predictors = predictors_abundance,
                   label  = "Abondance (modèle quantile, déployé)"),
  pres     = list(prefix = "shap_pres_",     predictors = predictors_presence,
                   label  = "Présence"),
  combined = list(prefix = "shap_combined_", predictors = unique(c(predictors_abundance, predictors_presence)),
                   label  = "Combined (référence principale)"),
  abundcv  = list(prefix = "shap_abundcv_",  predictors = predictors_abundance,
                   label  = "Abondance (modèle de comparaison caret)")
)
# ==============

# ============================================================
# 1. Importance globale — bar plot, pour chaque famille SHAP disponible
# ============================================================

for (fam_name in names(shap_families)) {
  fam       <- shap_families[[fam_name]]
  shap_cols <- paste0(fam$prefix, fam$predictors)
  shap_cols <- intersect(shap_cols, colnames(df_meteo_predictions))  # ignore si absent

  if (length(shap_cols) == 0) {
    cat("⚠ Pas de colonnes SHAP pour la famille '", fam_name, "' — ignorée\n", sep = "")
    next
  }

  # importance = moyenne de la valeur absolue du SHAP (plus c'est haut, plus la
  # variable pèse en moyenne sur la prédiction de ce modèle)
  shap_importance    <- colMeans(abs(df_meteo_predictions[, shap_cols, drop = FALSE]), na.rm = TRUE)
  shap_importance_df <- data.frame(
    variable   = gsub(fam$prefix, "", names(shap_importance)),
    importance = as.numeric(shap_importance)
  )

  p <- ggplot(shap_importance_df, aes(x = reorder(variable, importance), y = importance)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = paste("Importance moyenne des variables (|SHAP|) —", fam$label),
         x = "Variable", y = "Importance moyenne")

  print(p)
}

# ============================================================
# 2. Distribution SHAP — beeswarm (famille "combined" par défaut)
# ============================================================

# new (SHAP sur le modèle combined — le plus important) ====
# On illustre la distribution avec la famille "combined" car c'est elle qui explique
# la prédiction réellement publiée (mean_abundance_albopictus) — changez fam_plot
# pour "abund", "pres" ou "abundcv" si vous voulez inspecter un sous-modèle en particulier.
fam_plot   <- shap_families[["combined"]]
shap_cols  <- intersect(paste0(fam_plot$prefix, fam_plot$predictors), colnames(df_meteo_predictions))
# ==============

if (length(shap_cols) > 0) {
  shap_long <- df_meteo_predictions[, shap_cols, drop = FALSE] %>%
    mutate(row_id = row_number()) %>%
    tidyr::pivot_longer(-row_id, names_to = "variable", values_to = "shap_value") %>%
    mutate(variable = gsub(fam_plot$prefix, "", variable))

  feat_long <- df_meteo_predictions[, fam_plot$predictors, drop = FALSE] %>%
    mutate(row_id = row_number()) %>%
    tidyr::pivot_longer(-row_id, names_to = "variable", values_to = "feature_value")

  shap_long <- shap_long %>%
    left_join(feat_long, by = c("row_id", "variable"))

  print(
    ggplot(shap_long, aes(x = shap_value, y = variable, color = feature_value)) +
      geom_jitter(height = 0.2, alpha = 0.3, size = 0.8) +
      scale_color_gradient(low = "blue", high = "red") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      theme_minimal() +
      labs(title = paste("Impact des variables météo —", fam_plot$label),
           x = "Valeur SHAP", y = "Variable", color = "Valeur\nvariable")
  )
}

# ============================================================
# 3. Effet partiel d'une variable (TM_0_4, famille combined)
# ============================================================

col_tm04 <- paste0(fam_plot$prefix, "TM_0_4")
if (col_tm04 %in% colnames(df_meteo_predictions)) {
  print(
    ggplot(df_meteo_predictions, aes(x = TM_0_4, y = .data[[col_tm04]])) +
      geom_point(alpha = 0.3, color = "steelblue", size = 0.8) +
      geom_smooth(method = "loess", color = "red", se = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      theme_minimal() +
      labs(title = paste("Effet de TM_0_4 sur la prédiction —", fam_plot$label),
           x = "Température moyenne semaines 0-4", y = "Contribution SHAP")
  )
}

# ============================================================
# 4. Waterfall — prédiction individuelle la plus élevée (famille combined)
# ============================================================

# idx      : ligne (commune x semaine) ayant la prédiction combinée la plus forte
# baseline : valeur moyenne de la prédiction combinée sur tout le jeu de données —
#            point de départ du waterfall (équivalent de l'espérance du modèle)
idx      <- which.max(df_meteo_predictions$pred_combined_mean)
baseline <- mean(df_meteo_predictions$pred_combined_mean, na.rm = TRUE)

shap_cols_combined_all <- intersect(paste0(fam_plot$prefix, fam_plot$predictors),
                                     colnames(df_meteo_predictions))

if (length(shap_cols_combined_all) > 0) {
  waterfall_df <- data.frame(
    variable = gsub(fam_plot$prefix, "", shap_cols_combined_all),
    shap     = as.numeric(df_meteo_predictions[idx, shap_cols_combined_all])
  ) %>%
    arrange(desc(abs(shap))) %>%
    mutate(cumulative = cumsum(shap) + baseline,
           start      = lag(cumulative, default = baseline),
           color      = ifelse(shap >= 0, "Positif", "Négatif"))

  print(
    ggplot(waterfall_df, aes(x = reorder(variable, abs(shap)))) +
      geom_segment(aes(xend = variable, y = start, yend = cumulative, color = color),
                   linewidth = 6) +
      scale_color_manual(values = c("Positif" = "tomato", "Négatif" = "steelblue")) +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Explication prédiction max (ligne", idx, ") —", fam_plot$label),
           subtitle = paste("Prédiction combinée =", round(df_meteo_predictions$pred_combined_mean[idx], 2),
                            "| Baseline =", round(baseline, 2)),
           x = "Variable", y = "SHAP cumulé", color = "Contribution")
  )
}

# ============================================================
# 5. Variable dominante par commune — pour les prédictions élevées (Q75)
# ============================================================

if (length(shap_cols_combined_all) > 0) {
  print(
    df_meteo_predictions %>%
      mutate(dominant_var = gsub(fam_plot$prefix, "", shap_cols_combined_all[
        apply(abs(dplyr::select(., dplyr::all_of(shap_cols_combined_all))), 1, which.max)
      ])) %>%
      filter(pred_combined_mean > quantile(pred_combined_mean, 0.75, na.rm = TRUE)) %>%
      count(dominant_var, sort = TRUE) %>%
      ggplot(aes(x = reorder(dominant_var, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Variable dominante pour les prédictions élevées (Q75) —", fam_plot$label),
           x = "Variable", y = "Nombre d'observations")
  )
}
