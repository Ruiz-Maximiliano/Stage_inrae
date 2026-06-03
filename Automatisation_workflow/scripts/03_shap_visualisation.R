# ============================================================
# 03_shap_visualisation.R — Visualisation des valeurs SHAP
# À exécuter après le pipeline hebdomadaire
# ============================================================

library(here)
library(treeshap)
library(ggplot2)
library(dplyr)

source(here("config.R"))

path_models <- here("models")

res_abundance <- readRDS(file.path(path_models, "res_abundance_LOSO_quantile_rf.rds"))
rf_abundance_q <- res_abundance$model_quantile

predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")

# ============================================================
# 1. Calcul des valeurs SHAP
# ============================================================

X_abundance <- as.data.frame(df_meteo_predictions[, predictors_abundance])

unified_model <- ranger.unify(rf_abundance_q, X_abundance)
treeshap_result <- treeshap(unified_model, X_abundance)

shap_df <- as.data.frame(treeshap_result$shaps)
colnames(shap_df) <- paste0("shap_", colnames(shap_df))

cat("Valeurs SHAP calculées pour", nrow(shap_df), "observations\n")


# ============================================================
# 2. Importance globale — bar plot
# ============================================================

shap_importance <- colMeans(abs(shap_df))
shap_importance_df <- data.frame(
  variable   = gsub("shap_", "", names(shap_importance)),
  importance = as.numeric(shap_importance)
)

ggplot(shap_importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Importance moyenne des variables (|SHAP|)",
       x = "Variable", y = "Importance moyenne")


# ============================================================
# 3. Distribution SHAP — beeswarm
# ============================================================

shap_long <- bind_cols(
  df_meteo_predictions[, predictors_abundance],
  shap_df
) %>%
  tidyr::pivot_longer(cols = starts_with("shap_"),
                      names_to = "variable", values_to = "shap_value") %>%
  mutate(variable = gsub("shap_", "", variable))

feat_long <- df_meteo_predictions[, predictors_abundance] %>%
  mutate(row_id = row_number()) %>%
  tidyr::pivot_longer(-row_id, names_to = "variable", values_to = "feature_value")

shap_long <- shap_long %>%
  mutate(row_id = rep(seq_len(nrow(df_meteo_predictions)), length(predictors_abundance))) %>%
  left_join(feat_long, by = c("row_id", "variable"))

ggplot(shap_long, aes(x = shap_value, y = variable, color = feature_value)) +
  geom_jitter(height = 0.2, alpha = 0.3, size = 0.8) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  theme_minimal() +
  labs(title = "Impact des variables météo sur l'abondance prédite",
       x = "Valeur SHAP", y = "Variable", color = "Valeur\nvariable")


# ============================================================
# 4. Effet partiel TM_0_4
# ============================================================

ggplot(bind_cols(df_meteo_predictions["TM_0_4"], shap_df["shap_TM_0_4"]),
       aes(x = TM_0_4, y = shap_TM_0_4)) +
  geom_point(alpha = 0.3, color = "steelblue", size = 0.8) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  theme_minimal() +
  labs(title = "Effet de TM_0_4 sur l'abondance prédite",
       x = "Température moyenne semaines 0-4", y = "Contribution SHAP")


c# ============================================================
# 5. Waterfall — prédiction individuelle (max)
# ============================================================

idx      <- which.max(df_meteo_predictions$pred_combined_mean)
baseline <- mean(df_meteo_predictions$pred_combined_mean, na.rm = TRUE)

waterfall_df <- data.frame(
  variable = gsub("shap_", "", colnames(shap_df)),
  shap     = as.numeric(shap_df[idx, ])
) %>%
  arrange(desc(abs(shap))) %>%
  mutate(cumulative = cumsum(shap) + baseline,
         start      = lag(cumulative, default = baseline),
         color      = ifelse(shap >= 0, "Positif", "Négatif"))

ggplot(waterfall_df, aes(x = reorder(variable, abs(shap)))) +
  geom_segment(aes(xend = variable, y = start, yend = cumulative, color = color),
               linewidth = 6) +
  scale_color_manual(values = c("Positif" = "tomato", "Négatif" = "steelblue")) +
  coord_flip() +
  theme_minimal() +
  labs(title = paste("Explication prédiction max (ligne", idx, ")"),
       subtitle = paste("Prédiction =", round(df_meteo_predictions$pred_combined_mean[idx], 2),
                        "| Baseline =", round(baseline, 2)),
       x = "Variable", y = "SHAP cumulé", color = "Contribution")


# ============================================================
# 6. Variable dominante par commune
# ============================================================

shap_df_clean <- shap_df
colnames(shap_df_clean) <- paste0("shap_", predictors_abundance)
shap_cols <- colnames(shap_df_clean)

# Supprimer les colonnes SHAP déjà existantes dans df_meteo_predictions
df_clean <- df_meteo_predictions %>%
  dplyr::select(-any_of(shap_cols))

df_clean %>%
  bind_cols(shap_df_clean) %>%
  mutate(dominant_var = gsub("shap_", "", shap_cols[
    apply(abs(across(all_of(shap_cols))), 1, which.max)
  ])) %>%
  filter(pred_combined_mean > quantile(pred_combined_mean, 0.75, na.rm = TRUE)) %>%
  count(dominant_var, sort = TRUE) %>%
  ggplot(aes(x = reorder(dominant_var, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable dominante pour les prédictions élevées (Q75)",
       x = "Variable", y = "Nombre d'observations")

