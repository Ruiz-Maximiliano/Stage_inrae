###########################
# Two-part mosquito model with LOSO-CV uncertainty
# - Presence model: probabilistic RF classification
# - Abundance model: quantile RF regression
# - Combined prediction: expected abundance + uncertainty interval
###########################
# Code original de Paul (workflow_ruiz.R) — quasiment inchangé. Les seuls
# écarts par rapport à l'original sont marqués par un commentaire
# "# change : ..." à l'endroit exact où ils se produisent (chemins de
# fichiers adaptés à l'arborescence du pipeline, fonction
# predict_two_part_uncertainty centralisée dans 00_functions.R pour être
# partagée avec 02_hebdomadaire.R, un 4e RDS ajouté pour le SHAP, et la
# correction d'un bug (presence_perf non défini)).
library(tidyverse)
library(caret)
library(CAST)
library(ranger)
library(correlation)

# change : ajout de here()/source() — nécessaires pour intégrer ce script au
# pipeline (chemins relatifs à la racine du projet RStudio, et accès à
# predict_two_part_uncertainty() qui est centralisée dans 00_functions.R
# au lieu d'être redéfinie ici, pour ne pas dupliquer le même code dans
# 00_train_models.R ET 02_hebdomadaire.R).
library(here)
source(here("scripts", "00_functions.R"))

set.seed(123)

###########################
# 1. DATA PREPARATION
###########################

# change : chemin adapté — le fichier réel est dans data/, pas data/processed/
# (le code original pointait vers data/processed/df_to_model.csv, qui
# n'existe pas dans ce projet).
path_df_model <- here("data", "df_to_model.csv")
# change : dossier dédié pour les modèles (le code original sauvegardait les
# .rds directement dans le répertoire de travail) — nécessaire pour que
# 02_hebdomadaire.R puisse les retrouver de façon fiable via path_models.
path_models <- here("models")
dir.create(path_models, showWarnings = FALSE)

df_model <- read.csv(path_df_model) %>%
  dplyr::filter(site!="MURET") %>%
  filter(!is.na(Year))

# aggregate at site-week level
df_model <- df_model %>%
  relocate(effectif_jour, .before = RR_0_0) %>%
  group_by(site, Year, week) %>%
  summarise_at(vars(effectif_jour:photoperiod), mean, na.rm = TRUE) %>%
  ungroup()

# response variables
df_model <- df_model %>%
  rename(NB_ALBO_TOT = effectif_jour) %>%
  mutate(
    PRES_ALBO = ifelse(NB_ALBO_TOT > 0, "Presence", "Absence"),
    PRES_ALBO = factor(PRES_ALBO, levels = c("Presence", "Absence")),
    PRES_ALBO_NUMERIC = ifelse(PRES_ALBO == "Presence", 1, 0)
  ) %>%
  filter(!is.na(NB_ALBO_TOT)) %>%
  filter(site != "RENNES") %>%
  mutate(row_id = row_number())

###########################
# 2. SELECTED PREDICTORS
###########################
# Replace if needed by your final selected predictor sets
predictors_presence  <- c("TM_0_8", "UM_5_11")
predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")

###########################
# 3. HELPER FUNCTIONS
###########################

# Manual LOSO-CV quantile RF for abundance
cv_quantile_rf <- function(data,
                           predictors,
                           response = "NB_ALBO_TOT_LOG",
                           site_col = "site",
                           quantiles = c(0.05, 0.5, 0.95),
                           num.trees = 500,
                           mtry = NULL,
                           min.node.size = 5,
                           seed = 123) {
  sites <- unique(data[[site_col]])
  if (is.null(mtry)) {
    mtry <- max(1, floor(sqrt(length(predictors))))
  }
  fold_preds <- vector("list", length(sites))
  for (i in seq_along(sites)) {
    test_site <- sites[i]
    train_dat <- data %>% filter(.data[[site_col]] != test_site)
    test_dat  <- data %>% filter(.data[[site_col]] == test_site)
    set.seed(seed + i)
    mod_qrf <- ranger(
      dependent.variable.name = response,
      data = train_dat[, c(response, predictors), drop = FALSE],
      num.trees = num.trees,
      mtry = mtry,
      min.node.size = min.node.size,
      importance = "permutation",
      quantreg = TRUE,
      keep.inbag = TRUE
    )
    pred_q <- predict(
      mod_qrf,
      data = test_dat[, predictors, drop = FALSE],
      type = "quantiles",
      quantiles = quantiles
    )$predictions
    colnames(pred_q) <- paste0("pred_log_q", c("05", "50", "95"))
    fold_preds[[i]] <- bind_cols(
      test_dat,
      as.data.frame(pred_q)
    ) %>%
      mutate(
        fold = i,
        pred_abundance_q05 = exp(pred_log_q05),
        pred_abundance_q50 = exp(pred_log_q50),
        pred_abundance_q95 = exp(pred_log_q95),
        pred_abundance_interval_width = pred_abundance_q95 - pred_abundance_q05,
        covered = NB_ALBO_TOT >= pred_abundance_q05 &
          NB_ALBO_TOT <= pred_abundance_q95
      )
  }
  bind_rows(fold_preds)
}

# change : predict_two_part_uncertainty() était définie ici dans le code
# original. Elle est maintenant centralisée dans 00_functions.R (sourcée
# plus haut) car 02_hebdomadaire.R en a aussi besoin pour les prédictions
# hebdomadaires — la garder à un seul endroit évite que les deux fichiers
# divergent si elle est modifiée un jour. Le corps de la fonction est
# identique à l'original, voir 00_functions.R::predict_two_part_uncertainty().

###########################
# 4. PRESENCE MODEL (LOSO-CV)
###########################
df_model_presence <- df_model %>%
  dplyr::select(row_id, site, Year, week, NB_ALBO_TOT, PRES_ALBO, all_of(predictors_presence))

indices_cv_presence <- CAST::CreateSpacetimeFolds(
  df_model_presence,
  spacevar = "site",
  k = length(unique(df_model_presence$site))
)

tr_presence <- trainControl(
  method = "cv",
  index = indices_cv_presence$index,
  indexOut = indices_cv_presence$indexOut,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final",
  verboseIter = FALSE
)

mod_presence <- caret::train(
  x = df_model_presence[, predictors_presence],
  y = df_model_presence$PRES_ALBO,
  method = "ranger",
  tuneLength = 10,
  trControl = tr_presence,
  metric = "ROC",
  maximize = TRUE,
  preProcess = c("center", "scale"),
  importance = "permutation"
)

df_cv_presence <- mod_presence$pred %>%
  left_join(df_model_presence, by = c("rowIndex"="row_id")) %>%
  dplyr::select(
    rowIndex, pred, Presence, obs, site, week, Year, NB_ALBO_TOT,
    all_of(predictors_presence)
  ) %>%
  mutate(
    obs_num = ifelse(obs == "Presence", 1, 0),
    pred_presence_prob = Presence,
    pred_presence_class = pred,
    pred_presence_var = pred_presence_prob * (1 - pred_presence_prob),
    pred_presence_entropy = -(
      pred_presence_prob * log(pmax(pred_presence_prob, 1e-8)) +
        (1 - pred_presence_prob) * log(pmax(1 - pred_presence_prob, 1e-8))
    )
  )

###########################
# 5. ABUNDANCE MODEL (LOSO-CV + quantile RF)
###########################
df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT > 0) %>%
  dplyr::select(row_id, site, Year, week, NB_ALBO_TOT, PRES_ALBO, all_of(predictors_abundance)) %>%
  mutate(NB_ALBO_TOT_LOG = log(NB_ALBO_TOT))

# optional caret model for tuning / performance comparison
spearmcor <- function(data, lev = NULL, model = NULL) {
  out <- cor(x = data$pred, y = data$obs, method = "spearman")
  names(out) <- "spearman"
  out
}

indices_cv_abundance <- CAST::CreateSpacetimeFolds(
  df_model_abundance,
  spacevar = "site",
  k = length(unique(df_model_abundance$site))
)

tr_abundance <- trainControl(
  method = "cv",
  index = indices_cv_abundance$index,
  indexOut = indices_cv_abundance$indexOut,
  savePredictions = "final",
  summaryFunction = spearmcor
)

mod_abundance_cv <- caret::train(
  x = df_model_abundance[, predictors_abundance],
  y = df_model_abundance$NB_ALBO_TOT_LOG,
  method = "ranger",
  tuneLength = 10,
  trControl = tr_abundance,
  metric = "spearman",
  maximize = TRUE,
  preProcess = c("center", "scale"),
  importance = "permutation"
)

# LOSO-CV quantile RF
df_cv_abundance_quantiles <- cv_quantile_rf(
  data = df_model_abundance,
  predictors = predictors_abundance,
  response = "NB_ALBO_TOT_LOG",
  site_col = "site",
  quantiles = c(0.05, 0.5, 0.95),
  num.trees = 500,
  mtry = max(1, floor(sqrt(length(predictors_abundance)))),
  min.node.size = 5,
  seed = 123
)

###########################
# 6. FINAL MODELS ON FULL DATA
###########################
# final abundance quantile RF on all positive data
rf_abundance_q <- ranger(
  dependent.variable.name = "NB_ALBO_TOT_LOG",
  data = df_model_abundance[, c("NB_ALBO_TOT_LOG", predictors_abundance)],
  num.trees = 500,
  mtry = max(1, floor(sqrt(length(predictors_abundance)))),
  min.node.size = 5,
  importance = "permutation",
  quantreg = TRUE,
  keep.inbag = TRUE
)

###########################
# 7. MERGE LOSO-CV PRESENCE + ABUNDANCE
###########################
# Merge out-of-sample predictions by site/week/year
df_cv_combined <- df_cv_presence %>%
  dplyr::select(
    site, Year, week, NB_ALBO_TOT,
    pred_presence_prob, pred_presence_class,
    pred_presence_var, pred_presence_entropy
  ) %>%
  left_join(
    df_cv_abundance_quantiles %>%
      dplyr::select(
        site, Year, week, NB_ALBO_TOT,
        pred_abundance_q05, pred_abundance_q50, pred_abundance_q95,
        pred_abundance_interval_width, covered
      ),
    by = c("site", "Year", "week", "NB_ALBO_TOT")
  ) %>%
  mutate(
    # threshold-free expected abundance
    pred_expected_abundance = pred_presence_prob * pred_abundance_q50,
    # optional thresholded prediction
    pred_thresholded = ifelse(pred_presence_prob > 0.5, pred_abundance_q50, 0)
  )

###########################
# 8. PROPAGATE UNCERTAINTY ON FULL DATA OR NEW DATA
###########################
# example on the whole dataset
df_pred_uncertainty <- predict_two_part_uncertainty(
  newdata = df_model,
  mod_presence = mod_presence,
  rf_abundance_q = rf_abundance_q,
  predictors_presence = predictors_presence,
  predictors_abundance = predictors_abundance,
  n_sim = 2000
)

###########################
# 9. SIMPLE SUMMARIES
###########################
# Presence performance
library(pROC)
auc_by_site <- df_cv_presence %>%
  group_by(site) %>%
  summarise(
    auc = as.numeric(
      pROC::auc(
        roc(
          response = obs,
          predictor = pred_presence_prob,
          levels = c("Absence", "Presence"),
          direction = "<"
        )
      )
    ),
    n = n(),
    n_presence = sum(obs == "Presence"),
    n_absence = sum(obs == "Absence"),
    .groups = "drop"
  )

# Abundance performance
abundance_perf <- df_cv_abundance_quantiles %>%
  summarise(
    spearman = cor(NB_ALBO_TOT, pred_abundance_q50, method = "spearman"),
    pearson = cor(NB_ALBO_TOT, pred_abundance_q50, method = "pearson"),
    mae = mean(abs(NB_ALBO_TOT - pred_abundance_q50), na.rm = TRUE),
    coverage90 = mean(covered, na.rm = TRUE),
    mean_interval_width = mean(pred_abundance_interval_width, na.rm = TRUE)
  )

# By site
abundance_perf_by_site <- df_cv_abundance_quantiles %>%
  group_by(site) %>%
  summarise(
    spearman = cor(NB_ALBO_TOT, pred_abundance_q50, method = "spearman"),
    pearson = cor(NB_ALBO_TOT, pred_abundance_q50, method = "pearson"),
    mae = mean(abs(NB_ALBO_TOT - pred_abundance_q50), na.rm = TRUE),
    coverage90 = mean(covered, na.rm = TRUE),
    mean_interval_width = mean(pred_abundance_interval_width, na.rm = TRUE),
    n = n()
  )


expected_abundance_perf_by_site <- df_cv_combined %>%
  filter(!is.na(pred_expected_abundance)) %>%
  group_by(site) %>%
  summarise(
    spearman = cor(NB_ALBO_TOT, pred_expected_abundance, method = "spearman"),
    mae = mean(abs(NB_ALBO_TOT - pred_expected_abundance), na.rm = TRUE),
    n = n()
  )

cat("\n--- AUC présence par site ---\n");        print(auc_by_site)
cat("\n--- Performance abondance (global) ---\n"); print(abundance_perf)
cat("\n--- Performance abondance par site ---\n"); print(abundance_perf_by_site)
cat("\n--- Performance abondance attendue (présence x abondance) par site ---\n")
print(expected_abundance_perf_by_site)

###########################
# 10. SAVE OUTPUTS
###########################
saveRDS(
  list(
    model = mod_presence,
    df_cv = df_cv_presence,
    df_mod = df_model_presence
  ),
  file.path(path_models, "res_presence_LOSO_probabilistic.rds")  # change : chemin dans path_models (voir section 1)
)

saveRDS(
  list(
    model_cv = mod_abundance_cv,
    model_quantile = rf_abundance_q,
    df_cv_quantiles = df_cv_abundance_quantiles,
    df_mod = df_model_abundance
  ),
  file.path(path_models, "res_abundance_LOSO_quantile_rf.rds")  # change : chemin dans path_models
)

saveRDS(
  list(
    df_cv_combined = df_cv_combined,
    df_pred_uncertainty = df_pred_uncertainty,
    presence_perf = auc_by_site,  # change : le code original mettait "presence_perf = presence_perf",
                                   # mais cette variable n'existe pas (n'est jamais définie ailleurs dans
                                   # le script) — ça aurait fait planter le saveRDS(). auc_by_site est la
                                   # métrique de performance du modèle de présence calculée en section 9,
                                   # donc c'est elle qui correspond à l'intention du nom "presence_perf".
    abundance_perf = abundance_perf,
    abundance_perf_by_site = abundance_perf_by_site
  ),
  file.path(path_models, "res_two_part_LOSO_uncertainty_combined.rds")  # change : chemin dans path_models
)

# change : RDS additionnel, absent du code original — nécessaire pour le calcul
# SHAP dans 02_hebdomadaire.R, qui a besoin d'un jeu de données de référence
# (background) pour expliquer le modèle de présence (forêt de probabilité,
# voir compute_shap()/.shapley_exact() dans 00_functions.R). X_presence est
# effectivement utilisé (res_train$X_presence) ; X_abundance/df_model sont
# gardés en plus pour un usage futur éventuel (diagnostic, SHAP abondance).
X_presence  <- as.data.frame(df_model_presence[, predictors_presence])
X_abundance <- as.data.frame(df_model_abundance[, predictors_abundance])

saveRDS(
  list(X_presence  = X_presence,
       X_abundance = X_abundance,
       df_model    = df_model),
  file.path(path_models, "res_training_data.rds")
)

###########################
# 11. OPTIONAL PLOT: observed vs predicted abundance by site
###########################
library(ISOweek)
df_cv_combined$date <- ISOweek2date(
  paste0(df_cv_combined$Year, "-W", sprintf("%02d", df_cv_combined$week), "-1")
)
p_abundance <- ggplot(df_cv_combined, aes(x = date)) +
  geom_ribbon(
    aes(ymin = pred_abundance_q05, ymax = pred_abundance_q95),
    fill = "grey80", alpha = 0.6
  ) +
  geom_line(aes(y = pred_thresholded), color = "red", linewidth = 1) +
  geom_line(aes(y = NB_ALBO_TOT), color = "blue", linewidth = 1) +
  facet_wrap(~ site, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Week",
    y = "Egg abundance",
    title = "Observed vs predicted abundance (LOSO-CV quantile RF)",
    subtitle = "Grey band = 90% prediction interval"
  )
#ggsave(file.path(path_models, "plot_abundance_LOSO_quantile_rf.png"), p_abundance, width = 10, height = 6)  # change : chemin dans path_models, le reste (ggsave commenté) est identique à l'original

cat("\n✓ Entraînement terminé. Fichiers sauvegardés dans", path_models, "\n")
