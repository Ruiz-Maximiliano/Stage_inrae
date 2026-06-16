# ============================================================
# #new (3 - Séparer entraînement) ====
# SCRIPT 0 — Entraînement des modèles
# À exécuter UNE SEULE FOIS (ou lors d'un ré-entraînement)
# AVANT de lancer main.R / 01_initialisation.R
#
# Prérequis :
#   - data/df_to_model.csv   (données d'entraînement)
#
# Génère :
#   - models/res_presence_LOSO_probabilistic.rds
#   - models/res_abundance_LOSO_quantile_rf.rds
#   - models/res_training_data.rds   (données X pour SHAP dans le pipeline)
# ==============
# ============================================================

library(here)
library(dplyr)
library(tidyverse)
library(caret)
library(ranger)
library(CAST)

source(here("config.R"))

set.seed(123)

path_df_model <- here("data", "df_to_model.csv")
path_models   <- here("models")
dir.create(path_models, showWarnings = FALSE)

# ============================================================
# Paramètres d'entrée — prédicteurs
# ============================================================
predictors_presence  <- c("TM_0_8", "UM_5_11")
predictors_abundance <- c("TM_0_4", "UM_0_11", "RR_1_5")

# ============================================================
# 1. Chargement et préparation des données
# ============================================================

cat("Chargement des données d'entraînement...\n")

df_model <- read.csv(path_df_model) %>%
  dplyr::filter(site != "MURET") %>%
  filter(!is.na(Year))

df_model <- df_model %>%
  relocate(effectif_jour, .before = RR_0_0) %>%
  group_by(site, Year, week) %>%
  summarise_at(vars(effectif_jour:photoperiod), mean, na.rm = TRUE) %>%
  ungroup()

df_model <- df_model %>%
  rename(NB_ALBO_TOT = effectif_jour) %>%
  mutate(
    PRES_ALBO         = ifelse(NB_ALBO_TOT > 0, "Presence", "Absence"),
    PRES_ALBO         = factor(PRES_ALBO, levels = c("Presence", "Absence")),
    PRES_ALBO_NUMERIC = ifelse(PRES_ALBO == "Presence", 1, 0)
  ) %>%
  filter(!is.na(NB_ALBO_TOT)) %>%
  filter(site != "RENNES") %>%
  mutate(row_id = row_number())

# ============================================================
# 2. Modèle de présence (LOSO — Random Forest probabiliste)
# ============================================================

cat("Entraînement du modèle de présence...\n")

df_model_presence <- df_model %>%
  dplyr::select(row_id, site, Year, week, NB_ALBO_TOT, PRES_ALBO,
                all_of(predictors_presence))

indices_cv_presence <- CAST::CreateSpacetimeFolds(
  df_model_presence,
  spacevar = "site",
  k        = length(unique(df_model_presence$site))
)

tr_presence <- trainControl(
  method           = "cv",
  index            = indices_cv_presence$index,
  indexOut         = indices_cv_presence$indexOut,
  summaryFunction  = twoClassSummary,
  classProbs       = TRUE,
  savePredictions  = "final",
  verboseIter      = FALSE
)

mod_presence <- caret::train(
  x          = df_model_presence[, predictors_presence],
  y          = df_model_presence$PRES_ALBO,
  method     = "ranger",
  tuneLength = 10,
  trControl  = tr_presence,
  metric     = "ROC",
  maximize   = TRUE,
  preProcess = c("center", "scale"),
  importance = "permutation"
)

df_cv_presence <- mod_presence$pred %>%
  left_join(df_model_presence, by = c("rowIndex" = "row_id")) %>%
  dplyr::select(rowIndex, pred, Presence, obs, site, week, Year, NB_ALBO_TOT,
                all_of(predictors_presence)) %>%
  mutate(
    obs_num              = ifelse(obs == "Presence", 1, 0),
    pred_presence_prob   = Presence,
    pred_presence_class  = pred,
    pred_presence_var    = pred_presence_prob * (1 - pred_presence_prob),
    pred_presence_entropy = -(
      pred_presence_prob * log(pmax(pred_presence_prob, 1e-8)) +
        (1 - pred_presence_prob) * log(pmax(1 - pred_presence_prob, 1e-8))
    )
  )

# ============================================================
# 3. Modèle d'abondance (LOSO — Quantile Random Forest)
# ============================================================

cat("Entraînement du modèle d'abondance...\n")

df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT > 0) %>%
  dplyr::select(row_id, site, Year, week, NB_ALBO_TOT, PRES_ALBO,
                all_of(predictors_abundance)) %>%
  mutate(NB_ALBO_TOT_LOG = log(NB_ALBO_TOT))

cv_quantile_rf <- function(data, predictors, response = "NB_ALBO_TOT_LOG",
                            site_col = "site", quantiles = c(0.05, 0.5, 0.95),
                            num.trees = 500, mtry = NULL, min.node.size = 5,
                            seed = 123) {

  sites <- unique(data[[site_col]])
  if (is.null(mtry)) mtry <- max(1, floor(sqrt(length(predictors))))
  fold_preds <- vector("list", length(sites))

  for (i in seq_along(sites)) {
    test_site <- sites[i]
    train_dat <- data %>% filter(.data[[site_col]] != test_site)
    test_dat  <- data %>% filter(.data[[site_col]] == test_site)
    set.seed(seed + i)

    mod_qrf <- ranger(
      dependent.variable.name = response,
      data          = train_dat[, c(response, predictors), drop = FALSE],
      num.trees     = num.trees,
      mtry          = mtry,
      min.node.size = min.node.size,
      importance    = "permutation",
      quantreg      = TRUE,
      keep.inbag    = TRUE
    )

    pred_q <- predict(mod_qrf, data = test_dat[, predictors, drop = FALSE],
                      type = "quantiles", quantiles = quantiles)$predictions
    colnames(pred_q) <- paste0("pred_log_q", c("05", "50", "95"))

    fold_preds[[i]] <- bind_cols(test_dat, as.data.frame(pred_q)) %>%
      mutate(
        fold                        = i,
        pred_abundance_q05          = exp(pred_log_q05),
        pred_abundance_q50          = exp(pred_log_q50),
        pred_abundance_q95          = exp(pred_log_q95),
        pred_abundance_interval_width = pred_abundance_q95 - pred_abundance_q05,
        covered = NB_ALBO_TOT >= pred_abundance_q05 & NB_ALBO_TOT <= pred_abundance_q95
      )
  }
  bind_rows(fold_preds)
}

df_cv_abundance_quantiles <- cv_quantile_rf(
  data       = df_model_abundance,
  predictors = predictors_abundance,
  response   = "NB_ALBO_TOT_LOG",
  site_col   = "site",
  quantiles  = c(0.05, 0.5, 0.95),
  num.trees  = 500,
  mtry       = max(1, floor(sqrt(length(predictors_abundance)))),
  min.node.size = 5,
  seed       = 123
)

rf_abundance_q <- ranger(
  dependent.variable.name = "NB_ALBO_TOT_LOG",
  data          = df_model_abundance[, c("NB_ALBO_TOT_LOG", predictors_abundance)],
  num.trees     = 500,
  mtry          = max(1, floor(sqrt(length(predictors_abundance)))),
  min.node.size = 5,
  importance    = "permutation",
  quantreg      = TRUE,
  keep.inbag    = TRUE
)

# ============================================================
# 4. Sauvegarde des modèles (3 fichiers RDS)
# ============================================================

cat("Sauvegarde des modèles...\n")

# RDS 1 — modèle de présence + CV
saveRDS(
  list(model  = mod_presence,
       df_cv  = df_cv_presence,
       df_mod = df_model_presence,
       predictors = predictors_presence),
  file.path(path_models, "res_presence_LOSO_probabilistic.rds")
)

# RDS 2 — modèle d'abondance + CV
saveRDS(
  list(model_quantile  = rf_abundance_q,
       df_cv_quantiles = df_cv_abundance_quantiles,
       df_mod          = df_model_abundance,
       predictors      = predictors_abundance),
  file.path(path_models, "res_abundance_LOSO_quantile_rf.rds")
)

# RDS 3 — données d'entraînement pour le calcul SHAP dans le pipeline
# Contient les matrices X utilisées lors de l'entraînement de chaque modèle
X_presence  <- as.data.frame(df_model_presence[, predictors_presence])
X_abundance <- as.data.frame(df_model_abundance[, predictors_abundance])

saveRDS(
  list(X_presence  = X_presence,
       X_abundance = X_abundance,
       df_model    = df_model),
  file.path(path_models, "res_training_data.rds")
)

cat("\n✓ Entraînement terminé. 3 fichiers RDS sauvegardés dans", path_models, "\n")
cat("  - res_presence_LOSO_probabilistic.rds\n")
cat("  - res_abundance_LOSO_quantile_rf.rds\n")
cat("  - res_training_data.rds\n")
cat("\nVous pouvez maintenant lancer main.R\n")
