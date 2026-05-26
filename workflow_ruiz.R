library(terra)
library(sf)
library(purrr)
library(furrr)
library(dplyr)
install.packages(c('tibblify', 'testthat'))
install.packages("~openmeteo_0.2.4.tar", 
                 repos = NULL, 
                 type = "source")
library(openmeteo)
library(lubridate)
library(tidyverse)
library(data.table)


#============================================================================================
# Grid and Centroids
# Step 1: Define southern France bounding box (adjust if needed)
bbox <- st_bbox(c(xmin = 2.40, xmax = 4.30, ymin = 43.1, ymax = 44), crs = 4326)

# Step 2: Generate 5 km grid
grid <- st_make_grid(
  st_as_sfc(bbox),
  cellsize = 0.05,
  square = TRUE,
  what = "polygons"
)

grid_sf <- st_sf(geometry = grid)
# Just to see the grid ------
plot(st_geometry(grid_sf))

# Step 1: Compute centroids
centroids <- st_centroid(grid_sf)

# Just to see the centroids ------
plot(st_geometry(centroids), add = TRUE, col = "red", pch = 16, cex = 0.3)



#============================================================================================


# intersect with france to retain only relevant points
france <- st_read("Desktop/Stage/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)

centroids <- st_intersection(centroids,france) %>%
  dplyr::select(geometry)

# Step 2: Extract coordinates into a data frame
coords <- st_coordinates(centroids)

coords = round(coords,3)


coords = as.data.frame(coords)

coords$site <- seq(1:nrow(coords))

meteo_prep <- coords %>%
  group_by(row_number() %/% 20) %>%
  group_map(~.x) %>%
  map(.,~group_split(.,site))


plot(st_geometry(france), col = "lightgrey", border = "black")
plot(st_geometry(grid_sf), add = TRUE, border = "blue")
plot(st_geometry(centroids), add = TRUE, col = "red", pch = 16, cex = 0.3)

#============================================================================================
#New section
#Download weather data from today - 2 years for model re-train (in case is needed)

# 5. Make csv of weather history (from API)
meteo_historique <- data.frame()


# Make variables for an interval of 2 years (for data management)

start_date <- today() - years(2)
end_date <- today() - 1

for(i in 1:length(meteo_prep)){
  cat("Dealing with data package", i, "over", length(meteo_prep), "\n")
  
  th_meteo <- map(meteo_prep[[i]], ~openmeteo::weather_history(
    location = c(.$Y, .$X),
    daily = c("temperature_2m_mean", "relative_humidity_2m_mean", "precipitation_sum"),
    start = start_date,
    end = end_date))
  
  th_res <- map2_dfr(meteo_prep[[i]], th_meteo, ~bind_cols(.x, .y))
  meteo_historique <- rbind(meteo_historique, th_res)
  
  system('sleep 70')
}

# 6. Save
write.csv(meteo_historique, "Desktop/Stage/meteofrance_2023_2026_herault_complet.csv", row.names = FALSE)
#============================================================================================











######################################################
######### Téléchargement des données météo
######################################################

meteo <- read.csv("Desktop/Stage/meteofrance_2023_2026_herault_complet.csv")
meteo$date <- as.Date(meteo$date)

coords <- meteo %>%
  distinct(X, Y, site)

meteo_prep <- coords %>%
  group_by(row_number() %/% 20) %>%
  group_map(~.x) %>%
  map(.,~group_split(.,site))




meteo_future <- data.frame()



for(i in 1:length(meteo_prep)){
  
  cat("Dealing with data package",i,"over",length(meteo_prep),"\n")
  
  th_meteo <- map(meteo_prep[[i]], ~openmeteo::weather_forecast(
    location = c(.$Y, .$X),
    daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
    start = today(),
    end = today() + 15))
  
  th_res <- map2_dfr(meteo_prep[[i]], th_meteo, ~bind_cols(.x, .y,))
  
  meteo_future <- rbind(meteo_future, th_res)
  
  system('sleep 60') # to avoid status code 429 :  Minutely API request limit exceeded.
  
}



data.table::fwrite(meteo,"Desktop/Stage/meteofrance_2023_2026_herault_complet.csv")
meteo <- rbind(meteo, meteo_future)




meteo <- fread("Desktop/Stage/meteofrance_2023_2026_herault_complet.csv") %>%
  mutate(date = as.character(date))



######################################################
######### Creation des variables indépendantes
######################################################

meteo <- data.table(meteo)


meteo <- meteo %>%
  unique() %>%
  group_by(X,Y) %>%
  mutate(site = cur_group_id()) %>%
  ungroup() %>%
  relocate(site, 1) %>%
  data.table()

unique_coords <- unique(meteo[,c("site","X","Y")])

unique_coords_sf <- st_as_sf(unique_coords, coords = c("X", "Y"), crs = 4326)

france <- st_read("Desktop/Stage/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)


coords_retain <- st_intersection(unique_coords_sf,france)

coords_retain <- cbind(coords_retain, st_coordinates(coords_retain))
coords_retain <- st_drop_geometry(coords_retain)
coords_retain <- coords_retain[,c("site","X","Y")]

meteo <- meteo %>%
  filter(site %in% coords_retain$site) %>%
  mutate(date = as.Date(date)) %>%
  rename(TM = daily_temperature_2m_mean, RR = daily_precipitation_sum, UM = daily_relative_humidity_2m_mean) %>%
  dplyr::select(site ,date,RR,TM,UM)


lag_max <- 84

meteo2 <- meteo %>%
  dplyr::select(site,date) %>%
  mutate(year = year(date), week = week(date), weekday = wday(date)) %>%
  filter(weekday==1) %>%
  slice(rep(1:n(), each = lag_max)) %>%
  group_by(site, year, week) %>%
  mutate(lag_n = row_number()) %>%
  ungroup() %>%
  dplyr::select(-weekday) %>%
  rename(th_date = date) %>%
  mutate(date = th_date - lag_n) %>%
  data.table()


# summarizing to weeks
meteo3 <- meteo2 %>%
  left_join(meteo, by = c("date","site")) %>%
  pivot_longer(!(site:date), names_to = "var", values_to = 'val') %>%
  data.table()



#### Functions


fun_summarize_week <- function(meteo3,var_to_summarize,fun_summarize,new_var_name,n_days_agg){
  
  if(fun_summarize=="sum"){
    
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][  # lubridate::year assumed loaded
          , .(val = sum(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]
    
    
  } else if (fun_summarize == "mean"){
    
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][  # lubridate::year assumed loaded
          , .(val = mean(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]
    
  }  else if (fun_summarize == "max"){
    
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][  # lubridate::year assumed loaded
          , .(val = max(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]
    
  }  else if (fun_summarize == "min"){
    
    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][  # lubridate::year assumed loaded
          , .(val = min(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]
  }
  
  meteo3_summarize <- data.table(meteo3_summarize)
  
  return(meteo3_summarize)
  
}

df_meteo_pieges_summ <- fun_summarize_week(meteo3,"RR","sum","RR",7) %>%
  bind_rows(fun_summarize_week(meteo3,"TM","mean","TM",7)) %>%
  bind_rows(fun_summarize_week(meteo3,"UM","mean","UM",7))

## je ne sais pas pourquoi cela va jusque 8 parfois, mais on enleve pour avoir 7 (0+7, donc 8 en tout) semaines
df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n<12)

# function to create the data.frame for CCM
fun_ccm_df <- function(df_timeseries, varr, function_to_apply){
  
  df_timeseries_wide <- df_timeseries %>%
    filter(var==varr) %>%
    dplyr::select(-c("date","var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr,"_"))
  
  max_col <- ncol(df_timeseries_wide)
  
  for(i in 3:(max_col-1)){
    for(j in (i+1):max_col){
      column_name <- paste0(colnames(df_timeseries_wide[i]),"_",(j-2))
      if(function_to_apply=="mean"){
        df_timeseries_wide[column_name] <- rowMeans(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="sum"){
        df_timeseries_wide[column_name] <- rowSums(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="max"){
        df_timeseries_wide[column_name] <- max(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="min"){
        df_timeseries_wide[column_name] <- min(df_timeseries_wide[,i:j], na.rm = T)
      }
    }
  }
  
  for(i in 3:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }
  
  return(df_timeseries_wide)
  
}



df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ,"RR","sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ,"TM","mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ,"UM","mean")


df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide3)


df_meteo_predictions <- df_meteo_pieges_summ_wide_meteofrance %>%
  dplyr::select(site,th_date,TM_0_8,UM_5_11,TM_0_4, UM_0_11,RR_1_5,TM_0_0,RR_0_0,TM_0_5, UM_1_10, RR_1_10) %>%
  rename(date=th_date) %>%
  na.omit(.)

######################################################
######### Génération des prédictions et intégration présence + abundance
######################################################

###########################
# Two-part mosquito model with LOSO-CV uncertainty
# - Presence model: probabilistic RF classification
# - Abundance model: quantile RF regression
# - Combined prediction: expected abundance + uncertainty interval
###########################


library(caret)
library(CAST)
  library(ranger)
library(correlation)

set.seed(123)

###########################
# 1. DATA PREPARATION
###########################

df_model <- read.csv( "Desktop/Stage/df_to_model.csv") %>%
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

# Final two-part prediction with propagated uncertainty
predict_two_part_uncertainty <- function(newdata,
                                         mod_presence,
                                         rf_abundance_q,
                                         predictors_presence,
                                         predictors_abundance,
                                         n_sim = 2000) {
  
  # Presence probabilities
  pred_presence <- predict(
    mod_presence,
    newdata = newdata[, predictors_presence, drop = FALSE],
    type = "prob"
  )
  
  p <- pred_presence$Presence
  
  # Abundance quantiles on log scale
  pred_q <- predict(
    rf_abundance_q,
    data = newdata[, predictors_abundance, drop = FALSE],
    type = "quantiles",
    quantiles = c(0.05, 0.5, 0.95)
  )$predictions
  
  out <- newdata %>%
    mutate(
      pred_presence_prob = p,
      pred_presence_var = p * (1 - p),
      pred_presence_entropy = -(
        p * log(pmax(p, 1e-8)) +
          (1 - p) * log(pmax(1 - p, 1e-8))
      ),
      pred_log_abundance_q05 = pred_q[, 1],
      pred_log_abundance_q50 = pred_q[, 2],
      pred_log_abundance_q95 = pred_q[, 3],
      pred_abundance_q05 = exp(pred_log_abundance_q05),
      pred_abundance_q50 = exp(pred_log_abundance_q50),
      pred_abundance_q95 = exp(pred_log_abundance_q95),
      # threshold-free expected abundance
      pred_expected_abundance = pred_presence_prob * pred_abundance_q50
    )
  
  # propagate uncertainty from both models
  sim_res <- purrr::map_dfr(seq_len(nrow(out)), function(i) {
    p_i <- out$pred_presence_prob[i]
    mu_i <- out$pred_log_abundance_q50[i]
    q05_i <- out$pred_log_abundance_q05[i]
    q95_i <- out$pred_log_abundance_q95[i]
    
    # approximate sd on log scale from 90% interval
    sd_i <- (q95_i - q05_i) / (2 * 1.645)
    sd_i <- pmax(sd_i, 1e-6)
    
    z_sim <- rbinom(n_sim, size = 1, prob = p_i)
    a_log_sim <- rnorm(n_sim, mean = mu_i, sd = sd_i)
    a_sim <- exp(a_log_sim)
    y_sim <- z_sim * a_sim
    
    tibble(
      row_id = i,
      pred_combined_mean = mean(y_sim),
      pred_combined_q05 = quantile(y_sim, 0.05),
      pred_combined_q50 = quantile(y_sim, 0.50),
      pred_combined_q95 = quantile(y_sim, 0.95),
      pred_combined_sd = sd(y_sim)
    )
  })
  
  out %>%
    mutate(row_id = seq_len(n())) %>%
    left_join(sim_res, by = "row_id") %>%
    mutate(
      # optional legacy thresholded output
      pred_thresholded = ifelse(pred_presence_prob > 0.5, pred_abundance_q50, 0)
    )
}

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

df_cv_combined %>%
  filter(!is.na(pred_expected_abundance)) %>%
  group_by(site) %>%
  summarise(
    spearman = cor(NB_ALBO_TOT, pred_expected_abundance, method = "spearman"),
    mae = mean(abs(NB_ALBO_TOT - pred_expected_abundance), na.rm = TRUE),
    n = n()
  )


###########################
# 10. SAVE OUTPUTS
###########################

saveRDS(
  list(
    model = mod_presence,
    df_cv = df_cv_presence,
    df_mod = df_model_presence
  ),
  "Desktop/Stage/res_presence_LOSO_probabilistic.rds"
)

saveRDS(
  list(
    model_cv = mod_abundance_cv,
    model_quantile = rf_abundance_q,
    df_cv_quantiles = df_cv_abundance_quantiles,
    df_mod = df_model_abundance
  ),
  "Desktop/Stage/res_abundance_LOSO_quantile_rf.rds"
)

saveRDS(
  list(
    df_cv_combined = df_cv_combined,
    df_pred_uncertainty = df_pred_uncertainty,
    presence_perf = auc_by_site, #change made because there was no presence_perf
    abundance_perf = abundance_perf,
    abundance_perf_by_site = abundance_perf_by_site
  ),
  "Desktop/Stage/res_two_part_LOSO_uncertainty_combined.rds"
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

ggsave("Desktop/Stage/plot_abundance_LOSO_quantile_rf.png", p_abundance, width = 10, height = 6)


######################################################
######### Rasterisation
######################################################
# add X and Y because it was lost in feature engineering 
df_meteo_predictions <- df_meteo_predictions %>%
  left_join(coords_retain, by = "site") %>%
  dplyr::select(-c("X.x", "Y.x")) %>%
  rename(X = X.y, Y = Y.y)


df_meteo_predictions <- predict_two_part_uncertainty(
  newdata = df_meteo_predictions,
  mod_presence = mod_presence,
  rf_abundance_q = rf_abundance_q,
  predictors_presence = predictors_presence,
  predictors_abundance = predictors_abundance,
  n_sim = 2000
)


# to create a regular grid (evenly spaced)
grid_res <- 0.05

df_meteo_predictions <- df_meteo_predictions %>%
  mutate(
    X_snap = round(X / grid_res) * grid_res,
    Y_snap = round(Y / grid_res) * grid_res
  )

res_x <- min(diff(sort(unique(df_meteo_predictions$X_snap))))
res_y <- min(diff(sort(unique(df_meteo_predictions$Y_snap))))

# Compute raster extent from centers
xmin <- min(df_meteo_predictions$X_snap) - res_x / 2
xmax <- max(df_meteo_predictions$X_snap) + res_x / 2
ymin <- min(df_meteo_predictions$Y_snap) - res_y / 2
ymax <- max(df_meteo_predictions$Y_snap) + res_y / 2

# Create template raster
r_template <- rast(
  ext(xmin, xmax, ymin, ymax),
  resolution = c(res_x, res_y),
  crs = "+proj=longlat +datum=WGS84"
)

# Make sure date is character for naming
df_meteo_predictions$date <- as.character(df_meteo_predictions$date)

# Convert to SpatVector
v <- vect(df_meteo_predictions, geom = c("X_snap", "Y_snap"), crs = "EPSG:4326")

# Unique dates
dates <- unique(df_meteo_predictions$date)

# Rasterize each date separately
rasters <- lapply(dates, function(d) {
  v_d <- v[v$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "pred_combined_mean", fun = "mean")
  names(r) <- d
  r
  
})

# Combine into a single multilayer raster
r_stack <- rast(rasters)


######################################################
######### Aggrégation au departement et à la commune pour les prédictions entomo
######################################################

library(exactextractr)

library(DBI)

#con <- dbConnect(
#  RPostgres::Postgres(),
#  host = "postgresql-taconet.alwaysdata.net",
#  dbname = "taconet_albopictus",
#  port = 5432,
#  user = "taconet",
#  password = "HHKcue51"
#)


#limites_administratives <- st_read(con,"administrative_boundaries")
#st_write(limites_administratives, "Desktop/Stage/administrative_boundaries.gpkg")

limites_administratives <- st_read("Desktop/Stage/administrative_boundaries.gpkg")

mean_abundance <- exact_extract(r_stack, limites_administratives, c('mean')) %>%
  bind_cols(limites_administratives)  %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_abundance_albopictus") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(codgeo, libgeo, date, mean_abundance_albopictus) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_abundance_albopictus))

abundance <- mean_abundance %>%
  mutate(date = as.Date(date)) %>%
  mutate(mean_abundance_albopictus = round(mean_abundance_albopictus,1))

abundance$date=abundance$date+1



######################################################
######### Aggrégation au departement et à la commune pour les données météo
######################################################

meteo <- df_meteo_pieges_summ %>%
  filter(lag_n == 0) %>%
  dplyr::select(-c("th_date","lag_n")) %>%
  pivot_wider(names_from  = var, values_from = val) %>%
  left_join(coords_retain) %>%
  mutate(
    X_snap = round(X / grid_res) * grid_res,
    Y_snap = round(Y / grid_res) * grid_res
  ) %>%
  mutate(date = as.character(date))

res_x <- min(diff(sort(unique(meteo$X_snap))))
res_y <- min(diff(sort(unique(meteo$Y_snap))))

# Compute raster extent from centers
xmin <- min(meteo$X_snap) - res_x / 2
xmax <- max(meteo$X_snap) + res_x / 2
ymin <- min(meteo$Y_snap) - res_y / 2
ymax <- max(meteo$Y_snap) + res_y / 2

# Create template raster
r_template <- rast(
  ext(xmin, xmax, ymin, ymax),
  resolution = c(res_x, res_y),
  crs = "+proj=longlat +datum=WGS84"
)

# Convert to SpatVector
v_meteo <- vect(meteo, geom = c("X_snap", "Y_snap"), crs = "EPSG:4326")

# Unique dates
dates <- unique(meteo$date)


# Rasterize each date separately
rasters <- lapply(dates, function(d) {
  v_d <- v_meteo[v_meteo$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "TM", fun = "mean")
  names(r) <- d
  r
})

# Combine into a single multilayer raster
r_meteo_tm <- rast(rasters)


rasters <- lapply(dates, function(d) {
  v_d <- v_meteo[v_meteo$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "UM", fun = "mean")
  names(r) <- d
  r
})

# Combine into a single multilayer raster
r_meteo_um <- rast(rasters)


rasters <- lapply(dates, function(d) {
  v_d <- v_meteo[v_meteo$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "RR", fun = "mean")
  names(r) <- d
  r
})

# Combine into a single multilayer raster
r_meteo_rr <- rast(rasters)



library(exactextractr)

mean_temperature <- exact_extract(r_meteo_tm, limites_administratives, c('mean')) %>%
  bind_cols(limites_administratives) %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_temperature") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(codgeo, libgeo, date, mean_temperature) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_temperature)) %>%
  mutate(mean_temperature=round(mean_temperature,1))


mean_rainfall <- exact_extract(r_meteo_rr, limites_administratives, c('mean')) %>%
  bind_cols(limites_administratives) %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_rainfall") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(codgeo, libgeo, date, mean_rainfall) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_rainfall))  %>%
  mutate(mean_rainfall=round(mean_rainfall,1))


mean_humidity <- exact_extract(r_meteo_um, limites_administratives, c('mean')) %>%
  bind_cols(limites_administratives) %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_humidity") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(codgeo, libgeo,date, mean_humidity) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_humidity)) %>%
  mutate(mean_humidity=round(mean_humidity,1))


meteo <- left_join(mean_temperature,mean_rainfall)
meteo <- left_join(meteo,mean_humidity)

meteo$date <- meteo$date+2


######################################################
######### Publication dans postgis (sorties des modèles et données météo)
######################################################

albopictus_predictions <- left_join(meteo, abundance) %>%
  mutate(date_fin = date+7) %>%
  relocate(date_fin, .after = date) %>%
  mutate(last_update = as.Date(today()))

## Calculate Level
# Threshold for orange to red status
thresh_orange_red <- median(albopictus_predictions$mean_abundance_albopictus[which(albopictus_predictions$mean_abundance_albopictus>0 & !is.na(albopictus_predictions$mean_abundance_albopictus>0))])

albopictus_predictions <- albopictus_predictions %>%
  mutate(level_risk = case_when(mean_abundance_albopictus==0 | is.na(mean_abundance_albopictus) ~ "Faible",
                                mean_abundance_albopictus>0 & mean_abundance_albopictus < thresh_orange_red ~ "Modéré",
                                mean_abundance_albopictus >= thresh_orange_red ~ "Élevé"))

## Calculate trend
# calcul de la tendance
albopictus_predictions <- albopictus_predictions %>%
  arrange(codgeo, date) %>%
  group_by(codgeo) %>%
  mutate(
    trend = case_when(
      is.na(mean_abundance_albopictus) ~ NA_real_,
      is.na(lag(mean_abundance_albopictus)) ~ NA_real_,
      lag(mean_abundance_albopictus) == 0 ~ NA_real_,
      TRUE ~ 100 * (mean_abundance_albopictus - lag(mean_abundance_albopictus)) /
        lag(mean_abundance_albopictus)
    )
  ) %>%
  ungroup()  %>%
  mutate(trend = ifelse(is.na(trend), 0, round(trend)))

# classification des tendances
albopictus_predictions <- albopictus_predictions %>%
  mutate(
    class_trend = case_when(
      is.na(trend) ~ "Stable",
      trend > 20  ~ "En hausse",
      trend < -20 ~ "En baisse",
      TRUE           ~ "Stable"
    )
  )


####st_write(albopictus_predictions, dsn = con, layer = "albopictus_predictions",append = FALSE)

# DBI:::dbSendQuery(con,"create view albopictus_climate_suitability_weekly AS select b.libgeo, b.dep, b.level, a.*,  b.geometry from albopictus_predictions a left join administrative_boundaries b ON a.codgeo = b.codgeo")

####st_write(isobands, dsn = con, layer = "albopictus_climate_suitability_isobands",append = FALSE)

# DBI:::dbSendQuery(con,"create materialized view  albopictus_climate_suitability_isobands_herault as SELECT a.fid, a.date,  a.lo, a.hi, ST_Intersection(a.geometry, b.geometry) AS geometry FROM albopictus_climate_suitability_isobands a LEFT JOIN administrative_boundaries b ON ST_Intersects(a.geometry, b.geometry) WHERE b.dep = '34' and b.level='departement';")



# save locally to keep the db untouched
write.csv(albopictus_predictions, "Desktop/Stage/albopictus_predictions.csv", row.names = FALSE)


