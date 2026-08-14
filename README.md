# Aedes albopictus prediction pipeline

Operational R pipeline that predicts, every week, the presence and abundance of the tiger mosquito (*Aedes albopictus*) at the commune (and department) level in southern France — from raw weather data to a table consumed by a public website.

Built at **UMR MIVEGEC / UMR TETIS** (IRD, CNRS, Université de Montpellier), on top of a prediction model originally developed by **Paul Taconet** (postdoc, UMR MIVEGEC). This repository operationalizes that model: automated weekly runs, explainability (LIME), department-level coverage, and deployment on a research server with a public web interface.

## Why this matters

*Aedes albopictus* has colonized nearly all of southern France in two decades and is spreading north as the climate warms. It's a competent vector for dengue, chikungunya and Zika, and autochthonous outbreaks have already been documented in metropolitan France. Public health authorities need tools to anticipate activity peaks, target entomological surveillance, and time interventions — but machine-learning models that perform well academically rarely make it into operational use. This pipeline exists to close that gap: turning a validated prediction model into something that runs unattended, every week, and stays explainable.

- **Live predictions:** [albopictus.alwaysdata.net](http://albopictus.alwaysdata.net)
- **Scientific validation of the underlying model:** Taconet et al. (2025), *"Temperature is the key weather determinant of Aedes albopictus seasonal activity in southern France"*, bioRxiv. [Read the paper](https://www.biorxiv.org/content/10.1101/2025.10.10.681559v1.abstract)

## The model

Two-part approach: a presence/absence classifier (Random Forest, LOSO cross-validation, AUC > 0.9) combined with a quantile Random Forest for abundance (Spearman = 0.871, MAE = 7.17), trained on ovitrap records across 4 sites and validated over 397 communes in 6 departments of southern France. This repository's production deployment currently targets Hérault (department 34) — `admin_dep`/`admin_levels` in `config.R` control the scope and can be widened to any department covered by the training data.

## Getting started

### 1. Open the project

Open `Stage_inrae.Rproj` in RStudio. This automatically sets the working directory to the project root.

### 2. Configure credentials

Copy `config_template.R`, rename it to `config.R` and fill in your database credentials.

### 3. Train the models (once)

```r
source("scripts/00_train_models.R")
```

Trains the presence and abundance models and saves them (plus their LIME explainers) to `models/`.

### 4. Run Script 1 — initialisation (once)

```r
source("scripts/01_initialisation.R")
```

Downloads the full weather history, builds the historical predictions table, and backfills LIME explanations year by year.

### 5. Run the pipeline weekly (cron)

```r
source("main.R")
```

Runs `02_hebdomadaire.R` (weather refresh + predictions + LIME + publish), and optionally `07_seasonal_forecast_predictions.R` if `run_seasonal_forecast <- TRUE` in `config.R`.

## Pipeline scripts

### Core pipeline

- **`config.R`** — single source of truth for every parameter (study area, API/DB settings, flags). Loads the ROI (commune + department polygons) once so no other script has to.
- **`main.R`** — entry point: sources `01_initialisation.R` then `02_hebdomadaire.R`, and `07_seasonal_forecast_predictions.R` if enabled.
- **`00_functions_api.R`** — Open-Meteo API wrappers: `get_weather_history_batch()`, `get_weather_forecast_batch()`, `get_weather_seasonal_forecast_batch()`.
- **`00_functions_formats.R`** — spatial/temporal transforms: `make_grid()`, `aggregate_meteo_to_roi()` (grid → commune aggregation), `fun_summarize_week()` (daily lags → weekly predictors), `refresh_mean_views()`.
- **`00_functions_models.R`** — modeling and explainability: `predict_two_part_uncertainty()`, `compute_lime_explanation()` / `add_lime_explanations()` (parallelized via `furrr`). Also keeps the now-unused `compute_shap()`/`.shapley_exact()`, superseded by LIME.
- **`00_train_models.R`** — trains the presence and abundance models plus their LIME explainers, saved to `models/`. Run once, or whenever the underlying data changes.
- **`01_initialisation.R`** — one-time (or re-init) setup: builds the weather grid, downloads full history, runs the historical + recent prediction passes, then backfills LIME year by year over the whole history.
- **`02_hebdomadaire.R`** — the weekly cron job: refreshes weather (replaces stale forecast with real data; downloads new forecast if `run_forecast = TRUE`), aggregates by commune, generates two-part predictions, computes LIME, and publishes to `db_layer`.
- **`07_seasonal_forecast_predictions.R`** — exploratory: predictions several months out using Open-Meteo's seasonal forecast API. Writes only to a test table, never to production. Off by default (`run_seasonal_forecast`).

### Utilities

- **`08_seed_meteo_grid.R`** — one-off: seeds the grid-format weather table from the local CSV backup.
- **`09_export_predictions_csv.R`** — read-only export of a year of published predictions to a local CSV.
- **`10_creer_vues_moyennes_hebdo.R`** — manually (re)creates the `mean_10y`/`mean_2y` materialized views (normally refreshed automatically at the end of every weekly run).
- **`11_backfill_debut_historique.R`** — one-time fix for a data gap at the very start of the historical series.
- **`lime.R`** — reference script for testing/inspecting LIME explanations outside the main pipeline.
- **`debug_missing_communes.R`** / **`debug_semaine_2016_06_13.R`** — read-only diagnostic tools that reproduce the real aggregation/lag logic for one commune or one week, to trace where a NULL or a missing commune appears.
- **`scripts/legacy/`** — abandoned SHAP-era scripts, kept locally for reference only (not tracked in git).

## Project structure

```
Stage_inrae/
├── Stage_inrae.Rproj
├── README.md
├── config_template.R        ← copy and rename to config.R
├── config.R                 ← NOT committed (credentials)
├── main.R                   ← entry point (cron)
├── .gitignore
├── scripts/
│   ├── 00_functions_api.R, 00_functions_formats.R, 00_functions_models.R
│   ├── 00_train_models.R
│   ├── 01_initialisation.R
│   ├── 02_hebdomadaire.R
│   ├── 07_seasonal_forecast_predictions.R
│   ├── 08_..11_...R, lime.R, debug_*.R   ← utilities/diagnostics
│   └── legacy/               ← abandoned SHAP scripts, not committed
├── pipeline_test/            ← mirror of scripts/ for testing against isolated tables
├── data/
│   ├── administrative_boundaries.gpkg  ← à fournir
│   └── df_to_model.csv                 ← à fournir
└── models/                   ← généré par 00_train_models.R
    ├── res_presence_LOSO_probabilistic.rds
    ├── res_abundance_LOSO_quantile_rf.rds
    ├── explainer_presence.rds
    └── explainer_abundance.rds
```

## Dependencies

```r
here, terra, sf, dplyr, tidyverse, data.table, httr, jsonlite,
exactextractr, caret, ranger, CAST, lime, furrr, future,
DBI, RPostgres, lubridate, pROC, logr
```

## Credits

Prediction model and original pipeline: **Paul Taconet** (UMR MIVEGEC, IRD). Operationalization (this repository), weekly automation, department-level coverage, LIME explainability and deployment prep: **Maximiliano Ruiz**, Master 1 MIASHS internship (Université Paul-Valéry Montpellier 3), April–July 2026, supervised by Paul Taconet and Sophie Lèbre.
