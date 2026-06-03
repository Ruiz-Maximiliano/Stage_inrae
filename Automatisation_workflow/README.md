# Aedes albopictus prediction pipeline

Internship project at UMR MIVEGEC / TETIS (Montpellier, France) — Master 1 MIASHS, Université Paul-Valéry.

## Objective

Operationalize an existing R pipeline that generates weekly predictions of Aedes albopictus (tiger mosquito) abundance at the commune level in southern France, and eventually deploy it on a research server with a web interface.

## Getting started

### 1. Open the project

Open `Stage_inrae.Rproj` in RStudio. This automatically sets the working directory to the project root.

### 2. Configure credentials

Copy `config_template.R`, rename it to `config.R` and fill in your database credentials.

### 3. Add required data files

Place the following files in the `data/` folder:

- `data/administrative_boundaries.gpkg` — administrative boundaries (communes/departments)
- `data/df_to_model.csv` — mosquito trap observations used for model training

### 4. Run Script 1 (once)

```r
source("scripts/01_initialisation.R")
```

This will:
- Create the spatial grid and intersect with the study area
- Download weather history from OpenMeteo API (~2h depending on grid size)
- Train the two-part model (presence + abundance)
- Save `data/coords_grid.csv`, `data/raw/meteofrance_herault.csv` and model `.rds` files

### 5. Run Script 2 (every week)

```r
source("scripts/02_hebdomadaire.R")
```

This will:
- Replace last week's forecast data with historical data
- Download new forecast
- Run the model and generate predictions
- Publish results to PostgreSQL

## Project structure

```
Stage_inrae/
├── Stage_inrae.Rproj
├── README.md
├── config_template.R     ← copy and rename to config.R
├── config.R              ← NOT committed (credentials)
├── .gitignore
├── scripts/
│   ├── 00_functions.R    ← custom OpenMeteo functions
│   ├── 01_initialisation.R
│   └── 02_hebdomadaire.R
├── data/
│   ├── administrative_boundaries.gpkg  ← à fournir
│   ├── df_to_model.csv                 ← à fournir
│   ├── coords_grid.csv                 ← généré par Script 1
│   └── raw/
│       └── meteofrance_herault.csv     ← généré par Script 1
└── models/               ← généré par Script 1
    ├── res_presence_LOSO_probabilistic.rds
    └── res_abundance_LOSO_quantile_rf.rds
```

## Model performance

Trained on 4 sites (Bayonne, Murviel-les-Montpellier, Pérols, Saint-Médard-en-Jalles).

- Presence/absence AUC > 0.9 on all sites
- Abundance Spearman = 0.871, MAE = 7.17
- Coverage: 397 communes, 6 departments (11, 12, 13, 30, 34, 81)

## Dependencies

```r
here, terra, sf, dplyr, tidyverse, data.table, httr, jsonlite,
exactextractr, caret, ranger, CAST, DBI, RPostgres, lubridate, pROC
```
