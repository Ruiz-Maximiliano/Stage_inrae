# Aedes albopictus prediction pipeline

Internship project at UMR MIVEGEC / TETIS (Montpellier, France) — Master 1 MIASHS, Université Paul-Valéry.

## Objective

Operationalize an existing R pipeline that generates weekly predictions of Aedes albopictus (tiger mosquito) abundance at the commune level in southern France, and eventually deploy it on a research server with a web interface.

## What the pipeline does

- Downloads meteorological data from OpenMeteo API for a grid covering southern France
- Computes lagged weather variables (up to 12 weeks)
- Applies a two-part model (presence/absence + abundance) to predict mosquito activity
- Aggregates predictions by commune and publishes to a PostgreSQL database

## Status

- [x] Pipeline understood and documented
- [x] Pipeline running end to end
- [x] Custom OpenMeteo functions (replacing the package no longer on CRAN)
- [ ] Pipeline automation and scheduling
- [ ] Server deployment
- [ ] Web interface

## Dependencies

terra, sf, dplyr, tidyverse, data.table, httr, jsonlite, exactextractr, caret, ranger, CAST
