# Species distribution models for *Leporillus* stick-nest rats

## Overview
This repository contains the R code and data underlying hindcast species
distribution models (SDMs) for two Australian stick-nest rats,
*Leporillus apicalis* (lesser stick-nest rat) and *L. conditor* (greater
stick-nest rat). Ensemble SDMs are fitted to occurrence records and the
Krapp et al. (2021) palaeoclimate reconstruction, projected across the last
~40,000 years, and validated against dated stick-nest midden deposits.

This was produced by Annie G. Kraehe and July A. Pilowsky in 2025 -2026

## Contents

### Code (`/Analyses`)
- **`Krapp apicalis.Rmd`** — full pipeline for *L. apicalis*: data
  preparation, stratified environmental pseudo-absence sampling, ensemble
  SDM fitting (GLM, GAM, random forest, boosted trees) with spatial block
  cross-validation, hindcast projection, midden validation, and figure
  generation.
- **`Krapp conditor.Rmd`** — the identical pipeline for *L. conditor*. The
  two scripts differ only in the species name; each produces that species'
  model outputs independently.
- **`[Combined figures].Rmd`** — generates the combined, two-species
  figures used in the manuscript. This script depends on the outputs of
  both species scripts and must be run **after** both have completed.

### Data (`/Data`)
- **`raw/`** — unprocessed occurrence and midden records for both species.
- **`processed/`** — cleaned inputs used directly by the code
  (`apicalis.csv`, `conditor.csv`, `Dated-Middens.csv`).

### Results (`/Results`)
Figures, prediction rasters, and summary tables produced by the scripts.

## Reproducing the analysis
1. Install R (≥ [4.x]) and the packages listed at the top of each `.Rmd`
   (key dependencies: `tidysdm`, `tidymodels`, `pastclim`, `terra`, `sf`,
   `USE`).
2. The Krapp2021 palaeoclimate dataset is downloaded automatically via
   `pastclim::download_dataset("Krapp2021")`.
3. Run `Krapp apicalis.Rmd` and `Krapp conditor.Rmd` (in either order).
4. Run `[Combined figures].Rmd` to produce the publication figures.

## Data sensitivity and coordinate generalisation
To protect potentially sensitive ecological and cultural information, all
coordinate data in this repository have been generalised to a spatial
resolution of 0.01 decimal degrees (approximately 1 km). Because the
palaeoclimate layers used for modelling are at a much coarser (~0.5°)
resolution, this generalisation is far finer than the modelling grid and
does not affect model outputs.

for inquiries, contact: annie.kraehe@anu.edu.au
