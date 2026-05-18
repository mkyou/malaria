# malaria

Spatio-temporal analysis of malaria case counts across the micro-regions of
the Brazilian Legal Amazon. Bayesian hierarchical models are fit with INLA,
combining a spatial component, temporal components, and a space–time
interaction term. Both *P. vivax* and *P. falciparum* are modelled
separately, with one set of scripts per species.

## Data

The malaria notification data **are not bundled with the repository**.
They are fetched on demand by `scripts/0.download_data.R` from the public
release (CC BY 4.0) hosted on Mendeley Data:

> Monteiro, K., Silva Rocha, É. da, Rogério, S., Carvalho Maia, L. de,
> Peterka, C., Sampaio, V., Dourado, R., Lynn, T., & Endo, P. T. (2023).
> *Legal Amazon malaria notification records, Brazil, 2003–2022* (v2).
> Mendeley Data. https://doi.org/10.17632/9n6b97fsbd.2

The download script pulls the dataset from a pinned version, verifies its
SHA-256 checksum, restricts to the years used by the current analysis,
splits records into species by test code, and aggregates to the
(municipality, month) grain expected by the downstream wrangling scripts.

If Mendeley issues a new version and the `file_id` changes, resolve the new
identifier from the DOI page and update `DATASET_URL` in the script.

### `data/` layout

- `data/main_data/` — *(gitignored)* produced by `0.download_data.R`.
- `data/output_data/` — *(gitignored)* produced by `1.data_wrangling.R`.
- `data/support_data/` — versioned: IBGE municipality codes, yearly
  population estimates, and ERA5-derived relative humidity and temperature
  per municipality/month.
- `data/spatial_data/` — versioned: IBGE shapefiles and derived CSVs.

## Methodology

### Modelling units and predictors

Cases are aggregated to **micro-regions** (the spatial unit) and **months**
(the temporal unit). State-level models (e.g. `scripts/3.state_models/`)
exist for finer-grained checks but are not part of the main pipeline.

For each cell (micro-region × month), the response is the case count and
the offset is the log of the resident population. Predictions are
expressed as case rates per `100000` inhabitants in every script, so that
models, error metrics, and downstream corrections are directly
comparable.

### Random-effects structure

All formulas share the same hierarchical backbone:

- a **cyclic RW2** for the within-year month effect (seasonality),
- a **RW1** for the year effect (long-term trend),
- a **BYM2** spatial effect over the micro-region adjacency graph, and
- a **RW1** over a global month index, capturing residual temporal drift.

Two optional ingredients are explored on top of this backbone:

- **Climate covariates** (relative humidity and temperature), entered as
  linear fixed effects.
- A **space–time interaction** modelled as IID over the
  micro-region × month-index product, which absorbs cell-specific shocks
  that the additive structure cannot.

This yields four candidate formulas (with/without climate covariates ×
with/without interaction), fit for each likelihood family.

### Likelihood families

Three families are routinely fit and compared: **Poisson**, **negative
binomial**, and **Bell**. Two zero-inflated variants (zero-inflated
Poisson and zero-inflated negative binomial) are kept in the repo for
reference but were excluded from the current rerun because they did not
improve fit relative to their non-inflated counterparts.

### Model selection

The four formulas × three families are evaluated with **DIC** and
**WAIC**. The selected formula per family is then used by the
"best models" script (`6.best_models_*.R`) to generate the test-period
predictions consumed by the downstream analysis. The choice of best model
is recorded as a comment at the end of each fitting script.

### Train / test split

Years up to and including the cutoff are used as training; the remaining
years form the held-out test set. The cutoff is identical across all
scripts (`ano >= 2016` defines the test indices) so that error metrics
are directly comparable across families and species.

### Error analysis and post-hoc correction

After model selection, two layers of error analysis are run:

1. `4.error_analysis.R` — spatial maps of nominal and RMSLE errors for
   both species, using the best per-species model (Bell for *vivax*,
   Poisson for *falciparum*).
2. `5.1.error_correction_vivax.R` and `5.2.error_correction_falciparum.R`
   — a piecewise multiplicative correction. The selected model is used to
   classify cells into "near-zero" and "non-trivial" predictions; on the
   non-trivial regime, a no-intercept linear regression on the training
   years produces a scaling coefficient that is applied to the test-year
   predictions. The thresholds separating these regimes are visually
   calibrated on the predicted-vs-real scatter at the per-`100000` scale.

Loss functions (MAE, MBE, RMSE, NRMSE, RAE, RSE, RMSLE) live in
`scripts/loss_functions.R` and are sourced explicitly by any script that
needs them.

## Pipeline

```
0. scripts/0.download_data.R              # Mendeley -> data/main_data/
1. scripts/1.data_wrangling.R             # joins climate/cities -> data/output_data/
2. scripts/2.spatial_data_wrangling.R     # (optional — outputs/*.graph already versioned)
3. scripts/3.microrregion_models/         # INLA fits per micro-region
   3a. 0.vivax_microrregion_default.R     # default-formula baseline
   3b. 1.bell_*.R / 2.nbinomial_*.R /     # per-family selection (DIC/WAIC)
       3.poisson_*.R
   3c. 6.best_models_*.R                  # refits the best formula per family
                                          # and writes the prediction CSVs
4. scripts/4.error_analysis.R             # error maps
5. scripts/5.1.error_correction_*.R       # post-hoc multiplicative correction
```

### Prerequisites

- R 4.3+
- CRAN packages: `dplyr`, `readr`, `tidyr`, `digest`, `sf`, `spdep`,
  `geobr`, `ggplot2`, `Matrix`, `MASS`
- INLA: `install.packages("INLA",
  repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
  dep = TRUE)`
- System libs (Debian/Ubuntu):
  `libgdal-dev libproj-dev libgeos-dev libudunits2-dev libssl-dev
  libcurl4-openssl-dev libxml2-dev`

### Run

```r
Rscript scripts/0.download_data.R
Rscript scripts/1.data_wrangling.R
# pick a model to fit, e.g.:
Rscript scripts/3.microrregion_models/2.nbinomial_falciparum.R
# generate the prediction CSVs used downstream:
Rscript -e 'source("scripts/loss_functions.R"); source("scripts/3.microrregion_models/6.best_models_vivax.R")'
```

## Future work

To extend the window to the years currently available on Mendeley
(2019–2022), update `YEAR_END` in `scripts/0.download_data.R`.

For fresher or finer climate data, the free **Copernicus Climate Data
Store (CDS)** API serves monthly/hourly ERA5. It requires registration
and a key (https://cds.climate.copernicus.eu). To rebuild
`rhum_df`/`temp_df`, fetch `2m_temperature` and `2m_dewpoint_temperature`
and aggregate over each municipality polygon. The automation is not
included here so that credentials do not have to be managed inside the
repo.

## Licences

- Code: see `LICENSE` (if applicable).
- Malaria data: CC BY 4.0 — attribute Monteiro et al. (2023) in any
  republication.
- Maps: IBGE (open use).
