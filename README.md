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
SHA-256 checksum, restricts to the years used by the current analysis
(`YEAR_START`/`YEAR_END` in `0.download_data.R`, currently 2003–2022 — the
Mendeley v2 release covers the full range, we just weren't using all of it
before), splits records into species by test code, and aggregates to the
(municipality, month) grain expected by the downstream wrangling scripts.

If Mendeley issues a new version and the `file_id` changes, resolve the new
identifier from the DOI page and update `DATASET_URL` in the script.

### Support data (`data/support_data/`)

All data acquisition — malaria notifications and the support
covariates below — lives in `0.download_data.R`, organized into
numbered sections. Each section is idempotent: it checks for its own
output file under `data/` and skips the fetch if already present, so
re-running the script after a partial run doesn't redo completed work.
Run the whole script to regenerate anything; there's no per-covariate
script anymore.

| File | Section | Source | Coverage |
|---|---|---|---|
| `populacao_df.csv` | 2 | IBGE SIDRA API (tables 6579 annual estimates, 793 2007 count, 200 Census 2010, 4709 Census 2022) — primary source, queried directly | 2003–2022, 807 municipalities |
| `deforestation_df.csv` | 4 | PRODES/TerraBrasilis annual rate for the Legal Amazon, national aggregate, 2-year-lagged (see below) | 2003–2022 |
| `precip_df.csv` | 5 | ERA5 monthly-averaged reanalysis, `total_precipitation`, Copernicus Climate Data Store — primary source, direct API pull | 2003–2022, per microregion centroid |
| `rhum_df.csv`, `temp_df.csv` | 6 | ERA5 monthly-averaged reanalysis (`2m_temperature`, `2m_dewpoint_temperature`), Copernicus Climate Data Store; relative humidity derived via Magnus-Tetens | 2003–2022, per microregion centroid |

**Vegetation index (NDVI) was tried and dropped.** MODIS/Terra NDVI
(MOD13Q1, ORNL DAAC) had its own section, fetching one microregion at
a time with retry/timeout/resumable caching, but ORNL DAAC was too
slow/unresponsive to reliably complete a pull, and testing surfaced a
libcurl-level timeout that bypassed the retry logic and crashed the
whole script. Not worth continuing to fight an unreliable external
service for a covariate whose case was only partial to begin with:
NDVI overlaps conceptually with deforestation (both proxy land-cover
change) — PRODES measures clear-cut directly, NDVI is a noisier
indirect greenness signal also driven by seasonality, degradation, and
agriculture, not just clearing. The section has been removed from
`0.download_data.R`; `ndvi_df.csv` is no longer produced or joined in
`1.data_wrangling.R`.

**Deforestation caveat:** Section 4 tries TerraBrasilis's WFS first
(`prodes-legal-amz:yearly_deforestation`, summed by year — the actual
primary source), but that server is flaky (frequent 502s), so any year
that fails after retries silently falls back to a secondary-sourced
value (Wikipedia's PRODES table, cross-checked against INPE
press-release text surfaced via web search) and logs a loud warning
naming which years fell back. Check that warning before trusting
`deforestation_df.csv$source` — a row marked `secondary_unverified`
should not go into the manuscript as-is; re-run section 4 later to try
upgrading it to `primary_wfs`.

**ERA5 (rainfall) requires a Copernicus CDS API key** in `~/.cdsapirc`:
```
url: https://cds.climate.copernicus.eu/api
key: <your key>
```
Register at https://cds.climate.copernicus.eu. Section 5 makes one
request for the whole Legal Amazon bounding box and time range (not
one per region — much cheaper), then extracts values locally at each
microregion's centroid.

Microregion centroids (`data/spatial_data/micro_centroids.csv`, built
in section 3, shared by sections 5 and 6) are computed with
`sf::st_centroid()` — the real area-weighted centroid, no
approximation — directly from `data/spatial_data/sph_files/microrreg.shp`,
a shapefile already versioned in this repo (560 Brazilian microregions,
confirmed all-valid geometry), filtered to `LEGAL_AMAZON_STATES` (the 9
Legal Amazon states, also used by section 4), cross-checked against
`municipios_codigos.csv`'s own microregion-per-state breakdown — both
give the identical 107 microregions. Filtering by state rather than by
which microregions appear in the malaria panel is deliberate: it means
section 3 depends on nothing but the shapefile and
`municipios_codigos.csv`, not on sections 1-2 having already run. This
sidesteps `geobr` entirely: IBGE's geoserver (which `geobr` calls) was
consistently unreachable while this was built, while other IBGE
endpoints were fine, so the issue looks specific to that one
geoserver backend rather than IBGE overall — worth retrying `geobr`
directly another day if a fresher malha territorial is ever needed,
but there's no dependency on it right now.

(`data/spatial_data/micro_map.csv` is a separate, older file, still
used elsewhere for its `code_micro` column, but its `geom` column is
**not valid WKT** — `write_csv()` mangled the `sf` geometry when
`2.spatial_data_wrangling.R` first produced that file, serializing it
as deparsed R list text instead. An earlier version of section 3
recovered coordinates from that mangled text and computed the
centroid by hand with the shoelace formula; cross-checked against the
shapefile approach above, the two agreed to within ~0.005 degrees
(~500m) on average, so that recovery wasn't wrong, just unnecessary
now that a clean source is available locally.)

### `data/` layout

- `data/main_data/` — *(gitignored)* produced by `0.download_data.R`.
- `data/output_data/` — *(gitignored)* produced by `1.data_wrangling.R`.
- `data/support_data/` — versioned: IBGE municipality codes, population,
  deforestation, precipitation, relative humidity, and temperature —
  see table above.
- `data/spatial_data/` — versioned: IBGE shapefiles, derived CSVs, and
  microregion centroids.

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
binomial**, and **Bell**. Earlier iterations of the project also fit
zero-inflated Poisson and zero-inflated negative binomial variants, but
they did not improve fit relative to their non-inflated counterparts and
were retired from the pipeline.

### Model selection

The four formulas × three families are evaluated with **DIC** and
**WAIC**. Per-fit summaries (DIC/WAIC/CPO and fixed-effect posteriors)
are written to `results/model_selection.csv` by the helper
`scripts/model_selection_io.R`, so cross-family comparisons can be made
from one consolidated table. The selected formula per family is then
used by the "best models" script (`4.best_models_*.R`) to generate the
test-period predictions consumed by the downstream analysis.

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
   3a. 1.bell_*.R / 2.nbinomial_*.R /     # per-family selection (DIC/WAIC),
       3.poisson_*.R                      # logged to results/model_selection.csv
   3b. 4.best_models_*.R                  # refits the best formula per family
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
Rscript scripts/3.microrregion_models/4.best_models_vivax.R
```

## Results

All numbers below are on a held-out window with rates in **cases per
100 000 inhabitants**. Per-fit DIC/WAIC/CPO across all four formulas and
both species are in `results/model_selection.csv`; per-species test-set
metrics are in `results/test_metrics_microrregion_{vivax,falciparum}.csv`
(written by `4.best_models_*` over `ano >= 2016`, then overwritten by
`5.1`/`5.2` with `ano == 2018` plus the corrected variant) and
`results/test_metrics_am_vivax.csv` (`ano >= 2016`).

### Best-fit formula per family (DIC)

| species    | Bell | Poisson | Negative binomial |
|------------|:----:|:-------:|:-----------------:|
| vivax      | m3   | m3      | m3                |
| falciparum | m3   | m2      | m4                |

`mX` indexes the four formulas in the order they appear in the
methodology (with/without climate covariates × with/without space–time
interaction). For *falciparum* the Poisson winner is `m2` (climate
covariates, no interaction); for *vivax*, every family settles on `m3`
(interaction, no covariates), which means the IID space–time term
absorbs most of the climate signal in this dataset.

### Per-species pick on the test set

#### *P. vivax* — micro-region (Bell, m3)

The Bell fit dominates the per-species comparison and is the model
consumed downstream:

| variant             | MBE   | NRMSE  | RAE   | RMSLE | RSE   | cor    |
|---------------------|------:|-------:|------:|------:|------:|-------:|
| bell                |  95.2 | 0.1028 | 0.609 |  1.33 | 0.99  | 0.779  |
| poisson             |  97.5 | 0.1043 | 0.622 |  1.42 | 1.02  | 0.779  |
| nbinomial           |  97.6 | 0.1044 | 0.623 |  1.43 | 1.03  | 0.780  |
| bell + correction   | **15.8** | **0.0652** | **0.417** | **1.07** | **0.40** | 0.779  |

The piecewise multiplicative correction (§5.1) collapses the mean bias
by ~6× and cuts the relative-squared-error by ~60% without sacrificing
correlation — `bell_preds × 7.88` for cells where the raw prediction
exceeds 2 per 100 000.

#### *P. falciparum* — micro-region (Poisson, m2)

`poisson_2` is the per-family DIC winner and is the model consumed
downstream:

| variant                | MBE   | NRMSE  | RAE   | RMSLE | RSE   | cor    |
|------------------------|------:|-------:|------:|------:|------:|-------:|
| bell                   |  9.86 | 0.0972 | 0.569 | 0.930 | 1.00  | 0.505  |
| poisson                | 10.03 | 0.0978 | 0.579 | 1.032 | 1.01  | 0.470  |
| nbinomial              | 10.27 | 0.0982 | 0.584 | 1.124 | 1.02  | 0.529  |
| poisson + correction   | 10.15 | 0.0975 | 0.581 | 1.169 | 1.01  | 0.406  |

The three families are within striking distance on the test set, and
the piecewise correction (three regimes: `<1`, `[1, 4]`, `>4`) does not
clearly help — relative-error metrics barely move and `cor` drops by
~0.06. The decision to keep `poisson_2` is anchored on DIC and on its
parsimony (climate covariates, no space–time interaction), not on a
clean test-set win.

#### *P. vivax* — Amazonas state (Bell, m3)

A finer-grain Amazonas-state sanity check (`scripts/3.state_models/vivax_AM.R`):

| variant | MBE   | NRMSE  | RAE   | RMSLE | RSE   | cor    |
|---------|------:|-------:|------:|------:|------:|-------:|
| bell    | 239.1 | 0.0989 | 0.753 |  1.82 | 1.12  | 0.524  |

Larger MBE and lower correlation than at the micro-region grain are
expected: aggregating to the larger spatial unit smooths the response,
whereas the state-level Amazonas model resolves municipality-level
spikes that the additive structure cannot fully explain.

### Artefacts

- `results/preds_microrregion_{vivax,falciparum}_df.csv` — per-cell
  predictions for the three families plus the corrected prediction.
- `results/preds_am_vivax_df.csv` — Bell predictions at the
  AM-municipality grain.
- `results/erros_*_2018.png` / `results/erros_*_2018_corrigido.png` —
  spatial maps of nominal error before/after the multiplicative
  correction.
- `results/erros_*_rsle.png` — RMSLE maps for the selected species
  predictor.

A longer commentary on covariate-significance shifts and the
intra-family DIC reranking is in `refactor_changes.md`.

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
