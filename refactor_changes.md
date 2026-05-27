# Refactor changes — `feat/mendeley-data-migration` vs `main`

Living document. Lists every change introduced on the branch relative to
`main`, grouped by topic, with impact on results where applicable.
Updated as new model runs complete.

Last update: 2026-05-19 (rev 4).

---

## 1. Overview

The branch consolidates three blocks of work:

1. **Data source migration** — bundled CSVs replaced by an on-demand
   download from the public Mendeley Data release.
2. **Rate-scale standardization and pipeline cleanup** — all modelling
   and error-analysis code now operates on cases per 100 000
   inhabitants; obsolete assets removed.
3. **INLA stability fixes** *(commit `7170683`)* — `nbinomial` models
   stopped fitting after the migration; a PC prior on
   `f(idInteraction, model = 'iid')` plus a set of memory/safety guards
   make them tractable again.
4. **Zero-inflated families retired** *(commit `7170683`)* — the four
   `zeroinflatedpoisson` / `zeroinflatednbinomial` scripts were removed
   from the pipeline, and `6.best_models_*` was renamed to
   `4.best_models_*` to fill the gap.
5. **Error-correction re-run and AM re-fit** *(commit `7170683`)* —
   `4.error_analysis.R`, `5.1.error_correction_vivax.R`,
   `5.2.error_correction_falciparum.R`, and
   `scripts/3.state_models/vivax_AM.R` re-run end-to-end against the
   new prediction CSVs; downstream metrics CSVs and PNG maps refreshed.

Commits on the branch relative to `main`:

- `44abe09` — refactor: standardize rate scale, migrate to Mendeley
  data source, and tidy pipeline.
- `7170683` — refactor: drop zero-inflated models, stabilize INLA fits,
  and refresh results (PC prior, memory guards,
  `scripts/model_selection_io.R`, `results/model_selection.csv`,
  rename `6.best_models_*` → `4.best_models_*`, re-run of error
  analysis / correction / AM scripts, regenerated test metrics and
  PNG maps).

---

## 2. Data source migration (commit `44abe09`)

### 2.1 New download script

- **Added** `scripts/0.download_data.R` (196 lines):
  - Pulls `Dataset.csv` from the Mendeley Data v2 release (Monteiro
    et al., 2023, DOI `10.17632/9n6b97fsbd.2`, CC BY 4.0).
  - Verifies an SHA-256 checksum of the downloaded file.
  - Classifies records by test code into *falciparum* / *vivax*
    (mixed cases are dropped).
  - Aggregates to the (municipality, month) grain expected by
    downstream wrangling scripts.
  - Fills a population gap at 2013 by per-municipality linear
    intercensal interpolation.

### 2.2 Files removed from the repository (now generated locally)

- `data/main_data/falciparum_df.csv`, `data/main_data/vivax_df.csv`
  (~155k rows each).
- All `data/output_data/*.csv` (per state and microrregion).
- `data/raw/*`.
- The matching paths are now in `.gitignore`.

### 2.3 New support data

- `data/support_data/populacao_df.csv` (12 513 rows) — yearly
  municipality population, consumed by the download/wrangling steps.

### 2.4 Discontinued assets

- `malaria_models/app.R`
- `scripts/6.shiny_data_wrangling.R`
- `shiny/preds_microrregion_vivax_df.csv`
- PDFs under `material/`

### 2.5 Rename

- `scripts/0.loss_functions.R` → `scripts/loss_functions.R`
  (matches `source('scripts/loss_functions.R')` already used elsewhere).

### 2.6 README

- Rewritten in English.
- Documents the new data source, the `data/` layout, and the
  methodology as it stands now.

### 2.7 Data-content effects

Same row count (20 545) and same columns, but counts are higher in
the Mendeley extract. Example: Porto Velho, Jan/2003, falciparum:
**1 355** cases (Mendeley) vs **902** (legacy `main_data`). The shift
matters for likelihood behaviour — see §5.2.

---

## 3. Rate-scale standardization (commit `44abe09`)

Some scripts were on **cases per 1 000** and others on **per 100 000**
inhabitants, which made error metrics and correction thresholds
non-comparable. The entire pipeline was unified to **per 100 000**:

- `scripts/3.microrregion_models/1.bell_vivax.R` (`real_rates_all`
  and the rate in the final tibble — the latter still had `/1000`
  after the first pass).
- `scripts/3.microrregion_models/4.best_models_{falciparum,vivax}.R`
  (renamed from `6.best_models_*` after the zero-inflated scripts
  were removed).
- `scripts/3.state_models/vivax_AM.R` (edited and re-run in commit
  `7170683`; refreshes `results/preds_am_vivax_df.csv` and
  `results/test_metrics_am_vivax.csv`).

### 3.1 Piecewise correction thresholds

- `scripts/5.1.error_correction_vivax.R`: multiplicative-correction
  threshold migrated from **0.005 (per 1 000)** to **2 (per 100 000)**,
  visually recalibrated against `bell_preds` vs real on the 2016–2017
  training window.
- `scripts/5.2.error_correction_falciparum.R`: three piecewise
  regimes migrated to the same per-100 000 scale: **<1, [1, 4], >4**,
  visually recalibrated.
- Hard-coded regression coefficients previously embedded in comments
  (`10.9515`, `11.2528`, `14.892`, `54.337`) were removed; the
  scripts re-fit `lm()` per run, so stale numbers in comments were
  only noise.

---

## 4. INLA pipeline cleanup (commit `44abe09`)

Re-fit with **INLA 25.10.19** after the migration:

- `1.bell_{vivax,falciparum}.R`
- `2.nbinomial_{vivax,falciparum}.R`
- `3.poisson_{vivax,falciparum}.R`
- `4.best_models_{vivax,falciparum}.R` (re-run, regenerating the
  prediction CSVs).

Downstream:

- `scripts/4.error_analysis.R` now consumes the new prediction CSVs,
  which carry `bell_preds` / `nbinomial_preds` / `poisson_preds`.
  Materialises a `preds` column from the per-species winner (bell
  for vivax, poisson for falciparum). A pre-existing bug was also
  fixed: a stray `+` chained `theme(...)` into
  `ggsave('results/erros_am_vivax.png')` and broke the run.

---

## 5. INLA stability fixes (commit `7170683`)

### 5.1 Symptom

After the migration, `nbinomial` models stopped fitting on the
heavier formulas (segfault), while `bell` and `poisson` ran normally:

```
vb.correction aborted ...
Failed to factorize Q ...
Error: The Newton-Raphson optimizer did not converge
Segmentation fault (core dumped)
```

### 5.2 Cause

Models 3 and 4 include `f(idInteraction, model = 'iid')`, which adds
one IID level **per observation** (107 micro × 192 months = 20 544
levels = one per row). In `bell` and `poisson` it is identifiable
(plays the role of observation-level overdispersion). In `nbinomial`
(`size` parameter), the IID competes with the family's own dispersion
parameter for the same role → posterior becomes multi-modal →
Newton-Raphson on the log-hyperparameter scale diverges → segfault.

**Why now?** The Mendeley counts are higher (e.g. 1 355 vs 902),
overdispersion is more pronounced, and the IID × `size` competition
intensified. With smaller counts on `main`, the model fit by chance.

### 5.3 Applied fix

Across `2.nbinomial_*` and `4.best_models_*` (only the
`nbinomial_fit4` call there):

1. **PC prior** on the `idInteraction` precision:
   `f(idInteraction, model = 'iid', hyper = list(prec =
   list(prior = 'pc.prec', param = c(1, 0.01))))`.
   Weakly informative (`P(σ > 1) = 0.01` on the log-linear predictor),
   enough to identify the effect without strongly constraining it.
2. `inla.mode = 'compact'` — much lower memory footprint.
3. `control.predictor = list(compute = FALSE, link = 1)` in the
   model-selection scripts — avoids storing marginals for the ~20k
   linear predictors. `4.best_models_*` keeps `compute = TRUE`
   because it needs `summary.fitted.values$mode`.
4. `control.compute` drops `return.marginals`,
   `return.marginals.predictor`, and `config` in the selection scripts.
5. `control.inla = list(strategy = 'adaptive',
   control.vb = list(emergency = 100))`.
6. `safe = TRUE` — INLA retries with perturbed initial values on
   non-fatal failures.
7. `tryCatch` around `inla()` so one bad model does not abort the
   whole loop (selection scripts only).
8. `num.threads = '2:1'` — lower peak memory.

> ⚠️ The PC prior is required only for `nbinomial`. `bell` and
> `poisson` keep INLA's default log-gamma prior. Methodological
> implication: models 3 and 4 of `nbinomial` only fit *because of*
> this prior. Cross-family comparability is mildly affected. A
> sensitivity sweep over `u ∈ {0.5, 3}` is recommended before the
> final report.

### 5.4 New artefacts

- `scripts/model_selection_io.R` — `save_model_selection_row()`
  persists DIC/WAIC/CPO and fixed effects per fit into
  `results/model_selection.csv`, indexed by (family, species,
  model_id). Replaces the loose annotations previously kept in
  script comments.
- `results/model_selection.csv` — consolidated cross-family table.
- `logs/` (gitignored) — every script run now writes `stdout+stderr`
  to `logs/<script>.log`.

### 5.5 `4.best_models_*` adjustments

To make these scripts runnable headlessly:

- Added `source('scripts/loss_functions.R')` (they relied on the
  loss helpers being pre-sourced in an interactive session).
- Removed `View()` calls (interactive only).
- Added `iid_hyper` and applied it to `f(idInteraction, ...)` inside
  `formula4` (the nbinomial path).
- Same `inla.mode='compact'`, `num.threads='2:1'`, `safe = TRUE`,
  and `control.vb = list(emergency = 100)` for the nbinomial fit.

After re-running model selection on the branch (§6.2), the DIC
winners per family changed for `poisson falciparum` (m3 → m2) and
`nbinomial vivax` (m4 → m3). `4.best_models_*` was updated to use
the new picks:

| script                          | bell | poisson | nbinomial |
|---------------------------------|:----:|:-------:|:---------:|
| `4.best_models_falciparum.R`    | fit3 | **fit2** *(was fit3)* | fit4 |
| `4.best_models_vivax.R`         | fit3 | fit3    | **fit3** *(was fit4)* |

Practical consequence:

- `poisson_fit2` (falciparum) keeps `rhum + temp` but drops
  `f(idInteraction, iid)` — predictions become smoother.
- `nbinomial_fit3` (vivax) keeps `f(idInteraction, iid)` but drops
  `rhum + temp` — relies on the IID instead of climate covariates.

---

## 6. Impact on results

### 6.1 Caveat on absolute DIC/WAIC comparability

Counts changed (see §2.7), so the absolute likelihood scale shifted.
Compare **rankings within a (family, species)** between `main` and
the branch, not absolute DIC numbers.

### 6.2 Best model per family (selection by DIC)

| family                 | species    | main best | main DIC | branch best | branch DIC | change |
|------------------------|------------|----------:|---------:|------------:|-----------:|:------:|
| bell                   | falciparum |        m3 |   72 995 |          m3 |     83 725 |   —    |
| bell                   | vivax      |        m4 |  111 414 |          m3 |    121 725 | **m4 → m3** (tied in both) |
| nbinomial              | falciparum |        m4 |   83 208 |          m4 |     95 715 |   —    |
| nbinomial              | vivax      |        m4 |  130 167 |          m3 |    109 125 | **m4 → m3** |
| poisson                | falciparum |        m3 |   65 064 |          m2 |     35 443 | **m3 → m2** |
| poisson                | vivax      |        m3 |   97 385 |          m3 |    106 114 |   —    |

Notes:

- `bell vivax`: m3 and m4 differ by ~5 DIC units in both versions —
  practically a tie. The "change" is cosmetic.
- `nbinomial vivax`: a real ranking flip. m3 is now clearly best
  (Δ DIC ≈ 534 vs m4). Likely because the PC prior on the IID
  reduces the marginal benefit of adding covariates on top of it.
- `poisson falciparum`: a real ranking flip. m2 (covariates only,
  no IID) is now best. With higher counts, the covariates carry
  more information; the `f(idInteraction, iid)` route is no longer
  competitive.
- `poisson vivax m1/m2` have negative DIC and `CPO = -Inf` — a
  Poisson-likelihood blow-up against the new count distribution
  (predicted rate → 0 against actual zeros). Same pathology was
  present on `main`. These rows are recorded but should not be
  used for selection.

### 6.3 Covariate significance (rhum, temp)

Significance is read from the 95 % credible interval of the fixed
effect (`SIG` = CI excludes 0; `border` = CI just touches 0).
Main values come from script-embedded comments; branch values come
from `results/model_selection.csv`.

| family    | species    | model | covariate | main CI                | main   | branch CI                 | branch |
|-----------|------------|------:|-----------|------------------------|:------:|---------------------------|:------:|
| bell      | falciparum |     2 | rhum      | (both sig — see notes) | SIG    | [0.00128, 0.00294]        | SIG    |
| bell      | falciparum |     2 | temp      | (both sig)             | SIG    | [-0.00481, 0.00764]       | **not sig** |
| bell      | falciparum |     4 | temp      | [-0.001, 0.015]        | border | [-0.00563, 0.00844]       | not sig |
| bell      | vivax      |     2 | rhum      | [0.003, 0.004]         | SIG    | [0.00255, 0.00369]        | SIG    |
| bell      | vivax      |     2 | temp      | [0.023, 0.030]         | SIG    | [0.01512, 0.02638]        | SIG    |
| bell      | vivax      |     4 | rhum      | [0.002, 0.006]         | SIG    | [0.00214, 0.00563]        | SIG    |
| bell      | vivax      |     4 | temp      | [0.001, 0.013]         | SIG    | [-0.00184, 0.01078]       | **not sig** |
| nbinomial | falciparum |     2 | rhum      | (both sig)             | SIG    | [0.00109, 0.00560]        | SIG    |
| nbinomial | falciparum |     2 | temp      | (both sig)             | SIG    | [-0.00469, 0.00948]       | **not sig** |
| nbinomial | falciparum |     4 | rhum      | [-0.001, 0.004]        | not sig| [-0.00012, 0.00459]       | border |
| nbinomial | falciparum |     4 | temp      | [-0.001, 0.014]        | not sig| [-0.00540, 0.00888]       | not sig |
| nbinomial | vivax      |     2 | rhum      | (both sig)             | SIG    | [0.00373, 0.00701]        | SIG    |
| nbinomial | vivax      |     2 | temp      | (both sig)             | SIG    | [-0.00214, 0.01029]       | **not sig** |
| poisson   | falciparum |     2 | rhum      | (both sig)             | SIG    | [0.00271, 0.00367]        | SIG    |
| poisson   | falciparum |     2 | temp      | (both sig)             | SIG    | [0.00077, 0.00991]        | border |
| poisson   | falciparum |     4 | rhum      | not sig (dropped)      | not sig| —                         | n/a    |
| poisson   | falciparum |     4 | temp      | —                      | —      | [-0.00591, 0.00861]       | not sig |
| poisson   | vivax      |     2 | rhum      | (both sig)             | SIG    | [0.00468, 0.00525]        | SIG    |
| poisson   | vivax      |     2 | temp      | (both sig)             | SIG    | [0.03731, 0.04347]        | SIG    |
| poisson   | vivax      |     4 | rhum      | (not stated)           | ?      | [0.00243, 0.00590]        | SIG    |
| poisson   | vivax      |     4 | temp      | (not stated)           | ?      | [-0.00188, 0.01092]       | **not sig** |

Pattern: **`rhum` keeps its significance almost everywhere; `temp`
loses significance in several models that previously had it**
(notably `bell falciparum 2`, `bell vivax 4`, `nbinomial vivax 2`).
The Mendeley counts are larger and more heterogeneous within
micro-regions — the temporal/spatial random effects absorb more of
the temperature signal, leaving the fixed effect inside its
zero-coverage credible interval.

### 6.4 Per-species choice from test metrics

Source: `results/test_metrics_microrregion_{falciparum,vivax}.csv` as
rewritten by `5.1.error_correction_vivax.R` and
`5.2.error_correction_falciparum.R`. Smaller is better for everything
except `cor`. **MBE scale changed**: it is now in cases per 100 000
(was per 1 000), so the headline number jumps ~100× by design.

> ⚠️ Window mismatch: `4.best_models_*` writes per-family metrics over
> the full test window (`ano >= 2016`); 5.1/5.2 then overwrite the same
> CSVs with metrics computed only on `ano == 2018`, plus the
> piecewise-corrected variant. The numbers below come from this second
> write — they correspond to the 2018 evaluation window. The previous
> revision of this document quoted the 2016–2018 numbers from
> `4.best_models_*`; the rankings are preserved, but the absolute
> magnitudes are different.

DIC picks differ between `main` and this branch:

| species    | main picks                       | branch picks                     |
|------------|----------------------------------|----------------------------------|
| falciparum | bell_3 · poisson_3 · nbinomial_4 | bell_3 · **poisson_2** · nbinomial_4 |
| vivax      | bell_3 · poisson_3 · nbinomial_4 | bell_3 · poisson_3 · **nbinomial_3** |

#### Falciparum (test window = 2018)

| variant              | mbe   | nrmse  | rae   | rmsle | rse   | cor   |
|----------------------|------:|-------:|------:|------:|------:|------:|
| bell_3               |  9.86 | 0.0972 | 0.569 | 0.930 | 1.001 | 0.505 |
| poisson_2            | 10.03 | 0.0978 | 0.579 | 1.032 | 1.014 | 0.470 |
| nbinomial_4          | 10.27 | 0.0982 | 0.584 | 1.124 | 1.021 | 0.529 |
| poisson_2 + correct  | 10.15 | 0.0975 | 0.581 | 1.169 | 1.007 | 0.406 |

**Decision: `poisson_2` is kept as the per-species pick for
falciparum.** It is the DIC winner (Δ DIC ≈ 30 000 vs `bell_3`) and is
parsimonious (climate covariates, no space–time interaction). The
three families are close on the test set — `bell_3` wins three metrics
by ≤0.01, `nbinomial_4` has the best `cor` by ~0.025 — but the
differences are too small to overturn DIC. The piecewise correction
(three regimes: `<1`, `[1, 4]`, `>4`) does not help: `rmsle` worsens,
`cor` drops by ~0.06, and the other metrics barely move; the regression
fit on the `[1, 4]` regime explains only R² ≈ 0.07 of the variance, so
the correction is essentially noise on this scale.

#### Vivax (test window = 2018)

| variant            | mbe    | nrmse  | rae   | rmsle | rse   | cor   |
|--------------------|-------:|-------:|------:|------:|------:|------:|
| bell_3             |  95.22 | 0.1028 | 0.609 | 1.326 | 0.995 | 0.779 |
| poisson_3          |  97.45 | 0.1043 | 0.622 | 1.423 | 1.024 | 0.779 |
| nbinomial_3        |  97.61 | 0.1044 | 0.623 | 1.430 | 1.026 | 0.780 |
| bell_3 + correct   | **15.82** | **0.0652** | **0.417** | **1.067** | **0.400** | 0.779 |

- `bell_3` wins five of six metrics among the raw fits; `nbinomial_3`
  edges `cor` by 0.001 — practically a tie.
- The piecewise correction is unambiguously beneficial:
  `bell_preds × 7.88` for cells where the raw prediction exceeds 2 per
  100 000 collapses MBE by ~6× and cuts RSE by ~60% without sacrificing
  `cor`. R² of the calibration fit is 0.64, well above the falciparum
  case.

#### Amazonas state (test window = 2016–2018)

`scripts/3.state_models/vivax_AM.R` was re-run in commit `7170683`
against the Mendeley data and refreshed
`results/test_metrics_am_vivax.csv`:

| variant | mbe   | nrmse  | rae   | rmsle | rse   | cor   |
|---------|------:|-------:|------:|------:|------:|------:|
| bell    | 239.1 | 0.0989 | 0.753 | 1.824 | 1.121 | 0.524 |

Larger MBE and lower `cor` than at the micro-region grain are expected:
the AM municipality model resolves spikes the additive structure
cannot fully explain.

### 6.5 Summary of result-level deltas

- **Vivax pick (`bell_3`):** unchanged. Still dominates among raw
  fits, and the piecewise correction is a clear, sizable win
  (MBE 95 → 16, RSE 0.99 → 0.40, NRMSE 0.103 → 0.065 at unchanged
  `cor`).
- **Falciparum pick (`poisson_2`):** confirmed (DIC + parsimony) even
  though the test-set comparison is closer than for vivax and the
  piecewise correction does not help.
- **Per-family DIC winners:** changed for `nbinomial vivax`
  (m4 → m3) and `poisson falciparum` (m3 → m2).
- **Covariate significance:** `rhum` stable; `temp` loses
  significance in several models.
- **Amazonas state model:** re-fit cleanly under the new pipeline;
  metrics consistent with expectations at the municipality grain.

---

## 7. Pending items

- Prior-sensitivity sweep for the `idInteraction` PC prior
  (`u ∈ {0.5, 3}`) on the affected families, to confirm intra-family
  DIC ranking is robust.
- Optional: reconsider the piecewise correction for falciparum (the
  current `<1, [1, 4], >4` split adds no measurable benefit on the 2018
  evaluation window). Either drop the correction or replace it with a
  better-fitting family of correctors.

---

## 8. Change log of this document

- **2026-05-19 (rev 4)** — Falciparum pick decided: `poisson_2`.
  §1 / §3 / §5 retagged from "uncommitted" to commit `7170683`; new §1
  bullet 5 covers the error-correction and AM re-runs. §6.4 rewritten
  around the post-5.1/5.2 test_metrics CSVs (2018-only window) with
  added rows for the corrected variants and a new Amazonas-state
  subsection. §6.5 rewritten with the firm falciparum decision; §7
  drops the "pick a ranking criterion" item and the commit item (both
  done), keeps the prior-sensitivity sweep, and adds an optional item
  to reconsider the piecewise falciparum correction.
- **2026-05-19 (rev 3)** — Zero-inflated families dropped from the
  pipeline. Removed scripts
  `4.zeroinflatedpoisson_{falciparum,vivax}.R` and
  `5.zeroinflatednbinomial_{falciparum,vivax}.R`. Renamed
  `6.best_models_*` → `4.best_models_*`. Document scrubbed of
  ZIP/ZINB references in §1, §4, §5.1–§5.3, §5.5, §6.2, §6.3 and §7.
- **2026-05-19 (rev 2)** — `6.best_models_*` re-run with the
  updated DIC picks (poisson m2 for falciparum, nbinomial m3 for
  vivax). §5.5, §6.4, §6.5 and §7 rewritten with the new
  `test_metrics_microrregion_*.csv` numbers. Falciparum conclusion
  flipped from "barely poisson_3" to "criterion-dependent across
  three different families".
- **2026-05-19** — initial version (English). Covers commit
  `44abe09`, the §5 stability fixes, and full §6 results for the
  six fitted families (`bell`, `nbinomial`, `poisson`, both species).
