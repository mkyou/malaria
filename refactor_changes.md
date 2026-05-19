# Refactor changes — `feat/mendeley-data-migration` vs `main`

Living document. Lists every change introduced on the branch relative to
`main`, grouped by topic, with impact on results where applicable.
Updated as new model runs complete.

Last update: 2026-05-19.

---

## 1. Overview

The branch consolidates three blocks of work:

1. **Data source migration** — bundled CSVs replaced by an on-demand
   download from the public Mendeley Data release.
2. **Rate-scale standardization and pipeline cleanup** — all modelling
   and error-analysis code now operates on cases per 100 000
   inhabitants; obsolete assets removed.
3. **INLA stability fixes** *(this session, uncommitted)* —
   `nbinomial` models stopped fitting after the migration; a PC prior
   on `f(idInteraction, model = 'iid')` plus a set of memory/safety
   guards make them tractable again.
4. **Zero-inflated families retired** *(this session, uncommitted)* —
   the four `zeroinflatedpoisson` / `zeroinflatednbinomial` scripts
   were removed from the pipeline, and `6.best_models_*` was renamed
   to `4.best_models_*` to fill the gap.

Commits on the branch relative to `main`:

- `44abe09` — refactor: standardize rate scale, migrate to Mendeley
  data source, and tidy pipeline.
- *(uncommitted)* — INLA stability fixes for `nbinomial`, helper
  `scripts/model_selection_io.R`, `results/model_selection.csv`,
  `4.best_models_*` adjustments, and removal of the zero-inflated
  scripts.

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
- `scripts/3.state_models/vivax_AM.R` (edited only; not re-run).

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

## 5. INLA stability fixes (this session, uncommitted)

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

Source: `results/test_metrics_microrregion_{falciparum,vivax}.csv`.
Each row is one family's DIC pick. Smaller is better for everything
except `cor`. **MBE scale changed**: it is now in cases per 100 000
(was per 1 000), so the headline number jumps ~100× by design.

DIC picks differ between versions:

| species    | main picks                       | branch picks                     |
|------------|----------------------------------|----------------------------------|
| falciparum | bell_3 · poisson_3 · nbinomial_4 | bell_3 · **poisson_2** · nbinomial_4 |
| vivax      | bell_3 · poisson_3 · nbinomial_4 | bell_3 · poisson_3 · **nbinomial_3** |

#### Falciparum

| family    | source | mbe   | nrmse  | rae    | rmsle | rse    | cor    |
|-----------|--------|------:|-------:|-------:|------:|-------:|-------:|
| bell      | main   | 0.068 | 0.0804 | 0.571  | 0.183 | 1.000  | 0.609  |
| bell      | branch | 8.77  | 0.0810 | 0.566  | 0.882 | 0.985  | 0.579  |
| poisson   | main   | 0.069 | 0.0809 | 0.579  | 0.186 | 1.013  | 0.614  |
| poisson   | branch | **5.30**  | **0.0756** | 0.605  | 1.025 | **0.858**  | 0.453  |
| nbinomial | main   | 0.071 | 0.0815 | 0.589  | 0.189 | 1.028  | 0.628  |
| nbinomial | branch | 8.24  | 0.0794 | **0.554**  | **0.874** | 0.945  | 0.539  |

The picture on the branch is now a three-way trade-off:

- `poisson_2` (no IID, with covariates) wins **MBE, NRMSE and RSE**
  — three error-magnitude / scale metrics — but `cor` collapses to
  **0.453** (worst of the three). Without `f(idInteraction, iid)`
  the predictions are smoother, so average bias is small but the
  per-cell ranking of predicted vs observed degrades.
- `nbinomial_4` wins **RAE and RMSLE**, is second on `NRMSE`, and
  keeps a moderate `cor` of 0.539.
- `bell_3` retains the best `cor` of the three (0.579).

Compared with `main`:

- On `main`, `bell` led 4 of 6 metrics and the historic pick was
  `poisson_3` (highest `cor` and tightest PIT among the unsaturated
  alternatives, per the comments in the legacy script).
- On the branch, **`bell_3` is now the family with the highest
  `cor`** — the very criterion that used to justify picking
  `poisson_3` now points to `bell`.
- **Falciparum pick is genuinely ambiguous and depends on the
  ranking criterion** — call out below.

#### Vivax

| family    | source | mbe   | nrmse  | rae    | rmsle | rse    | cor    |
|-----------|--------|------:|-------:|-------:|------:|-------:|-------:|
| bell      | main   | 0.665 | 0.1005 | 0.631  | 0.561 | 1.046  | 0.791  |
| bell      | branch | **85.47** | **0.1019** | **0.590**  | **1.182** | **0.953**  | 0.7754 |
| poisson   | main   | 0.671 | 0.1010 | 0.636  | 0.568 | 1.057  | 0.787  |
| poisson   | branch | 88.00 | 0.1035 | 0.605  | 1.269 | 0.983  | **0.7758** |
| nbinomial | main   | 0.713 | 0.1052 | 0.674  | 0.635 | 1.146  | 0.780  |
| nbinomial | branch | 88.05 | 0.1036 | 0.605  | 1.272 | 0.984  | 0.7751 |

- `bell_3` led every metric on `main` and still leads **five of six**
  on the branch (mbe, nrmse, rae, rmsle, rse). `poisson_3` edges
  `cor` by 0.0004 — practically a tie.
- Note that on the branch `poisson_3` and `nbinomial_3` now sit on
  essentially the same metrics row: both keep `f(idInteraction, iid)`
  without covariates, and the negative-binomial dispersion is
  already absorbed by the IID, so the two fits converge in test
  performance.
- **Conclusion for vivax holds: `bell_3` remains the per-species
  pick.**

### 6.5 Summary of result-level deltas

- **Vivax pick (`bell 3`):** unchanged. Still dominates 5 of 6
  metrics.
- **Falciparum pick (`poisson 3`):** **does not survive** — the
  branch's DIC winner is now `poisson_2`, which dominates the
  scale-magnitude metrics but tanks `cor` (0.453). On the
  `cor` criterion that used to justify the historic choice, `bell_3`
  now wins. On RAE/RMSLE, `nbinomial_4` wins. Pick depends on
  criterion:
  - **cor-based**           → `bell_3`
  - **scale-error-based**   → `poisson_2`
  - **relative/ratio-based**→ `nbinomial_4`
- **Per-family DIC winners:** changed for `nbinomial vivax`
  (m4 → m3) and `poisson falciparum` (m3 → m2).
- **Covariate significance:** `rhum` stable; `temp` loses
  significance in several models.

---

## 7. Pending items

- Prior-sensitivity sweep for the `idInteraction` PC prior
  (`u ∈ {0.5, 3}`) on the affected families, to confirm intra-family
  DIC ranking is robust.
- **Pick a ranking criterion for falciparum** (cor, scale-error,
  or relative-error) and decide between `bell_3`, `poisson_2`, and
  `nbinomial_4`. Then keep `4.best_models_falciparum.R` aligned
  with that choice — at the moment it computes all three but does
  not declare a winner.
- Commit the §5 changes (PC prior, memory guards, model_selection_io,
  best_models updates, removal of zero-inflated scripts) once the
  falciparum decision is made.

---

## 8. Change log of this document

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
