#-------------------------------------------------------------------------
# 2.3.model_iteration.R
#
# CV harness for the Bell model's functional form. Family fixed to
# Bell (family comparison is scripts/3.*'s job).
#
# Each fold is a short rolling forecast: response NA'd out for the
# test window, trained on everything before it (idMes 1 to
# test_start - 1, always expanding, never a fixed rolling window) --
# approximates how INLA would actually be used, refit as new months
# of real data arrive. Posterior predictive intervals come from
# inla.posterior.sample() (summary.fitted.values' own quantiles only
# cover mean-function uncertainty, not observation noise).
#
# ano 2021-2022 is held out from every fold (see IDMES_HOLDOUT).
# Scored with scripts/loss_functions.R on the rate scale (cases/100k).
#-------------------------------------------------------------------------

library(readr)
library(dplyr)
library(splines)
library(INLA)

# 2:1 instead of the default 4:1 -- bym2 x ar1-grouped interaction
# terms OOM under full parallelism; halving threads + eb integration
# (see run_fold) fixes it.
inla.setOption(num.threads = '2:1')
source('scripts/loss_functions.R')

dir.create('results/model_iteration/models', recursive = TRUE, showWarnings = FALSE)

MICRO_PATH <- 'outputs/micro_map.graph'
N_POSTERIOR_SAMPLES <- 300
VIVAX <- 'P. vivax'
FALCIPARUM <- 'P. falciparum'

# idMes = 1 is 2003-01. 2021-01 is (2021-2003)*12 + 1 = 217 -- the
# final holdout starts there; no fold may touch idMes >= this.
IDMES_HOLDOUT <- 217

# idInteraction: paper's own Type I term, interaction(idArea, idMes)
# -- see formula_paper_vivax below. idArea2/idAno2/idArea3/mes2 are
# plain duplicates (INLA needs a distinct variable per f() call, even
# for the same index reused in a second random effect).
# n_tp_73/n_tp_81 (CNES) get their own 12-month lag here, at load
# time, before any fold split -- a lag only looks backward, so this
# can't leak future information, unlike z-scoring/splines (done
# per-fold, train-only, in run_fold below). Needed because CNES joins
# by (microregion, ano) alone: without the lag, a year's December
# snapshot would apply to that same year's Jan-Nov too, before it
# existed.
micro_v <- read_csv(
  'data/output_data/micro_reg_v_df.csv', show_col_types = FALSE
) |>
  arrange(codMicroRes, idMes) |>
  group_by(codMicroRes) |>
  mutate(
    n_tp_73 = dplyr::lag(n_tp_73, 12), n_tp_81 = dplyr::lag(n_tp_81, 12)
  ) |>
  ungroup() |>
  mutate(
    idAno = ano - min(ano) + 1L,
    idInteraction = as.numeric(interaction(idArea, idMes)),
    idArea2 = idArea, idAno2 = idAno, idArea3 = idArea, mes2 = mes
  )
micro_f <- read_csv(
  'data/output_data/micro_reg_f_df.csv', show_col_types = FALSE
) |>
  arrange(codMicroRes, idMes) |>
  group_by(codMicroRes) |>
  mutate(
    n_tp_73 = dplyr::lag(n_tp_73, 12), n_tp_81 = dplyr::lag(n_tp_81, 12)
  ) |>
  ungroup() |>
  mutate(
    idAno = ano - min(ano) + 1L,
    idInteraction = as.numeric(interaction(idArea, idMes)),
    idArea2 = idArea, idAno2 = idAno, idArea3 = idArea, mes2 = mes
  )

# z-scored per fold in run_fold (train-only), not here, to avoid
# leaking the test period's distribution into training. Only Models
# 4-6 use the resulting _z columns.
COVARIATES <- c('defor_lag2', 'precip_mm', 'temp', 'rhum', 'n_tp_73', 'n_tp_81')

# Sum-to-zero constraint for Model 3's month interaction: for each
# calendar month, the areas' deviations must sum to zero, or that
# term is confounded with the main month curve (mes, rw2 cyclic) and
# inla.posterior.sample() fails ("matrix is not positive definite").
# extraconstr on a grouped f() only supports constraining the
# ungrouped domain (107 areas), not the expanded area x month space --
# this is the direction INLA actually allows.
A_MONTH_CONSTR <- matrix(1, nrow = 1, ncol = length(unique(micro_v$idArea3)))

# One fold per horizon-sized block of test months, stepping forward
# by `step`, always training on everything before it.
build_folds <- function(test_start_min, test_end_max, horizon, step) {
  stopifnot(
    'test_end_max must not touch the final holdout' =
      test_end_max < IDMES_HOLDOUT
  )
  starts <- seq(test_start_min, test_end_max - horizon + 1, by = step)
  lapply(starts, function(s) {
    list(test_start = s, test_end = min(s + horizon - 1, test_end_max))
  })
}

# Vectorized Lambert W0 (Halley's method), positive branch only --
# needed to get Bell's variance (mean = theta*exp(theta)) from mu.
lambert_w0 <- function(x, tol = 1e-10, max_iter = 100) {
  w <- ifelse(x < 1, x, log(x))
  w[x <= 0] <- 0
  for (i in seq_len(max_iter)) {
    ew <- exp(w)
    wew <- w * ew
    denom <- ew * (w + 1) - (w + 2) * (wew - x) / (2 * w + 2)
    w_new <- w - (wew - x) / denom
    if (max(abs(w_new - w), na.rm = TRUE) < tol) return(w_new)
    w <- w_new
  }
  w
}

# One new Bell observation per posterior draw of mu, via a normal
# approximation at Bell's own exact mean/variance -- Bell(y)'s pmf
# needs the y-th Bell number, which overflows well before the case
# counts observed here.
simulate_bell <- function(mu) {
  theta <- lambert_w0(mu)
  variancia <- mu * (1 + theta)
  pmax(0, round(rnorm(length(mu), mean = mu, sd = sqrt(variancia))))
}

# Fits `formula` (Bell) on one fold, response NA'd out for the test
# window, and scores a genuine posterior predictive interval on the
# rate scale. previous_fit, if given, warm-starts the hyperparameter
# search from that fit's own mode (control.mode + restart=TRUE) --
# folds share the same formula, just more training rows each time, so
# the last fold's mode is a good starting guess for the next.
run_fold <- function(
  df, fold, formula, int_strategy = 'auto', previous_fit = NULL
) {
  d <- df |> filter(idMes <= fold$test_end)
  test_rows <- d$idMes >= fold$test_start & d$idMes <= fold$test_end
  test_idx <- which(test_rows)
  d$numCasos_fit <- ifelse(test_rows, NA, d$numCasos)
  formula_fold <- update.formula(formula, numCasos_fit ~ .)

  # Train-only mean/sd, reused for test rows. train_sd==0 (n_tp_81 in
  # the earliest folds, before that facility type existed in CNES)
  # would divide by zero and crash inla() -- zeroing the column out
  # for that fold is honest: there's no training-period variation for
  # the term to learn from yet.
  for (col in COVARIATES) {
    train_mean <- mean(d[[col]][!test_rows], na.rm = TRUE)
    train_sd <- sd(d[[col]][!test_rows], na.rm = TRUE)
    d[[paste0(col, '_z')]] <- if (is.na(train_sd) || train_sd == 0) {
      0
    } else {
      (d[[col]] - train_mean) / train_sd
    }
  }

  # Train-only spline basis, same reasoning as the z-scoring above --
  # fitting ns() on training rows only and predict()-ing it onto the
  # rest avoids leaking the test period's distribution into where the
  # curve can bend. Only Model 5+ use the resulting defor_lag2_ns*
  # columns.
  ns_defor_train <- ns(d$defor_lag2[!test_rows], df = 3)
  ns_defor_full <- predict(ns_defor_train, d$defor_lag2)
  for (j in seq_len(ncol(ns_defor_full))) {
    d[[sprintf('defor_lag2_ns%d', j)]] <- ns_defor_full[, j]
  }

  control_mode <- if (is.null(previous_fit)) {
    NULL
  } else {
    list(result = previous_fit, restart = TRUE)
  }

  t0 <- Sys.time()
  fit <- inla(
    formula = formula_fold, family = 'bell', data = d,
    working.directory = tempdir(),
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
    control.inla = list(int.strategy = int_strategy),
    control.mode = control_mode
  )
  fit_time_sec <- as.numeric(Sys.time() - t0, units = 'secs')

  pop_test <- d$populacao[test_rows]
  pred <- fit$summary.fitted.values$mode[test_rows] / pop_test * 1e5
  real <- d$numCasos[test_rows] / pop_test * 1e5

  samples <- inla.posterior.sample(N_POSTERIOR_SAMPLES, fit, seed = 1L)
  predictor_pos <- match(
    paste0('Predictor:', test_idx), rownames(samples[[1]]$latent)
  )
  eta_samples <- vapply(
    samples, function(s) s$latent[predictor_pos, 1],
    numeric(length(test_idx))
  )
  mu_samples <- exp(eta_samples)
  sim <- apply(mu_samples, 2, simulate_bell) / pop_test * 1e5
  ci_low <- apply(sim, 1, quantile, probs = 0.025)
  ci_high <- apply(sim, 1, quantile, probs = 0.975)

  metrics <- tibble(
    test_start = fold$test_start, test_end = fold$test_end,
    n_train = sum(!test_rows), n_test = sum(test_rows),
    dic = fit$dic$dic, waic = fit$waic$waic,
    mbe = mbe(real, pred), nrmse = nrmse(real, pred),
    rae = rae(real, pred), rmsle = rmsle(real, pred),
    rse = rse(real, pred), cor = cor(real, pred),
    # Should sit near 0.95 if honest.
    coverage_95 = mean(real >= ci_low & real <= ci_high),
    largura_95 = mean(ci_high - ci_low),
    fit_time_sec = fit_time_sec
  )
  list(metrics = metrics, fit = fit)
}

# Sequential, not lapply -- each fold's fit warm-starts the next.
run_cv <- function(df, folds, especie, formula, label, int_strategy = 'auto') {
  previous_fit <- NULL
  rows <- vector('list', length(folds))
  for (i in seq_along(folds)) {
    out <- run_fold(
      df, folds[[i]], formula,
      int_strategy = int_strategy, previous_fit = previous_fit
    )
    rows[[i]] <- out$metrics
    previous_fit <- out$fit
  }
  bind_rows(rows) |>
    mutate(especie = especie, familia = 'bell', modelo = label, .before = 1)
}

# 2012-01 (idMes 109) to 2020-12 (idMes 216), 3 months predicted per
# fold, refit every 12 months -- 9 rolling folds, comparable across
# every model in this script.
FOLDS_STANDARD <- build_folds(109, 216, horizon = 3, step = 12)

# Runs (or loads, if already run) one model's CV, cached at
# results/model_iteration/models/<label>.csv -- delete the file to
# force a re-run. `formula` is either one formula (both species) or
# list(vivax = ..., falciparum = ...) for species-specific structures.
run_model <- function(
  label, formula, folds = FOLDS_STANDARD, int_strategy = 'auto'
) {
  out_path <- sprintf('results/model_iteration/models/%s.csv', label)
  if (file.exists(out_path)) {
    message(sprintf('[skip] %s: %s already exists', label, out_path))
    return(read_csv(out_path, show_col_types = FALSE))
  }
  formula_v <- if (is.list(formula)) formula$vivax else formula
  formula_f <- if (is.list(formula)) formula$falciparum else formula
  resultados <- bind_rows(
    run_cv(micro_v, folds, VIVAX, formula_v, label, int_strategy),
    run_cv(micro_f, folds, FALCIPARUM, formula_f, label, int_strategy)
  )
  write_csv(resultados, out_path)
  resultados
}


# ===========================================================================
# MODEL 0: intercept only -- the floor every later structure must beat
# ===========================================================================

formula_model0 <- numCasos ~ offset(log(populacao))
resultados_model0 <- run_model('model0_intercept', formula_model0)

message('Model 0 -- intercept only, Bell, standard folds:')
print(resultados_model0, width = Inf, n = Inf)


# ===========================================================================
# MODELS 1-2: a single space x year Kronecker term (INLA's group/
# control.group), ar1 across years (not rw1 -- improper/rank-deficient,
# fails inla.posterior.sample() once grouped by area).
#   model1_iid_ar1_ano:  f(idArea, model='iid', group=idAno, ...)
#   model2_bym2_ar1_ano: same, with bym2 spatial smoothing per year
# int.strategy='eb' for both (bym2 needs it to avoid OOM).
# ===========================================================================

formula_model1 <- numCasos ~
  f(idArea, model = 'iid', group = idAno, control.group = list(model = 'ar1')) +
  offset(log(populacao))
resultados_model1 <- run_model(
  'model1_iid_ar1_ano', formula_model1, int_strategy = 'eb'
)

message('Model 1 -- iid space x ar1 time (grouped by year), standard folds:')
print(resultados_model1, width = Inf, n = Inf)

formula_model2 <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH, group = idAno,
    control.group = list(model = 'ar1')) +
  offset(log(populacao))
resultados_model2 <- run_model(
  'model2_bym2_ar1_ano', formula_model2, int_strategy = 'eb'
)

message('Model 2 -- bym2 space x ar1 time (grouped by year), standard folds:')
print(resultados_model2, width = Inf, n = Inf)


# ===========================================================================
# MODEL 3: separate main space (iid), main year-trend (ar1), main
# month-cycle (rw2 cyclic), plus two interactions -- area x ar1-year
# and area x rw2-cyclic-month (sum-to-zero constrained, see
# A_MONTH_CONSTR). iid, not bym2, throughout: every iid-vs-bym2
# ablation tried (main term alone, inside each interaction, combined)
# had iid tying or beating bym2 on every metric, far cheaper.
#
# Matches or beats models 1/2 on DIC/rse/cor while keeping the paper's
# own interpretable space/time/seasonality decomposition, without its
# cold-start interaction term.
# ===========================================================================

formula_model3 <- numCasos ~
  f(idArea, model = 'iid') +
  f(idAno, model = 'ar1') +
  f(mes, model = 'rw2', constr = TRUE, cyclic = TRUE) +
  f(idArea2, model = 'iid', group = idAno2,
    control.group = list(model = 'ar1')) +
  f(idArea3, model = 'iid', group = mes2,
    control.group = list(model = 'rw2', cyclic = TRUE, scale.model = FALSE),
    extraconstr = list(A = A_MONTH_CONSTR, e = 0)) +
  offset(log(populacao))
resultados_model3 <- run_model(
  'model3_separated_iid', formula_model3, int_strategy = 'eb'
)

message('Model 3 -- separated main effects + year/month interactions, iid:')
print(resultados_model3, width = Inf, n = Inf)


# ===========================================================================
# MODEL 4: Model 3 + defor_lag2/precip_mm/temp/rhum as flat fixed
# effects (one shared coefficient per covariate, all areas). Improves
# dic/rse for both species over Model 3.
#
# Each covariate was also tried as a random slope by area (per-area
# deviation from the shared slope): defor_lag2 showed no spatial
# heterogeneity (precision collapsed to the fixed effect). precip_mm/
# temp/rhum showed real heterogeneity and improved DIC, but hurt
# falciparum's rse on the full CV (its sparser case counts likely
# can't support 107 extra per-area slopes without overfitting) --
# not adopted; see results/model_iteration/models/rs_{precip_mm,temp,
# rhum}_{fixed,random}.csv for the standalone comparison.
# ===========================================================================

formula_model4 <- update(
  formula_model3, . ~ . + defor_lag2_z + precip_mm_z + temp_z + rhum_z
)
resultados_model4 <- run_model(
  'model4_covariates', formula_model4, int_strategy = 'eb'
)

message('Model 4 -- Model 3 + defor/precip/temp/rhum, flat fixed effects:')
print(resultados_model4, width = Inf, n = Inf)


# ===========================================================================
# MODEL 5: Model 4, but defor_lag2 as ns(defor_lag2, df=3) instead of
# linear (precip_mm/temp/rhum stay linear). df=3 (one interior knot)
# is minimal flexibility for a single bend -- excess flexibility has
# cost falciparum generalization elsewhere in this project (rejected
# random slopes above).
#
# By far the strongest single change tried: falciparum rse 0.319 ->
# 0.314, cor 0.915 -> 0.927; vivax rse 0.219 -> 0.177. DIC essentially
# unchanged for both (this is about shape, not overall fit).
# ===========================================================================

formula_model5 <- update(
  formula_model3,
  . ~ . + defor_lag2_ns1 + defor_lag2_ns2 + defor_lag2_ns3 +
    precip_mm_z + temp_z + rhum_z
)
resultados_model5 <- run_model(
  'model5_defor_ns', formula_model5, int_strategy = 'eb'
)

message('Model 5 -- Model 4, defor_lag2 as ns(df=3) instead of linear:')
print(resultados_model5, width = Inf, n = Inf)


# ===========================================================================
# MODEL 6 (final): Model 5 + one CNES facility-type covariate,
# different per species -- n_tp_73 (Unidade de Pronto Atendimento) for
# falciparum, n_tp_81 (Central de Regulacao, a referral/scheduling
# unit) for vivax. Both linear (ns() fails outright on these sparse,
# ~75% zero-heavy counts; a binary presence indicator loses real
# information vs. linear) -- chosen from a 3-candidate smell test
# (n_tp_32 dropped, not credible for either species).
#
# Does NOT robustly beat Model 5 on the full 9-fold CV (falciparum rse
# actually worsens slightly) -- kept alongside Model 5 as a second
# candidate rather than replacing it.
# ===========================================================================

formula_model6_vivax <- update(formula_model5, . ~ . + n_tp_81_z)
formula_model6_falciparum <- update(formula_model5, . ~ . + n_tp_73_z)
formula_model6 <- list(
  vivax = formula_model6_vivax, falciparum = formula_model6_falciparum
)
resultados_model6 <- run_model(
  'model6_cnes', formula_model6, int_strategy = 'eb'
)

message('Model 6 (final) -- Model 5 + species-specific CNES covariate:')
print(resultados_model6, width = Inf, n = Inf)


# ===========================================================================
# PAPER REPLICA: the paper's own best formula per species (see
# scripts/3.microrregion_models/4.best_models_{vivax,falciparum}.R),
# run through this script's rolling CV instead of the paper's original
# one-shot 2016+ blind forecast -- isolates how much of models 1-6's
# advantage is the new formula vs. just the fairer CV design. Family
# fixed to bell here even though falciparum's paper-reported best used
# poisson (the formula is what's under test).
#
# Genuinely different structure per species:
#   vivax:       bym2(idArea) + rw1(ano) + rw2(mes, cyclic) +
#     rw1(idMes) + iid(idInteraction), no covariates. idInteraction is
#     interaction(idArea, idMes) -- every test row's level is
#     necessarily unseen (a future month), so this term contributes
#     nothing for prediction: the "cold start" limitation flagged
#     early in this project's space-time exploration, now visible as
#     a real CV number.
#   falciparum:  same bym2/rw1/rw2/rw1 backbone, no interaction, plus
#     rhum + temp (raw scale, matching the original script).
# ===========================================================================

formula_paper_vivax <- numCasos ~
  f(mes, model = 'rw2', constr = TRUE, cyclic = TRUE) +
  f(ano, model = 'rw1', constr = TRUE) +
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1') +
  f(idInteraction, model = 'iid') +
  offset(log(populacao))

formula_paper_falciparum <- numCasos ~
  f(mes, model = 'rw2', constr = TRUE, cyclic = TRUE) +
  f(ano, model = 'rw1', constr = TRUE) +
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1') +
  rhum + temp +
  offset(log(populacao))

formula_paper_replica <- list(
  vivax = formula_paper_vivax, falciparum = formula_paper_falciparum
)

resultados_paper_replica <- run_model(
  'paper_best_replica', formula_paper_replica, int_strategy = 'eb'
)

message('Paper replica -- paper\'s own best formula, standard folds:')
print(resultados_paper_replica, width = Inf, n = Inf)


# ===========================================================================
# A tighter fold set (horizon=1, step=1, monthly refit) was tried for
# model0/1/2/3 and the paper replica -- a real walk-forward deployment
# simulation, parked as too heavy to run on every iteration. rse/cor
# improved sharply, but vivax's coverage_95 dropped well under nominal
# (model3: 97.8% -> 86.4%), unexplained so far. Raw results still on
# disk: results/model_iteration/models/{model0_intercept,
# model1_iid_ar1_ano,model2_bym2_ar1_ano,model3_separated_iid,
# paper_best_replica,glm_sem_covariaveis}_h1.csv.
# ===========================================================================


# ===========================================================================
# iteration_metrics.csv: every model against the same reference
# points, long format (one row per model/baseline x species), meant to
# keep growing as later models are added. Every row is scored on
# FOLDS_STANDARD.
#
# No row here for the paper's own bruto/corrected numbers (dist ==
# 'bell'/'poisson'/'new_bell'/'new_poi' in
# results/test_metrics_microrregion_*.csv) -- that used the paper's
# original single 2016+ blind window, a different CV design entirely.
# paper_best_replica is the fair comparison point instead: same
# formula, this script's own rolling CV.
# ===========================================================================

ITERATION_METRICS_OUT <- 'results/model_iteration/iteration_metrics.csv'

if (file.exists(ITERATION_METRICS_OUT)) {
  message(sprintf(
    '[skip] iteration_metrics: %s already exists', ITERATION_METRICS_OUT
  ))
  iteration_metrics <- read_csv(ITERATION_METRICS_OUT, show_col_types = FALSE)
} else {
  summarise_cv <- function(resultados, label) {
    resultados |>
      group_by(especie) |>
      summarise(
        dic = median(dic), mbe = median(mbe), nrmse = median(nrmse),
        rae = median(rae), rmsle = median(rmsle), rse = median(rse),
        cor = median(cor), coverage_95 = median(coverage_95),
        largura_95 = median(largura_95),
        fit_time_sec = median(fit_time_sec), .groups = 'drop'
      ) |>
      mutate(baseline = label, .before = 1)
  }

  # glm_sem_covariaveis: 2.2.eda.R's GLM (factor(codMicroRes) +
  # ns(ano,3) + factor(mes) + offset), refit per fold by a standalone
  # script (glm(), not inla()) -- written directly to
  # results/model_iteration/models/, same row shape as run_model()'s
  # output. dic is NA (not a Bayesian fit).
  glm_baseline <- summarise_cv(
    read_csv(
      'results/model_iteration/models/glm_sem_covariaveis.csv',
      show_col_types = FALSE
    ),
    'glm_sem_covariaveis'
  )

  model0_baseline <- summarise_cv(resultados_model0, 'model0_intercept')
  model1_baseline <- summarise_cv(resultados_model1, 'model1_iid_ar1_ano')
  model2_baseline <- summarise_cv(resultados_model2, 'model2_bym2_ar1_ano')
  model3_baseline <- summarise_cv(resultados_model3, 'model3_separated_iid')
  model4_baseline <- summarise_cv(resultados_model4, 'model4_covariates')
  model5_baseline <- summarise_cv(resultados_model5, 'model5_defor_ns')
  model6_baseline <- summarise_cv(resultados_model6, 'model6_cnes')
  paper_replica_baseline <- summarise_cv(
    resultados_paper_replica, 'paper_best_replica'
  )

  iteration_metrics <- bind_rows(
    glm_baseline, model0_baseline,
    model1_baseline, model2_baseline, model3_baseline, model4_baseline,
    model5_baseline, model6_baseline, paper_replica_baseline
  ) |>
    relocate(baseline, especie)
  write_csv(iteration_metrics, ITERATION_METRICS_OUT)
}

message('Iteration metrics, long format:')
print(iteration_metrics, width = Inf, n = Inf)
