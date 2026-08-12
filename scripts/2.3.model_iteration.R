#-------------------------------------------------------------------------
# 2.3.model_iteration.R
#
# Refines the functional form of the Bell model. Family fixed to Bell --
# family comparison is scripts/3.microrregion_models/*.R's job.
#
# Expanding-window CV inside ano < 2018, plus a confirmation fold on
# 2018-2020 (matches 2.2.eda.R's GLM benchmark). ano 2021-2022 stays
# untouched (R1.3 circularity).
#
# Test rows get Y=NA in the same inla() call -- one fit per fold, since
# the rw1/rw2/bym2 fields change extent with the training cutoff.
#
# Uses inla.posterior.sample() for real posterior predictive intervals:
# summary.fitted.values' quantiles only cover mean-function uncertainty,
# not observation noise. Bell draws use a normal approximation at its
# exact mean/variance (Bell numbers overflow at these case counts).
#
# Scored with scripts/loss_functions.R on the rate scale (cases/100k).
#-------------------------------------------------------------------------

library(readr)
library(dplyr)
library(INLA)

inla.setOption(num.threads = '4:1')
source('scripts/loss_functions.R')

dir.create('results/model_iteration', recursive = TRUE, showWarnings = FALSE)

MICRO_PATH <- 'outputs/micro_map.graph'
N_POSTERIOR_SAMPLES <- 300
VIVAX <- 'P. vivax'
FALCIPARUM <- 'P. falciparum'

micro_v <- read_csv(
  'data/output_data/micro_reg_v_df.csv', show_col_types = FALSE
)
micro_f <- read_csv(
  'data/output_data/micro_reg_f_df.csv', show_col_types = FALSE
)

# Z-scored per fold in run_fold, not here -- avoids leaking the test
# period's covariate distribution into a scale used on training rows.
COVARIATES <- c('defor_lag2', 'precip_mm', 'temp', 'rhum')

FOLDS <- list(
  list(train_end = 2011, test_start = 2012, test_end = 2013),
  list(train_end = 2013, test_start = 2014, test_end = 2015),
  list(train_end = 2015, test_start = 2016, test_end = 2017),
  list(train_end = 2017, test_start = 2018, test_end = 2020)
)

NB_SIZE_KEY <- 'size for the nbinomial observations (1/overdispersion)'

# Vectorized Lambert W0 (Halley's method), positive branch only.
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

# One new observation per posterior draw of mu. `size` is nbinomial's
# dispersion (matches R's rnbinom(size=, mu=) directly).
simulate_obs <- function(mu, fam, size = NULL) {
  if (fam == 'poisson') {
    return(rpois(length(mu), lambda = mu))
  }
  if (fam == 'nbinomial') {
    return(rnbinom(length(mu), size = size, mu = mu))
  }
  theta <- lambert_w0(mu)
  variancia <- mu * (1 + theta)
  pmax(0, round(rnorm(length(mu), mean = mu, sd = sqrt(variancia))))
}

# Fits `formula` (family `fam`) on one fold, response NA'd out for the
# test years, and scores a genuine posterior predictive interval
# (simulated, not read off summary.fitted.values) on the rate scale.
run_fold <- function(df, fold, formula, fam) {
  d <- df |> filter(ano <= fold$test_end)
  test_rows <- d$ano >= fold$test_start & d$ano <= fold$test_end
  test_idx <- which(test_rows)
  d$numCasos_fit <- ifelse(test_rows, NA, d$numCasos)
  formula_fold <- update.formula(formula, numCasos_fit ~ .)

  # Train-only mean/sd, reused for test rows.
  for (col in COVARIATES) {
    train_mean <- mean(d[[col]][!test_rows], na.rm = TRUE)
    train_sd <- sd(d[[col]][!test_rows], na.rm = TRUE)
    d[[paste0(col, '_z')]] <- (d[[col]] - train_mean) / train_sd
  }

  fit <- inla(
    formula = formula_fold, family = fam, data = d,
    working.directory = tempdir(),
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, config = TRUE)
  )

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

  if (fam == 'nbinomial') {
    size_samples <- vapply(
      samples, function(s) s$hyperpar[[NB_SIZE_KEY]], numeric(1)
    )
    sim <- sapply(seq_len(ncol(mu_samples)), function(j) {
      simulate_obs(mu_samples[, j], fam, size = size_samples[j])
    }) / pop_test * 1e5
  } else {
    sim <- apply(mu_samples, 2, simulate_obs, fam = fam) / pop_test * 1e5
  }
  ci_low <- apply(sim, 1, quantile, probs = 0.025)
  ci_high <- apply(sim, 1, quantile, probs = 0.975)

  tibble(
    test_start = fold$test_start, test_end = fold$test_end,
    n_train = sum(!test_rows), n_test = sum(test_rows),
    dic = fit$dic$dic, waic = fit$waic$waic,
    mbe = mbe(real, pred), nrmse = nrmse(real, pred),
    rae = rae(real, pred), rmsle = rmsle(real, pred),
    rse = rse(real, pred), cor = cor(real, pred),
    # Should sit near 0.95 if honest.
    coverage_95 = mean(real >= ci_low & real <= ci_high),
    largura_95 = mean(ci_high - ci_low)
  )
}

run_cv <- function(df, especie, formula, fam, label) {
  bind_rows(
    lapply(FOLDS, run_fold, df = df, formula = formula, fam = fam)
  ) |>
    mutate(especie = especie, familia = fam, modelo = label, .before = 1)
}


# ===========================================================================
# ITERATION 0: intercept only -- the floor every later structure must beat
# ===========================================================================

formula_intercept <- numCasos ~ offset(log(populacao))

resultados_intercepto <- bind_rows(
  run_cv(micro_v, VIVAX, formula_intercept, 'bell', 'intercept'),
  run_cv(micro_f, FALCIPARUM, formula_intercept, 'bell', 'intercept')
)

message('Iteration 0 -- intercept only, Bell, cross-validated:')
print(resultados_intercepto, width = Inf, n = Inf)


# ===========================================================================
# ITERATION 1: spatial baseline, then one addition at a time -- Bell
#
# bym2's phi came out ~0.94-0.96 for both species (one-off check fit) --
# the CAR component dominates, the adjacency graph is doing real work.
#
# Each temporal term (year trend, seasonal month, residual month-index)
# and each of 2.2.eda.R's four covariates (z-scored, linear) is added
# to the spatial baseline one at a time, not tested alone -- without a
# per-area term, between-microregion variance swamps any temporal
# signal. PC priors only (vague vs. PC already shown negligible for
# Bell).
# ===========================================================================

PC_PREC <- list(prior = 'pc.prec', param = c(1, 0.01))

formula_spatial <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  offset(log(populacao))

formula_trend <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(ano, model = 'rw1', constr = TRUE, hyper = list(prec = PC_PREC)) +
  offset(log(populacao))

formula_seasonal <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(mes, model = 'rw2', constr = TRUE, cyclic = TRUE,
    hyper = list(prec = PC_PREC)) +
  offset(log(populacao))

formula_residual <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  offset(log(populacao))

formula_defor <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  defor_lag2_z + offset(log(populacao))

formula_precip <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  precip_mm_z + offset(log(populacao))

formula_temp <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  temp_z + offset(log(populacao))

formula_rhum <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  rhum_z + offset(log(populacao))

resultados_iter1 <- bind_rows(
  run_cv(micro_v, VIVAX, formula_spatial, 'bell', 'spatial'),
  run_cv(micro_v, VIVAX, formula_trend, 'bell', 'spatial+trend'),
  run_cv(micro_v, VIVAX, formula_seasonal, 'bell', 'spatial+seasonal'),
  run_cv(micro_v, VIVAX, formula_residual, 'bell', 'spatial+residual'),
  run_cv(micro_v, VIVAX, formula_defor, 'bell', 'spatial+defor'),
  run_cv(micro_v, VIVAX, formula_precip, 'bell', 'spatial+precip'),
  run_cv(micro_v, VIVAX, formula_temp, 'bell', 'spatial+temp'),
  run_cv(micro_v, VIVAX, formula_rhum, 'bell', 'spatial+rhum'),
  run_cv(micro_f, FALCIPARUM, formula_spatial, 'bell', 'spatial'),
  run_cv(micro_f, FALCIPARUM, formula_trend, 'bell', 'spatial+trend'),
  run_cv(
    micro_f, FALCIPARUM, formula_seasonal, 'bell', 'spatial+seasonal'
  ),
  run_cv(
    micro_f, FALCIPARUM, formula_residual, 'bell', 'spatial+residual'
  ),
  run_cv(micro_f, FALCIPARUM, formula_defor, 'bell', 'spatial+defor'),
  run_cv(micro_f, FALCIPARUM, formula_precip, 'bell', 'spatial+precip'),
  run_cv(micro_f, FALCIPARUM, formula_temp, 'bell', 'spatial+temp'),
  run_cv(micro_f, FALCIPARUM, formula_rhum, 'bell', 'spatial+rhum')
)

message(
  'Iteration 1 -- spatial baseline + one addition at a time, ',
  'Bell, cross-validated:'
)
print(resultados_iter1, width = Inf, n = Inf)


# ===========================================================================
# ITERATION 2: spatial+residual baseline, then one addition at a time
#
# residual (idMes, unconstrained rw1) was Iteration 1's strongest single
# addition. Re-testing trend/seasonal/covariates on top of it checks
# whether they still add anything once it's already in.
# ===========================================================================

formula_base2 <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  offset(log(populacao))

formula_base2_trend <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  f(ano, model = 'rw1', constr = TRUE, hyper = list(prec = PC_PREC)) +
  offset(log(populacao))

formula_base2_seasonal <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  f(mes, model = 'rw2', constr = TRUE, cyclic = TRUE,
    hyper = list(prec = PC_PREC)) +
  offset(log(populacao))

formula_base2_defor <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  defor_lag2_z + offset(log(populacao))

formula_base2_precip <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  precip_mm_z + offset(log(populacao))

formula_base2_temp <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  temp_z + offset(log(populacao))

formula_base2_rhum <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  rhum_z + offset(log(populacao))

BASE2_LABEL <- 'spatial+residual'

resultados_iter2 <- bind_rows(
  run_cv(micro_v, VIVAX, formula_base2, 'bell', BASE2_LABEL),
  run_cv(
    micro_v, VIVAX, formula_base2_trend, 'bell',
    paste0(BASE2_LABEL, '+trend')
  ),
  run_cv(
    micro_v, VIVAX, formula_base2_seasonal, 'bell',
    paste0(BASE2_LABEL, '+seasonal')
  ),
  run_cv(
    micro_v, VIVAX, formula_base2_defor, 'bell',
    paste0(BASE2_LABEL, '+defor')
  ),
  run_cv(
    micro_v, VIVAX, formula_base2_precip, 'bell',
    paste0(BASE2_LABEL, '+precip')
  ),
  run_cv(
    micro_v, VIVAX, formula_base2_temp, 'bell',
    paste0(BASE2_LABEL, '+temp')
  ),
  run_cv(
    micro_v, VIVAX, formula_base2_rhum, 'bell',
    paste0(BASE2_LABEL, '+rhum')
  ),
  run_cv(micro_f, FALCIPARUM, formula_base2, 'bell', BASE2_LABEL),
  run_cv(
    micro_f, FALCIPARUM, formula_base2_trend, 'bell',
    paste0(BASE2_LABEL, '+trend')
  ),
  run_cv(
    micro_f, FALCIPARUM, formula_base2_seasonal, 'bell',
    paste0(BASE2_LABEL, '+seasonal')
  ),
  run_cv(
    micro_f, FALCIPARUM, formula_base2_defor, 'bell',
    paste0(BASE2_LABEL, '+defor')
  ),
  run_cv(
    micro_f, FALCIPARUM, formula_base2_precip, 'bell',
    paste0(BASE2_LABEL, '+precip')
  ),
  run_cv(
    micro_f, FALCIPARUM, formula_base2_temp, 'bell',
    paste0(BASE2_LABEL, '+temp')
  ),
  run_cv(
    micro_f, FALCIPARUM, formula_base2_rhum, 'bell',
    paste0(BASE2_LABEL, '+rhum')
  )
)

message(
  'Iteration 2 -- spatial+residual baseline + one addition at a ',
  'time, Bell, cross-validated:'
)
print(resultados_iter2, width = Inf, n = Inf)


# ===========================================================================
# ITERATION 3: spatial+trend+residual baseline, then covariates
#
# trend barely moved Iteration 2's DIC on top of residual, but it cut
# the 2016-2017 fold's bias more than residual alone (mbe 36.8 vs.
# 48.0 for vivax) -- keeping both trades a small DIC cost for
# robustness on that harder fold. Covariates tested one at a time on
# top of this base.
# ===========================================================================

formula_base3 <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(ano, model = 'rw1', constr = TRUE, hyper = list(prec = PC_PREC)) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  offset(log(populacao))

formula_base3_defor <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(ano, model = 'rw1', constr = TRUE, hyper = list(prec = PC_PREC)) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  defor_lag2_z + offset(log(populacao))

formula_base3_precip <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(ano, model = 'rw1', constr = TRUE, hyper = list(prec = PC_PREC)) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  precip_mm_z + offset(log(populacao))

formula_base3_temp <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(ano, model = 'rw1', constr = TRUE, hyper = list(prec = PC_PREC)) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  temp_z + offset(log(populacao))

formula_base3_rhum <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH) +
  f(ano, model = 'rw1', constr = TRUE, hyper = list(prec = PC_PREC)) +
  f(idMes, model = 'rw1', hyper = list(prec = PC_PREC)) +
  rhum_z + offset(log(populacao))

BASE3_LABEL <- 'spatial+trend+residual'

resultados_iter3 <- bind_rows(
  run_cv(micro_v, VIVAX, formula_base3, 'bell', BASE3_LABEL),
  run_cv(
    micro_v, VIVAX, formula_base3_defor, 'bell',
    paste0(BASE3_LABEL, '+defor')
  ),
  run_cv(
    micro_v, VIVAX, formula_base3_precip, 'bell',
    paste0(BASE3_LABEL, '+precip')
  ),
  run_cv(
    micro_v, VIVAX, formula_base3_temp, 'bell',
    paste0(BASE3_LABEL, '+temp')
  ),
  run_cv(
    micro_v, VIVAX, formula_base3_rhum, 'bell',
    paste0(BASE3_LABEL, '+rhum')
  ),
  run_cv(micro_f, FALCIPARUM, formula_base3, 'bell', BASE3_LABEL),
  run_cv(
    micro_f, FALCIPARUM, formula_base3_defor, 'bell',
    paste0(BASE3_LABEL, '+defor')
  ),
  run_cv(
    micro_f, FALCIPARUM, formula_base3_precip, 'bell',
    paste0(BASE3_LABEL, '+precip')
  ),
  run_cv(
    micro_f, FALCIPARUM, formula_base3_temp, 'bell',
    paste0(BASE3_LABEL, '+temp')
  ),
  run_cv(
    micro_f, FALCIPARUM, formula_base3_rhum, 'bell',
    paste0(BASE3_LABEL, '+rhum')
  )
)

message(
  'Iteration 3 -- spatial+trend+residual baseline + covariates, ',
  'Bell, cross-validated:'
)
print(resultados_iter3, width = Inf, n = Inf)
