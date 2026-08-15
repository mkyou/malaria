library(readr)
library(dplyr)
library(splines)
library(INLA)

inla.setOption(num.threads = '2:1')
source('scripts/loss_functions.R')

dir.create(
  'results/family_comparison/models', recursive = TRUE, showWarnings = FALSE
)

N_POSTERIOR_SAMPLES <- 300
VIVAX <- 'P. vivax'
FALCIPARUM <- 'P. falciparum'
IDMES_HOLDOUT <- 217

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
    idArea2 = idArea, idAno2 = idAno, idArea3 = idArea, mes2 = mes
  )

COVARIATES <- c('defor_lag2', 'precip_mm', 'temp', 'rhum', 'n_tp_73', 'n_tp_81')
A_MONTH_CONSTR <- matrix(1, nrow = 1, ncol = length(unique(micro_v$idArea3)))

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
FOLDS_STANDARD <- build_folds(109, 216, horizon = 3, step = 3)

RETRY_LOG_PATH <- 'results/family_comparison/models/retry_log.csv'

log_retry <- function(familia, modelo, especie, test_start, motivo, detalhe) {
  row <- tibble(
    timestamp = as.character(Sys.time()), familia = familia, modelo = modelo,
    especie = especie, test_start = test_start, motivo = motivo, detalhe = detalhe
  )
  write_csv(row, RETRY_LOG_PATH, append = file.exists(RETRY_LOG_PATH))
}

run_fold <- function(
  df, fold, formula, family, label, especie, int_strategy = 'eb',
  previous_fit = NULL
) {
  d <- df |> filter(idMes <= fold$test_end)
  test_rows <- d$idMes >= fold$test_start & d$idMes <= fold$test_end
  test_idx <- which(test_rows)
  d$numCasos_fit <- ifelse(test_rows, NA, d$numCasos)
  formula_fold <- update.formula(formula, numCasos_fit ~ .)

  for (col in COVARIATES) {
    train_mean <- mean(d[[col]][!test_rows], na.rm = TRUE)
    train_sd <- sd(d[[col]][!test_rows], na.rm = TRUE)
    d[[paste0(col, '_z')]] <- if (is.na(train_sd) || train_sd == 0) {
      0
    } else {
      (d[[col]] - train_mean) / train_sd
    }
  }

  ns_defor_train <- ns(d$defor_lag2[!test_rows], df = 3)
  ns_defor_full <- predict(ns_defor_train, d$defor_lag2)
  for (j in seq_len(ncol(ns_defor_full))) {
    d[[sprintf('defor_lag2_ns%d', j)]] <- ns_defor_full[, j]
  }

  fit_and_score <- function(use_warm_start) {
    control_mode <- if (!use_warm_start || is.null(previous_fit)) {
      NULL
    } else {
      list(result = previous_fit, restart = TRUE)
    }
    t0 <- Sys.time()
    fit <- inla(
      formula = formula_fold, family = family, data = d,
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

    samples <- inla.posterior.sample(N_POSTERIOR_SAMPLES, fit, seed = 0L)
    predictor_pos <- match(
      paste0('Predictor:', test_idx), rownames(samples[[1]]$latent)
    )
    eta_samples <- vapply(
      samples, function(s) s$latent[predictor_pos, 1],
      numeric(length(test_idx))
    )
    mu_samples <- exp(eta_samples)

    sim <- if (family == 'nbinomial') {
      size_samples <- vapply(
        samples,
        function(s) s$hyperpar[['size for the nbinomial observations (1/overdispersion)']],
        numeric(1)
      )
      sim_raw <- vapply(
        seq_len(ncol(mu_samples)),
        function(j) rnbinom(nrow(mu_samples), size = size_samples[j], mu = mu_samples[, j]),
        numeric(nrow(mu_samples))
      )
      sim_raw / pop_test * 1e5
    } else {
      apply(mu_samples, 2, function(mu) rpois(length(mu), lambda = mu)) / pop_test * 1e5
    }
    ci_low <- apply(sim, 1, quantile, probs = 0.025)
    ci_high <- apply(sim, 1, quantile, probs = 0.975)

    metrics <- tibble(
      test_start = fold$test_start, test_end = fold$test_end,
      n_train = sum(!test_rows), n_test = sum(test_rows),
      dic = fit$dic$dic, waic = fit$waic$waic,
      mbe = mbe(real, pred), nrmse = nrmse(real, pred),
      rae = rae(real, pred), rmsle = rmsle(real, pred),
      rse = rse(real, pred), cor = cor(real, pred),
      coverage_95 = mean(real >= ci_low & real <= ci_high),
      largura_95 = mean(ci_high - ci_low),
      fit_time_sec = fit_time_sec
    )
    list(metrics = metrics, fit = fit)
  }

  is_degenerate <- function(out) {
    m <- out$metrics
    is.na(m$cor) || m$cor < 0.3 || m$rse > 5
  }

  out <- tryCatch(
    fit_and_score(use_warm_start = TRUE),
    error = function(e) {
      message(sprintf(
        '  [warn] warm-started fit failed (%s) -- retrying cold', conditionMessage(e)
      ))
      log_retry(family, label, especie, fold$test_start, 'crash', conditionMessage(e))
      fit_and_score(use_warm_start = FALSE)
    }
  )
  if (is_degenerate(out)) {
    message(sprintf(
      '  [warn] degenerate warm-started fit (rse=%.2f, cor=%.2f) -- retrying cold',
      out$metrics$rse, out$metrics$cor
    ))
    log_retry(
      family, label, especie, fold$test_start, 'degenerate',
      sprintf('rse=%.2f cor=%.2f', out$metrics$rse, out$metrics$cor)
    )
    out <- fit_and_score(use_warm_start = FALSE)
  }
  out
}

run_cv <- function(
  df, folds, especie, formula, family, label, out_path, int_strategy = 'eb'
) {
  done <- if (file.exists(out_path)) {
    read_csv(out_path, show_col_types = FALSE)
  } else {
    tibble()
  }
  done_starts <- if (nrow(done) > 0) {
    done$test_start[done$especie == especie]
  } else {
    numeric(0)
  }
  remaining <- Filter(function(f) !(f$test_start %in% done_starts), folds)
  if (length(remaining) == 0) {
    message(sprintf('[skip] %s (%s): already complete', label, especie))
    return(invisible(NULL))
  }
  message(sprintf(
    '%s (%s): %d/%d folds done, %d remaining',
    label, especie, length(folds) - length(remaining), length(folds),
    length(remaining)
  ))

  previous_fit <- NULL
  for (fold in remaining) {
    out <- run_fold(
      df, fold, formula, family, label, especie,
      int_strategy = int_strategy, previous_fit = previous_fit
    )
    row <- out$metrics |>
      mutate(especie = especie, familia = family, modelo = label, .before = 1)
    write_csv(row, out_path, append = file.exists(out_path))
    previous_fit <- out$fit
    message(sprintf(
      '  test_start=%d  rse=%.3f  cor=%.3f  coverage_95=%.3f',
      fold$test_start, row$rse, row$cor, row$coverage_95
    ))
  }
  invisible(NULL)
}

run_model <- function(
  label, formula, family, folds = FOLDS_STANDARD, int_strategy = 'eb',
  path_suffix = ''
) {
  out_path <- sprintf(
    'results/family_comparison/models/%s_%s%s.csv', family, label, path_suffix
  )
  formula_v <- if (is.list(formula)) formula$vivax else formula
  formula_f <- if (is.list(formula)) formula$falciparum else formula
  run_cv(micro_v, folds, VIVAX, formula_v, family, label, out_path, int_strategy)
  run_cv(micro_f, folds, FALCIPARUM, formula_f, family, label, out_path, int_strategy)
  read_csv(out_path, show_col_types = FALSE)
}


formula_backbone <- numCasos ~
  f(idArea, model = 'iid') +
  f(idAno, model = 'ar1') +
  f(mes, model = 'rw2', constr = TRUE, cyclic = TRUE) +
  f(idArea2, model = 'iid', group = idAno2,
    control.group = list(model = 'ar1')) +
  f(idArea3, model = 'iid', group = mes2,
    control.group = list(model = 'rw2', cyclic = TRUE, scale.model = FALSE),
    extraconstr = list(A = A_MONTH_CONSTR, e = 0)) +
  offset(log(populacao))


formula_model5 <- update(
  formula_backbone,
  . ~ . + defor_lag2_ns1 + defor_lag2_ns2 + defor_lag2_ns3 +
    precip_mm_z + temp_z + rhum_z
)
formula_model6 <- list(
  vivax = update(formula_model5, . ~ . + n_tp_81_z),
  falciparum = update(formula_model5, . ~ . + n_tp_73_z)
)

FAMILIES <- c('poisson', 'nbinomial')

resultados <- list()
for (fam in FAMILIES) {
  message(sprintf('\n=== %s: model5 (shared formula) ===', fam))
  resultados[[paste0(fam, '_model5')]] <- run_model(
    'model5', formula_model5, fam
  )
  print(resultados[[paste0(fam, '_model5')]], width = Inf, n = Inf)

  message(sprintf('\n=== %s: model6 (+ species-specific CNES) ===', fam))
  resultados[[paste0(fam, '_model6')]] <- run_model(
    'model6', formula_model6, fam
  )
  print(resultados[[paste0(fam, '_model6')]], width = Inf, n = Inf)
}


FOLDS_ANNUAL <- build_folds(109, 216, horizon = 12, step = 12)

for (fam in FAMILIES) {
  message(sprintf('\n=== %s: model5, annual regime ===', fam))
  resultados[[paste0(fam, '_model5_annual')]] <- run_model(
    'model5', formula_model5, fam, folds = FOLDS_ANNUAL, path_suffix = '_annual'
  )
  print(resultados[[paste0(fam, '_model5_annual')]], width = Inf, n = Inf)

  message(sprintf('\n=== %s: model6, annual regime ===', fam))
  resultados[[paste0(fam, '_model6_annual')]] <- run_model(
    'model6', formula_model6, fam, folds = FOLDS_ANNUAL, path_suffix = '_annual'
  )
  print(resultados[[paste0(fam, '_model6_annual')]], width = Inf, n = Inf)
}


BEST_MODELS_OUT <- 'results/best_models.csv'

if (file.exists(BEST_MODELS_OUT)) {
  message(sprintf('[skip] best_models: %s already exists', BEST_MODELS_OUT))
  best_models <- read_csv(BEST_MODELS_OUT, show_col_types = FALSE)
} else {
  summarise_cv <- function(res, familia, modelo) {
    res |>
      group_by(especie) |>
      summarise(
        dic = median(dic), mbe = median(mbe), nrmse = median(nrmse),
        rae = median(rae), rmsle = median(rmsle), rse = median(rse),
        cor = median(cor), coverage_95 = median(coverage_95),
        largura_95 = median(largura_95),
        fit_time_sec = median(fit_time_sec), .groups = 'drop'
      ) |>
      mutate(familia = familia, modelo = modelo, .before = 1)
  }

  pick_best <- function(res5, res6, familia) {
    s5 <- summarise_cv(res5, familia, 'model5')
    s6 <- summarise_cv(res6, familia, 'model6')
    bind_rows(s5, s6) |>
      group_by(especie) |>
      slice_min(rse, n = 1, with_ties = FALSE) |>
      ungroup()
  }

  bell_best <- read_csv(
    'results/model_iteration/models/model5_defor_ns.csv', show_col_types = FALSE
  ) |>
    summarise_cv('bell', 'model5')

  poisson_best <- pick_best(
    resultados$poisson_model5, resultados$poisson_model6, 'poisson'
  )
  nbinomial_best <- pick_best(
    resultados$nbinomial_model5, resultados$nbinomial_model6, 'nbinomial'
  )

  best_models <- bind_rows(bell_best, poisson_best, nbinomial_best) |>
    relocate(familia, especie, modelo)
  write_csv(best_models, BEST_MODELS_OUT)
}

message('\nBest model per distribution family:')
print(best_models, width = Inf, n = Inf)
