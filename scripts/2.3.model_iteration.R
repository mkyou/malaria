library(readr)
library(dplyr)
library(splines)
library(INLA)

inla.setOption(num.threads = '2:1')
source('scripts/loss_functions.R')

dir.create(
  'results/model_iteration/models', recursive = TRUE, showWarnings = FALSE
)
dir.create(
  'results/model_iteration/residuals', recursive = TRUE, showWarnings = FALSE
)

MICRO_PATH <- 'outputs/micro_map.graph'
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

simulate_bell <- function(mu) {
  theta <- lambert_w0(mu)
  variancia <- mu * (1 + theta)
  pmax(0, round(rnorm(length(mu), mean = mu, sd = sqrt(variancia))))
}

RETRY_LOG_PATH <- 'results/model_iteration/models/retry_log.csv'

log_retry <- function(label, especie, test_start, motivo, detalhe) {
  row <- tibble(
    timestamp = as.character(Sys.time()), familia = 'bell', modelo = label,
    especie = especie, test_start = test_start, motivo = motivo,
    detalhe = detalhe
  )
  write_csv(row, RETRY_LOG_PATH, append = file.exists(RETRY_LOG_PATH))
}

run_fold <- function(
  df, fold, formula, label, especie, int_strategy = 'eb', previous_fit = NULL
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
      formula = formula_fold, family = 'bell', data = d,
      working.directory = tempdir(),
      control.predictor = list(compute = TRUE, link = 1),
      control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
      control.inla = list(int.strategy = int_strategy),
      control.mode = control_mode
    )
    fit_time_sec <- as.numeric(Sys.time() - t0, units = 'secs')

    dt <- d[test_rows, ]
    pop_test <- dt$populacao
    pred <- fit$summary.fitted.values$mode[test_rows] / pop_test * 1e5
    real <- dt$numCasos / pop_test * 1e5

    samples <- inla.posterior.sample(N_POSTERIOR_SAMPLES, fit, seed = 0L)
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
      coverage_95 = mean(real >= ci_low & real <= ci_high),
      largura_95 = mean(ci_high - ci_low),
      fit_time_sec = fit_time_sec
    )
    residuals <- tibble(
      codMicroRes = dt$codMicroRes, nomeMicroRes = dt$nomeMicroRes,
      siglaUF = dt$siglaUF, ano = dt$ano, mes = dt$mes, idMes = dt$idMes,
      populacao = pop_test, real_taxa = real, pred_taxa = pred,
      residual = real - pred, ci_low = ci_low, ci_high = ci_high,
      covered = real >= ci_low & real <= ci_high, test_start = fold$test_start
    )
    list(metrics = metrics, residuals = residuals, fit = fit)
  }

  is_degenerate <- function(out) {
    m <- out$metrics
    is.na(m$cor) || m$cor < 0.3 || m$rse > 5
  }

  out <- tryCatch(
    fit_and_score(use_warm_start = TRUE),
    error = function(e) {
      message(sprintf(
        '  [warn] warm-started fit failed (%s) -- retrying cold',
        conditionMessage(e)
      ))
      log_retry(label, especie, fold$test_start, 'crash', conditionMessage(e))
      fit_and_score(use_warm_start = FALSE)
    }
  )
  if (is_degenerate(out)) {
    message(sprintf(
      paste0(
        '  [warn] degenerate warm-started fit (rse=%.2f, cor=%.2f) ',
        '-- retrying cold'
      ),
      out$metrics$rse, out$metrics$cor
    ))
    log_retry(
      label, especie, fold$test_start, 'degenerate',
      sprintf('rse=%.2f cor=%.2f', out$metrics$rse, out$metrics$cor)
    )
    out <- fit_and_score(use_warm_start = FALSE)
  }
  out
}

run_cv <- function(
  df, folds, especie, formula, label, out_path, residuals_path = NULL,
  int_strategy = 'eb'
) {
  done <- if (file.exists(out_path)) {
    read_csv(out_path, show_col_types = FALSE)
  } else {
    tibble()
  }
  metrics_starts <- if (nrow(done) > 0) {
    done$test_start[done$especie == especie]
  } else {
    numeric(0)
  }
  if (is.null(residuals_path)) {
    done_starts <- metrics_starts
  } else {
    done_residuals <- if (file.exists(residuals_path)) {
      read_csv(residuals_path, show_col_types = FALSE)
    } else {
      tibble()
    }
    residuals_starts <- if (nrow(done_residuals) > 0) {
      unique(done_residuals$test_start[done_residuals$especie == especie])
    } else {
      numeric(0)
    }
    done_starts <- intersect(metrics_starts, residuals_starts)
    partial_starts <- setdiff(metrics_starts, residuals_starts)
    if (length(partial_starts) > 0 && nrow(done) > 0) {
      done |>
        filter(!(especie == !!especie & test_start %in% partial_starts)) |>
        write_csv(out_path)
    }
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
      df, fold, formula, label, especie,
      int_strategy = int_strategy, previous_fit = previous_fit
    )
    row <- out$metrics |>
      mutate(especie = especie, familia = 'bell', modelo = label, .before = 1)
    write_csv(row, out_path, append = file.exists(out_path))
    if (!is.null(residuals_path)) {
      residuals_rows <- out$residuals |>
        mutate(especie = especie, familia = 'bell', modelo = label, .before = 1)
      write_csv(
        residuals_rows, residuals_path, append = file.exists(residuals_path)
      )
    }
    previous_fit <- out$fit
    message(sprintf(
      '  test_start=%d  rse=%.3f  cor=%.3f  coverage_95=%.3f',
      fold$test_start, row$rse, row$cor, row$coverage_95
    ))
  }
  invisible(NULL)
}

FOLDS_STANDARD <- build_folds(109, 216, horizon = 3, step = 3)

run_model <- function(
  label, formula, folds = FOLDS_STANDARD, int_strategy = 'eb',
  save_residuals = FALSE
) {
  out_path <- sprintf('results/model_iteration/models/%s.csv', label)
  residuals_path <- if (save_residuals) {
    sprintf('results/model_iteration/residuals/%s.csv', label)
  } else {
    NULL
  }
  formula_v <- if (is.list(formula)) formula$vivax else formula
  formula_f <- if (is.list(formula)) formula$falciparum else formula
  run_cv(
    micro_v, folds, VIVAX, formula_v, label, out_path, residuals_path,
    int_strategy
  )
  run_cv(
    micro_f, folds, FALCIPARUM, formula_f, label, out_path, residuals_path,
    int_strategy
  )
  read_csv(out_path, show_col_types = FALSE)
}


formula_model0 <- numCasos ~ offset(log(populacao))
resultados_model0 <- run_model('model0_intercept', formula_model0)

message('Model 0 -- intercept only, Bell, standard folds:')
print(resultados_model0, width = Inf, n = Inf)


formula_model1 <- numCasos ~
  f(idArea, model = 'iid', group = idAno, control.group = list(model = 'ar1')) +
  offset(log(populacao))
resultados_model1 <- run_model(
  'model1_iid_ar1_ano', formula_model1
)

message('Model 1 -- iid space x ar1 time (grouped by year), standard folds:')
print(resultados_model1, width = Inf, n = Inf)

formula_model2 <- numCasos ~
  f(idArea, model = 'bym2', graph = MICRO_PATH, group = idAno,
    control.group = list(model = 'ar1')) +
  offset(log(populacao))
resultados_model2 <- run_model(
  'model2_bym2_ar1_ano', formula_model2
)

message('Model 2 -- bym2 space x ar1 time (grouped by year), standard folds:')
print(resultados_model2, width = Inf, n = Inf)


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
  'model3_separated_iid', formula_model3
)

message('Model 3 -- separated main effects + year/month interactions, iid:')
print(resultados_model3, width = Inf, n = Inf)


formula_model4 <- update(
  formula_model3, . ~ . + defor_lag2_z + precip_mm_z + temp_z + rhum_z
)
resultados_model4 <- run_model(
  'model4_covariates', formula_model4
)

message('Model 4 -- Model 3 + defor/precip/temp/rhum, flat fixed effects:')
print(resultados_model4, width = Inf, n = Inf)


formula_model5 <- update(
  formula_model3,
  . ~ . + defor_lag2_ns1 + defor_lag2_ns2 + defor_lag2_ns3 +
    precip_mm_z + temp_z + rhum_z
)
resultados_model5 <- run_model(
  'model5_defor_ns', formula_model5, save_residuals = TRUE
)

message('Model 5 -- Model 4, defor_lag2 as ns(df=3) instead of linear:')
print(resultados_model5, width = Inf, n = Inf)


FOLDS_ANNUAL <- build_folds(109, 216, horizon = 12, step = 12)
resultados_model5_annual <- run_model(
  'model5_defor_ns_annual', formula_model5, folds = FOLDS_ANNUAL,
  save_residuals = TRUE
)

message('Model 5, annual regime:')
print(resultados_model5_annual, width = Inf, n = Inf)


formula_model6_vivax <- update(formula_model5, . ~ . + n_tp_81_z)
formula_model6_falciparum <- update(formula_model5, . ~ . + n_tp_73_z)
formula_model6 <- list(
  vivax = formula_model6_vivax, falciparum = formula_model6_falciparum
)
resultados_model6 <- run_model(
  'model6_cnes', formula_model6, save_residuals = TRUE
)

message('Model 6 (final) -- Model 5 + species-specific CNES covariate:')
print(resultados_model6, width = Inf, n = Inf)


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
  'paper_best_replica', formula_paper_replica
)

message('Paper replica -- paper\'s own best formula, standard folds:')
print(resultados_paper_replica, width = Inf, n = Inf)


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
        dic = mean(dic), mbe = mean(mbe), nrmse = mean(nrmse),
        rae = mean(rae), rmsle = mean(rmsle), rse = mean(rse),
        cor = mean(cor), coverage_95 = mean(coverage_95),
        largura_95 = mean(largura_95),
        fit_time_sec = mean(fit_time_sec), .groups = 'drop'
      ) |>
      mutate(baseline = label, .before = 1)
  }

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
