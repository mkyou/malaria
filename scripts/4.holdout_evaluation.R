library(readr)
library(dplyr)
library(splines)
library(INLA)

inla.setOption(num.threads = '2:1')
source('scripts/loss_functions.R')

dir.create('results/holdout/models', recursive = TRUE, showWarnings = FALSE)
dir.create('results/holdout/residuals', recursive = TRUE, showWarnings = FALSE)

N_POSTERIOR_SAMPLES <- 300
VIVAX <- 'P. vivax'
FALCIPARUM <- 'P. falciparum'

build_folds <- function(test_start_min, test_end_max, horizon, step) {
  starts <- seq(test_start_min, test_end_max - horizon + 1, by = step)
  lapply(starts, function(s) {
    list(test_start = s, test_end = min(s + horizon - 1, test_end_max))
  })
}
FOLDS_H3 <- build_folds(217, 240, horizon = 3, step = 3)
FOLDS_H12 <- build_folds(217, 240, horizon = 12, step = 12)
FOLDS_H24 <- build_folds(217, 240, horizon = 24, step = 24)

COVARIATES <- c('defor_lag2', 'precip_mm', 'temp', 'rhum', 'n_tp_73', 'n_tp_81')

load_species <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
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
}
micro_v <- load_species('data/output_data/micro_reg_v_df.csv')
micro_f <- load_species('data/output_data/micro_reg_f_df.csv')
A_MONTH_CONSTR <- matrix(1, nrow = 1, ncol = length(unique(micro_v$idArea3)))

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

RETRY_LOG_PATH <- 'results/holdout/models/retry_log.csv'
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

    dt <- d[test_rows, ]
    pop_test <- dt$populacao
    pred <- fit$summary.fitted.values$mode[test_rows] / pop_test * 1e5
    real <- dt$numCasos / pop_test * 1e5

    samples <- inla.posterior.sample(N_POSTERIOR_SAMPLES, fit, seed = 0L)
    predictor_pos <- match(
      paste0('Predictor:', test_idx), rownames(samples[[1]]$latent)
    )
    eta_samples <- vapply(
      samples, function(s) s$latent[predictor_pos, 1], numeric(length(test_idx))
    )
    mu_samples <- matrix(exp(eta_samples), nrow = length(test_idx))

    sim <- if (family == 'bell') {
      apply(mu_samples, 2, simulate_bell) / pop_test * 1e5
    } else if (family == 'poisson') {
      apply(mu_samples, 2, function(mu) rpois(length(mu), lambda = mu)) / pop_test * 1e5
    } else {
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
  df, folds, especie, formula, family, label, metrics_path, residuals_path,
  int_strategy = 'eb'
) {
  done <- if (file.exists(metrics_path)) {
    read_csv(metrics_path, show_col_types = FALSE)
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
    metrics_row <- out$metrics |>
      mutate(especie = especie, familia = family, modelo = label, .before = 1)
    residuals_rows <- out$residuals |>
      mutate(especie = especie, familia = family, modelo = label, .before = 1)
    write_csv(metrics_row, metrics_path, append = file.exists(metrics_path))
    write_csv(residuals_rows, residuals_path, append = file.exists(residuals_path))
    previous_fit <- out$fit
    message(sprintf(
      '  test_start=%d  rse=%.3f  cor=%.3f  coverage_95=%.3f',
      fold$test_start, metrics_row$rse, metrics_row$cor, metrics_row$coverage_95
    ))
  }
  invisible(NULL)
}

run_model <- function(label, family, folds, horizon_tag) {
  metrics_path <- sprintf('results/holdout/models/%s_%s_%s.csv', family, label, horizon_tag)
  residuals_path <- sprintf('results/holdout/residuals/%s_%s_%s.csv', family, label, horizon_tag)
  run_cv(micro_v, folds, VIVAX, formula_model5, family, label, metrics_path, residuals_path)
  run_cv(micro_f, folds, FALCIPARUM, formula_model5, family, label, metrics_path, residuals_path)
  invisible(read_csv(metrics_path, show_col_types = FALSE))
}

HORIZONS <- list(h3 = FOLDS_H3, h12 = FOLDS_H12, h24 = FOLDS_H24)
FAMILIES <- c('bell', 'poisson', 'nbinomial')

for (fam in FAMILIES) {
  for (htag in names(HORIZONS)) {
    message(sprintf('\n=== %s / model5 / %s ===', fam, htag))
    res <- run_model('model5', fam, HORIZONS[[htag]], htag)
    print(res, width = Inf, n = Inf)
  }
}

cat('\nDone.\n')
