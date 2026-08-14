#-------------------------------------------------------------------------
# 2.2.eda.R
#
# Covariate exploration: does deforestation/precip/temp/humidity carry
# signal? Reads 1.data_wrangling.R's panel, ano <= 2020 throughout.
#
# Two separate questions:
#   1. In-time: does a covariate explain case rate using its
#      same-period value, once microregion/time are controlled for?
#   2. Realistic-lag: same question, using only covariate values that
#      would actually be available at prediction time (ERA5 lag 1
#      month, deforestation lag 2 years -- see 0.download_data.R).
#
# Sections: 1) correlation (temporal + spatial), 1b) trend/seasonal/
# remainder decomposition, 2) in-time GLM, 3) realistic-lag predictive
# GLM (train < 2018 / test 2018-2020). Correlations and decompositions
# use rate (cases per 100k), not raw counts -- microregions vary too
# much in population for a raw-count correlation to mean anything
# comparable across them. CNES (health facilities) EDA lives in
# 2.1.eda.R, not here -- it grew out of that script's chronic-hotspot
# question, not this one's covariate-signal question.
#
# Sections 2-3 share a mean structure that mirrors the paper's Bell
# model: microregion fixed effect (stand-in for bym2), natural spline
# in ano (stand-in for rw1 trend, and able to extrapolate into unseen
# test years unlike factor(ano)), factor(mes) seasonality.
#
# No figures here -- tables only (results/eda/*.csv). The figures this
# file used to produce were cut; 2.1.eda.R still has the spatial/
# trend/seasonality/hotspot ones.
#-------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)

dir.create('results/eda', recursive = TRUE, showWarnings = FALSE)

micro_reg_v <- read_csv(
  'data/output_data/micro_reg_v_df.csv',
  show_col_types = FALSE
) |>
  mutate(especie = 'P. vivax')

micro_reg_f <- read_csv(
  'data/output_data/micro_reg_f_df.csv',
  show_col_types = FALSE
) |>
  mutate(especie = 'P. falciparum')

panel <- bind_rows(micro_reg_v, micro_reg_f) |>
  mutate(
    data = as.Date(sprintf('%d-%02d-01', ano, mes)),
    taxa = numCasos / populacao * 1e5
  ) |>
  filter(ano <= 2020)

rm(micro_reg_v, micro_reg_f)

COVARIATES <- c('defor_lag2', 'precip_mm', 'temp', 'rhum')


# ===========================================================================
# SECTION 1: Covariate vs. case rate -- deforestation lag comparison
#
# Same-year deforestation could never actually be used as a predictor
# (PRODES publishes with a lag) -- this is how much signal is given up
# by using the lag that's actually available.
# ===========================================================================

corr_defor_comparacao <- panel |>
  group_by(especie, codMicroRes) |>
  summarise(
    cor_atual = suppressWarnings(cor(
      taxa,
      defor_km2,
      use = 'complete.obs'
    )),
    cor_lag2 = suppressWarnings(cor(
      taxa,
      defor_lag2,
      use = 'complete.obs'
    )),
    .groups = 'drop'
  )

resumo_defor_lag <- corr_defor_comparacao |>
  group_by(especie) |>
  summarise(
    cor_atual_mediana = median(cor_atual, na.rm = TRUE),
    cor_lag2_mediana = median(cor_lag2, na.rm = TRUE),
    perda_mediana = median(cor_atual - cor_lag2, na.rm = TRUE),
    .groups = 'drop'
  )

message(
  'Correlation with deforestation, 
  same-year vs. 2-year-lagged (median across microregions):'
)
print(resumo_defor_lag)


# ===========================================================================
# SECTION 1b: Trend/seasonal/remainder decomposition
#
# Raw correlation conflates shared trend, shared seasonality, and
# genuine short-term covariation. stats::decompose() (additive,
# frequency=12) separates them, per microregion, summarized by the
# median -- an aggregate decomposition would hide microregion-level
# heterogeneity the same way an aggregate correlation would.
#
# defor_lag2's "seasonal" component is a decomposition artifact, not
# real signal: PRODES is annual (a step function, not smooth), and
# decompose()'s moving-average trend misreads the step's residual as
# seasonality. Excluded from the summary below for that reason.
# ===========================================================================

to_decomp <- function(x) {
  decompose(
    ts(x, start = c(2003, 1), frequency = 12),
    type = 'additive'
  )
}

decomp_vars <- c('P. vivax', 'P. falciparum', COVARIATES)

micro_wide <- panel |>
  select(codMicroRes, especie, data, taxa, all_of(COVARIATES)) |>
  pivot_wider(
    id_cols = c(codMicroRes, data, all_of(COVARIATES)),
    names_from = especie,
    values_from = taxa
  ) |>
  arrange(codMicroRes, data)

decompose_micro <- function(df) {
  decs <- lapply(df[decomp_vars], to_decomp)
  names(decs) <- decomp_vars
  expand.grid(
    especie = c('P. vivax', 'P. falciparum'),
    covariavel = COVARIATES,
    stringsAsFactors = FALSE
  ) |>
    rowwise() |>
    mutate(
      Raw = suppressWarnings(cor(
        df[[especie]],
        df[[covariavel]],
        use = 'complete.obs'
      )),
      Trend = suppressWarnings(cor(
        decs[[especie]]$trend,
        decs[[covariavel]]$trend,
        use = 'complete.obs'
      )),
      Seasonal = suppressWarnings(cor(
        decs[[especie]]$seasonal,
        decs[[covariavel]]$seasonal,
        use = 'complete.obs'
      )),
      Remainder = suppressWarnings(cor(
        decs[[especie]]$random,
        decs[[covariavel]]$random,
        use = 'complete.obs'
      ))
    ) |>
    ungroup()
}

decomp_por_micro <- micro_wide |>
  group_split(codMicroRes) |>
  lapply(function(df) {
    decompose_micro(df) |> mutate(codMicroRes = df$codMicroRes[1])
  }) |>
  bind_rows() |>
  mutate(Seasonal = ifelse(covariavel == 'defor_lag2', NA, Seasonal))

decomp_summary <- decomp_por_micro |>
  group_by(especie, covariavel) |>
  summarise(
    Raw = median(Raw, na.rm = TRUE),
    Trend = median(Trend, na.rm = TRUE),
    Seasonal = median(Seasonal, na.rm = TRUE),
    Remainder = median(Remainder, na.rm = TRUE),
    .groups = 'drop'
  )

message('Median (across microregions) component correlation with case count:')
print(decomp_summary)

decomp_summary |>
  write_csv('results/eda/covariates_decomposition_correlation.csv')

rm(
  corr_defor_comparacao,
  resumo_defor_lag,
  to_decomp,
  decomp_vars,
  micro_wide,
  decompose_micro,
  decomp_por_micro,
  decomp_summary
)


# ===========================================================================
# SECTION 2: In-time explanatory power (GLM)
#
# Same-period covariate value, fit on all of ano <= 2020 (no split --
# that's section 3). Z-scored so rate ratios mean "effect of a 1-SD
# change," comparable across covariates. Reports rate ratios (95% CI)
# and deviance explained vs. the same structure without covariates.
# ===========================================================================

library(splines)

panel_z <- panel |>
  mutate(across(all_of(COVARIATES), ~ as.numeric(scale(.x))))

fit_intime <- function(df) {
  base <- glm(
    numCasos ~ factor(codMicroRes) +
      ns(ano, df = 3) +
      factor(mes) +
      offset(log(populacao)),
    family = poisson,
    data = df
  )
  full <- update(base, . ~ . + defor_lag2 + precip_mm + temp + rhum)
  list(base = base, full = full)
}

fits_intime <- lapply(
  c('P. vivax', 'P. falciparum'),
  function(sp) fit_intime(panel_z |> filter(especie == sp))
)
names(fits_intime) <- c('P. vivax', 'P. falciparum')

rate_ratios <- bind_rows(lapply(names(fits_intime), function(sp) {
  full <- fits_intime[[sp]]$full
  coefs <- coef(summary(full))
  ci <- confint.default(full, parm = COVARIATES)
  tibble(
    especie = sp,
    covariavel = COVARIATES,
    rate_ratio = exp(coefs[COVARIATES, 'Estimate']),
    ci_low = exp(ci[, 1]),
    ci_high = exp(ci[, 2]),
    p_valor = coefs[COVARIATES, 'Pr(>|z|)']
  )
}))

message(
  'In-time rate ratios (per 1-SD increase in same-period covariate value):'
)
print(rate_ratios)

lrt_resultados <- bind_rows(lapply(names(fits_intime), function(sp) {
  a <- anova(fits_intime[[sp]]$base, fits_intime[[sp]]$full, test = 'Chisq')
  tibble(
    especie = sp,
    deviance_base = a$`Resid. Dev`[1],
    deviance_full = a$`Resid. Dev`[2],
    deviance_explicada_pct = 100 *
      (a$`Resid. Dev`[1] - a$`Resid. Dev`[2]) /
      a$`Resid. Dev`[1],
    p_valor = a$`Pr(>Chi)`[2]
  )
}))

message(
  'In-time: deviance explained by adding all 4 covariates (likelihood-ratio test):'
)
print(lrt_resultados)

rate_ratios |> write_csv('results/eda/covariates_intime_rate_ratios.csv')
lrt_resultados |> write_csv('results/eda/covariates_intime_deviance.csv')

rm(panel_z, fit_intime, fits_intime, rate_ratios, lrt_resultados)


# ===========================================================================
# SECTION 2b: In-time explanatory power -- CNES candidates
#
# All 45 CNES covariates in one joint model is unstable (severe
# multicollinearity between facility counts -- 2.1.eda.R's own attempt
# at a joint model needed dispersion-corrected SEs and still barely
# converged). Tested one at a time instead, against the same base
# structure as section 2, restricted to the 17 that cleared
# |cor_transversal| > 0.2 in 2.1.eda.R section 5's screening -- the
# question here is "does this ONE facility type explain anything on
# its own," not "which combination is best." ano >= 2005 (no CNES
# source before that).
# ===========================================================================

ESPECIES <- c('P. vivax', 'P. falciparum')
CNES_PATTERN <- '^n_(estabelecimentos|vinc_sus|atendamb|atendhos|urgemerg|tp_)'
CNES_SHORTLIST <- c(
  'n_tp_32', 'n_tp_15', 'n_tp_73', 'n_tp_20', 'n_tp_72', 'n_tp_68',
  'n_tp_36', 'n_tp_39', 'n_tp_05', 'n_tp_74', 'n_tp_02', 'n_tp_01',
  'n_tp_22', 'n_tp_81', 'n_tp_43', 'n_estabelecimentos', 'n_tp_75'
)

panel_cnes_z <- panel |>
  filter(ano >= 2005) |>
  mutate(across(all_of(CNES_SHORTLIST), ~ as.numeric(scale(.x))))

fit_base_only <- function(df) {
  glm(
    numCasos ~ factor(codMicroRes) +
      ns(ano, df = 3) +
      factor(mes) +
      offset(log(populacao)),
    family = poisson,
    data = df
  )
}

bases_cnes <- lapply(ESPECIES, function(sp) {
  fit_base_only(panel_cnes_z |> filter(especie == sp))
})
names(bases_cnes) <- ESPECIES

rate_ratios_cnes <- bind_rows(lapply(ESPECIES, function(sp) {
  df <- panel_cnes_z |> filter(especie == sp)
  base <- bases_cnes[[sp]]
  bind_rows(lapply(CNES_SHORTLIST, function(col) {
    full <- update(base, as.formula(paste('. ~ . +', col)), data = df)
    coefs <- coef(summary(full))
    ci <- confint.default(full, parm = col)
    a <- anova(base, full, test = 'Chisq')
    tibble(
      especie = sp,
      covariavel = col,
      rate_ratio = exp(coefs[col, 'Estimate']),
      ci_low = exp(ci[1, 1]),
      ci_high = exp(ci[1, 2]),
      p_valor = coefs[col, 'Pr(>|z|)'],
      deviance_explicada_pct = 100 *
        (a$`Resid. Dev`[1] - a$`Resid. Dev`[2]) / a$`Resid. Dev`[1]
    )
  }))
}))

message(
  'In-time rate ratios, CNES shortlist (per 1-SD increase, one ',
  'covariate at a time):'
)
print(rate_ratios_cnes, n = Inf)

rate_ratios_cnes |> write_csv('results/eda/cnes_intime_rate_ratios.csv')

rm(ESPECIES, panel_cnes_z, fit_base_only, bases_cnes, rate_ratios_cnes)


# ===========================================================================
# SECTION 3: Realistic-lag predictive power (GLM, train/test)
#
# Same structure as section 2, covariates re-lagged to values actually
# available at prediction time (ERA5 lag 1 month). Trained on ano < 2018,
# tested on 2018-2020. Scored with scripts/loss_functions.R, same rate
# scale as the paper's own reported metrics -- rough reference, not a
# strict head-to-head (the paper's test window isn't necessarily
# 2018-2020).
# ===========================================================================

source('scripts/loss_functions.R')

N_POSTERIOR_SAMPLES <- 300

# Frequentist analog of 2.3.model_iteration.R's inla.posterior.sample()
# use: samples the linear predictor from its asymptotic-normal sampling
# distribution (predict.glm's se.fit, i.e. parameter uncertainty), then
# simulates one new Poisson observation per draw (observation noise) --
# same two layers of uncertainty the INLA posterior predictive
# combines, just via the GLM's own asymptotics instead of a real
# posterior.
predictive_interval_poisson <- function(fit, newdata, pop) {
  pr <- predict(fit, newdata, type = 'link', se.fit = TRUE)
  eta_samples <- vapply(
    seq_len(N_POSTERIOR_SAMPLES),
    function(i) rnorm(length(pr$fit), pr$fit, pr$se.fit),
    numeric(length(pr$fit))
  )
  mu_samples <- exp(eta_samples)
  sim <- apply(mu_samples, 2, function(mu) rpois(length(mu), mu)) / pop * 1e5
  tibble(
    ci_low = apply(sim, 1, quantile, probs = 0.025),
    ci_high = apply(sim, 1, quantile, probs = 0.975)
  )
}

panel_lagged <- panel |>
  arrange(codMicroRes, idMes) |>
  group_by(codMicroRes) |>
  mutate(
    precip_mm = lag(precip_mm, 1),
    temp = lag(temp, 1),
    rhum = lag(rhum, 1)
  ) |>
  ungroup() |>
  filter(!is.na(precip_mm)) |>
  mutate(across(all_of(COVARIATES), ~ as.numeric(scale(.x))))

fit_predictive <- function(especie_alvo) {
  treino <- panel_lagged |> filter(especie == especie_alvo, ano < 2018)
  teste <- panel_lagged |>
    filter(especie == especie_alvo, ano >= 2018, ano <= 2020)

  base <- glm(
    numCasos ~ factor(codMicroRes) +
      ns(ano, df = 3) +
      factor(mes) +
      offset(log(populacao)),
    family = poisson,
    data = treino
  )
  full <- update(base, . ~ . + defor_lag2 + precip_mm + temp + rhum)

  real <- teste$numCasos / teste$populacao * 1e5
  pred_base <- predict(base, teste, type = 'response') / teste$populacao * 1e5
  pred_full <- predict(full, teste, type = 'response') / teste$populacao * 1e5

  ci_base <- predictive_interval_poisson(base, teste, teste$populacao)
  ci_full <- predictive_interval_poisson(full, teste, teste$populacao)

  tibble(
    especie = especie_alvo,
    modelo = c('sem_covariaveis', 'com_covariaveis'),
    mbe = c(mbe(real, pred_base), mbe(real, pred_full)),
    nrmse = c(nrmse(real, pred_base), nrmse(real, pred_full)),
    rae = c(rae(real, pred_base), rae(real, pred_full)),
    rmsle = c(rmsle(real, pred_base), rmsle(real, pred_full)),
    rse = c(rse(real, pred_base), rse(real, pred_full)),
    cor = c(cor(real, pred_base), cor(real, pred_full)),
    # Should sit near 0.95 if honest.
    coverage_95 = c(
      mean(real >= ci_base$ci_low & real <= ci_base$ci_high),
      mean(real >= ci_full$ci_low & real <= ci_full$ci_high)
    ),
    largura_95 = c(
      mean(ci_base$ci_high - ci_base$ci_low),
      mean(ci_full$ci_high - ci_full$ci_low)
    )
  )
}

resultados_preditivos <- bind_rows(
  fit_predictive('P. vivax'),
  fit_predictive('P. falciparum')
)

message(
  'Realistic-lag predictive test (rate scale, test = 2018-2020), 
  with vs. without covariates:'
)
print(resultados_preditivos)

message(
  'For reference, the paper\'s own test-set metrics (different test window):'
)
print(read_csv(
  'results/test_metrics_microrregion_vivax.csv',
  show_col_types = FALSE
))
print(read_csv(
  'results/test_metrics_microrregion_falciparum.csv',
  show_col_types = FALSE
))

resultados_preditivos |> write_csv('results/eda/covariates_predictive_test.csv')

rm(
  N_POSTERIOR_SAMPLES, predictive_interval_poisson, panel_lagged,
  fit_predictive, resultados_preditivos
)


# ===========================================================================
# SECTION 3b: Realistic-lag predictive power -- CNES candidates
#
# Same 17-covariate shortlist as section 2b, one at a time, same
# train < 2018 / test 2018-2020 split -- but CNES needs its own lag,
# distinct from ERA5's. 1.data_wrangling.R joins CNES by (microregion,
# ano) alone, so a given year's December snapshot is attached to that
# SAME year's January through November too -- eleven of those twelve
# months come before the snapshot exists. Lagged here by 12 months
# (one full year) on the monthly panel, so ano X uses ano X-1's
# snapshot throughout -- the same realism fix section 3 applies to
# ERA5/deforestation, just not caught for CNES until now.
# ===========================================================================

panel_cnes_lagged <- panel |>
  arrange(codMicroRes, idMes) |>
  group_by(codMicroRes) |>
  mutate(across(all_of(CNES_SHORTLIST), ~ dplyr::lag(.x, 12))) |>
  ungroup() |>
  filter(ano >= 2006) |>
  mutate(across(all_of(CNES_SHORTLIST), ~ as.numeric(scale(.x))))

fit_predictive_cnes <- function(especie_alvo) {
  treino <- panel_cnes_lagged |> filter(especie == especie_alvo, ano < 2018)
  teste <- panel_cnes_lagged |>
    filter(especie == especie_alvo, ano >= 2018, ano <= 2020)

  base <- glm(
    numCasos ~ factor(codMicroRes) +
      ns(ano, df = 3) +
      factor(mes) +
      offset(log(populacao)),
    family = poisson,
    data = treino
  )

  real <- teste$numCasos / teste$populacao * 1e5
  pred_base <- predict(base, teste, type = 'response') / teste$populacao * 1e5

  bind_rows(lapply(CNES_SHORTLIST, function(col) {
    full <- update(base, as.formula(paste('. ~ . +', col)), data = treino)
    pred_full <- predict(full, teste, type = 'response') / teste$populacao * 1e5
    tibble(
      especie = especie_alvo,
      covariavel = col,
      mbe_base = mbe(real, pred_base),
      mbe_full = mbe(real, pred_full),
      rse_base = rse(real, pred_base),
      rse_full = rse(real, pred_full),
      cor_base = cor(real, pred_base),
      cor_full = cor(real, pred_full)
    )
  }))
}

resultados_preditivos_cnes <- bind_rows(
  fit_predictive_cnes('P. vivax'),
  fit_predictive_cnes('P. falciparum')
)

message(
  'Realistic-lag predictive test, CNES shortlist (rate scale, ',
  'test = 2018-2020), one covariate at a time vs. base:'
)
print(resultados_preditivos_cnes, n = Inf)

resultados_preditivos_cnes |>
  write_csv('results/eda/cnes_predictive_test.csv')

rm(
  CNES_PATTERN, CNES_SHORTLIST, panel_cnes_lagged, fit_predictive_cnes,
  resultados_preditivos_cnes
)
