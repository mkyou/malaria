#-------------------------------------------------------------------------
# 2.2.eda.R
#
# Covariate exploration, for the Scientific Reports revision (see
# paper_response/REVIEW_RESPONSE_PLAN.md's "reframe decision": does
# bringing in deforestation/rainfall/temperature/humidity actually
# carry signal?). Reads 1.data_wrangling.R's microregion x month
# panel. Restricted to ano <= 2020 throughout -- 2018-2020 is this
# analysis's own test window (section 3), 2021-2022 is the untouched
# final holdout.
#
# Two questions, kept deliberately separate rather than conflated into
# one model:
#   1. In-time: does a covariate explain case rate once microregion
#      and time are controlled for, using its *same-period* value?
#      Answers "is there a real relationship here at all."
#   2. Realistic-lag / predictive: same question, using only covariate
#      values that would actually have been published in time for a
#      forecast -- ERA5 lag 1 month (its monthly mean has ~1 month
#      structural lag to compute, well inside Copernicus's documented
#      5-day-to-3-month latency), deforestation lag 2 years (PRODES's
#      consolidated-data lag is 6-18 months after the PRODES-year
#      ends, so lag 2y has comfortable margin; lag 1y would be
#      underwater for early-year predictions). Answers "could this
#      actually work as a predictor," not just "is it correlated."
#
# Sections:
#   1. Covariate vs. case rate: temporal and spatial correlation, plus
#      (1b) a trend/seasonal/remainder decomposition of the temporal
#      story, since raw correlation conflates the three
#   2. In-time explanatory power (GLM)
#   3. Realistic-lag predictive power (GLM, train < 2018 / test 2018-2020)
#
# Sections 2-3 share one mean structure, mirroring the paper's actual
# Bell spatio-temporal model rather than something EDA-only and
# disposable: microregion as a fixed effect (stand-in for
# f(idArea, model='bym2')), a natural spline in ano (stand-in for the
# smooth f(ano, model='rw1') year trend), and factor(mes) (calendar-
# month seasonality, stand-in for the cyclic f(mes, model='rw2')). Not
# factor(idMes)/factor(ano): section 3 predicts into test years the
# fit never saw, and dummy variables can't extrapolate to a level they
# were never fit on -- a natural spline can (linear beyond the
# boundary knots by construction). This is a plain Poisson GLM
# standing in for a diagnostic question ("is there something here"),
# not a preview of the real INLA fit -- there's no GLM equivalent of
# BYM2's spatial smoothing or the random-walk priors.
#
# Figures saved to results/eda/, same conventions as 2.1.eda.R: no
# baked-in titles (captions go in LaTeX), species double-encoded by
# shape/linetype *and* color (grayscale-safe, still reads well in
# color), Liberation Sans via ragg, theme_bw()-based. Choropleths here
# use a diverging scale (correlation runs -1 to 1), unlike 2.1's
# sequential rate scale.
#-------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(sf)
library(ragg)

LEGAL_AMAZON_STATES <- c('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')

ESPECIE_LINETYPES <- c('P. vivax' = 'solid', 'P. falciparum' = 'dashed')
ESPECIE_SHAPES <- c('P. vivax' = 1, 'P. falciparum' = 4)
ESPECIE_COLORS <- c('P. vivax' = '#0072B2', 'P. falciparum' = '#D55E00')
CORR_GRADIENT <- c('#2166ac', '#f7f7f7', '#b2182b')
BASE_THEME <- theme_bw(base_size = 13) +
  theme(text = element_text(family = 'Liberation Sans'))
MAP_THEME <- BASE_THEME +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

dir.create('results/eda', recursive = TRUE, showWarnings = FALSE)

micro_reg_v <- read_csv('data/output_data/micro_reg_v_df.csv', show_col_types = FALSE) |>
  mutate(especie = 'P. vivax')

micro_reg_f <- read_csv('data/output_data/micro_reg_f_df.csv', show_col_types = FALSE) |>
  mutate(especie = 'P. falciparum')

panel <- bind_rows(micro_reg_v, micro_reg_f) |>
  mutate(data = as.Date(sprintf('%d-%02d-01', ano, mes))) |>
  filter(ano <= 2020)

rm(micro_reg_v, micro_reg_f)

COVARIATES <- c('defor_lag2', 'precip_mm', 'temp', 'rhum')
COVARIATE_LABELS <- c(
  defor_lag2 = 'Deforestation (lag 2y)',
  precip_mm = 'Precipitation',
  temp = 'Temperature',
  rhum = 'Relative humidity'
)


# ===========================================================================
# SECTION 1: Covariate vs. case rate -- temporal and spatial correlation
#
# Temporal: region-wide monthly series, case rate (both species) and
# each covariate, z-scored so wildly different units (km2, mm, Kelvin,
# %) can share one panel and co-movement is visually readable.
#
# Spatial: per-microregion Pearson correlation between the covariate
# and case counts across that microregion's own time series (2003-2020),
# mapped -- shows where the relationship is strong/weak/reversed,
# which an aggregate correlation number can't. Deforestation is state-,
# not microregion-grain, so its map shows blocks of same-state
# microregions sharing one value, not genuine within-state texture.
#
# Purely descriptive: no controls for microregion/time here (that's
# what sections 2-3's GLMs are for). This is "do these move together
# at all," not "does the relationship survive controlling for
# confounders."
# ===========================================================================

serie_taxas <- panel |>
  group_by(especie, data) |>
  summarise(
    numCasos = sum(numCasos, na.rm = TRUE),
    populacao = sum(populacao, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(taxa = numCasos / populacao * 1e5) |>
  group_by(especie) |>
  mutate(valor_z = as.numeric(scale(taxa))) |>
  ungroup() |>
  transmute(data, serie = especie, valor_z)

serie_covar_regional <- panel |>
  distinct(codMicroRes, data, defor_lag2, precip_mm, temp, rhum) |>
  group_by(data) |>
  summarise(across(all_of(COVARIATES), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') |>
  pivot_longer(all_of(COVARIATES), names_to = 'covariavel', values_to = 'valor') |>
  group_by(covariavel) |>
  mutate(valor_z = as.numeric(scale(valor))) |>
  ungroup() |>
  transmute(data, covariavel, serie = 'Covariate', valor_z)

serie_combined <- bind_rows(lapply(COVARIATES, function(cv) {
  bind_rows(
    serie_taxas |> mutate(covariavel = cv),
    serie_covar_regional |> filter(covariavel == cv)
  )
})) |>
  mutate(covariavel = factor(COVARIATE_LABELS[covariavel], levels = COVARIATE_LABELS))

SERIE_LINETYPES <- c(ESPECIE_LINETYPES, 'Covariate' = 'dotted')
SERIE_COLORS <- c(ESPECIE_COLORS, 'Covariate' = 'black')

p <- serie_combined |>
  ggplot(aes(x = data, y = valor_z, linetype = serie, color = serie)) +
  geom_line() +
  scale_linetype_manual(values = SERIE_LINETYPES, name = NULL) +
  scale_color_manual(values = SERIE_COLORS, name = NULL) +
  facet_wrap(~covariavel, ncol = 2) +
  labs(x = NULL, y = 'Standardized value (z-score)') +
  BASE_THEME
ggsave('results/eda/covariates_series.png', p, width = 12, height = 8, device = agg_png)

micro_sf <- st_read('data/spatial_data/sph_files/microrreg.shp', quiet = TRUE) |>
  mutate(code_micro = as.numeric(CD_MICRO)) |>
  filter(SIGLA_UF %in% LEGAL_AMAZON_STATES) |>
  select(code_micro)

corr_espacial <- panel |>
  group_by(especie, codMicroRes) |>
  summarise(
    across(
      all_of(COVARIATES),
      ~ suppressWarnings(cor(numCasos, .x, use = 'complete.obs')),
      .names = 'cor_{.col}'
    ),
    .groups = 'drop'
  ) |>
  pivot_longer(starts_with('cor_'), names_prefix = 'cor_', names_to = 'covariavel', values_to = 'correlacao')

for (sp in unique(panel$especie)) {
  slug <- ifelse(sp == 'P. vivax', 'vivax', 'falciparum')

  p <- micro_sf |>
    inner_join(corr_espacial |> filter(especie == sp), by = c('code_micro' = 'codMicroRes')) |>
    mutate(covariavel = factor(COVARIATE_LABELS[covariavel], levels = COVARIATE_LABELS)) |>
    ggplot() +
    geom_sf(aes(fill = correlacao), color = 'black', linewidth = .1) +
    scale_fill_gradient2(
      low = CORR_GRADIENT[1], mid = CORR_GRADIENT[2], high = CORR_GRADIENT[3],
      midpoint = 0, limits = c(-1, 1), name = 'Correlation'
    ) +
    facet_wrap(~covariavel, ncol = 2) +
    MAP_THEME
  ggsave(
    sprintf('results/eda/map_%s_covariate_correlation.png', slug), p,
    width = 9, height = 8, device = agg_png
  )
}

# Same-year deforestation (defor_km2) could never actually be used as
# a predictor (PRODES publishes with a lag), so this isn't "which one
# to use" -- it's "how much signal is given up by using the lag that's
# actually available."
corr_defor_comparacao <- panel |>
  group_by(especie, codMicroRes) |>
  summarise(
    cor_atual = suppressWarnings(cor(numCasos, defor_km2, use = 'complete.obs')),
    cor_lag2 = suppressWarnings(cor(numCasos, defor_lag2, use = 'complete.obs')),
    .groups = 'drop'
  )

p <- corr_defor_comparacao |>
  ggplot(aes(x = cor_atual, y = cor_lag2, shape = especie, color = especie)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'grey50') +
  geom_point(alpha = .6) +
  scale_shape_manual(values = ESPECIE_SHAPES, name = NULL) +
  scale_color_manual(values = ESPECIE_COLORS, name = NULL) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) +
  labs(
    x = 'Correlation with same-year deforestation',
    y = 'Correlation with 2-year-lagged deforestation'
  ) +
  BASE_THEME
ggsave('results/eda/deforestation_lag_comparison.png', p, width = 7, height = 7, device = agg_png)

resumo_defor_lag <- corr_defor_comparacao |>
  group_by(especie) |>
  summarise(
    cor_atual_mediana = median(cor_atual, na.rm = TRUE),
    cor_lag2_mediana = median(cor_lag2, na.rm = TRUE),
    perda_mediana = median(cor_atual - cor_lag2, na.rm = TRUE),
    .groups = 'drop'
  )

message('Correlation with deforestation, same-year vs. 2-year-lagged (median across microregions):')
print(resumo_defor_lag)


# ===========================================================================
# SECTION 1b: Trend/seasonal/remainder decomposition
#
# Raw correlation (above) conflates three different sources of
# covariation: shared long-run trend, shared within-year seasonality,
# and genuine short-term (month-to-month) covariation. Classical
# additive decomposition (stats::decompose(), frequency=12) separates
# them, per microregion (its own 216-month series), summarized by the
# median across microregions -- an aggregate-level decomposition would
# hide microregion-level heterogeneity the same way an aggregate
# correlation number would.
#
# defor_lag2's "seasonal" component is a decomposition ARTIFACT, not
# real signal: PRODES data is annual, so defor_lag2 is a step function
# (constant for 12 months, then jumps every January) rather than a
# smoothly-varying series. decompose()'s moving-average trend doesn't
# track a step perfectly, and the leftover systematic residual near
# each January boundary gets misread as "seasonality" purely because
# it recurs at the same calendar position every year -- confirmed by
# its seasonal variance being far larger than the other three
# covariates', despite deforestation being the one series that
# structurally cannot have real within-year seasonality. Excluded from
# the summary below for that reason.
# ===========================================================================

to_decomp <- function(x) decompose(ts(x, start = c(2003, 1), frequency = 12), type = 'additive')

decomp_vars <- c('P. vivax', 'P. falciparum', COVARIATES)

micro_wide <- panel |>
  select(codMicroRes, especie, data, numCasos, all_of(COVARIATES)) |>
  pivot_wider(
    id_cols = c(codMicroRes, data, all_of(COVARIATES)),
    names_from = especie, values_from = numCasos
  ) |>
  arrange(codMicroRes, data)

decompose_micro <- function(df) {
  decs <- lapply(df[decomp_vars], to_decomp)
  names(decs) <- decomp_vars
  expand.grid(especie = c('P. vivax', 'P. falciparum'), covariavel = COVARIATES, stringsAsFactors = FALSE) |>
    rowwise() |>
    mutate(
      Raw = suppressWarnings(cor(df[[especie]], df[[covariavel]], use = 'complete.obs')),
      Trend = suppressWarnings(cor(decs[[especie]]$trend, decs[[covariavel]]$trend, use = 'complete.obs')),
      Seasonal = suppressWarnings(cor(decs[[especie]]$seasonal, decs[[covariavel]]$seasonal, use = 'complete.obs')),
      Remainder = suppressWarnings(cor(decs[[especie]]$random, decs[[covariavel]]$random, use = 'complete.obs'))
    ) |>
    ungroup()
}

decomp_por_micro <- micro_wide |>
  group_split(codMicroRes) |>
  lapply(function(df) decompose_micro(df) |> mutate(codMicroRes = df$codMicroRes[1])) |>
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

decomp_summary |> write_csv('results/eda/covariates_decomposition_correlation.csv')

# Illustration: relative humidity (the covariate with the sharpest gap
# between raw and seasonal correlation) decomposed alongside both
# species' case rates, so "trend/seasonal/remainder" has a concrete
# picture to point to. Region-wide aggregate series, since one clean
# picture teaches the concept better than 107 -- the median table
# above is the evidence, this is illustration.
taxas_wide <- panel |>
  group_by(especie, data) |>
  summarise(
    numCasos = sum(numCasos, na.rm = TRUE),
    populacao = sum(populacao, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(taxa = numCasos / populacao * 1e5) |>
  select(data, especie, taxa) |>
  pivot_wider(names_from = especie, values_from = taxa)

covar_regional_wide <- panel |>
  distinct(codMicroRes, data, defor_lag2, precip_mm, temp, rhum) |>
  group_by(data) |>
  summarise(across(all_of(COVARIATES), ~ mean(.x, na.rm = TRUE)), .groups = 'drop')

full_series <- taxas_wide |> left_join(covar_regional_wide, by = 'data') |> arrange(data)

decomps <- lapply(full_series[decomp_vars], to_decomp)
names(decomps) <- decomp_vars

componentes_long <- function(var_name, label) {
  d <- decomps[[var_name]]
  tibble(
    data = full_series$data,
    Observed = as.numeric(d$x),
    Trend = as.numeric(d$trend),
    Seasonal = as.numeric(d$seasonal),
    Remainder = as.numeric(d$random)
  ) |>
    pivot_longer(-data, names_to = 'component', values_to = 'valor') |>
    group_by(component) |>
    mutate(valor_z = as.numeric(scale(valor))) |>
    ungroup() |>
    mutate(serie = label)
}

ilustracao <- bind_rows(
  componentes_long('P. vivax', 'P. vivax'),
  componentes_long('P. falciparum', 'P. falciparum'),
  componentes_long('rhum', 'Covariate')
) |>
  mutate(component = factor(component, levels = c('Observed', 'Trend', 'Seasonal', 'Remainder')))

p <- ilustracao |>
  ggplot(aes(x = data, y = valor_z, linetype = serie, color = serie)) +
  geom_line() +
  scale_linetype_manual(values = SERIE_LINETYPES, name = NULL) +
  scale_color_manual(values = SERIE_COLORS, name = NULL) +
  facet_wrap(~component, ncol = 1, scales = 'free_y') +
  labs(x = NULL, y = 'Standardized value (z-score)') +
  BASE_THEME
ggsave(
  'results/eda/covariates_decomposition_example_rhum.png', p,
  width = 12, height = 10, device = agg_png
)

rm(p, sp, slug, serie_taxas, serie_covar_regional, serie_combined, SERIE_LINETYPES,
   SERIE_COLORS, corr_espacial, corr_defor_comparacao, resumo_defor_lag,
   taxas_wide, covar_regional_wide, full_series, to_decomp, decomp_vars, decomps,
   micro_wide, decompose_micro, decomp_por_micro, decomp_summary,
   componentes_long, ilustracao)


# ===========================================================================
# SECTION 2: In-time explanatory power (GLM)
#
# Does each covariate explain case counts, using its *same-period*
# value, once microregion and time are controlled for (mean structure
# in the file header)? Fit on all of ano <= 2020 -- no train/test
# split here, that's section 3, which specifically needs an
# out-of-sample test. Covariates are z-scored before fitting so rate
# ratios mean "effect of a one-SD change" for all four, comparable
# despite being on wildly different scales (km2, mm, Kelvin, %).
# Reports each covariate's rate ratio (95% CI) and the deviance
# explained versus the same fixed-effect structure without covariates
# (likelihood-ratio test).
# ===========================================================================

library(splines)

panel_z <- panel |>
  mutate(across(all_of(COVARIATES), ~ as.numeric(scale(.x))))

fit_intime <- function(df) {
  base <- glm(
    numCasos ~ factor(codMicroRes) + ns(ano, df = 3) + factor(mes) +
      offset(log(populacao)),
    family = poisson, data = df
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

message('In-time rate ratios (per 1-SD increase in same-period covariate value):')
print(rate_ratios)

lrt_resultados <- bind_rows(lapply(names(fits_intime), function(sp) {
  a <- anova(fits_intime[[sp]]$base, fits_intime[[sp]]$full, test = 'Chisq')
  tibble(
    especie = sp,
    deviance_base = a$`Resid. Dev`[1],
    deviance_full = a$`Resid. Dev`[2],
    deviance_explicada_pct = 100 * (a$`Resid. Dev`[1] - a$`Resid. Dev`[2]) / a$`Resid. Dev`[1],
    p_valor = a$`Pr(>Chi)`[2]
  )
}))

message('In-time: deviance explained by adding all 4 covariates (likelihood-ratio test):')
print(lrt_resultados)

rate_ratios |> write_csv('results/eda/covariates_intime_rate_ratios.csv')
lrt_resultados |> write_csv('results/eda/covariates_intime_deviance.csv')

rm(panel_z, fit_intime, fits_intime, rate_ratios, lrt_resultados)


# ===========================================================================
# SECTION 3: Realistic-lag predictive power (GLM, train/test)
#
# Same mean structure as section 2, but covariates re-lagged to values
# that would actually have been available at prediction time: ERA5
# (precip/temp/rhum) lag 1 month; deforestation stays lag 2 years
# (already realistic, see file header). Trained on ano < 2018,
# evaluated out-of-sample on 2018-2020 -- 2021-2022 stays untouched.
# This is the test that actually answers "could this work as a
# predictor," not just "is it correlated" (section 2) or "is it
# correlated at all, anywhere" (section 1).
#
# Scored with scripts/loss_functions.R -- the same mbe/nrmse/rae/
# rmsle/rse/cor the paper's own models report
# (results/test_metrics_microrregion_{vivax,falciparum}.csv), on the
# same rate scale (cases per 100k, not raw counts), so these numbers
# are actually readable against something. That test window is not
# necessarily 2018-2020 though, so treat this as a rough "where does
# this stand" read, not a strict head-to-head.
# ===========================================================================

source('scripts/loss_functions.R')

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
  teste <- panel_lagged |> filter(especie == especie_alvo, ano >= 2018, ano <= 2020)

  base <- glm(
    numCasos ~ factor(codMicroRes) + ns(ano, df = 3) + factor(mes) +
      offset(log(populacao)),
    family = poisson, data = treino
  )
  full <- update(base, . ~ . + defor_lag2 + precip_mm + temp + rhum)

  real <- teste$numCasos / teste$populacao * 1e5
  pred_base <- predict(base, teste, type = 'response') / teste$populacao * 1e5
  pred_full <- predict(full, teste, type = 'response') / teste$populacao * 1e5

  tibble(
    especie = especie_alvo,
    modelo = c('sem_covariaveis', 'com_covariaveis'),
    mbe = c(mbe(real, pred_base), mbe(real, pred_full)),
    nrmse = c(nrmse(real, pred_base), nrmse(real, pred_full)),
    rae = c(rae(real, pred_base), rae(real, pred_full)),
    rmsle = c(rmsle(real, pred_base), rmsle(real, pred_full)),
    rse = c(rse(real, pred_base), rse(real, pred_full)),
    cor = c(cor(real, pred_base), cor(real, pred_full))
  )
}

resultados_preditivos <- bind_rows(
  fit_predictive('P. vivax'),
  fit_predictive('P. falciparum')
)

message('Realistic-lag predictive test (rate scale, test = 2018-2020), with vs. without covariates:')
print(resultados_preditivos)

message('For reference, the paper\'s own test-set metrics (different test window):')
print(read_csv('results/test_metrics_microrregion_vivax.csv', show_col_types = FALSE))
print(read_csv('results/test_metrics_microrregion_falciparum.csv', show_col_types = FALSE))

resultados_preditivos |> write_csv('results/eda/covariates_predictive_test.csv')

rm(panel_lagged, fit_predictive, resultados_preditivos)
