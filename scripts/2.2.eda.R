#-------------------------------------------------------------------------
# 2.2.eda.R
#
# Covariate exploration, for the Scientific Reports revision (see
# paper_response/REVIEW_RESPONSE_PLAN.md's "reframe decision": does
# bringing in deforestation/rainfall/temperature/humidity actually
# carry signal?). Reads 1.data_wrangling.R's microregion x month
# panel. Restricted to ano <= 2020 throughout the whole script --
# 2018-2020 is reserved as this analysis's own test window and
# 2021-2022 is the untouched final holdout; see section 2 (not yet
# written) for why.
#
# Two questions, kept deliberately separate rather than conflated into
# one model:
#   1. In-time: does a covariate explain case rate once microregion
#      and time are controlled for, using the *same-period* covariate
#      value? Answers "is there a real relationship here at all."
#   2. Realistic-lag / predictive: same question, but using only
#      covariate values that would actually have been published in
#      time for a forecast -- ERA5 lag 1 month (~1 month structural
#      lag to compute a monthly mean, well inside the ~5-day-to-3-
#      month latency Copernicus documents), deforestation already
#      lag 2 years (PRODES's own consolidated-data lag is 6-18 months
#      after the PRODES-year ends, so lag 2y has comfortable margin;
#      lag 1y would be underwater for early-year predictions -- see
#      commit for sources). Answers "could this actually work as a
#      predictor," not just "is it correlated."
#
# Sections:
#   1. Covariate vs. case rate: temporal and spatial correlation, plus
#      (1b) a trend/seasonal/remainder decomposition of the temporal
#      story, since raw correlation conflates the three (this file)
#   2. In-time explanatory power (GLM)               [not yet written]
#   3. Realistic-lag predictive power (GLM, train/test) [not yet written]
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
# which an aggregate correlation number can't. deforestation is state-,
# not microregion-grain, so its map shows blocks of same-state
# microregions sharing one value, not genuine within-state texture --
# expected, not a bug.
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

# How much correlation does the lag actually cost? defor_km2 (same
# year, what 1.data_wrangling.R now also keeps) vs. defor_lag2 (2
# years, the realistic/usable one, per PRODES's own publication
# lag -- see the header). Same-year deforestation could never actually
# be used as a predictor (it isn't published yet), so this isn't "which
# one to use" -- it's "how much signal do we give up by using the one
# that's actually available."
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
# them. Done per microregion (its own 216-month series, 2003-2020),
# same as the correlation maps and the deforestation lag comparison
# above, then summarized by the median across microregions -- not one
# decomposition of the region-wide aggregate, which would hide
# microregion-level heterogeneity the same way a single aggregate
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

# No separate plot for this table -- Raw/Trend/Seasonal/Remainder x
# covariate x species is only 4x4x2 = 32 numbers, the CSV reads fine
# on its own without a redundant figure.

# Illustration: relative humidity (the covariate with the sharpest gap
# between raw and seasonal correlation) decomposed alongside both
# species' case rates, so "trend/seasonal/remainder" has a concrete
# picture to point to instead of just the summary numbers above. Uses
# the region-wide aggregate series (not per-microregion, unlike the
# summary table above) purely because one clean picture teaches the
# concept better than 107 -- it's not meant to stand in as evidence,
# the median table above is.
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
