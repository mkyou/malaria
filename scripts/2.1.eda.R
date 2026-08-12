#-------------------------------------------------------------------------
# 2.1.eda.R
#
# Exploratory figures to motivate the modelling approach, for the
# Scientific Reports revision (see paper_response/REVIEW_RESPONSE_PLAN.md).
# Reads 1.data_wrangling.R's microregion x month panel and the Legal
# Amazon shapefile. Paper-facing: no titles/subtitles baked into the
# figures (captions are set in LaTeX); species is distinguished by
# shape/linetype rather than color (print/grayscale-safe) everywhere
# except the choropleths, where color is the rate itself, not species;
# one consistent font/size (Liberation Sans -- an Arial
# metric-equivalent, rendered via ragg so the family is actually
# honored) throughout.
#
# Sections:
#   1. Case time series (total and by state)
#   2. Spatial distribution of cases: a trend view (5 snapshot years)
#      and a seasonality view (one month per meteorological season,
#      averaged across all 20 years) -- one file per species per view,
#      faceted 2 columns wide, rather than one file per year/month.
#      Vivax and falciparum share the same color scale within each
#      view (trend maps share one scale, seasonality maps share
#      another), so colors are directly comparable species to species.
#   3. Mean-variance relationship / overdispersion (R2.1: empirical
#      justification for Bell/NegBin over Poisson, before any model
#      is fit)
#
# Figures saved to results/eda/. Choropleth style (theme_bw(), the
# blue-to-red scale_fill_gradientn()) follows scripts/4.error_analysis.R.
# Uses the local shapefile (data/spatial_data/sph_files/microrreg.shp)
# rather than geobr::read_micro_region(), unlike that script -- geobr's
# backend has repeatedly been unreachable this session (see
# 0.download_data.R section 3 and simulations/run_sim.R's comments).
#-------------------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(ragg)

LEGAL_AMAZON_STATES <- c('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')

# Shared across every figure. Species is black-on-white: shape (point
# geoms) or linetype (line geoms), never color -- readable in
# grayscale print. Rate (choropleths) is the one place color is used,
# via the blue-to-red gradient scripts/4.error_analysis.R already
# established. BASE_THEME fixes font (Liberation Sans, an Arial
# metric-equivalent already installed) and size (13pt base, vs
# ggplot2's default 11) consistently too; ggsave always uses
# device = agg_png so the family is actually respected (the default
# png device on Linux frequently ignores non-alias family names).
ESPECIE_SHAPES <- c('P. vivax' = 1, 'P. falciparum' = 4)
ESPECIE_LINETYPES <- c('P. vivax' = 'solid', 'P. falciparum' = 'dashed')
RATE_GRADIENT <- c('#d7e1ee', '#991f17')
BASE_THEME <- theme_bw(base_size = 13) +
  theme(text = element_text(family = 'Liberation Sans'))

dir.create('results/eda', recursive = TRUE, showWarnings = FALSE)

micro_reg_v <- read_csv('data/output_data/micro_reg_v_df.csv', show_col_types = FALSE) |>
  mutate(especie = 'P. vivax')

micro_reg_f <- read_csv('data/output_data/micro_reg_f_df.csv', show_col_types = FALSE) |>
  mutate(especie = 'P. falciparum')

panel <- bind_rows(micro_reg_v, micro_reg_f) |>
  mutate(data = as.Date(sprintf('%d-%02d-01', ano, mes)))

rm(micro_reg_v, micro_reg_f)


# ===========================================================================
# SECTION 1: Case time series (total and by state)
#
# Rate per 100,000 inhabitants, matching the convention used everywhere
# else in the pipeline (see README's "Modelling units and predictors").
# ===========================================================================

serie_total <- panel |>
  group_by(especie, data) |>
  summarise(
    numCasos = sum(numCasos, na.rm = TRUE),
    populacao = sum(populacao, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(taxa = numCasos / populacao * 1e5)

p <- serie_total |>
  ggplot(aes(x = data, y = taxa, linetype = especie)) +
  geom_line() +
  scale_linetype_manual(values = ESPECIE_LINETYPES, name = NULL) +
  labs(x = NULL, y = 'Cases per 100,000 inhabitants') +
  BASE_THEME
ggsave('results/eda/series_total.png', p, width = 10, height = 5, device = agg_png)

serie_estado <- panel |>
  group_by(especie, siglaUF, data) |>
  summarise(
    numCasos = sum(numCasos, na.rm = TRUE),
    populacao = sum(populacao, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(taxa = numCasos / populacao * 1e5)

p <- serie_estado |>
  ggplot(aes(x = data, y = taxa, linetype = especie)) +
  geom_line() +
  scale_linetype_manual(values = ESPECIE_LINETYPES, name = NULL) +
  facet_wrap(~siglaUF, scales = 'free_y') +
  labs(x = NULL, y = 'Cases per 100,000 inhabitants') +
  BASE_THEME
ggsave('results/eda/series_by_state.png', p, width = 12, height = 8, device = agg_png)


# ===========================================================================
# SECTION 2: Spatial distribution of cases -- trend and seasonality
#
# One file per species per view: trend (5 snapshot years, 2003/2008/
# 2013/2018/2022) and seasonality (one representative month per
# meteorological season -- Jan/Apr/Jul/Oct -- averaged across all 20
# years), each faceted 2 columns wide. Within each view, vivax and
# falciparum share one color scale (fixed to that view's full data
# range) so the color itself is comparable across species, not just
# within one map.
#
# Population is averaged (not summed) across the months being
# combined, since it's already an annual figure interpolated flat
# within each year (0.download_data.R section 1) -- summing it would
# double-count. Seasonality's rate is the mean of each year's monthly
# rate, not a sum-then-divide -- summing 20 years of one calendar
# month before dividing would give a cumulative total, not a typical
# monthly rate, and wouldn't be comparable to section 1's time series.
# ===========================================================================

micro_sf <- st_read('data/spatial_data/sph_files/microrreg.shp', quiet = TRUE) |>
  mutate(code_micro = as.numeric(CD_MICRO)) |>
  filter(SIGLA_UF %in% LEGAL_AMAZON_STATES) |>
  select(code_micro)

MAP_THEME <- BASE_THEME +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

TREND_YEARS <- c(2003, 2008, 2013, 2018, 2022)

tendencia <- panel |>
  filter(ano %in% TREND_YEARS) |>
  group_by(especie, codMicroRes, ano) |>
  summarise(
    numCasos = sum(numCasos, na.rm = TRUE),
    populacao = mean(populacao, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(taxa = numCasos / populacao * 1e5)

trend_limits <- range(tendencia$taxa, na.rm = TRUE)

sazonalidade <- panel |>
  mutate(
    mes_label = factor(month.abb[mes], levels = month.abb),
    taxa_ano = numCasos / populacao * 1e5
  ) |>
  group_by(especie, codMicroRes, mes_label) |>
  summarise(taxa = mean(taxa_ano, na.rm = TRUE), .groups = 'drop')

season_limits <- range(sazonalidade$taxa, na.rm = TRUE)

# One representative month per (meteorological) season rather than
# all 12 -- the color scale above still comes from all 12 months, so
# it isn't narrowed by only plotting these four.
SEASON_MONTHS <- c('Jan', 'Apr', 'Jul', 'Oct')

sazonalidade_season <- sazonalidade |>
  filter(mes_label %in% SEASON_MONTHS) |>
  mutate(mes_label = factor(as.character(mes_label), levels = SEASON_MONTHS))

for (sp in unique(panel$especie)) {
  slug <- ifelse(sp == 'P. vivax', 'vivax', 'falciparum')

  p <- micro_sf |>
    inner_join(
      tendencia |> filter(especie == sp),
      by = c('code_micro' = 'codMicroRes')
    ) |>
    ggplot() +
    geom_sf(aes(fill = taxa), color = 'black', linewidth = .1) +
    scale_fill_gradientn(
      colours = RATE_GRADIENT, limits = trend_limits,
      name = 'Cases per 100,000'
    ) +
    facet_wrap(~ano, ncol = 2) +
    MAP_THEME
  ggsave(
    sprintf('results/eda/map_%s_trend.png', slug), p,
    width = 9, height = 11, device = agg_png
  )

  p <- micro_sf |>
    inner_join(
      sazonalidade_season |> filter(especie == sp),
      by = c('code_micro' = 'codMicroRes')
    ) |>
    ggplot() +
    geom_sf(aes(fill = taxa), color = 'black', linewidth = .1) +
    scale_fill_gradientn(
      colours = RATE_GRADIENT, limits = season_limits,
      name = 'Cases per 100,000'
    ) +
    facet_wrap(~mes_label, ncol = 2) +
    MAP_THEME
  ggsave(
    sprintf('results/eda/map_%s_seasonality.png', slug), p,
    width = 9, height = 8, device = agg_png
  )
}

rm(p, sp, slug)


# ===========================================================================
# SECTION 3: Mean-variance relationship / overdispersion (R2.1)
#
# A microregion's raw case counts move with three things at once:
# genuine extra-Poisson dispersion, the two-decade decline (section
# 1), and seasonality (section 2) -- pooling across time and just
# taking mean/variance per microregion (or even per microregion-year)
# bakes the trend and season into "variance" that has nothing to do
# with overdispersion, overstating the case for Bell/NegBin. Each
# (microregion, year, month) cell has exactly one observation, so
# there's no way to get a within-cell empirical variance directly --
# controlling for all three means fitting a mean model instead:
#
#   numCasos ~ factor(codMicroRes) + factor(ano) + factor(mes) + offset(log(populacao))
#
# a plain Poisson GLM (not the paper's Bell/NegBin model -- just a
# mean structure to control for these factors), fit on data through
# 2020. Its fitted value mu_hat *is* the Poisson-consistent
# expectation once microregion, year, and month are accounted for; the
# squared residual (numCasos - mu_hat)^2, averaged by microregion, is
# the corresponding empirical variance with the same three factors
# controlled for. Same plot as a naive per-microregion mean/variance
# scatter, but both axes now come from the controlled model instead of
# raw pooled counts. Under a Poisson process, variance == mean
# (reference line, in its own legend entry) -- points well above it
# justify Bell/NegBin, whose variance isn't pinned to the mean the way
# Poisson's is.
# ===========================================================================

fit_overdisp <- function(df) {
  fit <- glm(
    numCasos ~ factor(codMicroRes) + factor(ano) + factor(mes) +
      offset(log(populacao)),
    family = poisson, data = df
  )
  # Pearson dispersion statistic (sum of squared Pearson residuals over
  # residual df) -- the standard, citable overdispersion test statistic
  # once the mean model is controlled for; phi >> 1 is the formal
  # version of what the mean-variance scatter shows visually.
  pearson_disp <- sum(residuals(fit, type = 'pearson')^2) / df.residual(fit)

  by_micro <- df |>
    mutate(mu_hat = fitted(fit), resid_sq = (numCasos - mu_hat)^2) |>
    group_by(codMicroRes) |>
    summarise(media = mean(mu_hat), variancia = mean(resid_sq), .groups = 'drop')

  list(by_micro = by_micro, pearson_dispersion = pearson_disp, df_residual = df.residual(fit))
}

overdisp_v <- fit_overdisp(panel |> filter(especie == 'P. vivax', ano <= 2020))
overdisp_f <- fit_overdisp(panel |> filter(especie == 'P. falciparum', ano <= 2020))

overdisp <- bind_rows(
  overdisp_v$by_micro |> mutate(especie = 'P. vivax'),
  overdisp_f$by_micro |> mutate(especie = 'P. falciparum')
) |>
  filter(media > 0)

p <- overdisp |>
  ggplot(aes(x = media, y = variancia)) +
  geom_point(aes(shape = especie), color = 'black', alpha = .6) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = 'Poisson (variance = mean)')) +
  scale_shape_manual(values = ESPECIE_SHAPES, name = NULL) +
  scale_linetype_manual(values = 'dashed', name = NULL) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Mean case count (log scale)', y = 'Variance (log scale)') +
  BASE_THEME
ggsave('results/eda/overdispersion.png', p, width = 8, height = 6, device = agg_png)

vmr <- overdisp |>
  group_by(especie) |>
  summarise(vmr_mediano = median(variancia / media), .groups = 'drop')

resumo <- vmr |>
  left_join(
    tibble(
      especie = c('P. vivax', 'P. falciparum'),
      pearson_dispersion = c(overdisp_v$pearson_dispersion, overdisp_f$pearson_dispersion),
      df_residual = c(overdisp_v$df_residual, overdisp_f$df_residual)
    ),
    by = 'especie'
  )

message(
  'Overdispersion evidence, by species, controlling for ',
  'microregion/year/month (1 = Poisson-consistent):'
)
print(resumo)

# Saved as the citable evidence behind the overdispersion.png plot:
# the per-microregion values behind each point, and the headline
# statistics (Pearson dispersion phi, median variance-to-mean ratio).
overdisp |> write_csv('results/eda/overdispersion_by_microregion.csv')
resumo |> write_csv('results/eda/overdispersion_summary.csv')

rm(panel, micro_sf, tendencia, sazonalidade, sazonalidade_season,
   trend_limits, season_limits, SEASON_MONTHS, fit_overdisp, overdisp,
   overdisp_v, overdisp_f, vmr, resumo, serie_total, serie_estado, p)
