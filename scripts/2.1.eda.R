#-------------------------------------------------------------------------
# 2.1.eda.R
#
# Exploratory figures motivating the modelling approach.
# Reads 1.data_wrangling.R's microregion x month panel and the Legal
# Amazon shapefile.
#
# Paper-facing conventions: no titles/subtitles baked into figures
# (captions are set in LaTeX); species distinguished by shape/linetype
# and color, so figures stay legible in grayscale print but still read
# well in color; Liberation Sans (an Arial metric-equivalent) via ragg
# for a consistent font/size; choropleth rate uses a white-to-red
# sequential palette (rate is never negative, so no diverging midpoint
# applies).
#
# Sections:
#   1. Case time series (total and by state)
#   2. Spatial distribution of cases: trend (5 snapshot years) and
#      seasonality (one month per meteorological season), one file per
#      species per view, shared color scale within each view so vivax
#      and falciparum are directly comparable.
#   3. Mean-variance relationship / overdispersion (R2.1: empirical
#      justification for Bell/NegBin over Poisson)
#
# Figures saved to results/eda/. Uses the local shapefile
# (data/spatial_data/sph_files/microrreg.shp).
#-------------------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(ragg)

LEGAL_AMAZON_STATES <- c('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')

ESPECIE_SHAPES <- c('P. vivax' = 1, 'P. falciparum' = 4)
ESPECIE_LINETYPES <- c('P. vivax' = 'solid', 'P. falciparum' = 'dashed')
ESPECIE_COLORS <- c('P. vivax' = '#0072B2', 'P. falciparum' = '#D55E00')
RATE_GRADIENT <- c('#f7f7f7', '#b2182b')
BASE_THEME <- theme_bw(base_size = 13) +
  theme(text = element_text(family = 'Liberation Sans'))

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
  ggplot(aes(x = data, y = taxa, linetype = especie, color = especie)) +
  geom_line() +
  scale_linetype_manual(values = ESPECIE_LINETYPES, name = NULL) +
  scale_color_manual(values = ESPECIE_COLORS, name = NULL) +
  labs(x = NULL, y = 'Cases per 100,000 inhabitants') +
  BASE_THEME
ggsave(
  'results/eda/series_total.png',
  p,
  width = 10,
  height = 5,
  device = agg_png
)

serie_estado <- panel |>
  group_by(especie, siglaUF, data) |>
  summarise(
    numCasos = sum(numCasos, na.rm = TRUE),
    populacao = sum(populacao, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(taxa = numCasos / populacao * 1e5)

p <- serie_estado |>
  ggplot(aes(x = data, y = taxa, linetype = especie, color = especie)) +
  geom_line() +
  scale_linetype_manual(values = ESPECIE_LINETYPES, name = NULL) +
  scale_color_manual(values = ESPECIE_COLORS, name = NULL) +
  facet_wrap(~siglaUF, scales = 'free_y') +
  labs(x = NULL, y = 'Cases per 100,000 inhabitants') +
  BASE_THEME
ggsave(
  'results/eda/series_by_state.png',
  p,
  width = 12,
  height = 8,
  device = agg_png
)


# ===========================================================================
# SECTION 2: Spatial distribution of cases -- trend and seasonality
#
# One file per species per view: trend (5 snapshot years) and
# seasonality (one representative month per meteorological season,
# averaged across all years), each faceted 2 columns wide. Vivax and
# falciparum share one color scale per view, fixed to that view's full
# data range, so color is comparable across species.
#
# Population is averaged (not summed) across combined months, since
# it's an annual figure interpolated flat within each year.
# Seasonality's rate is the mean of each year's
# monthly rate, not sum-then-divide, so it stays comparable to section
# 1's time series scale.
# ===========================================================================

micro_sf <- st_read(
  'data/spatial_data/sph_files/microrreg.shp',
  quiet = TRUE
) |>
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

# One representative month per meteorological season. The color
# scale above still comes from all 12 months, so it isn't narrowed by
# only plotting these four.
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
      colours = RATE_GRADIENT,
      limits = trend_limits,
      name = 'Cases per 100,000'
    ) +
    facet_wrap(~ano, ncol = 2) +
    MAP_THEME
  ggsave(
    sprintf('results/eda/map_%s_trend.png', slug),
    p,
    width = 9,
    height = 11,
    device = agg_png
  )

  p <- micro_sf |>
    inner_join(
      sazonalidade_season |> filter(especie == sp),
      by = c('code_micro' = 'codMicroRes')
    ) |>
    ggplot() +
    geom_sf(aes(fill = taxa), color = 'black', linewidth = .1) +
    scale_fill_gradientn(
      colours = RATE_GRADIENT,
      limits = season_limits,
      name = 'Cases per 100,000'
    ) +
    facet_wrap(~mes_label, ncol = 2) +
    MAP_THEME
  ggsave(
    sprintf('results/eda/map_%s_seasonality.png', slug),
    p,
    width = 9,
    height = 8,
    device = agg_png
  )
}

rm(p, sp, slug)


# ===========================================================================
# SECTION 3: Mean-variance relationship / overdispersion (R2.1)
#
# A microregion's raw case counts mix genuine extra-Poisson dispersion
# with the two-decade trend (section 1) and seasonality (section 2), so
# a naive per-microregion mean/variance would overstate the case for
# Bell/NegBin. Each (microregion, year, month) cell has exactly one
# observation, so a within-cell empirical variance isn't available
# directly -- controlling for all three means fitting a mean model
# instead:
#
#   numCasos ~ factor(codMicroRes) + factor(ano) + factor(mes) +
#      offset(log(populacao))
#
# a plain Poisson GLM (not the paper's Bell/NegBin), fit on data
# through 2020. Its fitted value is the Poisson-consistent expectation
# once microregion/year/month are accounted for; the squared residual,
# averaged by microregion, is the corresponding controlled variance.
# Under a Poisson process, variance == mean (reference line) -- points
# above it justify Bell/NegBin, whose variance isn't pinned to the mean.
# ===========================================================================

fit_overdisp <- function(df) {
  fit <- glm(
    numCasos ~ factor(codMicroRes) +
      factor(ano) +
      factor(mes) +
      offset(log(populacao)),
    family = poisson,
    data = df
  )
  # Pearson dispersion statistic: standard overdispersion test once the
  # mean model is controlled for; phi >> 1 confirms what the plot shows
  # visually.
  pearson_disp <- sum(residuals(fit, type = 'pearson')^2) / df.residual(fit)

  by_micro <- df |>
    mutate(mu_hat = fitted(fit), resid_sq = (numCasos - mu_hat)^2) |>
    group_by(codMicroRes) |>
    summarise(
      media = mean(mu_hat),
      variancia = mean(resid_sq),
      .groups = 'drop'
    )

  list(
    by_micro = by_micro,
    pearson_dispersion = pearson_disp,
    df_residual = df.residual(fit)
  )
}

overdisp_v <- fit_overdisp(panel |> filter(especie == 'P. vivax', ano <= 2020))
overdisp_f <- fit_overdisp(
  panel |> filter(especie == 'P. falciparum', ano <= 2020)
)

overdisp <- bind_rows(
  overdisp_v$by_micro |> mutate(especie = 'P. vivax'),
  overdisp_f$by_micro |> mutate(especie = 'P. falciparum')
) |>
  filter(media > 0)

p <- overdisp |>
  ggplot(aes(x = media, y = variancia)) +
  geom_point(aes(shape = especie, color = especie), alpha = .6) +
  geom_abline(
    aes(
      slope = 1,
      intercept = 0,
      linetype = 'Poisson (variance = mean)'
    ),
    color = 'black'
  ) +
  scale_shape_manual(values = ESPECIE_SHAPES, name = NULL) +
  scale_color_manual(values = ESPECIE_COLORS, name = NULL) +
  scale_linetype_manual(values = 'dashed', name = NULL) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Mean case count (log scale)', y = 'Variance (log scale)') +
  BASE_THEME
ggsave(
  'results/eda/overdispersion_by_microregion.png',
  p,
  width = 8,
  height = 6,
  device = agg_png
)

vmr <- overdisp |>
  group_by(especie) |>
  summarise(vmr_mediano = median(variancia / media), .groups = 'drop')

resumo <- vmr |>
  left_join(
    tibble(
      especie = c('P. vivax', 'P. falciparum'),
      pearson_dispersion = c(
        overdisp_v$pearson_dispersion,
        overdisp_f$pearson_dispersion
      ),
      df_residual = c(overdisp_v$df_residual, overdisp_f$df_residual)
    ),
    by = 'especie'
  )

message(
  'Overdispersion evidence, by species, controlling for ',
  'microregion/year/month (1 = Poisson-consistent):'
)
print(resumo)

rm(
  panel,
  micro_sf,
  tendencia,
  sazonalidade,
  sazonalidade_season,
  trend_limits,
  season_limits,
  SEASON_MONTHS,
  fit_overdisp,
  overdisp,
  overdisp_v,
  overdisp_f,
  vmr,
  resumo,
  serie_total,
  serie_estado,
  p
)
