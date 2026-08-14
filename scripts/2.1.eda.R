#-------------------------------------------------------------------------
# 2.1.eda.R
#
# Exploratory figures motivating the modelling approach. Reads
# 1.data_wrangling.R's microregion x month panel and the Legal Amazon
# shapefile.
#
# Sections: 1) case time series, 2) spatial trend/seasonality maps,
# 3) mean-variance / overdispersion (R2.1), 4) chronic spatial
# hotspots, 5) health facilities (CNES) screening against them.
# Rate (cases per 100k) throughout, not raw counts -- microregions
# vary too much in population for a raw-count correlation/decomposition
# to mean anything comparable across them.
#
# Figures saved to results/eda/.
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
  mutate(
    data = as.Date(sprintf('%d-%02d-01', ano, mes)),
    taxa = numCasos / populacao * 1e5
  )

rm(micro_reg_v, micro_reg_f)


# ===========================================================================
# SECTION 1: Case time series (total and by state)
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
# One file per species per view, 2-column facets. Vivax and falciparum
# share one color scale per view so they're directly comparable.
# Seasonality's rate is the mean of each year's monthly rate, not
# sum-then-divide, to stay comparable to section 1's scale.
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

# One representative month per meteorological season.
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
# A naive per-microregion mean/variance would conflate genuine
# overdispersion with the trend and seasonality already shown above, so
# controlling for microregion/year/month means fitting a mean model
# first (plain Poisson GLM, through 2020) and comparing each
# microregion's fitted mean to its squared-residual variance. Under a
# Poisson process variance == mean (reference line); points above it
# justify Bell/NegBin.
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


# ===========================================================================
# SECTION 4: Chronic spatial hotspots
#
# bym2's spatial smoothing borrows strength from neighboring
# microregions -- good for noisy small areas, but it pulls a
# genuinely, persistently extreme area toward its neighbors' lower
# level (2.3.model_iteration.R found this shows up as systematic
# underestimation for exactly these areas). This documents that a
# small, recurring set of microregions accounts for a disproportionate
# share of the most extreme rate cells across the whole 2003-2022
# series, not just recent years.
#
# Ranked by how often each microregion lands in its own species' top
# 1% of rate cells (a per-species threshold, since case magnitudes
# differ a lot between vivax and falciparum) -- a frequency count, not
# a single peak, so a one-off spike doesn't outrank a chronic hotspot.
# ===========================================================================

annual <- panel |>
  group_by(especie, codMicroRes, nomeMicroRes, siglaUF, ano) |>
  summarise(
    casos = sum(numCasos, na.rm = TRUE),
    pop = mean(populacao, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(taxa_anual = casos / pop * 1e5)

taxa_mediana <- annual |>
  group_by(especie, codMicroRes, nomeMicroRes, siglaUF) |>
  summarise(taxa_mediana_anual = median(taxa_anual), .groups = 'drop')

freq_extremos <- panel |>
  mutate(taxa = numCasos / populacao * 1e5) |>
  group_by(especie) |>
  mutate(p99 = quantile(taxa, .99, na.rm = TRUE)) |>
  ungroup() |>
  group_by(especie, codMicroRes, nomeMicroRes, siglaUF) |>
  summarise(n_top1pct = sum(taxa >= p99), .groups = 'drop')

hotspots <- freq_extremos |>
  left_join(
    taxa_mediana,
    by = c('especie', 'codMicroRes', 'nomeMicroRes', 'siglaUF')
  ) |>
  arrange(especie, desc(n_top1pct))

top_hotspots <- hotspots |>
  group_by(especie) |>
  slice_max(n_top1pct, n = 10) |>
  ungroup()

message(
  'Chronic spatial hotspots -- top 10 per species by frequency in ',
  'the top 1% of rate cells, 2003-2022:'
)
print(top_hotspots, n = Inf)

top_hotspots |> write_csv('results/eda/hotspots_ranking.csv')

for (sp in unique(panel$especie)) {
  slug <- ifelse(sp == 'P. vivax', 'vivax', 'falciparum')

  p <- micro_sf |>
    inner_join(
      hotspots |> filter(especie == sp),
      by = c('code_micro' = 'codMicroRes')
    ) |>
    ggplot() +
    geom_sf(aes(fill = n_top1pct), color = 'black', linewidth = .1) +
    scale_fill_gradientn(
      colours = RATE_GRADIENT,
      name = 'Months in own\ntop 1% of rate'
    ) +
    MAP_THEME
  ggsave(
    sprintf('results/eda/map_%s_hotspots.png', slug),
    p,
    width = 7,
    height = 8,
    device = agg_png
  )
}

# ===========================================================================
# SECTION 5: Health facilities (CNES) -- screening
#
# Grew out of section 4's chronic-hotspot question, not out of
# 2.2.eda.R's covariate-signal one -- does any of CNES's 45 candidate
# covariates (facility-type counts plus SUS/ambulatory/hospital/
# emergency flags, see 0.download_data.R section 7) explain WHICH
# microregions are chronic hotspots? Screened two ways:
#   - temporal: per-microregion trend/remainder decomposition (same
#     method as 2.2.eda.R section 1b) -- does a covariate's short-term
#     fluctuation track case rate's, within an area, net of shared
#     trend/season?
#   - cross-sectional: does a microregion's average covariate LEVEL
#     track its average case rate, across microregions -- a between-
#     area difference, not a within-area one, which is what the
#     chronic-hotspot question above actually is.
# Both matter: the temporal test catches the same trend-conflation
# pitfall 2.2.eda.R's decomposition exists for (most raw correlations
# here are dominated by a shared secular trend -- facility counts grew
# while case rates fell, 2005-2022 -- with near-zero remainder), the
# cross-sectional one is what actually surfaced signal.
#
# ano < 2005 has no CNES source (0.download_data.R section 7).
# ===========================================================================

CNES_PATTERN <- '^n_(estabelecimentos|vinc_sus|atendamb|atendhos|urgemerg|tp_)'
CNES_COLS <- names(panel)[grepl(CNES_PATTERN, names(panel))]

panel_cnes <- panel |> filter(ano >= 2005, ano <= 2020)

# Full trend/seasonal/remainder decomposition, same method as
# 2.2.eda.R section 1b. Seasonal is forced NA for every CNES column,
# same reasoning as defor_lag2 there: these are December snapshots
# broadcast flat across 12 months, a step function, not a smoothly-
# varying series -- decompose()'s moving-average trend misreads the
# step's residual as seasonality, not real within-year signal.
cnes_temporal <- bind_rows(lapply(CNES_COLS, function(col) {
  bind_rows(lapply(c('P. vivax', 'P. falciparum'), function(sp) {
    sub <- panel_cnes |> filter(especie == sp)
    per_micro <- sub |>
      group_split(codMicroRes) |>
      lapply(function(d) {
        col_bad <- sd(d[[col]], na.rm = TRUE) == 0 || any(is.na(d[[col]]))
        if (nrow(d) < 24 || col_bad) {
          return(NULL)
        }
        dv <- decompose(
          ts(d$taxa, start = c(2005, 1), frequency = 12),
          type = 'additive'
        )
        dc <- decompose(
          ts(d[[col]], start = c(2005, 1), frequency = 12),
          type = 'additive'
        )
        tibble(
          raw = suppressWarnings(cor(d$taxa, d[[col]], use = 'complete.obs')),
          trend = suppressWarnings(
            cor(dv$trend, dc$trend, use = 'complete.obs')
          ),
          remainder = suppressWarnings(
            cor(dv$random, dc$random, use = 'complete.obs')
          )
        )
      }) |>
      bind_rows()
    if (nrow(per_micro) == 0) {
      return(NULL)
    }
    tibble(
      covariavel = col,
      especie = sp,
      raw_mediana = median(per_micro$raw, na.rm = TRUE),
      trend_mediana = median(per_micro$trend, na.rm = TRUE),
      seasonal_mediana = NA_real_,
      remainder_mediana = median(per_micro$remainder, na.rm = TRUE),
      n_micro = nrow(per_micro)
    )
  }))
}))

cnes_transversal <- panel_cnes |>
  group_by(codMicroRes, especie) |>
  summarise(
    taxa_media = sum(numCasos) / mean(populacao) * 1e5 / n_distinct(ano),
    across(all_of(CNES_COLS), ~ mean(.x, na.rm = TRUE)),
    .groups = 'drop'
  )

cnes_cross <- bind_rows(lapply(CNES_COLS, function(col) {
  bind_rows(lapply(c('P. vivax', 'P. falciparum'), function(sp) {
    d <- cnes_transversal |> filter(especie == sp)
    tibble(
      covariavel = col,
      especie = sp,
      cor_transversal = suppressWarnings(cor(
        d$taxa_media,
        d[[col]],
        use = 'complete.obs',
        method = 'spearman'
      ))
    )
  }))
}))

cnes_screening <- cnes_temporal |>
  left_join(cnes_cross, by = c('covariavel', 'especie')) |>
  arrange(desc(abs(cor_transversal)))

message(
  'CNES screening, all 45 candidates -- temporal remainder vs. ',
  'cross-sectional level:'
)
print(cnes_screening, n = 20)
cnes_screening |> write_csv('results/eda/cnes_covariates_screening.csv')

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
  annual,
  taxa_mediana,
  freq_extremos,
  hotspots,
  top_hotspots,
  sp,
  slug,
  p,
  CNES_PATTERN,
  CNES_COLS,
  panel_cnes,
  cnes_temporal,
  cnes_transversal,
  cnes_cross,
  cnes_screening
)
