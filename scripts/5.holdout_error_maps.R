library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(ragg)

LEGAL_AMAZON_STATES <- c('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')
RATE_GRADIENT <- c('#f7f7f7', '#b2182b')
BASE_THEME <- theme_bw(base_size = 13) +
  theme(text = element_text(family = 'Liberation Sans'))
MAP_THEME <- BASE_THEME +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

dir.create('results/holdout/maps', recursive = TRUE, showWarnings = FALSE)

micro_sf <- st_read(
  'data/spatial_data/sph_files/microrreg.shp', quiet = TRUE
) |>
  mutate(code_micro = as.numeric(CD_MICRO)) |>
  filter(SIGLA_UF %in% LEGAL_AMAZON_STATES) |>
  select(code_micro)

HORIZON_LABELS <- c(
  h3 = '3 months', h12 = '1 year', h24 = '2 years (full holdout)'
)

residuos <- bind_rows(lapply(names(HORIZON_LABELS), function(h) {
  read_csv(
    sprintf('results/holdout/residuals/nbinomial_model5_%s.csv', h),
    show_col_types = FALSE
  ) |>
    mutate(horizonte = factor(
      HORIZON_LABELS[h], levels = HORIZON_LABELS, ordered = TRUE
    ))
}))


painel <- bind_rows(
  read_csv('data/output_data/micro_reg_v_df.csv', show_col_types = FALSE) |>
    mutate(especie = 'P. vivax'),
  read_csv('data/output_data/micro_reg_f_df.csv', show_col_types = FALSE) |>
    mutate(especie = 'P. falciparum')
) |>
  select(especie, codMicroRes, idMes, numCasos, populacao)

hist_baseline <- bind_rows(lapply(sort(unique(residuos$test_start)), function(ts) {
  painel |>
    filter(idMes < ts) |>
    group_by(especie, codMicroRes) |>
    summarise(
      hist_mean = sum(numCasos) / sum(populacao) * 1e5, .groups = 'drop'
    ) |>
    mutate(test_start = ts)
}))

residuos <- residuos |>
  left_join(hist_baseline, by = c('especie', 'codMicroRes', 'test_start'))


erros <- residuos |>
  group_by(especie, horizonte, codMicroRes) |>
  summarise(
    rse = sum((real_taxa - pred_taxa)^2) / sum((real_taxa - hist_mean)^2),
    rmsle = sqrt(mean((log(real_taxa + 1) - log(pmax(pred_taxa, 0) + 1))^2)),
    .groups = 'drop'
  )

RSE_SCALE <- scale_fill_gradient2(
  low = '#2166ac', mid = '#f7f7f7', high = '#b2182b', midpoint = 1,
  limits = c(0, 2), oob = scales::squish,
  name = 'RSE vs. area\'s\nown historical\nrate (1 = ties it,\ncapped at 2)'
)

plot_error_map <- function(metric, out_file, fill_scale) {
  p <- micro_sf |>
    inner_join(erros, by = c('code_micro' = 'codMicroRes')) |>
    ggplot() +
    geom_sf(aes(fill = .data[[metric]]), color = 'black', linewidth = .1) +
    fill_scale +
    facet_grid(horizonte ~ especie) +
    MAP_THEME
  ggsave(out_file, p, width = 8.27, height = 9.5, device = agg_png)
}

plot_error_map('rse', 'results/holdout/maps/map_errors_rse.png', RSE_SCALE)
plot_error_map(
  'rmsle', 'results/holdout/maps/map_errors_rmsle.png',
  scale_fill_gradientn(
    colours = RATE_GRADIENT, limits = c(0, 2), oob = scales::squish,
    name = 'RMSLE\n(capped at 2)'
  )
)


erros_ano <- residuos |>
  group_by(especie, horizonte, ano, codMicroRes) |>
  summarise(
    rse = sum((real_taxa - pred_taxa)^2) / sum((real_taxa - hist_mean)^2),
    rmsle = sqrt(mean((log(real_taxa + 1) - log(pmax(pred_taxa, 0) + 1))^2)),
    .groups = 'drop'
  ) |>
  mutate(ano = factor(ano))

plot_error_trend_map <- function(metric, out_file, fill_scale) {
  p <- micro_sf |>
    inner_join(erros_ano, by = c('code_micro' = 'codMicroRes')) |>
    ggplot() +
    geom_sf(aes(fill = .data[[metric]]), color = 'black', linewidth = .1) +
    fill_scale +
    facet_grid(especie + ano ~ horizonte) +
    MAP_THEME
  ggsave(out_file, p, width = 8.27, height = 11, device = agg_png)
}

plot_error_trend_map('rse', 'results/holdout/maps/map_errors_trend_rse.png', RSE_SCALE)
plot_error_trend_map(
  'rmsle', 'results/holdout/maps/map_errors_trend_rmsle.png',
  scale_fill_gradientn(
    colours = RATE_GRADIENT, limits = c(0, 2), oob = scales::squish,
    name = 'RMSLE\n(capped at 2)'
  )
)

message('Holdout error maps written to results/holdout/maps/.')
