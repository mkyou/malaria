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

build_baseline <- function(window = NULL) {
  bind_rows(lapply(sort(unique(residuos$test_start)), function(ts) {
    lo <- if (is.null(window)) 1 else max(1, ts - window)
    painel |>
      filter(idMes >= lo, idMes < ts) |>
      group_by(especie, codMicroRes) |>
      summarise(
        baseline_mean = sum(numCasos) / sum(populacao) * 1e5, .groups = 'drop'
      ) |>
      mutate(test_start = ts)
  }))
}

hist_baseline <- build_baseline() |> rename(hist_mean = baseline_mean)
ma12_baseline <- build_baseline(window = 12) |> rename(ma12_mean = baseline_mean)

residuos <- residuos |>
  left_join(hist_baseline, by = c('especie', 'codMicroRes', 'test_start')) |>
  left_join(ma12_baseline, by = c('especie', 'codMicroRes', 'test_start'))


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

plot_error_map <- function(data, metric, out_file, fill_scale) {
  p <- micro_sf |>
    inner_join(data, by = c('code_micro' = 'codMicroRes')) |>
    ggplot() +
    geom_sf(aes(fill = .data[[metric]]), color = 'black', linewidth = .1) +
    fill_scale +
    facet_grid(horizonte ~ especie) +
    MAP_THEME
  ggsave(out_file, p, width = 8.27, height = 9.5, device = agg_png)
}

plot_error_map(erros, 'rse', 'results/holdout/maps/map_errors_rse.png', RSE_SCALE)
plot_error_map(
  erros, 'rmsle', 'results/holdout/maps/map_errors_rmsle.png',
  scale_fill_gradientn(
    colours = RATE_GRADIENT, limits = c(0, 2), oob = scales::squish,
    name = 'RMSLE\n(capped at 2)'
  )
)

erros_ma12 <- residuos |>
  group_by(especie, horizonte, codMicroRes) |>
  summarise(
    denom = sum((real_taxa - ma12_mean)^2),
    rse = sum((real_taxa - pred_taxa)^2) / denom,
    rmsle = sqrt(mean((log(real_taxa + 1) - log(pmax(pred_taxa, 0) + 1))^2)),
    .groups = 'drop'
  ) |>
  mutate(rse = if_else(denom == 0, NA_real_, rse))

plot_error_map(
  erros_ma12, 'rse', 'results/holdout/maps/map_errors_rse_vs_ma12.png',
  scale_fill_gradient2(
    low = '#2166ac', mid = '#f7f7f7', high = '#b2182b', midpoint = 1,
    limits = c(0, 2), oob = scales::squish, na.value = 'grey60',
    name = 'RSE vs. area\'s\nown trailing\n12-month average\n(1 = ties it,\ncapped at 2,\ngrey = zero cases\nin reference window)'
  )
)

hotspots <- read_csv('results/eda/hotspots_ranking.csv', show_col_types = FALSE) |>
  distinct(especie, codMicroRes) |>
  mutate(is_hotspot = TRUE)

erros_ma12 <- erros_ma12 |>
  left_join(hotspots, by = c('especie', 'codMicroRes')) |>
  mutate(is_hotspot = if_else(is.na(is_hotspot), FALSE, is_hotspot))

summarise_ma12 <- function(data) {
  data |>
    group_by(horizonte, especie) |>
    summarise(
      n_valido = sum(!is.na(rse)),
      n_zero_incidencia = sum(is.na(rse)),
      modelo_vence = sum(rse < 1, na.rm = TRUE),
      ma12_vence = sum(rse >= 1, na.rm = TRUE),
      pct_modelo_vence = round(100 * mean(rse < 1, na.rm = TRUE)),
      rse_mediano = round(median(rse, na.rm = TRUE), 3),
      rmsle_mediano = round(median(rmsle, na.rm = TRUE), 3),
      .groups = 'drop'
    )
}

resumo_ma12 <- bind_rows(
  summarise_ma12(erros_ma12) |> mutate(cenario = 'completo', .before = 1),
  summarise_ma12(erros_ma12 |> filter(is_hotspot)) |> mutate(cenario = 'apenas_hotspots', .before = 1),
  summarise_ma12(erros_ma12 |> filter(!is_hotspot)) |> mutate(cenario = 'sem_hotspots', .before = 1)
)
write_csv(resumo_ma12, 'results/holdout/rse_vs_ma12_summary.csv')


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
