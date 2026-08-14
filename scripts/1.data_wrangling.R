#-------------------------------------------------------------------------
# 1.data_wrangling.R
#
# Turns the raw, country-wide data from 0.download_data.R into the
# microregion-grain panel the models in scripts/3.microrregion_models/
# actually read. Not idempotent like 0.download_data.R.
#
# Sections:
#   1. Malaria panel        (municipality -> microregion, Legal Amazon filter)
#   2. Microregion spatial structure (adjacency graph + flat map lookup + idArea)
#   3. Covariates            (deforestation, precipitation, temperature, humidity)
#-------------------------------------------------------------------------

library(dplyr)
library(readr)

# The 9 states making up the Brazilian Legal Amazon -- must match
# LEGAL_AMAZON_STATES in 0.download_data.R.
LEGAL_AMAZON_STATES <- c('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')

# ===========================================================================
# SECTION 1: Malaria panel (municipality -> microregion, Legal Amazon filter)
#
# falciparum_df/vivax_df (data/main_data) cover all of Brazil --
# built from a country-wide population base, post-SIDRA-migration --
# so filter down to the 9 Legal Amazon states here, before any
# aggregation. Everything downstream (microregion grain, covariate
# joins) assumes this scope.
# ===========================================================================

falciparum_df <- read_csv('data/main_data/falciparum_df.csv')
vivax_df <- read_csv('data/main_data/vivax_df.csv')

falciparum_df <- falciparum_df |>
  mutate(ano = 2002 + ceiling(id_mes / 12))

vivax_df <- vivax_df |>
  mutate(ano = 2002 + ceiling(id_mes / 12))

cities_df <- read_csv('data/support_data/municipios_codigos.csv') |>
  select(cod_mun, nome_mun, cod_micro_reg, nome_micro_reg,
         cod_UF, sigla_UF)

falciparum_df <- falciparum_df |>
  inner_join(cities_df, by = c('codMunRes' = 'cod_mun')) |>
  filter(sigla_UF %in% LEGAL_AMAZON_STATES)

vivax_df <- vivax_df |>
  inner_join(cities_df, by = c('codMunRes' = 'cod_mun')) |>
  filter(sigla_UF %in% LEGAL_AMAZON_STATES)

rm(cities_df)

vivax_df <- vivax_df |>
  rename(
    idMes = id_mes,
    nomeMunRes = nome_mun,
    codMicroRes = cod_micro_reg,
    nomeMicroRes = nome_micro_reg,
    codUF = cod_UF,
    siglaUF = sigla_UF
  )

falciparum_df <- falciparum_df |>
  rename(
    idMes = id_mes,
    nomeMunRes = nome_mun,
    codMicroRes = cod_micro_reg,
    nomeMicroRes = nome_micro_reg,
    codUF = cod_UF,
    siglaUF = sigla_UF
  )

vivax_df <- vivax_df |>
  mutate(mes = idMes - 12 * (ceiling(idMes / 12) - 1))

falciparum_df <- falciparum_df |>
  mutate(mes = idMes - 12 * (ceiling(idMes / 12) - 1))

# Aggregate to microregion x month -- the only grain this pipeline
# produces now.
micro_reg_v <- vivax_df |>
  group_by(codMicroRes, nomeMicroRes, idMes, mes, ano, codUF, siglaUF) |>
  summarise(
    populacao = sum(populacao, na.rm = TRUE),
    numCasos = sum(numCasos, na.rm = TRUE),
    .groups = 'drop'
  )

micro_reg_f <- falciparum_df |>
  group_by(codMicroRes, nomeMicroRes, idMes, mes, ano, codUF, siglaUF) |>
  summarise(
    populacao = sum(populacao, na.rm = TRUE),
    numCasos = sum(numCasos, na.rm = TRUE),
    .groups = 'drop'
  )

rm(falciparum_df, vivax_df)


# ===========================================================================
# SECTION 2: Microregion spatial structure
#
# Adjacency graph (for INLA's bym2 spatial random effect) and a flat,
# geometry-free microregion lookup table. Built from the same local
# shapefile and LEGAL_AMAZON_STATES filter as 0.download_data.R
# section 3 (that section builds centroids from it; this one builds
# adjacency).
#
# Runs before section 3 (not after, like the numbering in
# 0.download_data.R might suggest) because idArea -- the integer each
# microregion maps to inside outputs/micro_map.graph -- has to be
# joined onto the case panel before it's written out.
# ===========================================================================

library(sf)
library(spdep)
library(INLA)

uf_lookup <- read_csv(
  'data/support_data/municipios_codigos.csv',
  trim_ws = TRUE, show_col_types = FALSE
)

expected_codes <- uf_lookup |>
  filter(sigla_UF %in% LEGAL_AMAZON_STATES) |>
  pull(cod_micro_reg) |> unique()

micro_sf <- st_read('data/spatial_data/sph_files/microrreg.shp', quiet = TRUE) |>
  mutate(code_micro = as.numeric(CD_MICRO)) |>
  filter(SIGLA_UF %in% LEGAL_AMAZON_STATES)

stopifnot(
  'shapefile state filter disagrees with municipios_codigos.csv' =
    setequal(micro_sf$code_micro, expected_codes)
)

dir.create('outputs', recursive = TRUE, showWarnings = FALSE)
nb2INLA('outputs/micro_map.graph', poly2nb(micro_sf))

# idArea is micro_sf's row position at the exact moment the graph
# above was built -- INLA's bym2 indexes neighbors positionally, not
# by code, so this is the only correct way to know which integer each
# microregion maps to. Written into micro_map.csv and joined onto the
# case panel in section 3 below, so nothing downstream has to
# reconstruct it (e.g. via pmatch() against row order in a separate
# file) and risk drifting out of sync with the graph's own numbering.
micro_lookup <- micro_sf |>
  st_drop_geometry() |>
  transmute(
    abbrev_state = SIGLA_UF, code_micro, name_micro = NM_MICRO,
    idArea = row_number()
  )

micro_lookup |> write_csv('data/spatial_data/micro_map.csv')

rm(uf_lookup, expected_codes, micro_sf)


# ===========================================================================
# SECTION 3: Covariates (deforestation, precipitation, temperature,
# humidity, health facilities, chronic hotspot intensity)
#
# See 0.download_data.R sections 4-7. Deforestation joins by (state,
# ano) -- state, not microregion, since section 4 fetches a per-state
# breakdown. precip/temp/rhum join by (microregion, ano, mes), already
# monthly. CNES joins by (microregion, ano) only -- it's an annual
# (December) snapshot, so the same year's value is shared across all
# 12 months; 2003-2004 have no CNES source at all, left NA. Population
# is already included above, via the sum() in the microregion
# aggregation.
#
# `hotspots`: test-only feature (2.1.eda.R section 4's static, whole-
# period ranking is the EDA evidence; this is its time-varying form).
# For microregion x ano, the count of months in the 3 years BEFORE ano
# (not including ano itself) that microregion was at or above its
# species' own top-1% rate threshold. Threshold is fixed, computed
# once from that species' full 2003-2022 history -- a scale reference,
# not a per-fold-sensitive statistic. No backfill for years without 3
# full prior years (ano 2003-2005): left NA, same as any other
# genuinely-missing lag.
# ===========================================================================

compute_hotspots <- function(df) {
  p99 <- quantile(df$numCasos / df$populacao * 1e5, .99, na.rm = TRUE)
  df |>
    mutate(taxa = numCasos / populacao * 1e5) |>
    group_by(codMicroRes, ano) |>
    summarise(n_p99_ano = sum(taxa >= p99), .groups = 'drop') |>
    arrange(codMicroRes, ano) |>
    group_by(codMicroRes) |>
    mutate(
      hotspots = lag(n_p99_ano, 1) + lag(n_p99_ano, 2) + lag(n_p99_ano, 3)
    ) |>
    ungroup() |>
    select(codMicroRes, ano, hotspots)
}

hotspots_v <- compute_hotspots(micro_reg_v)
hotspots_f <- compute_hotspots(micro_reg_f)

deforestation_df <- read_csv('data/support_data/deforestation_df.csv') |>
  select(siglaUF = state, ano, defor_km2 = km2, defor_lag2 = km2_lag2)

precip_df <- read_csv('data/support_data/precip_df.csv')
temp_df <- read_csv('data/support_data/temp_df.csv')
rhum_df <- read_csv('data/support_data/rhum_df.csv')
cnes_df <- read_csv('data/support_data/cnes_df.csv') |>
  rename(codMicroRes = cod_micro_reg)

idArea_lookup <- micro_lookup |> select(code_micro, idArea)

micro_reg_v <- micro_reg_v |>
  left_join(deforestation_df, by = c('siglaUF', 'ano')) |>
  left_join(precip_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(temp_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(rhum_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(cnes_df, by = c('codMicroRes', 'ano')) |>
  left_join(hotspots_v, by = c('codMicroRes', 'ano')) |>
  left_join(idArea_lookup, by = c('codMicroRes' = 'code_micro')) |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

micro_reg_f <- micro_reg_f |>
  left_join(deforestation_df, by = c('siglaUF', 'ano')) |>
  left_join(precip_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(temp_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(rhum_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(cnes_df, by = c('codMicroRes', 'ano')) |>
  left_join(hotspots_f, by = c('codMicroRes', 'ano')) |>
  left_join(idArea_lookup, by = c('codMicroRes' = 'code_micro')) |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

stopifnot(
  'idArea missing for some vivax microregion-months' =
    sum(is.na(micro_reg_v$idArea)) == 0,
  'idArea missing for some falciparum microregion-months' =
    sum(is.na(micro_reg_f$idArea)) == 0
)

dir.create('data/output_data', recursive = TRUE, showWarnings = FALSE)

micro_reg_v |> write_csv('data/output_data/micro_reg_v_df.csv')
micro_reg_f |> write_csv('data/output_data/micro_reg_f_df.csv')

rm(compute_hotspots, hotspots_v, hotspots_f, deforestation_df, precip_df,
   temp_df, rhum_df, cnes_df, micro_lookup, idArea_lookup, micro_reg_v,
   micro_reg_f)
