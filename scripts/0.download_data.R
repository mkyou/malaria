#-------------------------------------------------------------------------
# 0.download_data.R
#
# All data acquisition for the project lives here, in sections. Each
# section is idempotent: it checks for its own output file(s) under
# data/ and skips the fetch if already present, so re-running this
# script after a partial run (or just to check it still works) doesn't
# re-download everything.
#
# Sections:
#   1. Population                   (IBGE SIDRA API, primary source)
#   2. Malaria notifications        (Mendeley Data, public CC BY 4.0) -- reads
#                                    populacao_df.csv, so must run after section 1
#   3. Microregion centroids        (shared setup for sections 5-6; filtered
#                                    by state, no dependency on sections 1-2)
#   4. Deforestation                (PRODES/TerraBrasilis, state x year)
#   5. Precipitation                (ERA5 / Copernicus CDS)
#   6. Temperature & relative humidity (ERA5 / Copernicus CDS)
#
# See README.md's "Data" section for the source table, caveats (PRODES
# is primary-sourced 2008-2022 but secondary/estimated 2003-2007; the
# microregion geometry recovery in section 3 exists because write_csv()
# mangled the original geom column), and requirements (a Copernicus CDS
# API key in ~/.cdsapirc for sections 5-6).
#-------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)

# The 9 states making up the Brazilian Legal Amazon -- shared by section 3
# (microregion filtering) and section 4 (deforestation, state x year).
LEGAL_AMAZON_STATES <- c('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')

# ===========================================================================
# SECTION 1: Population (IBGE SIDRA, primary source)
#
# Runs before malaria notifications (section 2) because that section
# reads populacao_df.csv.
#
# SIDRA table 6579 ("População residente estimada") does not cover every year.
# Years around a Census or population count are published as separate tables:
#   - 2003-2006, 2008-2009, 2011-2021: table 6579 (annual estimates)
#   - 2007: table 793 ("Contagem da População 2007")
#   - 2010: table 200 (Census 2010)
#   - 2022: table 4709 (Census 2022)
# ===========================================================================

POPULACAO_OUT <- 'data/support_data/populacao_df.csv'

dir.create('data/support_data', recursive = TRUE, showWarnings = FALSE)

if (file.exists(POPULACAO_OUT)) {
  message(sprintf('[skip] section 1: %s already exists', POPULACAO_OUT))
} else {
  library(sidrar)

  message('[info] fetching municipality universe for the query scope')
  muni_codes <- read_csv(
    'data/support_data/municipios_codigos.csv',
    trim_ws = TRUE,
    show_col_types = FALSE
  ) |>
    pull(cod_mun) |>
    unique()

  fetch_pop_year <- function(
    table,
    variable,
    period,
    classific = NULL,
    category = NULL
  ) {
    args <- list(x = table, variable = variable, period = period, geo = 'City')
    if (!is.null(classific)) {
      args$classific <- classific
      args$category <- category
    }
    d <- do.call(get_sidra, args)
    d |>
      transmute(
        codMunRes = as.numeric(`Município (Código)`),
        ano = as.numeric(Ano),
        populacao = as.numeric(Valor)
      ) |>
      filter(codMunRes %in% muni_codes)
  }

  message(
    '[fetch] 6579: annual estimates, 2003-2006, 2008-2009, 2011-2021 (one year per call)'
  )
  est_years <- c(2003:2006, 2008:2009, 2011:2021)
  est <- bind_rows(lapply(est_years, function(y) {
    message(sprintf('  - %d', y))
    fetch_pop_year(6579, 9324, as.character(y))
  }))

  message('[fetch] 793: 2007 (Population Count)')
  cont_2007 <- fetch_pop_year(793, 93, '2007')

  message('[fetch] 200: 2010 (2010 Census)')
  censo_2010 <- fetch_pop_year(
    200,
    93,
    '2010',
    classific = c('c1', 'c2', 'c58'),
    category = list(0, 0, 0)
  )

  message('[fetch] 4709: 2022 (2022 Census)')
  censo_2022 <- fetch_pop_year(4709, 93, '2022')

  pop <- bind_rows(est, cont_2007, censo_2010, censo_2022) |>
    mutate(codMunRes6 = as.numeric(substr(as.character(codMunRes), 1, 6))) |>
    select(codMunRes6, codMunRes, ano, populacao) |>
    arrange(codMunRes, ano)

  # Municipalities emancipated after 2003 genuinely have no primary
  # record before emancipation (e.g. Mojuí dos Campos, PA, code
  # 1504752, emancipated from Santarém in 2013 -- no entry in
  # 2007/2010). This is expected; section 2's interpolation fills it in.
  year_counts <- table(pop$codMunRes)
  incomplete <- year_counts[year_counts < 18]
  if (length(incomplete) > 0) {
    warning(
      sprintf(
        '[warn] %d municipalities with more than 2 missing years 
        (beyond what emancipations would explain): %s
        ',
        length(incomplete),
        paste(names(incomplete), collapse = ', ')
      )
    )
  }
  gap_small <- year_counts[year_counts %in% 18:19]
  if (length(gap_small) > 0) {
    message(sprintf(
      '
      [info] %d municipalities with 1-2 missing years 
      (expected for post-2003 emancipations): %s
      ',
      length(gap_small),
      paste(names(gap_small), collapse = ', ')
    ))
  }

  stopifnot(
    'expected at least 18 years per municipality (2003-2022)' = all(
      year_counts >= 18
    ),
    'no missing years overall' = setequal(unique(pop$ano), 2003:2022)
  )

  write_csv(pop, POPULACAO_OUT)
  message(sprintf(
    '[done] populacao_df.csv: primary SIDRA source, %d-%d (%d rows, %d municipalities)',
    min(pop$ano),
    max(pop$ano),
    nrow(pop),
    length(unique(pop$codMunRes))
  ))

  rm(
    muni_codes,
    est,
    cont_2007,
    censo_2010,
    censo_2022,
    pop,
    year_counts,
    incomplete,
    gap_small
  )
}


# ===========================================================================
# SECTION 2: Malaria notifications (SIVEP mirror, Mendeley Data)
#
# Monteiro K. et al. (2023). Legal Amazon malaria notification records,
# Brazil, 2003-2022 (v2). Mendeley Data. doi: 10.17632/9n6b97fsbd.2
# License: CC BY 4.0.
# ===========================================================================

MENDELEY_DATASET_ID <- '9n6b97fsbd'
MENDELEY_VERSION <- 2
MENDELEY_DOI <- '10.17632/9n6b97fsbd.2'

# Direct URL for the pinned /2 version (validated manually).
# Fallback: if the file_id changes, resolve the new one from the DOI at
# https://data.mendeley.com/datasets/9n6b97fsbd/2 and update the URL.
DATASET_URL <- paste0(
  'https://data.mendeley.com/public-files/datasets/',
  MENDELEY_DATASET_ID,
  '/files/e22ce7ac-daaf-4d93-a98d-6e0d3fbe643d/file_downloaded'
)

# Checksum of Dataset.csv v2 -- catches silent content changes.
DATASET_SHA256 <- '3241ff18e67b5ee63b92145976c883268c839771da17c669fd350c7f844b8954'

YEAR_START <- 2003
YEAR_END <- 2022

# Test-result code classification (dataset's atributos.csv):
#   02 P. falciparum
#   03 P. falciparum + P. falciparum gametocytes
#   04 P. vivax
#   05 P. falciparum + P. vivax (mixed -- excluded)
#   06 P. vivax + P. vivax gametocytes
#   07 P. falciparum gametocytes
#   08-11 other (excluded)
FALCIPARUM_CODES <- c(2, 3, 7)
VIVAX_CODES <- c(4, 6)

RAW_DIR <- 'data/raw'
RAW_FILE <- file.path(RAW_DIR, 'Dataset.csv')
MAIN_DATA_DIR <- 'data/main_data'
FALCIPARUM_OUT <- file.path(MAIN_DATA_DIR, 'falciparum_df.csv')
VIVAX_OUT <- file.path(MAIN_DATA_DIR, 'vivax_df.csv')

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MAIN_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create('data/spatial_data', recursive = TRUE, showWarnings = FALSE)

if (file.exists(FALCIPARUM_OUT) && file.exists(VIVAX_OUT)) {
  message(
    sprintf(
      '[skip] section 2: %s and %s already exist',
      FALCIPARUM_OUT,
      VIVAX_OUT
    )
  )
} else {
  if (!file.exists(RAW_FILE)) {
    message(sprintf('[download] %s -> %s', DATASET_URL, RAW_FILE))
    utils::download.file(DATASET_URL, RAW_FILE, mode = 'wb', quiet = FALSE)
  } else {
    message(sprintf('[skip] %s already exists', RAW_FILE))
  }

  # Verify checksum (requires the `digest` package -- available by default on R 4+).
  if (requireNamespace('digest', quietly = TRUE)) {
    observed <- digest::digest(file = RAW_FILE, algo = 'sha256')
    if (!identical(observed, DATASET_SHA256)) {
      warning(sprintf(
        'Dataset.csv SHA-256 checksum mismatch.\n  
        expected: %s\n  
        got: %s\n
        Version %s\'s content may have changed. Check %s.',
        DATASET_SHA256,
        observed,
        MENDELEY_VERSION,
        paste0(
          'https://data.mendeley.com/datasets/',
          MENDELEY_DATASET_ID,
          '/',
          MENDELEY_VERSION
        )
      ))
    } else {
      message('[ok] SHA-256 matches')
    }
  } else {
    message(
      '[warn] `digest` package unavailable -- skipping checksum verification'
    )
  }

  raw <- read_csv(
    RAW_FILE,
    col_types = cols(
      Date = col_date(),
      Municipality = col_character(),
      `Test results` = col_double(),
      Notifications = col_integer()
    )
  )

  # Municipality mapping (6 digits) -> (7 digits, microregion, etc.)
  cities_df <- read_csv(
    'data/support_data/municipios_codigos.csv',
    trim_ws = TRUE
  ) |>
    select(cod_mun) |>
    mutate(cod_mun = as.character(cod_mun)) |>
    mutate(cod_mun6 = substr(cod_mun, 1, 6))

  populacao_df <- read_csv(
    'data/support_data/populacao_df.csv',
    col_types = cols(
      codMunRes6 = col_character(),
      codMunRes = col_character(),
      ano = col_integer(),
      populacao = col_integer()
    )
  )

  # Interpolate with `approx()` per municipality (extending flat at the
  # boundaries, rule=2) for municipalities emancipated after 2003 with
  # no population record before emancipation -- e.g. Mojuí dos Campos,
  # PA (code 1504752, split from Santarém in 2013), whose real data
  # only starts in 2012. Already checked: harmless, since its pre-2013
  # case counts are also structurally zero (filed under Santarém's
  # code before the split) and both municipalities aggregate into the
  # same microregion (15002) in 1.data_wrangling.R, so the frozen
  # population and the zero case count wash out together at the grain
  # the models actually use.
  populacao_df <- populacao_df |>
    group_by(codMunRes6, codMunRes) |>
    complete(ano = seq(YEAR_START, YEAR_END)) |>
    mutate(populacao = {
      if (sum(!is.na(populacao)) >= 2) {
        as.integer(round(approx(ano, populacao, xout = ano, rule = 2)$y))
      } else {
        populacao
      }
    }) |>
    ungroup()

  stopifnot(sum(is.na(populacao_df$populacao)) == 0)

  raw <- raw |>
    mutate(
      ano = as.integer(format(Date, '%Y')),
      mes = as.integer(format(Date, '%m'))
    ) |>
    filter(ano >= YEAR_START, ano <= YEAR_END) |>
    mutate(
      id_mes = (ano - YEAR_START) * 12L + mes,
      tipo = case_when(
        `Test results` %in% FALCIPARUM_CODES ~ 'falciparum',
        `Test results` %in% VIVAX_CODES ~ 'vivax',
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(tipo))

  # Aggregate by (municipality, id_mes, tipo)
  agg <- raw |>
    group_by(codMunRes6 = Municipality, id_mes, tipo) |>
    summarise(numCasos = sum(Notifications), .groups = 'drop')

  # Municipality universe comes from populacao_df.csv (canonical after the migration)
  muni_universe <- populacao_df |>
    distinct(codMunRes6, codMunRes)

  # Cross-product (muni x id_mes x tipo) with numCasos = 0 where absent
  panel <- expand_grid(
    muni_universe,
    id_mes = seq_len((YEAR_END - YEAR_START + 1L) * 12L),
    tipo = c('falciparum', 'vivax')
  ) |>
    left_join(agg, by = c('codMunRes6', 'id_mes', 'tipo')) |>
    mutate(
      numCasos = ifelse(is.na(numCasos), 0L, numCasos),
      ano = YEAR_START + (id_mes - 1L) %/% 12L
    ) |>
    left_join(populacao_df, by = c('codMunRes6', 'codMunRes', 'ano')) |>
    select(codMunRes6, codMunRes, id_mes, numCasos, populacao, tipo)

  falciparum_df <- panel |> filter(tipo == 'falciparum') |> select(-tipo)
  vivax_df <- panel |> filter(tipo == 'vivax') |> select(-tipo)

  stopifnot(nrow(falciparum_df) == nrow(vivax_df))

  write_csv(falciparum_df, FALCIPARUM_OUT)
  write_csv(vivax_df, VIVAX_OUT)

  message(sprintf(
    '[done] falciparum_df.csv: %s rows | vivax_df.csv: %s rows',
    nrow(falciparum_df),
    nrow(vivax_df)
  ))
  message(sprintf(
    '[done] total cases in %d-%d -> falciparum: %s | vivax: %s',
    YEAR_START,
    YEAR_END,
    sum(falciparum_df$numCasos),
    sum(vivax_df$numCasos)
  ))

  rm(
    raw,
    cities_df,
    populacao_df,
    agg,
    muni_universe,
    panel,
    falciparum_df,
    vivax_df
  )
}


# ===========================================================================
# SECTION 3: Microregion centroids (shared setup for sections 5-6)
#
# Needed to extract per-microregion values from the gridded ERA5
# products. Read from the shapefile already versioned at
# data/spatial_data/sph_files/microrreg.shp (560 Brazilian
# microregions, valid MULTIPOLYGON geometry, confirmed 560/560 valid
# with sf::st_is_valid()) rather than refetched via geobr. This local
# shapefile is filtered to LEGAL_AMAZON_STATES and centroided directly
# with sf::st_centroid(), the real area-weighted centroid, no
# approximation.
# ===========================================================================

CENTROIDS_OUT <- 'data/spatial_data/micro_centroids.csv'

if (file.exists(CENTROIDS_OUT)) {
  message(sprintf('[skip] section 3: %s already exists', CENTROIDS_OUT))
} else {
  library(sf)

  micro_sf <- st_read('data/spatial_data/sph_files/microrreg.shp', quiet = TRUE)
  stopifnot('shapefile has invalid geometries' = all(st_is_valid(micro_sf)))

  uf_lookup <- read_csv(
    'data/support_data/municipios_codigos.csv',
    trim_ws = TRUE,
    show_col_types = FALSE
  ) |>
    distinct(sigla_UF, cod_UF)

  expected_codes <- read_csv(
    'data/support_data/municipios_codigos.csv',
    trim_ws = TRUE,
    show_col_types = FALSE
  ) |>
    filter(sigla_UF %in% LEGAL_AMAZON_STATES) |>
    pull(cod_micro_reg) |>
    unique()

  micro_sf <- micro_sf |>
    mutate(code_micro = as.numeric(CD_MICRO)) |>
    filter(SIGLA_UF %in% LEGAL_AMAZON_STATES)

  stopifnot(
    'shapefile state filter disagrees with municipios_codigos.csv' = setequal(
      micro_sf$code_micro,
      expected_codes
    )
  )

  coords <- micro_sf |> st_centroid() |> st_coordinates()

  centroids <- micro_sf |>
    st_drop_geometry() |>
    transmute(abbrev_state = SIGLA_UF, code_micro, name_micro = NM_MICRO) |>
    left_join(uf_lookup, by = c('abbrev_state' = 'sigla_UF')) |>
    transmute(
      code_state = cod_UF,
      abbrev_state,
      code_micro,
      name_micro,
      lon = coords[, 1],
      lat = coords[, 2]
    )

  stopifnot(
    'centroid extraction failed for some microregions' = sum(is.na(
      centroids$lon
    )) ==
      0
  )

  write_csv(centroids, CENTROIDS_OUT)
  message(
    sprintf(
      '[done] micro_centroids.csv: %d microregions (from shapefile, exact centroids)',
      nrow(centroids)
    )
  )

  rm(micro_sf, uf_lookup, expected_codes, coords, centroids)
}


# ===========================================================================
# SECTION 4: Deforestation (PRODES / TerraBrasilis, state x year, 2-year lag)
#
# State-level, 2-year lag: PRODES publishes each year late.
#
# Primary source: TerraBrasilis WFS (prodes-legal-amz:yearly_
# deforestation), which already carries a `state` field, summed by
# (state, year). Only covers 2008-2022 (current PRODES methodology) --
# confirmed complete, 135/135 (state, year) cells. The pre-2008 layer
# (accumulated_deforestation_2007) is NOT a usable annual source: it's
# a cumulative snapshot, not per-year increments (summing it gives
# <3 km² for 2003-2006 and ~718,000 km² for 2007 -- nonsense). So
# 2003-2007 has no primary or secondary state-level source at all;
# those years use the secondary-sourced NATIONAL total (see README),
# allocated across states by each state's average real share of
# 2008-2010 (PA ~48%, MT ~19%, ...). That's an estimate, not a
# measurement -- flagged via `source = secondary_unverified_state_
# allocated`, not to be read as real state data for those years.
#
# 2001-2002 use the same secondary source too, but only to make
# km2_lag2 (the 2-year lag actually used downstream) complete from
# ano=2003 onward -- YEAR_START is 2003, so 2001/2002 rows themselves
# are dropped from the written file after the lag is computed.
#
# Server is flaky (frequent 502s), paginates at 50000 features -- fetched
# year-by-year with retries and pagination.
# ===========================================================================

DEFORESTATION_OUT <- 'data/support_data/deforestation_df.csv'

if (file.exists(DEFORESTATION_OUT)) {
  message(sprintf('[skip] section 4: %s already exists', DEFORESTATION_OUT))
} else {
  library(httr)

  WFS_BASE <- 'https://terrabrasilis.dpi.inpe.br/geoserver/ows'
  WFS_PAGE_SIZE <- 50000

  fetch_prodes_year <- function(year, type_name, max_retries = 4) {
    all_pages <- list()
    start_index <- 0
    repeat {
      result <- NULL
      for (attempt in seq_len(max_retries)) {
        resp <- tryCatch(
          GET(
            WFS_BASE,
            query = list(
              service = 'WFS',
              version = '2.0.0',
              request = 'GetFeature',
              typeName = type_name,
              propertyName = 'state,year,area_km',
              outputFormat = 'csv',
              CQL_FILTER = sprintf('year=%d', year),
              startIndex = start_index,
              count = WFS_PAGE_SIZE
            ),
            timeout(90)
          ),
          error = function(e) NULL
        )
        if (
          !is.null(resp) &&
            status_code(resp) == 200 &&
            length(content(resp, 'raw')) > 20
        ) {
          result <- resp
          break
        }
        Sys.sleep(6)
      }
      if (is.null(result)) {
        return(NULL)
      }
      page <- read_csv(
        content(result, 'text', encoding = 'UTF-8'),
        show_col_types = FALSE
      )
      if (nrow(page) == 0) {
        break
      }
      all_pages[[length(all_pages) + 1]] <- page
      if (nrow(page) < WFS_PAGE_SIZE) {
        break
      }
      start_index <- start_index + WFS_PAGE_SIZE
    }
    if (length(all_pages) == 0) {
      return(NULL)
    }
    bind_rows(all_pages)
  }

  message(
    '[fetch] TerraBrasilis WFS: prodes-legal-amz:yearly_deforestation, 2008-2022, 
    by state (primary source attempt)'
  )
  primary_attempt_years <- 2008:2022
  wfs_results <- vector('list', length(primary_attempt_years))
  names(wfs_results) <- primary_attempt_years
  for (y in primary_attempt_years) {
    message(sprintf('  - %d', y))
    wfs_results[[as.character(y)]] <- fetch_prodes_year(
      y,
      'prodes-legal-amz:yearly_deforestation'
    )
  }

  failed_years <- primary_attempt_years[sapply(wfs_results, is.null)]
  if (length(failed_years) > 0) {
    warning(sprintf(
      '[warn] PRODES primary-source fetch failed for %d year(s) even by state: %s.
      These years will have NO deforestation data 
      (no state-level secondary source exists to fall back to)',
      length(failed_years),
      paste(failed_years, collapse = ', ')
    ))
  }

  primary_state <- bind_rows(wfs_results) |>
    group_by(state, ano = year) |>
    summarise(km2 = sum(area_km, na.rm = TRUE), .groups = 'drop') |>
    mutate(source = 'primary_wfs')

  STATES <- LEGAL_AMAZON_STATES

  # Secondary-sourced NATIONAL totals (unverified -- see README), used
  # only for 2001-2007 (2001-2002 solely to complete km2_lag2 for
  # ano=2003-2004, see above), allocated across states by their
  # average real share of the 2008-2010 total. Wikipedia's "Estimates
  # of the rates of deforestation in the Amazon rainforest from 1970
  # to 2022" table (INPE/FAO-sourced), cross-checked against INPE
  # press-release text -- confirmed 2003-2007 here still match that
  # table to within 1 km² (2006: 14286 here vs 14285 there, negligible).
  secondary_national_km2 <- c(
    `2001` = 18165,
    `2002` = 21651,
    `2003` = 25396,
    `2004` = 27772,
    `2005` = 19014,
    `2006` = 14286,
    `2007` = 11651
  )
  state_shares <- primary_state |>
    filter(ano %in% 2008:2010) |>
    group_by(state) |>
    summarise(km2 = sum(km2), .groups = 'drop') |>
    mutate(share = km2 / sum(km2)) |>
    select(state, share)
  stopifnot(
    'state shares do not sum to 1' = abs(sum(state_shares$share) - 1) < 1e-6
  )

  allocated <- tidyr::crossing(
    state = STATES,
    ano = as.integer(names(secondary_national_km2))
  ) |>
    left_join(state_shares, by = 'state') |>
    mutate(
      km2 = secondary_national_km2[as.character(ano)] * share,
      source = 'secondary_unverified_state_allocated'
    ) |>
    select(state, ano, km2, source)

  prodes <- bind_rows(primary_state, allocated) |>
    complete(state = STATES, ano = 2001:2022) |>
    arrange(state, ano) |>
    group_by(state) |>
    mutate(km2_lag2 = lag(km2, 2)) |>
    ungroup() |>
    filter(ano >= YEAR_START)

  write_csv(prodes, DEFORESTATION_OUT)
  message(sprintf(
    '[done] deforestation_df.csv: %d rows (%d states x %d years), 
    %d primary_wfs / %d secondary_unverified_state_allocated',
    nrow(prodes),
    length(STATES),
    length(2003:2022),
    sum(prodes$source == 'primary_wfs', na.rm = TRUE),
    sum(prodes$source == 'secondary_unverified_state_allocated', na.rm = TRUE)
  ))
  if (any(is.na(prodes$km2))) {
    warning(
      sprintf(
        '[warn] %d (state, year) cells still have no deforestation value at all',
        sum(is.na(prodes$km2))
      )
    )
  }

  rm(
    fetch_prodes_year,
    primary_attempt_years,
    wfs_results,
    failed_years,
    primary_state,
    STATES,
    secondary_national_km2,
    state_shares,
    allocated,
    prodes
  )
}


# ===========================================================================
# SECTION 5: Precipitation (ERA5 / Copernicus CDS)
#
# One request for the whole Legal Amazon bounding box and full time
# range, then local
# extraction at each microregion centroid.
#
# Requires a CDS API key in ~/.cdsapirc:
#   url: https://cds.climate.copernicus.eu/api
#   key: <your key>
# Register at https://cds.climate.copernicus.eu
#
# Units: ERA5's monthly-averaged-reanalysis total_precipitation is a
# mean DAILY rate for that month (metres/day), not a monthly total;
# converted to mm/day here.
# ===========================================================================

PRECIP_OUT <- 'data/support_data/precip_df.csv'

if (file.exists(PRECIP_OUT)) {
  message(sprintf('[skip] section 5: %s already exists', PRECIP_OUT))
} else {
  library(ecmwfr)
  library(terra)

  # wf_request() reads the key from the keyring, not from ~/.cdsapirc
  # directly, and the keyring is per-process (not persisted) -- so it
  # must be registered here, every fresh run.
  cdsapirc <- readLines('~/.cdsapirc')
  cds_key <- trimws(sub('^key:\\s*', '', grep('^key:', cdsapirc, value = TRUE)))
  wf_set_key(key = cds_key, user = 'ecmwfr')

  ERA5_NC <- 'data/raw/era5_precip_2003_2022.nc'

  if (!file.exists(ERA5_NC)) {
    message(
      '[fetch] ERA5 total_precipitation, Legal Amazon bbox,
      2003-2022 (single CDS request)'
    )
    req <- list(
      dataset_short_name = 'reanalysis-era5-single-levels-monthly-means',
      product_type = 'monthly_averaged_reanalysis',
      variable = 'total_precipitation',
      year = as.character(2003:2022),
      month = sprintf('%02d', 1:12),
      time = '00:00',
      area = c(5, -75, -20, -40), # N, W, S, E -- buffer around Legal Amazon
      format = 'netcdf',
      target = basename(ERA5_NC)
    )
    wf_request(
      request = req,
      transfer = TRUE,
      path = dirname(ERA5_NC),
      time_out = 3600
    )
  } else {
    message(sprintf('[skip] %s already exists', ERA5_NC))
  }

  r <- rast(ERA5_NC)
  cent <- read_csv(CENTROIDS_OUT, show_col_types = FALSE)
  pts <- vect(cent, geom = c('lon', 'lat'), crs = 'EPSG:4326')
  vals <- terra::extract(r, pts)
  times <- as.Date(time(r))

  precip <- vals |>
    select(-ID) |>
    bind_cols(cent |> select(code_micro, name_micro)) |>
    pivot_longer(starts_with('tp_'), names_to = 'layer', values_to = 'tp_m') |>
    mutate(
      layer_idx = as.integer(sub('tp_', '', layer)),
      date = times[layer_idx],
      ano = as.integer(format(date, '%Y')),
      mes = as.integer(format(date, '%m')),
      precip_mm = tp_m * 1000
    ) |>
    select(codMicroRes = code_micro, ano, mes, precip_mm)

  stopifnot(sum(is.na(precip$precip_mm)) == 0)

  write_csv(precip, PRECIP_OUT)
  message(
    sprintf(
      '[done] precip_df.csv: %d rows, %d-%d',
      nrow(precip),
      min(precip$ano),
      max(precip$ano)
    )
  )

  rm(cdsapirc, cds_key, r, cent, pts, vals, times, precip)
}


# ===========================================================================
# SECTION 6: Temperature & relative humidity (ERA5 / Copernicus CDS)
#
# Replaces the legacy rhum_df.csv/temp_df.csv (municipality-grain,
# undocumented source -- no script in the repo ever produced them).
# Rebuilt here at microregion grain, same method as section 5: one CDS
# request for the whole time range/bbox, extracted at the centroids.
#
# ERA5 has no direct relative-humidity variable at single levels, so
# rhum is derived from 2m_temperature and 2m_dewpoint_temperature via
# the Magnus-Tetens approximation (standard meteorological practice).
# temp is kept in Kelvin, matching the legacy file's convention.
# ===========================================================================

TEMP_OUT <- 'data/support_data/temp_df.csv'
RHUM_OUT <- 'data/support_data/rhum_df.csv'

if (file.exists(TEMP_OUT) && file.exists(RHUM_OUT)) {
  message(sprintf(
    '[skip] section 6: %s and %s already exist',
    TEMP_OUT,
    RHUM_OUT
  ))
} else {
  library(ecmwfr)
  library(terra)

  cdsapirc <- readLines('~/.cdsapirc')
  cds_key <- trimws(sub('^key:\\s*', '', grep('^key:', cdsapirc, value = TRUE)))
  wf_set_key(key = cds_key, user = 'ecmwfr')

  ERA5_TEMP_NC <- 'data/raw/era5_temp_dewpoint_2003_2022.nc'

  if (!file.exists(ERA5_TEMP_NC)) {
    message(
      '[fetch] ERA5 2m_temperature + 2m_dewpoint_temperature, Legal Amazon bbox, 
      2003-2022'
    )
    req <- list(
      dataset_short_name = 'reanalysis-era5-single-levels-monthly-means',
      product_type = 'monthly_averaged_reanalysis',
      variable = c('2m_temperature', '2m_dewpoint_temperature'),
      year = as.character(2003:2022),
      month = sprintf('%02d', 1:12),
      time = '00:00',
      area = c(5, -75, -20, -40), # N, W, S, E -- buffer around Legal Amazon
      format = 'netcdf',
      target = basename(ERA5_TEMP_NC)
    )
    wf_request(
      request = req,
      transfer = TRUE,
      path = dirname(ERA5_TEMP_NC),
      time_out = 3600
    )
  } else {
    message(sprintf('[skip] %s already exists', ERA5_TEMP_NC))
  }

  r <- rast(ERA5_TEMP_NC)
  cent <- read_csv(CENTROIDS_OUT, show_col_types = FALSE)
  pts <- vect(cent, geom = c('lon', 'lat'), crs = 'EPSG:4326')

  # terra names combined-variable NetCDF layers "<var>_<layer_index>"
  # (e.g. "t2m_1".."t2m_240"), each variable keeping its own time axis
  # in the same order -- same pattern as section 5's single-variable case.
  extract_var <- function(r, prefix) {
    rv <- r[[grep(paste0('^', prefix, '_'), names(r))]]
    vals <- terra::extract(rv, pts)
    times <- as.Date(time(rv))
    vals |>
      select(-ID) |>
      bind_cols(cent |> select(code_micro)) |>
      pivot_longer(
        starts_with(paste0(prefix, '_')),
        names_to = 'layer',
        values_to = prefix
      ) |>
      mutate(
        layer_idx = as.integer(sub(paste0(prefix, '_'), '', layer)),
        date = times[layer_idx],
        ano = as.integer(format(date, '%Y')),
        mes = as.integer(format(date, '%m'))
      ) |>
      select(code_micro, ano, mes, all_of(prefix))
  }

  meteo <- extract_var(r, 't2m') |>
    inner_join(extract_var(r, 'd2m'), by = c('code_micro', 'ano', 'mes'))

  # Magnus-Tetens approximation for relative humidity from temperature
  # and dewpoint (both in Kelvin here, converted to Celsius for the formula).
  meteo <- meteo |>
    mutate(
      t_c = t2m - 273.15,
      td_c = d2m - 273.15,
      rhum = 100 *
        exp((17.625 * td_c) / (243.04 + td_c)) /
        exp((17.625 * t_c) / (243.04 + t_c))
    )

  stopifnot(
    'relative humidity out of [0, 100] range' = all(
      meteo$rhum >= 0 & meteo$rhum <= 100
    )
  )

  temp_df <- meteo |> transmute(codMicroRes = code_micro, ano, mes, temp = t2m)
  rhum_df <- meteo |> transmute(codMicroRes = code_micro, ano, mes, rhum)

  stopifnot(sum(is.na(temp_df$temp)) == 0, sum(is.na(rhum_df$rhum)) == 0)

  write_csv(temp_df, TEMP_OUT)
  write_csv(rhum_df, RHUM_OUT)
  message(sprintf(
    '[done] temp_df.csv + rhum_df.csv: %d rows each, %d-%d',
    nrow(temp_df),
    min(temp_df$ano),
    max(temp_df$ano)
  ))

  rm(cdsapirc, cds_key, r, cent, pts, extract_var, meteo, temp_df, rhum_df)
}
