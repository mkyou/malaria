#-------------------------------------------------------------------------
# 0.download_data.R
#
# Baixa os dados de notificação de malária da Amazônia Legal (SIVEP) do
# repositório público no Mendeley Data e os converte para o formato
# esperado pelos demais scripts (data/main_data/{falciparum,vivax}_df.csv).
#
# Fonte:
#   Monteiro K. et al. (2023). Legal Amazon malaria notification records,
#   Brazil, 2003-2022 (v2). Mendeley Data. doi: 10.17632/9n6b97fsbd.2
#   Licença: CC BY 4.0.
#
# Janela: 2003-01 a 2018-12 (compatível com a análise atual).
# Para janela estendida (até 2022) ou clima mais recente/granular,
# ver seção "Atualização futura" no README.
#-------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)


MENDELEY_DATASET_ID <- '9n6b97fsbd'
MENDELEY_VERSION    <- 2
MENDELEY_DOI        <- '10.17632/9n6b97fsbd.2'

# URL direta da versão fixa /2 (validada manualmente).
# Fallback: caso o file_id mude, resolva pelo DOI em
# https://data.mendeley.com/datasets/9n6b97fsbd/2 e atualize a URL.
DATASET_URL <- paste0(
  'https://data.mendeley.com/public-files/datasets/',
  MENDELEY_DATASET_ID,
  '/files/e22ce7ac-daaf-4d93-a98d-6e0d3fbe643d/file_downloaded'
)

# Checksum do Dataset.csv v2 — detecta alteração silenciosa do conteúdo.
DATASET_SHA256 <- '3241ff18e67b5ee63b92145976c883268c839771da17c669fd350c7f844b8954'

YEAR_START <- 2003
YEAR_END   <- 2018

# Classificação dos códigos de teste (atributos.csv do dataset):
#   02 P. falciparum
#   03 P. falciparum + gametócitos de P. falciparum
#   04 P. vivax
#   05 P. falciparum + P. vivax (misto — excluído)
#   06 P. vivax + gametócitos de P. vivax
#   07 gametócitos de P. falciparum
#   08-11 outros (excluídos)
FALCIPARUM_CODES <- c(2, 3, 7)
VIVAX_CODES      <- c(4, 6)

RAW_DIR <- 'data/raw'
RAW_FILE <- file.path(RAW_DIR, 'Dataset.csv')


dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create('data/main_data', recursive = TRUE, showWarnings = FALSE)

if (!file.exists(RAW_FILE)) {
  message(sprintf('[download] %s -> %s', DATASET_URL, RAW_FILE))
  utils::download.file(DATASET_URL, RAW_FILE, mode = 'wb', quiet = FALSE)
} else {
  message(sprintf('[skip] %s já existe', RAW_FILE))
}

# Verifica checksum (requer pacote `digest` — disponível por padrão no R 4+).
if (requireNamespace('digest', quietly = TRUE)) {
  observed <- digest::digest(file = RAW_FILE, algo = 'sha256')
  if (!identical(observed, DATASET_SHA256)) {
    warning(sprintf(
      'Checksum SHA-256 do Dataset.csv divergente.\n  esperado: %s\n  obtido:   %s\nO conteúdo da versão %s pode ter sido alterado. Verifique em %s.',
      DATASET_SHA256, observed, MENDELEY_VERSION,
      paste0('https://data.mendeley.com/datasets/', MENDELEY_DATASET_ID,
             '/', MENDELEY_VERSION)
    ))
  } else {
    message('[ok] SHA-256 confere')
  }
} else {
  message('[warn] pacote `digest` indisponível — pulando verificação de checksum')
}


raw <- read_csv(
  RAW_FILE,
  col_types = cols(
    Date          = col_date(),
    Municipality  = col_character(),
    `Test results`= col_double(),
    Notifications = col_integer()
  )
)

# Mapeamento município (6 dígitos) -> (7 dígitos, microrregião, etc.)
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
    codMunRes  = col_character(),
    ano        = col_integer(),
    populacao  = col_integer()
  )
)

# 2013 está ausente para 400 munis no arquivo de suporte (lacuna herdada
# da extração inicial). IBGE entrega projeções intercensitárias lineares
# entre censos, então interpolamos com `approx()` por município, preenchendo
# extremos por carregamento (rule=2). Resultado idempotente: anos já
# presentes ficam inalterados.
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
      `Test results` %in% VIVAX_CODES      ~ 'vivax',
      TRUE                                 ~ NA_character_
    )
  ) |>
  filter(!is.na(tipo))

# Agrega por (município, id_mes, tipo)
agg <- raw |>
  group_by(codMunRes6 = Municipality, id_mes, tipo) |>
  summarise(numCasos = sum(Notifications), .groups = 'drop')


# Universo de munis vem do populacao_df.csv (canônico após a migração)
muni_universe <- populacao_df |>
  distinct(codMunRes6, codMunRes)

# Cross-product (muni x id_mes x tipo) com numCasos = 0 onde ausente
panel <- expand_grid(
  muni_universe,
  id_mes = seq_len((YEAR_END - YEAR_START + 1L) * 12L),
  tipo   = c('falciparum', 'vivax')
) |>
  left_join(agg, by = c('codMunRes6', 'id_mes', 'tipo')) |>
  mutate(
    numCasos = ifelse(is.na(numCasos), 0L, numCasos),
    ano = YEAR_START + (id_mes - 1L) %/% 12L
  ) |>
  left_join(populacao_df, by = c('codMunRes6', 'codMunRes', 'ano')) |>
  select(codMunRes6, codMunRes, id_mes, numCasos, populacao, tipo)


falciparum_df <- panel |>
  filter(tipo == 'falciparum') |>
  select(-tipo)

vivax_df <- panel |>
  filter(tipo == 'vivax') |>
  select(-tipo)

stopifnot(nrow(falciparum_df) == nrow(vivax_df))

write_csv(falciparum_df, 'data/main_data/falciparum_df.csv')
write_csv(vivax_df,      'data/main_data/vivax_df.csv')

message(sprintf(
  '[done] falciparum_df.csv: %s linhas | vivax_df.csv: %s linhas',
  nrow(falciparum_df), nrow(vivax_df)
))
message(sprintf(
  '[done] total de casos no período %d-%d -> falciparum: %s | vivax: %s',
  YEAR_START, YEAR_END,
  sum(falciparum_df$numCasos), sum(vivax_df$numCasos)
))
