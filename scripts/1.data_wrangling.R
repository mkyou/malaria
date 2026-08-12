library(dplyr)
library(readr)

falciparum_df = read_csv('data/main_data/falciparum_df.csv')
vivax_df = read_csv('data/main_data/vivax_df.csv')

falciparum_df = falciparum_df |>
  mutate(ano = 2002 + ceiling(id_mes/12))

vivax_df = vivax_df |>
  mutate(ano = 2002 + ceiling(id_mes/12))

cities_df = read_csv('data/support_data/municipios_codigos.csv') |>
  select(cod_mun, nome_mun, cod_micro_reg, nome_micro_reg, 
         cod_UF, sigla_UF)

falciparum_df = falciparum_df |> inner_join(
  cities_df, by = c('codMunRes' = 'cod_mun')
)

vivax_df = vivax_df |> inner_join(
  cities_df, by = c('codMunRes' = 'cod_mun')
)

rm(cities_df)

vivax_df = vivax_df |>
  rename(
    idMes = id_mes,
    nomeMunRes = nome_mun,
    codMicroRes = cod_micro_reg,
    nomeMicroRes = nome_micro_reg,
    codUF = cod_UF,
    siglaUF = sigla_UF
  )

falciparum_df = falciparum_df |>
  rename(
    idMes = id_mes,
    nomeMunRes = nome_mun,
    codMicroRes = cod_micro_reg,
    nomeMicroRes = nome_micro_reg,
    codUF = cod_UF,
    siglaUF = sigla_UF
  )

vivax_df = vivax_df |>
  mutate(mes = idMes - 12 * (ceiling(idMes/12) - 1))

falciparum_df = falciparum_df |>
  mutate(mes = idMes - 12 * (ceiling(idMes/12) - 1))

#creating y columns (NA when I want to predict)
vivax_df = vivax_df |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

falciparum_df = falciparum_df |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

vivax_df |> select(siglaUF) |> unique()

vivax_df |> filter(siglaUF == 'PA') |> 
  write_csv('data/output_data/pa_v_df.csv')

vivax_df |> filter(siglaUF == 'TO') |> 
  write_csv('data/output_data/to_v_df.csv')

vivax_df |> filter(siglaUF == 'MA') |> 
  write_csv('data/output_data/ma_v_df.csv')

vivax_df |> filter(siglaUF == 'MT') |> 
  write_csv('data/output_data/mt_v_df.csv')

vivax_df |> filter(siglaUF == 'AC') |> 
  write_csv('data/output_data/ac_v_df.csv')

vivax_df |> filter(siglaUF == 'RO') |> 
  write_csv('data/output_data/ro_v_df.csv')

vivax_df |> filter(siglaUF == 'RR') |> 
  write_csv('data/output_data/rr_v_df.csv')

vivax_df |> filter(siglaUF == 'AM') |> 
  write_csv('data/output_data/am_v_df.csv')

vivax_df |> filter(siglaUF == 'AP') |> 
  write_csv('data/output_data/ap_v_df.csv')

falciparum_df |> filter(siglaUF == 'PA') |> 
  write_csv('data/output_data/pa_f_df.csv')

falciparum_df |> filter(siglaUF == 'TO') |> 
  write_csv('data/output_data/to_f_df.csv')

falciparum_df |> filter(siglaUF == 'MA') |> 
  write_csv('data/output_data/ma_f_df.csv')

falciparum_df |> filter(siglaUF == 'MT') |> 
  write_csv('data/output_data/mt_f_df.csv')

falciparum_df |> filter(siglaUF == 'AC') |> 
  write_csv('data/output_data/ac_f_df.csv')

falciparum_df |> filter(siglaUF == 'RO') |> 
  write_csv('data/output_data/ro_f_df.csv')

falciparum_df |> filter(siglaUF == 'RR') |> 
  write_csv('data/output_data/rr_f_df.csv')

falciparum_df |> filter(siglaUF == 'AM') |> 
  write_csv('data/output_data/am_f_df.csv')

falciparum_df |> filter(siglaUF == 'AP') |> 
  write_csv('data/output_data/ap_f_df.csv')

micro_reg_v = vivax_df |> group_by(codMicroRes, nomeMicroRes,
                                   idMes, mes, ano, codUF, siglaUF) |>
  summarise(
    populacao = sum(populacao, na.rm = T),
    numCasos = sum(numCasos, na.rm = T)
  )

micro_reg_f = falciparum_df |> group_by(codMicroRes, nomeMicroRes,
                                        idMes, mes, ano, codUF,
                                        siglaUF) |>
  summarise(
    populacao = sum(populacao, na.rm = T),
    numCasos = sum(numCasos, na.rm = T)
  )

# New/rebuilt covariates (deforestation, rainfall, temperature,
# relative humidity) -- see 0.download_data.R sections 4-6.
# All built directly at microregion (precip, temp, rhum) or state
# (deforestation) grain, so they join in here rather than at the
# municipality level above. Deforestation joins by (state, ano) --
# state, not national, since section 4 fetches a per-state breakdown.
deforestation_df = read_csv('data/support_data/deforestation_df.csv') |>
  select(siglaUF = state, ano, defor_lag2 = km2_lag2)

precip_df = read_csv('data/support_data/precip_df.csv')
temp_df = read_csv('data/support_data/temp_df.csv')
rhum_df = read_csv('data/support_data/rhum_df.csv')

micro_reg_v = micro_reg_v |>
  left_join(deforestation_df, by = c('siglaUF', 'ano')) |>
  left_join(precip_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(temp_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(rhum_df, by = c('codMicroRes', 'ano', 'mes'))

micro_reg_f = micro_reg_f |>
  left_join(deforestation_df, by = c('siglaUF', 'ano')) |>
  left_join(precip_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(temp_df, by = c('codMicroRes', 'ano', 'mes')) |>
  left_join(rhum_df, by = c('codMicroRes', 'ano', 'mes'))

micro_reg_v = micro_reg_v |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

micro_reg_f = micro_reg_f |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

micro_reg_v |> write_csv('data/output_data/micro_reg_v_df.csv')
micro_reg_f |> write_csv('data/output_data/micro_reg_f_df.csv')

rm(falciparum_df, vivax_df, micro_reg_v, micro_reg_f)
