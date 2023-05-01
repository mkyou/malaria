library(dplyr)
library(readr)

#reading data
falciparum_df = read_csv('data/main_data/falciparum_df.csv')
vivax_df = read_csv('data/main_data/vivax_df.csv')

#creating year column
falciparum_df = falciparum_df |>
  mutate(ano = 2002 + ceiling(id_mes/12))

vivax_df = vivax_df |>
  mutate(ano = 2002 + ceiling(id_mes/12))

#get cities names and state columns
cities_df = read_csv('data/support_data/municipios_codigos.csv') |>
  select(cod_mun, nome_mun, cod_micro_reg, nome_micro_reg, 
         cod_UF, sigla_UF)

#falciparum
falciparum_df = falciparum_df |> inner_join(
  cities_df, by = c('codMunRes' = 'cod_mun')
)

#vivax
vivax_df = vivax_df |> inner_join(
  cities_df, by = c('codMunRes' = 'cod_mun')
)

#remove temp_df
rm(cities_df)

#renaming columns
vivax_df = vivax_df |>
  rename(
    idMes = id_mes,
    agriPecu = agri_pecu,
    turViaj = tur_viaj,
    gariMine = gari_mine,
    roadHuntwood = road_hunt_wood,
    nomeMunRes = nome_mun,
    codMicroRes = cod_micro_reg,
    nomeMicroRes = nome_micro_reg,
    codUF = cod_UF,
    siglaUF = sigla_UF
  )

falciparum_df = falciparum_df |>
  rename(
    idMes = id_mes,
    agriPecu = agri_pecu,
    turViaj = tur_viaj,
    gariMine = gari_mine,
    roadHuntwood = road_hunt_wood,
    nomeMunRes = nome_mun,
    codMicroRes = cod_micro_reg,
    nomeMicroRes = nome_micro_reg,
    codUF = cod_UF,
    siglaUF = sigla_UF
  )

#creating month columns
vivax_df = vivax_df |>
  mutate(mes = idMes - 12 * (ceiling(idMes/12) - 1))

falciparum_df = falciparum_df |>
  mutate(mes = idMes - 12 * (ceiling(idMes/12) - 1))

#creating y columns (NA when I want to predict)
vivax_df = vivax_df |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

falciparum_df = falciparum_df |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

#get covariates
vivax_df = vivax_df |> left_join(
  read_csv('data/support_data/rhum_df.csv'),
  by = c('codMunRes', 'ano', 'mes')
) |> left_join(
  read_csv('data/support_data/temp_df.csv'),
  by = c('codMunRes', 'ano', 'mes')
)

falciparum_df = falciparum_df |> left_join(
  read_csv('data/support_data/rhum_df.csv'),
  by = c('codMunRes', 'ano', 'mes')
) |> left_join(
  read_csv('data/support_data/temp_df.csv'),
  by = c('codMunRes', 'ano', 'mes')
)

#looking at states
vivax_df |> select(siglaUF) |> unique()

#creating state dfs
#vivax---------------------------------------------------------------------
#pa
vivax_df |> filter(siglaUF == 'PA') |> 
  write_csv('data/output_data/pa_v_df.csv')

#to
vivax_df |> filter(siglaUF == 'TO') |> 
  write_csv('data/output_data/to_v_df.csv')

#ma
vivax_df |> filter(siglaUF == 'MA') |> 
  write_csv('data/output_data/ma_v_df.csv')

#mt
vivax_df |> filter(siglaUF == 'MT') |> 
  write_csv('data/output_data/mt_v_df.csv')

#ac
vivax_df |> filter(siglaUF == 'AC') |> 
  write_csv('data/output_data/ac_v_df.csv')

#ro
vivax_df |> filter(siglaUF == 'RO') |> 
  write_csv('data/output_data/ro_v_df.csv')

#rr
vivax_df |> filter(siglaUF == 'RR') |> 
  write_csv('data/output_data/rr_v_df.csv')

#am
vivax_df |> filter(siglaUF == 'AM') |> 
  write_csv('data/output_data/am_v_df.csv')

#ap
vivax_df |> filter(siglaUF == 'AP') |> 
  write_csv('data/output_data/ap_v_df.csv')

#falciparum----------------------------------------------------------------
#pa
falciparum_df |> filter(siglaUF == 'PA') |> 
  write_csv('data/output_data/pa_f_df.csv')

#to
falciparum_df |> filter(siglaUF == 'TO') |> 
  write_csv('data/output_data/to_f_df.csv')

#ma
falciparum_df |> filter(siglaUF == 'MA') |> 
  write_csv('data/output_data/ma_f_df.csv')

#mt
falciparum_df |> filter(siglaUF == 'MT') |> 
  write_csv('data/output_data/mt_f_df.csv')

#ac
falciparum_df |> filter(siglaUF == 'AC') |> 
  write_csv('data/output_data/ac_f_df.csv')

#ro
falciparum_df |> filter(siglaUF == 'RO') |> 
  write_csv('data/output_data/ro_f_df.csv')

#rr
falciparum_df |> filter(siglaUF == 'RR') |> 
  write_csv('data/output_data/rr_f_df.csv')

#am
falciparum_df |> filter(siglaUF == 'AM') |> 
  write_csv('data/output_data/am_f_df.csv')

#ap
falciparum_df |> filter(siglaUF == 'AP') |> 
  write_csv('data/output_data/ap_f_df.csv')

#get data by microrregion-------------------------------------------------
micro_reg_v = vivax_df |> group_by(codMicroRes, nomeMicroRes,
                                   idMes, mes, ano, codUF, siglaUF) |>
  summarise(
    populacao = sum(populacao, na.rm = T),
    numCasos = sum(numCasos, na.rm = T),
    rhum = mean(rhum, na.rm = T),
    temp = mean(temp, na.rm = T)
  )

#falciparum
micro_reg_f = falciparum_df |> group_by(codMicroRes, nomeMicroRes,
                                        idMes, mes, ano, codUF, 
                                        siglaUF) |>
  summarise(
    populacao = sum(populacao, na.rm = T),
    numCasos = sum(numCasos, na.rm = T),
    rhum = mean(rhum, na.rm = T),
    temp = mean(temp, na.rm = T)
  )

#creating Y column
micro_reg_v = micro_reg_v |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

micro_reg_f = micro_reg_f |>
  mutate(Y = ifelse(ano >= 2016, NA, numCasos))

#write both
micro_reg_v |> write_csv('data/output_data/micro_reg_v_df.csv')
micro_reg_f |> write_csv('data/output_data/micro_reg_f_df.csv')

#remove data from R
rm(falciparum_df, vivax_df, micro_reg_v, micro_reg_f)
