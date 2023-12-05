#get spatial data


#union of falciparum and vivax info
df = read_csv('results/preds_microrregion_falciparum_df.csv') |>
  mutate(
    ano = as.numeric(ano),
    mes = as.numeric(mes),
    new_preds = new_poi_preds,
    new_difs = new_difs_poi,
    new_difs_rmsle = new_difs_rmsle_poi,
    type = 'Falciparum'
  ) |> 
  select(codMicroRes, nomeMicroRes, ano, mes, real, 
         difs_bell, difs_rmsle_bell, difs_nbinomial, difs_rmsle_nbinomial,
         difs_poisson, difs_rmsle_poisson, new_preds,
         new_difs, new_difs_rmsle, type) |>
  union_all(
    read_csv('results/preds_microrregion_vivax_df.csv') |>
      mutate(
        ano = as.numeric(ano),
        mes = as.numeric(mes),
        new_preds = new_bell_preds,
        new_difs = new_difs_bell,
        new_difs_rmsle = new_difs_rmsle_bell,
        type = 'Vivax'
      ) |> select(codMicroRes, nomeMicroRes, ano, mes, real, 
                  difs_bell, difs_rmsle_bell, difs_nbinomial, 
                  difs_rmsle_nbinomial,
                  difs_poisson, difs_rmsle_poisson, new_preds,
                  new_difs, new_difs_rmsle, type)
    )
  
#showing data
df |> head()

#make difs_rmsle columns into rows
errors_df = df |>
  select(codMicroRes, nomeMicroRes, ano, mes, difs_bell,
         difs_nbinomial, difs_poisson, new_difs, type) |>
  mutate(
    cod_micro_res = codMicroRes,
    nome_micro_res = nomeMicroRes,
    metric = 'Ordinary Error'
  ) |> 
  select(cod_micro_res, nome_micro_res, ano, mes, difs_bell,
         difs_nbinomial, difs_poisson, new_difs, type, metric) |>
  union_all(
    df |> 
      select(codMicroRes, nomeMicroRes, ano, mes, difs_rmsle_bell,
             difs_rmsle_nbinomial, difs_rmsle_poisson, new_difs_rmsle, type) |>
      mutate(
        cod_micro_res = codMicroRes,
        nome_micro_res = nomeMicroRes,
        difs_bell = difs_rmsle_bell,
        difs_nbinomial = difs_rmsle_nbinomial,
        difs_poisson = difs_rmsle_poisson,
        new_difs = new_difs_rmsle,
        metric = 'Root Squared Logarithmic Error'
      ) |>
      select(cod_micro_res, nome_micro_res, ano, mes, difs_bell,
             difs_nbinomial, difs_poisson, new_difs, type, metric)
  )

#showing
errors_df |> head()

#write
errors_df |> write_csv('results/general_errors.csv')
  
  

   
  