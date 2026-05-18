library(readr)
  library(INLA)
  library(dplyr)
  
  micro_path = 'outputs/micro_map.graph'
  
  micro_v = read_csv('data/output_data/micro_reg_v_df.csv')
  
  micro_spatial = read_csv('data/spatial_data/micro_map.csv')
  
  
  image(inla.graph2matrix(inla.read.graph(micro_path)), xlab = '', ylab = '')
  
  micro_v$idArea = pmatch(
    micro_v$codMicroRes,
    micro_spatial$code_micro,
    duplicates.ok = T
  )
  
  micro_v$idArea2 = micro_v$idArea
  
  micro_v$idInteraction = as.numeric(interaction(micro_v$idArea, 
                                                 micro_v$idMes))
  
  real_rates_all = micro_v$numCasos*100000/micro_v$populacao
  test_idx = which(micro_v$ano >= 2016)
real_rates_test = real_rates_all[test_idx]
  
  formula2 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) + 
    f(ano, model = 'rw1', constr = T) +
    f(idArea, model = 'bym2', graph = micro_path) +
    f(idMes, model = 'rw1') + 
    rhum + temp + offset(log(populacao))
  
  formula3 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) + 
    f(ano, model = 'rw1', constr = T) +
    f(idArea, model = 'bym2', graph = micro_path) +
    f(idMes, model = 'rw1') +
    f(idInteraction, model = 'iid') + offset(log(populacao))
  
  formula4 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) + 
    f(ano, model = 'rw1', constr = T) +
    f(idArea, model = 'bym2', graph = micro_path) +
    f(idMes, model = 'rw1') +
    f(idInteraction, model = 'iid') +
    rhum + temp + offset(log(populacao))
  
  bell_fit3 = inla(
    formula = formula3, family = 'bell', data = micro_v,
    working.directory = tempdir(),
    control.predictor = list(compute = T, link = 1),
    control.compute = list(dic = T, waic = T, cpo = T),
    verbose = F
  )
  
  bell_fit3_rate_all = bell_fit3$summary.fitted.values$mode*
    100000/micro_v$populacao
  bell_rate_test = bell_fit3_rate_all[test_idx]
  
  poisson_fit3 = inla(
    formula = formula3, family = 'poisson', data = micro_v,
    working.directory = tempdir(),
    control.predictor = list(compute = T, link = 1),
    control.compute = list(dic = T, waic = T, cpo = T),
    verbose = F
  )
  
  poisson_fit3_rate_all = poisson_fit3$summary.fitted.values$mode*
    100000/micro_v$populacao
  poi_rate_test = poisson_fit3_rate_all[test_idx]
  
  nbinomial_fit4 = inla(
    formula = formula4, family = 'nbinomial', data = micro_v,
    working.directory = tempdir(),
    control.predictor = list(compute = T, link = 1),
    control.compute = list(dic = T, waic = T, cpo = T),
    verbose = F
  )
  
  nbinomial_fit4_rate_all = nbinomial_fit4$summary.fitted.values$mode*
    100000/micro_v$populacao
  nbinomial_rate_test = nbinomial_fit4_rate_all[test_idx]
  
  
  test_errors_vivax = dplyr::tibble(
    dist = c('bell', 'poisson', 'nbinomial'),
    
    mbe = c(
      mbe(real_rates_test, bell_rate_test),
      mbe(real_rates_test, poi_rate_test),
      mbe(real_rates_test, nbinomial_rate_test)
    ),
    
    nrmse = c(
      nrmse(real_rates_test, bell_rate_test),
      nrmse(real_rates_test, poi_rate_test),
      nrmse(real_rates_test, nbinomial_rate_test)
    ),
    
    rae = c(
      rae(real_rates_test, bell_rate_test),
      rae(real_rates_test, poi_rate_test),
      rae(real_rates_test, nbinomial_rate_test)
    ),
    
    rmsle = c(
      rmsle(real_rates_test, bell_rate_test),
      rmsle(real_rates_test, poi_rate_test),
      rmsle(real_rates_test, nbinomial_rate_test)
    ),
    
    rse = c(
      rse(real_rates_test, bell_rate_test),
      rse(real_rates_test, poi_rate_test),
      rse(real_rates_test, nbinomial_rate_test)
    ),
    
    cor = c(
      cor(real_rates_test, bell_rate_test),
      cor(real_rates_test, poi_rate_test),
      cor(real_rates_test, nbinomial_rate_test)
    )
    
  )
  
  test_errors_vivax |> View()
  test_errors_vivax |> write_csv('results/test_metrics_microrregion_vivax.csv')
  
  micro_v$bell_preds = bell_fit3_rate_all
  micro_v$nbinomial_preds = nbinomial_fit4_rate_all
  micro_v$poisson_preds = poisson_fit3_rate_all
  micro_v$real = real_rates_all
  
  micro_v |>
    select(
      codUF, siglaUF, codMicroRes, nomeMicroRes, ano, mes,
      real, bell_preds, nbinomial_preds, poisson_preds
    ) |> write_csv('results/preds_microrregion_vivax_df.csv')
