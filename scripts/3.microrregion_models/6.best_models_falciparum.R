library(readr)
library(INLA)
library(dplyr)

#preparing data------------------------------------------------------------
#path
micro_path = 'outputs/micro_map.graph'

#read data
micro_f = read_csv('data/output_data/micro_reg_f_df.csv')

#spatial data
micro_spatial = read_csv('data/spatial_data/micro_map.csv')

#adjacency matrix
image(inla.graph2matrix(inla.read.graph(micro_path)), xlab = '', ylab = '')

#creating id area
micro_f$idArea = pmatch(
  micro_f$codMicroRes,
  micro_spatial$code_micro,
  duplicates.ok = T
)

micro_f$idArea2 = micro_f$idArea

#creating id interaction (between area and time)
micro_f$idInteraction = as.numeric(interaction(micro_f$idArea, 
                                               micro_f$idMes))

real_rates_all = micro_f$numCasos*1000/micro_f$populacao
real_rates_test = real_rates_all[(20545 - 3852: 20544)]

#formulas---------------------------------------------------------------
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

#best vivax models-------------------------------------------------------
#bell
bell_fit3 = inla(
  formula = formula3, family = 'bell', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#bell predicts
bell_fit3_rate_all = bell_fit3$summary.fitted.values$mode*
  1000/micro_f$populacao
bell_rate_test = bell_fit3_rate_all[(20545 - 3852: 20544)]

#poisson
poisson_fit3 = inla(
  formula = formula3, family = 'poisson', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#poisson predicts
poisson_fit3_rate_all = poisson_fit3$summary.fitted.values$mode*
  1000/micro_f$populacao
poi_rate_test = poisson_fit3_rate_all[(20545 - 3852: 20544)]

#nbinomial
nbinomial_fit4 = inla(
  formula = formula4, family = 'nbinomial', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#nbinomial predicts
nbinomial_fit4_rate_all = nbinomial_fit4$summary.fitted.values$mode*
  1000/micro_f$populacao
nbinomial_rate_test = nbinomial_fit4_rate_all[(20545 - 3852: 20544)]

#tables of errors----------------------------------------------------------
test_errors_falciparum = dplyr::tibble(
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

#write results
test_errors_falciparum |> View()
test_errors_falciparum |> 
  write_csv('results/test_metrics_microrregion_falciparum.csv')

micro_f$preds = poisson_fit3_rate_all
micro_f$real = real_rates_all

micro_f |>
  select(
    codUF, siglaUF, codMicroRes, nomeMicroRes, ano, mes,
    real, preds
  ) |> write_csv('results/preds_microrregion_falciparum_df.csv')


