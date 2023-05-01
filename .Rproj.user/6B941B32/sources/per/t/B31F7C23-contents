library(readr)
library(INLA)
library(dplyr)

#preparing data------------------------------------------------------------
#path
micro_path = 'outputs/micro_map.graph'

#read data
micro_v = read_csv('data/output_data/micro_reg_v_df.csv')

#spatial data
micro_spatial = read_csv('data/spatial_data/micro_map.csv')

#adjacency matrix
image(inla.graph2matrix(inla.read.graph(micro_path)), xlab = '', ylab = '')

#creating id area
micro_v$idArea = pmatch(
  micro_v$codMicroRes,
  micro_spatial$code_micro,
  duplicates.ok = T
)

micro_v$idArea2 = micro_v$idArea

#creating id interaction (between area and time)
micro_v$idInteraction = as.numeric(interaction(micro_v$idArea, 
                                               micro_v$idMes))

real_rates_all = micro_v$numCasos*100000/micro_v$populacao
real_rates_test = real_rates_all[(20545 - 3852: 20544)]

#formulas---------------------------------------------------------------
formula2 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) + 
  f(ano, model = 'rw1', constr = T) +
  f(idArea, model = 'bym2', graph = micro_path) +
  f(idMes, model = 'rw1') + 
  rhum + temp

formula3 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) + 
  f(ano, model = 'rw1', constr = T) +
  f(idArea, model = 'bym2', graph = micro_path) +
  f(idMes, model = 'rw1') +
  f(idInteraction, model = 'iid')

formula4 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) + 
  f(ano, model = 'rw1', constr = T) +
  f(idArea, model = 'bym2', graph = micro_path) +
  f(idMes, model = 'rw1') +
  f(idInteraction, model = 'iid') +
  rhum + temp

#best vivax models-------------------------------------------------------
#bell
bell_fit3 = inla(
  formula = formula3, family = 'bell', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#bell predicts
bell_fit3_rate_all = bell_fit3$summary.fitted.values$mode*
  100000/micro_v$populacao
bell_rate_test = bell_fit3_rate_all[(20545 - 3852: 20544)]

#poisson
poisson_fit3 = inla(
  formula = formula3, family = 'poisson', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#poisson predicts
poisson_fit3_rate_all = poisson_fit3$summary.fitted.values$mode*
  100000/micro_v$populacao
poi_rate_test = poisson_fit3_rate_all[(20545 - 3852: 20544)]

#nbinomial
nbinomial_fit3 = inla(
  formula = formula3, family = 'nbinomial', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#nbinomial predicts
nbinomial_fit3_rate_all = nbinomial_fit3$summary.fitted.values$mode*
  100000/micro_v$populacao
nbinomial_rate_test = nbinomial_fit3_rate_all[(20545 - 3852: 20544)]

#zeroinflatedpoisson
zeroinflatedpoisson1_fit2 = inla(
  formula = formula2, family = 'zeroinflatedpoisson1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#zip predicts
zero_poi_fit3_rate_all = zeroinflatedpoisson1_fit2$summary.fitted.values$mode*
  100000/micro_v$populacao
zero_poi_rate_test = zero_poi_fit3_rate_all[(20545 - 3852: 20544)]

#zeroinflatednbinomial
zeroinflatednbinomial1_fit4 = inla(
  formula = formula4, family = 'zeroinflatednbinomial1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#zinb predicts
zinb_fit4_rate_all = zeroinflatednbinomial1_fit4$summary.fitted.values$mode*
  100000/micro_v$populacao
zero_nbinomial_rate_test = zinb_fit4_rate_all[(20545 - 3852: 20544)]

#tables of errors----------------------------------------------------------
test_errors_vivax = dplyr::tibble(
  dist = c('bell', 'poisson', 'nbinomial', 'zero_poisson', 
           'zero_nbinomial'),
  
  mbe = c(
    mbe(real_rates_test, bell_rate_test),
    mbe(real_rates_test, poi_rate_test),
    mbe(real_rates_test, nbinomial_rate_test),
    mbe(real_rates_test, zero_poi_rate_test),
    mbe(real_rates_test, zero_nbinomial_rate_test)
  ),
  
  nrmse = c(
    nrmse(real_rates_test, bell_rate_test),
    nrmse(real_rates_test, poi_rate_test),
    nrmse(real_rates_test, nbinomial_rate_test),
    nrmse(real_rates_test, zero_poi_rate_test),
    nrmse(real_rates_test, zero_nbinomial_rate_test)
  ),
  
  rae = c(
    rae(real_rates_test, bell_rate_test),
    rae(real_rates_test, poi_rate_test),
    rae(real_rates_test, nbinomial_rate_test),
    rae(real_rates_test, zero_poi_rate_test),
    rae(real_rates_test, zero_nbinomial_rate_test)
  ),
  
  rmsle = c(
    rmsle(real_rates_test, bell_rate_test),
    rmsle(real_rates_test, poi_rate_test),
    rmsle(real_rates_test, nbinomial_rate_test),
    rmsle(real_rates_test, zero_poi_rate_test),
    rmsle(real_rates_test, zero_nbinomial_rate_test)
  ),
  
  rse = c(
    rse(real_rates_test, bell_rate_test),
    rse(real_rates_test, poi_rate_test),
    rse(real_rates_test, nbinomial_rate_test),
    rse(real_rates_test, zero_poi_rate_test),
    rse(real_rates_test, zero_nbinomial_rate_test)
  ),
  
  cor = c(
    cor(real_rates_test, bell_rate_test),
    cor(real_rates_test, poi_rate_test),
    cor(real_rates_test, nbinomial_rate_test),
    cor(real_rates_test, zero_poi_rate_test),
    cor(real_rates_test, zero_nbinomial_rate_test)
  )
  
)

#write results
test_errors_vivax |> View()
test_errors_vivax |> write_csv('results/test_metrics_microrregion_vivax.csv')

micro_v$preds = bell_fit3_rate_all
micro_v$real = real_rates_all

micro_v |>
  select(
    codUF, siglaUF, codMicroRes, nomeMicroRes, ano, mes,
    real, preds
  ) |> write_csv('results/preds_microrregion_vivax_df.csv')
