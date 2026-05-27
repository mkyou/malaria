library(readr)
library(INLA)
library(dplyr)

source('scripts/loss_functions.R')
inla.setOption(num.threads = '2:1')
inla.setOption(inla.mode = 'compact')

iid_hyper <- list(prec = list(prior = 'pc.prec', param = c(1, 0.01)))

micro_path = 'outputs/micro_map.graph'

micro_f = read_csv('data/output_data/micro_reg_f_df.csv')

micro_spatial = read_csv('data/spatial_data/micro_map.csv')

image(inla.graph2matrix(inla.read.graph(micro_path)), xlab = '', ylab = '')

micro_f$idArea = pmatch(
  micro_f$codMicroRes,
  micro_spatial$code_micro,
  duplicates.ok = T
)

micro_f$idArea2 = micro_f$idArea

micro_f$idInteraction = as.numeric(interaction(micro_f$idArea, 
                                               micro_f$idMes))

real_rates_all = micro_f$numCasos*100000/micro_f$populacao
test_idx = which(micro_f$ano >= 2016)
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
  f(idInteraction, model = 'iid', hyper = iid_hyper) +
  rhum + temp + offset(log(populacao))

bell_fit3 = inla(
  formula = formula3, family = 'bell', data = micro_f,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

bell_fit3_rate_all = bell_fit3$summary.fitted.values$mode*
  100000/micro_f$populacao
bell_rate_test = bell_fit3_rate_all[test_idx]

poisson_fit2 = inla(
  formula = formula2, family = 'poisson', data = micro_f,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

poisson_fit2_rate_all = poisson_fit2$summary.fitted.values$mode*
  100000/micro_f$populacao
poi_rate_test = poisson_fit2_rate_all[test_idx]

nbinomial_fit4 = inla(
  formula = formula4, family = 'nbinomial', data = micro_f,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  control.inla = list(
    strategy = 'adaptive',
    control.vb = list(emergency = 100)
  ),
  safe = TRUE,
  verbose = F
)

nbinomial_fit4_rate_all = nbinomial_fit4$summary.fitted.values$mode*
  100000/micro_f$populacao
nbinomial_rate_test = nbinomial_fit4_rate_all[test_idx]

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

test_errors_falciparum |>
  write_csv('results/test_metrics_microrregion_falciparum.csv')

micro_f$bell_preds = bell_fit3_rate_all
micro_f$nbinomial_preds = nbinomial_fit4_rate_all
micro_f$poisson_preds = poisson_fit2_rate_all
micro_f$real = real_rates_all

micro_f |>
  select(
    codUF, siglaUF, codMicroRes, nomeMicroRes, ano, mes,
    real, bell_preds, nbinomial_preds, poisson_preds
  ) |> write_csv('results/preds_microrregion_falciparum_df.csv')


