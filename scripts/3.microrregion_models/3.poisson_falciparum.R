library(readr)
library(INLA)
library(dplyr)

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


formula1 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) +
  f(ano, model = 'rw1', constr = T) +
  f(idArea, model = 'bym2', graph = micro_path) +
  f(idMes, model = 'rw1') +
  offset(log(populacao))

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

#formula4 drops rhum since it was not significant in formula2
formula4 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) +
  f(ano, model = 'rw1', constr = T) +
  f(idArea, model = 'bym2', graph = micro_path) +
  f(idMes, model = 'rw1') +
  f(idInteraction, model = 'iid') +
  temp + offset(log(populacao))

poisson_fit1 = inla(
  formula = formula1, family = 'poisson', data = micro_f,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

poisson_fit1 |> summary()
hist(poisson_fit1$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

poisson_fit2 = inla(
  formula = formula2, family = 'poisson', data = micro_f,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

poisson_fit2 |> summary()
hist(poisson_fit2$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

poisson_fit3 = inla(
  formula = formula3, family = 'poisson', data = micro_f,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

poisson_fit3 |> summary()
hist(poisson_fit3$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

poisson_fit4 = inla(
  formula = formula4, family = 'poisson', data = micro_f,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

poisson_fit4 |> summary()
hist(poisson_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#best model by DIC/WAIC: fit3
poisson_rate_all = poisson_fit3$summary.fitted.values$mode*
  100000/micro_f$populacao
poisson_rate_test = poisson_rate_all[test_idx]

tibble(
  dist = 'poisson',
  mbe = c(
    mbe(real_rates_test, poisson_rate_test)
  ),
  nrmse = c(
    nrmse(real_rates_test, poisson_rate_test)
  ),
  rae = c(
    rae(real_rates_test, poisson_rate_test)
  ),
  rmsle = c(
    rmsle(real_rates_test, poisson_rate_test)
  ),
  rse = c(
    rse(real_rates_test, poisson_rate_test)
  ),
  cor = c(
    cor(real_rates_test, poisson_rate_test)
  )
)

plot(poisson_rate_test, real_rates_test)
