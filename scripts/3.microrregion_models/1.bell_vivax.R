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

formula4 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) +
  f(ano, model = 'rw1', constr = T) +
  f(idArea, model = 'bym2', graph = micro_path) +
  f(idMes, model = 'rw1') +
  f(idInteraction, model = 'iid') +
  rhum + temp + offset(log(populacao))

bell_fit1 = inla(
  formula = formula1, family = 'bell', data = micro_v,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

bell_fit1 |> summary()
hist(bell_fit1$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

bell_fit2 = inla(
  formula = formula2, family = 'bell', data = micro_v,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

bell_fit2 |> summary()
hist(bell_fit2$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

bell_fit3 = inla(
  formula = formula3, family = 'bell', data = micro_v,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

bell_fit3 |> summary()
hist(bell_fit3$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

bell_fit4 = inla(
  formula = formula4, family = 'bell', data = micro_v,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

bell_fit4 |> summary()
hist(bell_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#best model by DIC/WAIC: fit3
bell_rate_all = bell_fit3$summary.fitted.values$mode*
  100000/micro_v$populacao
bell_rate_test = bell_rate_all[test_idx]

tibble(
  dist = 'bell',
  mbe = c(
    mbe(real_rates_test, bell_rate_test)
  ),
  nrmse = c(
    nrmse(real_rates_test, bell_rate_test)
  ),
  rae = c(
    rae(real_rates_test, bell_rate_test)
  ),
  rmsle = c(
    rmsle(real_rates_test, bell_rate_test)
  ),
  rse = c(
    rse(real_rates_test, bell_rate_test)
  ),
  cor = c(
    cor(real_rates_test, bell_rate_test)
  )
)

plot(bell_rate_test, real_rates_test)
