library(readr)
library(INLA)

am_path = 'outputs/am_map.graph'

am_v = read_csv('data/output_data/am_v_df.csv')

am_spatial = read_csv('data/spatial_data/am_map.csv')

image(inla.graph2matrix(inla.read.graph(am_path)), xlab = '', ylab = '')

am_v$idArea = pmatch(
  am_v$codMunRes,
  am_spatial$code_muni,
  duplicates.ok = T
)

am_v$idArea2 = am_v$idArea

am_v$idInteraction = as.numeric(interaction(am_v$idArea, 
                                               am_v$idMes))

real_rates_all = am_v$numCasos*100000/am_v$populacao
test_idx = which(am_v$ano >= 2016)
real_rates_test = real_rates_all[test_idx]

formula3 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) + 
  f(ano, model = 'rw1', constr = T) +
  f(idArea, model = 'bym2', graph = am_path) +
  f(idMes, model = 'rw1') +
  f(idInteraction, model = 'iid') + offset(log(populacao))

bell_fit = inla(
  formula = formula3, family = 'bell', data = am_v,
  working.directory = tempdir(),
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

bell_fit |> summary()  
hist(bell_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

bell_rate_all = bell_fit$summary.fitted.values$mode*100000/am_v$populacao
bell_rate_test = bell_rate_all[test_idx]


test_error = tibble(
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

test_error |> View()
test_error |> write_csv('results/test_metrics_am_vivax.csv')

plot(bell_rate_test, real_rates_test)

am_v = am_v |>
  dplyr::mutate(bell = bell_rate_all,
                real = real_rates_all)

am_v |>
  select(
    codUF, siglaUF, codMunRes, nomeMunRes, ano, mes,
    real, bell
  ) |> write_csv('results/preds_am_vivax_df.csv')
