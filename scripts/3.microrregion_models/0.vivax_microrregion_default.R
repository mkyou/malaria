library(readr)
library(INLA)

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

real_rates_all = micro_v$numCasos*100000/micro_v$populacao
real_rates_test = real_rates_all[(20545 - 3852: 20544)]

#formulas------------------------------------------------------------------
formula1 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) + 
  f(ano, model = 'rw1', constr = T) + 
  f(idArea, model = 'bym2', graph = micro_path) +
  f(idMes, model = 'rw1') +
  offset(log(populacao))

#poisson-------------------------------------------------------------------
poi_fit = inla(
  formula = formula1, family = 'poisson', data = micro_v,
  working.directory = 'D:/INLA/', 
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

poi_fit |> summary()  
hist(poi_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#poisson predicted rate all and test
poi_rate_all = poi_fit$summary.fitted.values$mode*100000/micro_v$populacao
poi_rate_test = poi_rate_all[(20545 - 3852: 20544)]


#bell----------------------------------------------------------------------
bell_fit = inla(
  formula = formula1, family = 'bell', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

bell_fit |> summary()  
hist(bell_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#bell predicted rate all and test
bell_rate_all = bell_fit$summary.fitted.values$mode*
  100000/micro_v$populacao
bell_rate_test = bell_rate_all[(20545 - 3852: 20544)]

#nbinomial-----------------------------------------------------------------
nbinomial_fit = inla(
  formula = formula1, family = 'nbinomial', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

nbinomial_fit |> summary()  
hist(nbinomial_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

nbinomial_rate_all = nbinomial_fit$summary.fitted.values$mode*
  100000/micro_v$populacao
nbinomial_rate_test = nbinomial_rate_all[(20545 - 3852: 20544)]

#zero poisson--------------------------------------------------------------
zero_poi_fit = inla(
  formula = formula1, family = 'zeroinflatedpoisson1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

zero_poi_fit |> summary()  
hist(zero_poi_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

zero_poi_rate_all = zero_poi_fit$summary.fitted.values$mode*
  100000/micro_v$populacao
zero_poi_rate_test = zero_poi_rate_all[(20545 - 3852: 20544)]

#zero nbinomial------------------------------------------------------------
zero_nbinomial_fit = inla(
  formula = formula1, family = 'zeroinflatednbinomial1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

zero_nbinomial_fit |> summary()  
hist(zero_nbinomial_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

zero_nbinomial_rate_all = zero_nbinomial_fit$summary.fitted.values$mode*
  100000/micro_v$populacao
zero_nbinomial_rate_test = zero_nbinomial_rate_all[(20545 - 3852: 20544)]


#tables of errors----------------------------------------------------------
test_errors = dplyr::tibble(
  dist = c('bell', 'poisson', 'nbinomial', 'zero_poisson', 
           'zero_nbinomial'),
  
  mae = c(
    mae(real_rates_test, bell_rate_test),
    mae(real_rates_test, poi_rate_test),
    mae(real_rates_test, nbinomial_rate_test),
    mae(real_rates_test, zero_poi_rate_test),
    mae(real_rates_test, zero_nbinomial_rate_test)
  ),
  
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
  
  rmse = c(
    rmse(real_rates_test, bell_rate_test),
    rmse(real_rates_test, poi_rate_test),
    rmse(real_rates_test, nbinomial_rate_test),
    rmse(real_rates_test, zero_poi_rate_test),
    rmse(real_rates_test, zero_nbinomial_rate_test)
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

#train errors
train_errors = dplyr::tibble(
  dist = c('bell', 'poisson', 'nbinomial', 'zero_poisson', 
           'zero_nbinomial'),
  
  mae = c(
    mae(real_rates_all, bell_rate_all),
    mae(real_rates_all, poi_rate_all),
    mae(real_rates_all, nbinomial_rate_all),
    mae(real_rates_all, zero_poi_rate_all),
    mae(real_rates_all, zero_nbinomial_rate_all)
  ),
  
  mbe = c(
    mbe(real_rates_all, bell_rate_all),
    mbe(real_rates_all, poi_rate_all),
    mbe(real_rates_all, nbinomial_rate_all),
    mbe(real_rates_all, zero_poi_rate_all),
    mbe(real_rates_all, zero_nbinomial_rate_all)
  ),
  
  nrmse = c(
    nrmse(real_rates_all, bell_rate_all),
    nrmse(real_rates_all, poi_rate_all),
    nrmse(real_rates_all, nbinomial_rate_all),
    nrmse(real_rates_all, zero_poi_rate_all),
    nrmse(real_rates_all, zero_nbinomial_rate_all)
  ),
  
  rae = c(
    rae(real_rates_all, bell_rate_all),
    rae(real_rates_all, poi_rate_all),
    rae(real_rates_all, nbinomial_rate_all),
    rae(real_rates_all, zero_poi_rate_all),
    rae(real_rates_all, zero_nbinomial_rate_all)
  ),
  
  rmse = c(
    rmse(real_rates_all, bell_rate_all),
    rmse(real_rates_all, poi_rate_all),
    rmse(real_rates_all, nbinomial_rate_all),
    rmse(real_rates_all, zero_poi_rate_all),
    rmse(real_rates_all, zero_nbinomial_rate_all)
  ),
  
  rmsle = c(
    rmsle(real_rates_all, bell_rate_all),
    rmsle(real_rates_all, poi_rate_all),
    rmsle(real_rates_all, nbinomial_rate_all),
    rmsle(real_rates_all, zero_poi_rate_all),
    rmsle(real_rates_all, zero_nbinomial_rate_all)
  ),
  
  rse = c(
    rse(real_rates_all, bell_rate_all),
    rse(real_rates_all, poi_rate_all),
    rse(real_rates_all, nbinomial_rate_all),
    rse(real_rates_all, zero_poi_rate_all),
    rse(real_rates_all, zero_nbinomial_rate_all)
  ),
  
  cor = c(
    cor(real_rates_all, bell_rate_all),
    cor(real_rates_all, poi_rate_all),
    cor(real_rates_all, nbinomial_rate_all),
    cor(real_rates_all, zero_poi_rate_all),
    cor(real_rates_all, zero_nbinomial_rate_all)
  )
  
)


train_errors |> View()
test_errors |> View()

#create columns with predictions-------------------------------------------
micro_v = micro_v |>
  dplyr::mutate(bell = bell_rate_all,
                poisson = poi_rate_all,
                nbinomial = nbinomial_rate_all,
                zero_poisson = zero_poi_rate_all,
                zero_nbinomial = zero_nbinomial_rate_all,
                real_rate = real_rates_all)

#results per state---------------------------------------------------------
micro_v_sum = micro_v |> dplyr::filter(ano >= 2016) |>
  dplyr::group_by(mes, ano, codUF) |>
  dplyr::summarise(real_rates_all = sum(real_rate),
                   bell = sum(bell),
                   poisson = sum(poisson),
                   nbinomial = sum(nbinomial),
                   zero_poisson = sum(zero_poisson),
                   zero_nbinomial = sum(zero_nbinomial))

#error per state check
state_errors = dplyr::tibble(
  dist = c('bell', 'poisson', 'nbinomial', 'zero_poisson', 
           'zero_nbinomial'),
  
  mae = c(
    mae(micro_v_sum$real_rates_all, micro_v_sum$bell),
    mae(micro_v_sum$real_rates_all, micro_v_sum$poisson),
    mae(micro_v_sum$real_rates_all, micro_v_sum$nbinomial),
    mae(micro_v_sum$real_rates_all, micro_v_sum$zero_poisson),
    mae(micro_v_sum$real_rates_all, micro_v_sum$zero_nbinomial)
  ),
  
  mbe = c(
    mbe(micro_v_sum$real_rates_all, micro_v_sum$bell),
    mbe(micro_v_sum$real_rates_all, micro_v_sum$poisson),
    mbe(micro_v_sum$real_rates_all, micro_v_sum$nbinomial),
    mbe(micro_v_sum$real_rates_all, micro_v_sum$zero_poisson),
    mbe(micro_v_sum$real_rates_all, micro_v_sum$zero_nbinomial)
  ),
  
  nrmse = c(
    nrmse(micro_v_sum$real_rates_all, micro_v_sum$bell),
    nrmse(micro_v_sum$real_rates_all, micro_v_sum$poisson),
    nrmse(micro_v_sum$real_rates_all, micro_v_sum$nbinomial),
    nrmse(micro_v_sum$real_rates_all, micro_v_sum$zero_poisson),
    nrmse(micro_v_sum$real_rates_all, micro_v_sum$zero_nbinomial)
  ),
  
  rae = c(
    rae(micro_v_sum$real_rates_all, micro_v_sum$bell),
    rae(micro_v_sum$real_rates_all, micro_v_sum$poisson),
    rae(micro_v_sum$real_rates_all, micro_v_sum$nbinomial),
    rae(micro_v_sum$real_rates_all, micro_v_sum$zero_poisson),
    rae(micro_v_sum$real_rates_all, micro_v_sum$zero_nbinomial)
  ),
  
  rmse = c(
    rmse(micro_v_sum$real_rates_all, micro_v_sum$bell),
    rmse(micro_v_sum$real_rates_all, micro_v_sum$poisson),
    rmse(micro_v_sum$real_rates_all, micro_v_sum$nbinomial),
    rmse(micro_v_sum$real_rates_all, micro_v_sum$zero_poisson),
    rmse(micro_v_sum$real_rates_all, micro_v_sum$zero_nbinomial)
  ),
  
  rmsle = c(
    rmsle(micro_v_sum$real_rates_all, micro_v_sum$bell),
    rmsle(micro_v_sum$real_rates_all, micro_v_sum$poisson),
    rmsle(micro_v_sum$real_rates_all, micro_v_sum$nbinomial),
    rmsle(micro_v_sum$real_rates_all, micro_v_sum$zero_poisson),
    rmsle(micro_v_sum$real_rates_all, micro_v_sum$zero_nbinomial)
  ),
  
  rse = c(
    rse(micro_v_sum$real_rates_all, micro_v_sum$bell),
    rse(micro_v_sum$real_rates_all, micro_v_sum$poisson),
    rse(micro_v_sum$real_rates_all, micro_v_sum$nbinomial),
    rse(micro_v_sum$real_rates_all, micro_v_sum$zero_poisson),
    rse(micro_v_sum$real_rates_all, micro_v_sum$zero_nbinomial)
  ),
  
  cor = c(
    cor(micro_v_sum$real_rates_all, micro_v_sum$bell),
    cor(micro_v_sum$real_rates_all, micro_v_sum$poisson),
    cor(micro_v_sum$real_rates_all, micro_v_sum$nbinomial),
    cor(micro_v_sum$real_rates_all, micro_v_sum$zero_poisson),
    cor(micro_v_sum$real_rates_all, micro_v_sum$zero_nbinomial)
  )
  
)

state_errors |> View()
