library(readr)
library(INLA)

#preparing data------------------------------------------------------------
#path
am_path = 'outputs/am_map.graph'

#read data
am_v = read_csv('data/output_data/am_v_df.csv')

#spatial data
am_spatial = read_csv('data/spatial_data/am_map.csv')

#adjacency matrix
image(inla.graph2matrix(inla.read.graph(am_path)), xlab = '', ylab = '')

#creating id area
am_v$idArea = pmatch(
  am_v$codMunRes,
  am_spatial$code_muni,
  duplicates.ok = T
)

am_v$idArea2 = am_v$idArea

real_rates_all = am_v$numCasos*100000/am_v$populacao
real_rates_test = real_rates_all[(11905 - 2232: 11904)]

#formulas------------------------------------------------------------------
formula1 = Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) + 
  f(ano, model = 'rw1', constr = T) + 
  f(idArea, model = 'bym2', graph = am_path) +
  f(idMes, model = 'rw1') +
  offset(log(populacao))

#poisson-------------------------------------------------------------------
poi_fit = inla(
  formula = formula1, family = 'poisson', data = am_v,
  working.directory = 'D:/INLA/', 
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

poi_fit |> summary()  
hist(poi_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#poisson predicted rate all and test
poi_rate_all = poi_fit$summary.fitted.values$mode*100000/am_v$populacao
poi_rate_test = poi_rate_all[(11905 - 2232: 11904)]


#bell----------------------------------------------------------------------
bell_fit = inla(
  formula = formula1, family = 'bell', data = am_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

bell_fit |> summary()  
hist(bell_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#bell predicted rate all and test
bell_rate_all = bell_fit$summary.fitted.values$mode*100000/am_v$populacao
bell_rate_test = bell_rate_all[(11905 - 2232: 11904)]

#nbinomial-----------------------------------------------------------------
nbinomial_fit = inla(
  formula = formula1, family = 'nbinomial', data = am_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

nbinomial_fit |> summary()  
hist(nbinomial_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

nbinomial_rate_all = nbinomial_fit$summary.fitted.values$mode*
  100000/am_v$populacao
nbinomial_rate_test = nbinomial_rate_all[(11905 - 2232: 11904)]

#zero poisson--------------------------------------------------------------
zero_poi_fit = inla(
  formula = formula1, family = 'zeroinflatedpoisson1', data = am_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

zero_poi_fit |> summary()  
hist(zero_poi_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

zero_poi_rate_all = zero_poi_fit$summary.fitted.values$mode*
  100000/am_v$populacao
zero_poi_rate_test = zero_poi_rate_all[(11905 - 2232: 11904)]

#zero nbinomial------------------------------------------------------------
zero_nbinomial_fit = inla(
  formula = formula1, family = 'zeroinflatednbinomial1', data = am_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

zero_nbinomial_fit |> summary()  
hist(zero_nbinomial_fit$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

zero_nbinomial_rate_all = zero_nbinomial_fit$summary.fitted.values$mode*
  100000/am_v$populacao
zero_nbinomial_rate_test = zero_nbinomial_rate_all[(11905 - 2232: 11904)]


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
am_v = am_v |>
  dplyr::mutate(bell = bell_rate_all,
                poisson = poi_rate_all,
                nbinomial = nbinomial_rate_all,
                zero_poisson = zero_poi_rate_all,
                zero_nbinomial = zero_nbinomial_rate_all,
                real_rate = real_rates_all)

#results per state---------------------------------------------------------
am_v_sum = am_v |> dplyr::filter(ano >= 2016) |>
  dplyr::group_by(mes, ano) |>
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
    mae(am_v_sum$real_rates_all, am_v_sum$bell),
    mae(am_v_sum$real_rates_all, am_v_sum$poisson),
    mae(am_v_sum$real_rates_all, am_v_sum$nbinomial),
    mae(am_v_sum$real_rates_all, am_v_sum$zero_poisson),
    mae(am_v_sum$real_rates_all, am_v_sum$zero_nbinomial)
  ),
  
  mbe = c(
    mbe(am_v_sum$real_rates_all, am_v_sum$bell),
    mbe(am_v_sum$real_rates_all, am_v_sum$poisson),
    mbe(am_v_sum$real_rates_all, am_v_sum$nbinomial),
    mbe(am_v_sum$real_rates_all, am_v_sum$zero_poisson),
    mbe(am_v_sum$real_rates_all, am_v_sum$zero_nbinomial)
  ),
  
  nrmse = c(
    nrmse(am_v_sum$real_rates_all, am_v_sum$bell),
    nrmse(am_v_sum$real_rates_all, am_v_sum$poisson),
    nrmse(am_v_sum$real_rates_all, am_v_sum$nbinomial),
    nrmse(am_v_sum$real_rates_all, am_v_sum$zero_poisson),
    nrmse(am_v_sum$real_rates_all, am_v_sum$zero_nbinomial)
  ),
  
  rae = c(
    rae(am_v_sum$real_rates_all, am_v_sum$bell),
    rae(am_v_sum$real_rates_all, am_v_sum$poisson),
    rae(am_v_sum$real_rates_all, am_v_sum$nbinomial),
    rae(am_v_sum$real_rates_all, am_v_sum$zero_poisson),
    rae(am_v_sum$real_rates_all, am_v_sum$zero_nbinomial)
  ),
  
  rmse = c(
    rmse(am_v_sum$real_rates_all, am_v_sum$bell),
    rmse(am_v_sum$real_rates_all, am_v_sum$poisson),
    rmse(am_v_sum$real_rates_all, am_v_sum$nbinomial),
    rmse(am_v_sum$real_rates_all, am_v_sum$zero_poisson),
    rmse(am_v_sum$real_rates_all, am_v_sum$zero_nbinomial)
  ),
  
  rmsle = c(
    rmsle(am_v_sum$real_rates_all, am_v_sum$bell),
    rmsle(am_v_sum$real_rates_all, am_v_sum$poisson),
    rmsle(am_v_sum$real_rates_all, am_v_sum$nbinomial),
    rmsle(am_v_sum$real_rates_all, am_v_sum$zero_poisson),
    rmsle(am_v_sum$real_rates_all, am_v_sum$zero_nbinomial)
  ),
  
  rse = c(
    rse(am_v_sum$real_rates_all, am_v_sum$bell),
    rse(am_v_sum$real_rates_all, am_v_sum$poisson),
    rse(am_v_sum$real_rates_all, am_v_sum$nbinomial),
    rse(am_v_sum$real_rates_all, am_v_sum$zero_poisson),
    rse(am_v_sum$real_rates_all, am_v_sum$zero_nbinomial)
  ),
  
  cor = c(
    cor(am_v_sum$real_rates_all, am_v_sum$bell),
    cor(am_v_sum$real_rates_all, am_v_sum$poisson),
    cor(am_v_sum$real_rates_all, am_v_sum$nbinomial),
    cor(am_v_sum$real_rates_all, am_v_sum$zero_poisson),
    cor(am_v_sum$real_rates_all, am_v_sum$zero_nbinomial)
  )
  
)

state_errors |> View()
