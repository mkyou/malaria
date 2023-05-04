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


#formulas------------------------------------------------------------------
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

#zeroinflatednbinomial1 fit---------------------------------------------------
zeroinflatednbinomial1_fit1 = inla(
  formula = formula1, family = 'zeroinflatednbinomial1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 132332.26; DIC saturated = NA; WAIC = 132391.37; 
zeroinflatednbinomial1_fit1 |> summary()
#PIT com frequência de classes chegando próximo aos 6000
hist(zeroinflatednbinomial1_fit1$cpo$pit, breaks = 10, main = '', 
     xlab = 'PIT')

#formula2
zeroinflatednbinomial1_fit2 = inla(
  formula = formula2, family = 'zeroinflatednbinomial1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 132195.55; DIC saturado = 17364.40; WAIC = 132254.38
#ambas as variáveis significativas pelo intervalo de credibilidade
#modelo pouco melhor em relação ao primeiro
zeroinflatednbinomial1_fit2 |> summary()
#PIT igual ao do modelo 1
hist(zeroinflatednbinomial1_fit2$cpo$pit, breaks = 10, main = '', 
     xlab = 'PIT')

#formula3
zeroinflatednbinomial1_fit3 = inla(
  formula = formula3, family = 'zeroinflatednbinomial1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 130226.27; DIC saturado = 22500.13; WAIC = 130507.92
zeroinflatednbinomial1_fit3 |> summary()
#PIT piorou
hist(zeroinflatednbinomial1_fit3$cpo$pit, breaks = 10, main = '', 
     xlab = 'PIT')


#formula4
zeroinflatednbinomial1_fit4 = inla(
  formula = formula4, family = 'zeroinflatednbinomial1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 130217.66; DIC saturado = 22273.86; WAIC = 130599.95
#pequena melhora em termos do DIC
#ambas as variáveis significantes
zeroinflatednbinomial1_fit4 |> summary()
#PIT similar ao anterior
hist(zeroinflatednbinomial1_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#vamos de modelo 4

#check fit4 predictions
zeroinflatednbinomial1_rate_all = zeroinflatednbinomial1_fit4$
  summary.fitted.values$mode*
  100000/micro_v$populacao
zeroinflatednbinomial1_rate_test = zeroinflatednbinomial1_rate_all[
  (20545 - 3852:20544)]

tibble(
  dist = 'zeroinflatednbinomial1',
  mbe = c(
    mbe(real_rates_test, zeroinflatednbinomial1_rate_test)
  ),
  nrmse = c(
    nrmse(real_rates_test, zeroinflatednbinomial1_rate_test)
  ),
  rae = c(
    rae(real_rates_test, zeroinflatednbinomial1_rate_test)
  ),
  rmsle = c(
    rmsle(real_rates_test, zeroinflatednbinomial1_rate_test)
  ),
  rse = c(
    rse(real_rates_test, zeroinflatednbinomial1_rate_test)
  ),
  cor = c(
    cor(real_rates_test, zeroinflatednbinomial1_rate_test)
  )
)

plot(zeroinflatednbinomial1_rate_test, real_rates_test)
