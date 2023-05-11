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

real_rates_all = micro_v$numCasos*1000/micro_v$populacao
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

#bell fit-------------------------------------------------------------------
bell_fit1 = inla(
  formula = formula1, family = 'bell', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 233963.63; DIC saturado = 151603.73; WAIC = 1016623.69; 
bell_fit1 |> summary()
#PIT com frequência de classes chegando próximo aos 6000
hist(bell_fit1$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula2
bell_fit2 = inla(
  formula = formula2, family = 'bell', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 234280.40; DIC saturado = 151916.78; WAIC = 1029008.68
#ambas as variáveis significativas pelo intervalo de credibilidade
bell_fit2 |> summary()
#PIT igual ao do modelo 1
hist(bell_fit2$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula3
bell_fit3 = inla(
  formula = formula3, family = 'bell', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 111419.25; DIC saturado = 27307.29; WAIC = 109818.58
#Melhora significativa em realação ao modelo anterior
bell_fit3 |> summary()
#piora significativa no PIT do modelo
hist(bell_fit3$cpo$pit, breaks = 10, main = '', xlab = 'PIT')


#formula4
bell_fit4 = inla(
  formula = formula4, family = 'bell', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 111414.37; DIC saturado = 27302.40; WAIC = 109766.96
#piora em relação ao modelo anterior.
#ambas as covariáveis consideradas significantes
bell_fit4 |> summary()
#PIT levemente pior que o do modelo anterior também
hist(bell_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#Ao menos em termos de DIC, o melhor modelo foi o 4.

#check fit4 predictions
bell_rate_all = bell_fit4$summary.fitted.values$mode*
  1000/micro_v$populacao
bell_rate_test = bell_rate_all[(20545 - 3852: 20544)]

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
