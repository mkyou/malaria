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

#poisson fit-----------------------------------------------------------------
poisson_fit1 = inla(
  formula = formula1, family = 'poisson', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = -160485.23; DIC saturated = -220633.30; WAIC = 8686103.55; 
poisson_fit1 |> summary()
#PIT com frequência de classes chegando próximo aos 6000
hist(poisson_fit1$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula2
poisson_fit2 = inla(
  formula = formula2, family = 'poisson', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = -148685.57; DIC saturado = -2.19e+23; WAIC = 8688521.22
#ambas as variáveis significativas pelo intervalo de credibilidade
#modelo pior em relação ao primeiro
poisson_fit2 |> summary()
#PIT igual ao do modelo 1
hist(poisson_fit2$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula3
poisson_fit3 = inla(
  formula = formula3, family = 'poisson', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 97406.88; DIC saturado = -5.86e+23; WAIC = 96182.68
#Melhora significativa em realação ao modelo anterior
poisson_fit3 |> summary()
#piora significativa no PIT do modelo
hist(poisson_fit3$cpo$pit, breaks = 10, main = '', xlab = 'PIT')


#formula4
poisson_fit4 = inla(
  formula = formula4, family = 'poisson', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 97441.30; DIC saturado = -5.86e+23; WAIC = 96233.18
#piora em relação ao modelo anterior.
#ambas as covariáveis consideradas significantes
poisson_fit4 |> summary()
#PIT levemente pior que o do modelo anterior também
hist(poisson_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#Ao menos em termos de DIC, o melhor modelo foi o 3.

#check fit3 predictions
poisson_rate_all = poisson_fit3$summary.fitted.values$mode*
  100000/micro_v$populacao
poisson_rate_test = poisson_rate_all[(20545 - 3852: 20544)]

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
