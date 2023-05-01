library(readr)
library(INLA)
library(dplyr)

#preparing data------------------------------------------------------------
#path
micro_path = 'outputs/micro_map.graph'

#read data
micro_f = read_csv('data/output_data/micro_reg_f_df.csv')

#spatial data
micro_spatial = read_csv('data/spatial_data/micro_map.csv')

#adjacency matrix
image(inla.graph2matrix(inla.read.graph(micro_path)), xlab = '', ylab = '')

#creating id area
micro_f$idArea = pmatch(
  micro_f$codMicroRes,
  micro_spatial$code_micro,
  duplicates.ok = T
)

micro_f$idArea2 = micro_v$idArea

#creating id interaction (between area and time)
micro_f$idInteraction = as.numeric(interaction(micro_f$idArea, 
                                               micro_f$idMes))

real_rates_all = micro_f$numCasos*100000/micro_f$populacao
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
  temp

#bell fit-------------------------------------------------------------------
bell_fit1 = inla(
  formula = formula1, family = 'bell', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 136842.25; DIC saturated = 86483.33; WAIC = 331305.95; 
bell_fit1 |> summary()
#PIT com frequência de classes chegando próximo aos 5000
hist(bell_fit1$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula2
bell_fit2 = inla(
  formula = formula2, family = 'bell', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 135473.12; DIC saturated = 85125.88; WAIC = 335196.54
#ambas as variáveis significativas pelo intervalo de credibilidade
bell_fit2 |> summary()
#PIT igual ao do modelo 1
hist(bell_fit2$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula3
bell_fit3 = inla(
  formula = formula3, family = 'bell', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 73000.16; DIC saturated = 22088.34; WAIC = 72143.11
#Melhora significativa em realação ao modelo anterior
bell_fit3 |> summary()
#piora significativa no PIT do modelo
hist(bell_fit3$cpo$pit, breaks = 10, main = '', xlab = 'PIT')


#formula4
bell_fit4 = inla(
  formula = formula4, family = 'bell', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 73078.64; DIC saturated = 22166.64; WAIC = 72250.26
#piora em relação ao modelo anterior.
#rhum não considerada significante. Rodando novamente sem ela.
bell_fit4 |> summary()
#PIT levemente pior que o do modelo anterior também
hist(bell_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#Ao menos em termos de DIC, o melhor modelo foi o 3.

#check fit3 predictions
bell_rate_all = bell_fit3$summary.fitted.values$mode*
  100000/micro_f$populacao
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
