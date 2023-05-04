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
  temp + offset(log(populacao))

#zeroinflatedpoisson1 fit----------------------------------------------------
zeroinflatedpoisson1_fit1 = inla(
  formula = formula1, family = 'zeroinflatedpoisson1', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 78831.29; DIC saturated = 12800.35; WAIC = 1710146.39; 
zeroinflatedpoisson1_fit1 |> summary()
#PIT com frequência de classes chegando próximo aos 5000
hist(zeroinflatedpoisson1_fit1$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula2
zeroinflatedpoisson1_fit2 = inla(
  formula = formula2, family = 'zeroinflatedpoisson1', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 84332.17; DIC saturated = -1.56e+19; WAIC = 1658993.09
#ambas as variáveis significativas pelo intervalo de credibilidade
#melhor que modelo 1
zeroinflatedpoisson1_fit2 |> summary()
#PIT igual ao do modelo 1
hist(zeroinflatedpoisson1_fit2$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula3
zeroinflatedpoisson1_fit3 = inla(
  formula = formula3, family = 'zeroinflatedpoisson1', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = Não convergiu; DIC saturated = Não convergiu; WAIC = Não convergiu
#Não rodou
zeroinflatedpoisson1_fit3 |> summary()
#piora significativa no PIT do modelo
hist(zeroinflatedpoisson1_fit3$cpo$pit, breaks = 10, main = '', xlab = 'PIT')


#formula4
zeroinflatedpoisson1_fit4 = inla(
  formula = formula4, family = 'zeroinflatedpoisson1', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = Não convergiu; DIC saturated = Não convergiu; WAIC = Não convergiu
#não convergiu
zeroinflatedpoisson1_fit4 |> summary()
#PIT levemente pior que o do modelo anterior também
hist(zeroinflatedpoisson1_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#Ao menos em termos de DIC, o melhor modelo foi o 1.

#check fit3 predictions
zeroinflatedpoisson1_rate_all = zeroinflatedpoisson1_fit1$
  summary.fitted.values$mode*
  100000/micro_f$populacao
zeroinflatedpoisson1_rate_test = zeroinflatedpoisson1_rate_all[(20545 - 3852: 
                                                                  20544)]

tibble(
  dist = 'zeroinflatedpoisson1',
  mbe = c(
    mbe(real_rates_test, zeroinflatedpoisson1_rate_test)
  ),
  nrmse = c(
    nrmse(real_rates_test, zeroinflatedpoisson1_rate_test)
  ),
  rae = c(
    rae(real_rates_test, zeroinflatedpoisson1_rate_test)
  ),
  rmsle = c(
    rmsle(real_rates_test, zeroinflatedpoisson1_rate_test)
  ),
  rse = c(
    rse(real_rates_test, zeroinflatedpoisson1_rate_test)
  ),
  cor = c(
    cor(real_rates_test, zeroinflatedpoisson1_rate_test)
  )
)

plot(zeroinflatedpoisson1_rate_test, real_rates_test)
