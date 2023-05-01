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

#zeroinflatedpoisson1 fit-----------------------------------------------------
zeroinflatedpoisson1_fit1 = inla(
  formula = formula1, family = 'zeroinflatedpoisson1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = -172913.10; DIC saturated = -254735.29; WAIC = 8655479.62; 
zeroinflatedpoisson1_fit1 |> summary()
#PIT com frequência de classes chegando próximo aos 6000
hist(zeroinflatedpoisson1_fit1$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula2
zeroinflatedpoisson1_fit2 = inla(
  formula = formula2, family = 'zeroinflatedpoisson1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = -161337.26; DIC saturado = -2.18e+23; WAIC = 8561571.37
#ambas as variáveis significativas pelo intervalo de credibilidade
#modelo pouco melhor em relação ao primeiro
zeroinflatedpoisson1_fit2 |> summary()
#PIT igual ao do modelo 1
hist(zeroinflatedpoisson1_fit2$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula3
zeroinflatedpoisson1_fit3 = inla(
  formula = formula3, family = 'zeroinflatedpoisson1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = -; DIC saturado = -; WAIC = -
#Não convergiu
zeroinflatedpoisson1_fit3 |> summary()
#Não convergiu
hist(zeroinflatedpoisson1_fit3$cpo$pit, breaks = 10, main = '', xlab = 'PIT')


#formula4
zeroinflatedpoisson1_fit4 = inla(
  formula = formula4, family = 'zeroinflatedpoisson1', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = -; DIC saturado = -; WAIC = -
#Não convergiu
zeroinflatedpoisson1_fit4 |> summary()
#Não convergiu
hist(zeroinflatedpoisson1_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#Os modelos 1 e 2 tiveram ambos resultados um pouco confusos.
#Com base no WAIC (a única métrica positiva), vamos de modelo 2.

#check fit2 predictions
zeroinflatedpoisson1_rate_all = zeroinflatedpoisson1_fit2$
  summary.fitted.values$mode*
  100000/micro_v$populacao
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
