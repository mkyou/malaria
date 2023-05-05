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

#nbinomial fit-------------------------------------------------------------------
nbinomial_fit1 = inla(
  formula = formula1, family = 'nbinomial', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 132334.24; WAIC = 132393.25; 
nbinomial_fit1 |> summary()
#PIT com frequência de classes chegando próximo aos 2500
hist(nbinomial_fit1$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula2
nbinomial_fit2 = inla(
  formula = formula2, family = 'nbinomial', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 132296.33; WAIC = 132355.88
#ambas as variáveis significativas pelo intervalo de credibilidade
nbinomial_fit2 |> summary()
#PIT igual ao do modelo 1
hist(nbinomial_fit2$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula3
nbinomial_fit3 = inla(
  formula = formula3, family = 'nbinomial', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 130422.1; WAIC = 130668.45
#Melhora significativa em realação ao modelo anterior
nbinomial_fit3 |> summary()
#piora significativa no PIT do modelo
hist(nbinomial_fit3$cpo$pit, breaks = 10, main = '', xlab = 'PIT')


#formula4
nbinomial_fit4 = inla(
  formula = formula4, family = 'nbinomial', data = micro_v,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 130166.51; WAIC = 130598.41
#piora em relação ao modelo anterior.
#ambas as covariáveis consideradas significantes
nbinomial_fit4 |> summary()
#PIT levemente pior que o do modelo anterior também
hist(nbinomial_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#Ao menos em termos de DIC, o melhor modelo foi o 3.

#check fit3 predictions
nbinomial_rate_all = nbinomial_fit4$summary.fitted.values$mode*
  100000/micro_v$populacao
nbinomial_rate_test = nbinomial_rate_all[(20545 - 3852: 20544)]

tibble(
  dist = 'nbinomial',
  mbe = c(
    mbe(real_rates_test, nbinomial_rate_test)
  ),
  nrmse = c(
    nrmse(real_rates_test, nbinomial_rate_test)
  ),
  rae = c(
    rae(real_rates_test, nbinomial_rate_test)
  ),
  rmsle = c(
    rmsle(real_rates_test, nbinomial_rate_test)
  ),
  rse = c(
    rse(real_rates_test, nbinomial_rate_test)
  ),
  cor = c(
    cor(real_rates_test, nbinomial_rate_test)
  )
)

plot(nbinomial_rate_test, real_rates_test)
