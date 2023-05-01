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
  temp + rhum

#nbinomial fit-------------------------------------------------------------------
nbinomial_fit1 = inla(
  formula = formula1, family = 'nbinomial', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 84406.20; WAIC = 84471.37; 
nbinomial_fit1 |> summary()
#PIT com frequência de classes chegando próximo aos 5000, não balanceado
hist(nbinomial_fit1$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula2
nbinomial_fit2 = inla(
  formula = formula2, family = 'nbinomial', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 84345.43; DIC saturated = 14362.84; WAIC = 84412.01
#ambas as variáveis significativas pelo intervalo de credibilidade
nbinomial_fit2 |> summary()
#PIT igual ao do modelo 1
hist(nbinomial_fit2$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#formula3
nbinomial_fit3 = inla(
  formula = formula3, family = 'nbinomial', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 83209.12; DIC saturated = 17424.65; WAIC = 83437.69
#Melhora significativa em realação ao modelo anterior
nbinomial_fit3 |> summary()
#piora significativa no PIT do modelo
hist(nbinomial_fit3$cpo$pit, breaks = 10, main = '', xlab = 'PIT')


#formula4
nbinomial_fit4 = inla(
  formula = formula4, family = 'nbinomial', data = micro_f,
  working.directory = 'D:/INLA/',
  control.predictor = list(compute = T, link = 1),
  control.compute = list(dic = T, waic = T, cpo = T),
  verbose = F
)

#DIC = 83173.28; DIC saturated = 17322.90; WAIC = 83461.40
#melhora um pouco em relação ao modelo anterior.
#nenhuma das duas consideradas significantes, mas contribuem para 
#a queda do DIC
nbinomial_fit4 |> summary()
#PIT levemente pior que o do modelo anterior também
hist(nbinomial_fit4$cpo$pit, breaks = 10, main = '', xlab = 'PIT')

#Ao menos em termos de DIC, o melhor modelo foi o 3.

#check fit3 predictions
nbinomial_rate_all = nbinomial_fit4$summary.fitted.values$mode*
  100000/micro_f$populacao
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
