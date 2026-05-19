library(readr)
library(INLA)
library(dplyr)

source('scripts/model_selection_io.R')
inla.setOption(num.threads = '4:1')

micro_path = 'outputs/micro_map.graph'
micro_f = read_csv('data/output_data/micro_reg_f_df.csv')
micro_spatial = read_csv('data/spatial_data/micro_map.csv')

micro_f$idArea = pmatch(
  micro_f$codMicroRes,
  micro_spatial$code_micro,
  duplicates.ok = T
)

micro_f$idArea2 = micro_f$idArea

micro_f$idInteraction = as.numeric(interaction(micro_f$idArea,
                                               micro_f$idMes))

formulas = list(
  Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) +
    f(ano, model = 'rw1', constr = T) +
    f(idArea, model = 'bym2', graph = micro_path) +
    f(idMes, model = 'rw1') +
    offset(log(populacao)),

  Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) +
    f(ano, model = 'rw1', constr = T) +
    f(idArea, model = 'bym2', graph = micro_path) +
    f(idMes, model = 'rw1') +
    rhum + temp + offset(log(populacao)),

  Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) +
    f(ano, model = 'rw1', constr = T) +
    f(idArea, model = 'bym2', graph = micro_path) +
    f(idMes, model = 'rw1') +
    f(idInteraction, model = 'iid') + offset(log(populacao)),

  # formula4 drops rhum since it was not significant in formula2
  Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) +
    f(ano, model = 'rw1', constr = T) +
    f(idArea, model = 'bym2', graph = micro_path) +
    f(idMes, model = 'rw1') +
    f(idInteraction, model = 'iid') +
    temp + offset(log(populacao))
)

for (i in seq_along(formulas)) {
  message('Fitting poisson falciparum model ', i)
  fit = inla(
    formula = formulas[[i]], family = 'poisson', data = micro_f,
    working.directory = tempdir(),
    control.predictor = list(compute = T, link = 1),
    control.compute = list(dic = T, waic = T, cpo = T),
    verbose = T
  )
  save_model_selection_row(
    family = 'poisson', species = 'falciparum', model_id = i, fit = fit
  )
  rm(fit)
  gc()
}
