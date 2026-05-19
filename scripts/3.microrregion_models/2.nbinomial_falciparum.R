library(readr)
library(INLA)
library(dplyr)

source('scripts/model_selection_io.R')
inla.setOption(num.threads = '2:1')
inla.setOption(inla.mode = 'compact')

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

# PC prior on the per-observation IID precision to keep idInteraction
# identifiable against the nbinomial size parameter (otherwise the two
# compete and the hyperpar Newton-Raphson diverges -> segfault).
iid_hyper <- list(prec = list(prior = 'pc.prec', param = c(1, 0.01)))

formulas <- list(
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
    f(idInteraction, model = 'iid', hyper = iid_hyper) +
    offset(log(populacao)),

  Y ~ f(mes, model = 'rw2', constr = T, cyclic = T) +
    f(ano, model = 'rw1', constr = T) +
    f(idArea, model = 'bym2', graph = micro_path) +
    f(idMes, model = 'rw1') +
    f(idInteraction, model = 'iid', hyper = iid_hyper) +
    temp + rhum + offset(log(populacao))
)

for (i in seq_along(formulas)) {
  message('Fitting nbinomial falciparum model ', i)
  fit <- tryCatch(
    inla(
      formula = formulas[[i]], family = 'nbinomial', data = micro_f,
      working.directory = tempdir(),
      control.predictor = list(compute = FALSE, link = 1),
      control.compute = list(
        dic = T, waic = T, cpo = T,
        return.marginals = FALSE,
        return.marginals.predictor = FALSE,
        config = FALSE
      ),
      control.inla = list(
        strategy = 'adaptive',
        control.vb = list(emergency = 100)
      ),
      safe = TRUE,
      verbose = T
    ),
    error = function(e) {
      message('Model ', i, ' failed: ', conditionMessage(e))
      NULL
    }
  )
  if (!is.null(fit)) {
    save_model_selection_row(
      family = 'nbinomial', species = 'falciparum', model_id = i, fit = fit
    )
  }
  rm(fit)
  gc()
}
