library(INLA)
library(MASS)
library(tidyr)
library(dplyr)
library(Matrix)
library(ggplot2)

source("simulations/preliminar.R")
source("scripts/0.loss_functions.R")

simulate_poisson <- function(W, n_years, n_months, 
                             beta, sd_month, n_sim = 1)
{
  n_regions <- nrow(W)
  
  sim_list <- vector("list", n_sim)
  
  for (s in 1:n_sim) {
    data_sim <- expand.grid(
      region = 1:n_regions,
      month  = 1:n_months,
      year   = 1:n_years
    )
    
    # Covariables
    data_sim$X1 <- runif(nrow(data_sim), 0, 1)
    data_sim$X2 <- runif(nrow(data_sim), 0, 1)
    
    # Efectos aleatorios
    gamma_month <- simulate_RW1(n_months, sd_esp = sd_month)
    nu <- simulate_bym2(W = W)$S
    
    # Simular datos
    data_sim$eta <-
      gamma_month[data_sim$month] +
      nu[data_sim$region] +
      beta[1] * data_sim$X1 +
      beta[2] * data_sim$X2
    
    # Respuesta Poisson
    for (i in seq_len(nrow(data_sim))) {
      lambda_i <- exp(data_sim$eta)[i]
      data_sim$Y[i] <- rpois(1, lambda_i)
    }
    
    data_sim$sim_id <- s
    sim_list[[s]] <- data_sim
  }
  
  if (n_sim == 1) {
    return(sim_list[[1]])
  } else {
    return(bind_rows(sim_list))
  }
}

simulate_bell <- function(W, n_years, n_months, 
                             beta, sd_month, n_sim = 1)
{
  n_regions <- nrow(W)
  
  sim_list <- vector("list", n_sim)
  
  for (s in 1:n_sim) {
    data_sim <- expand.grid(
      region = 1:n_regions,
      month  = 1:n_months,
      year   = 1:n_years
    )
    
    # Covariables
    data_sim$X1 <- runif(nrow(data_sim), 0, 1)
    data_sim$X2 <- runif(nrow(data_sim), 0, 1)
    
    # Efectos aleatorios
    gamma_month <- simulate_RW1(n_months, sd_esp = sd_month)
    nu <- simulate_bym2(W = W)$S
    
    # Simular datos
    data_sim$eta <-
      gamma_month[data_sim$month] +
      nu[data_sim$region] +
      beta[1] * data_sim$X1 +
      beta[2] * data_sim$X2
    
    # Respuesta Bell
    for (i in seq_len(nrow(data_sim))) {
      lambda_i <- pracma::lambertWp(exp(data_sim$eta)[i])
      data_sim$Y[i] <- rbell(1, lambda_i)
    }
    
    data_sim$sim_id <- s
    sim_list[[s]] <- data_sim
  }
  
  if (n_sim == 1) {
    return(sim_list[[1]])
  } else {
    return(bind_rows(sim_list))
  }
}

simulate_nbinom <- function(W, n_years, n_months, 
                          beta, sd_month, n_sim = 1)
{
  n_regions <- nrow(W)
  
  sim_list <- vector("list", n_sim)
  
  for (s in 1:n_sim) {
    data_sim <- expand.grid(
      region = 1:n_regions,
      month  = 1:n_months,
      year   = 1:n_years
    )
    
    # Covariables
    data_sim$X1 <- runif(nrow(data_sim), 0, 1)
    data_sim$X2 <- runif(nrow(data_sim), 0, 1)
    
    # Efectos aleatorios
    gamma_month <- simulate_RW1(n_months, sd_esp = sd_month)
    nu <- simulate_bym2(W = W)$S
    
    # Simular datos
    data_sim$eta <-
      gamma_month[data_sim$month] +
      nu[data_sim$region] +
      beta[1] * data_sim$X1 +
      beta[2] * data_sim$X2
    
    # Respuesta Bell
    for (i in seq_len(nrow(data_sim))) {
      lambda_i <- exp(data_sim$eta)[i]
      data_sim$Y[i] <- rnbinom(1, mu = lambda_i, size = 4)
    }
    
    data_sim$sim_id <- s
    sim_list[[s]] <- data_sim
  }
  
  if (n_sim == 1) {
    return(sim_list[[1]])
  } else {
    return(bind_rows(sim_list))
  }
}

run_single_simulation <- function(W, n_years, n_months,
                                  beta, sd_month, sim_id) {
  # data_sim <- simulate_poisson(
  #   W = W,
  #   n_months  = n_months,
  #   n_years   = n_years,
  #   beta      = beta,
  #   sd_month  = sd_month,
  #   n_sim = 1
  # )
  
  # data_sim <- simulate_bell(
  #   W = W,
  #   n_months  = n_months,
  #   n_years   = n_years,
  #   beta      = beta,
  #   sd_month  = sd_month,
  #   n_sim = 1
  # )
  
  data_sim <- simulate_nbinom(
    W = W,
    n_months  = n_months,
    n_years   = n_years,
    beta      = beta,
    sd_month  = sd_month,
    n_sim = 1
  )
  
  # Ajustar modelo
  formula <- Y ~
    X1 + X2 + 
    f(month, model = 'rw1', constr = TRUE, cyclic = TRUE) +
    f(region, model = 'bym2', graph = am_path)
  
  pois_fit <- inla(
    formula = formula,
    family = 'poisson',
    data = data_sim,
    working.directory = 'D:/INLA/',
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    verbose = FALSE
  )
  
  bell_fit <- inla(
    formula = formula,
    family = 'bell',
    data = data_sim,
    working.directory = 'D:/INLA/',
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    verbose = FALSE
  )
  
  nbin_fit <- inla(
    formula = formula,
    family = 'nbinomial',
    data = data_sim,
    working.directory = 'D:/INLA/',
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    verbose = FALSE
  )
  
  # Coeficientes
  betas_pois <- pois_fit$summary.fixed$mean[-1]
  betas_bell <- bell_fit$summary.fixed$mean[-1]
  betas_nbin <- nbin_fit$summary.fixed$mean[-1]
  
  betas_list <- list(
    Poisson = betas_pois,
    Bell    = betas_bell,
    NegBin  = betas_nbin
  )
  
  out1 <- purrr::map_dfr(
    names(betas_list),
    ~ tibble(
      sim_id = sim_id,
      model = .x,
      beta1   = betas_list[[.x]][1],
      beta2   = betas_list[[.x]][2]
    )
  )
  
  # Predicción
  predicted_pois <- pois_fit$summary.fitted.values$mode
  predicted_bell <- bell_fit$summary.fitted.values$mode
  predicted_nbin <- nbin_fit$summary.fitted.values$mode
  
  pred_list <- list(
    Poisson = predicted_pois,
    Bell    = predicted_bell,
    NegBin  = predicted_nbin
  )
  
  out2 <- purrr::map_dfr(
    names(pred_list),
    ~ tibble(
      sim_id = sim_id,
      model = .x,
      mbe   = mbe(pred_list[[.x]], data_sim$Y),
      nrmse = nrmse(pred_list[[.x]], data_sim$Y),
      rae   = rae(pred_list[[.x]], data_sim$Y),
      rmsle = rmsle(pred_list[[.x]], data_sim$Y),
      rse   = rse(pred_list[[.x]], data_sim$Y)
    )
  )
  
  return(list(fixed = out1, fitted.values = out2))
}

run_simulations <- function(n_sim, W, n_years, n_months = 12, 
                            beta = c(1.5, -2.5), sd_month = 0.05) {
  
  betas <- vector("list", n_sim)
  results <- vector("list", n_sim)
  
  for (s in 1:n_sim) {
    cat("Simulation:", s, "of", n_sim, "\r")
    aux <- run_single_simulation(
      W = W,
      n_months = n_months,
      n_years = n_years,
      beta = beta,
      sd_month = sd_month,
      sim_id = s
    )
    
    betas[[s]] <- aux$fixed
    results[[s]] <- aux$fitted.values
  }
  
  return(list(fixed = bind_rows(betas), 
              fitted.values = bind_rows(results)))
}

# Ejecutar para diferentes horizontes de tiempo ---------------------------

set.seed(2025)

n_sim <- 10
am_path <- 'outputs/am_map.graph'
W <- as.matrix(inla.graph2matrix(inla.read.graph(am_path)))

run1 <- run_simulations(n_sim, W, n_years = 2)
run2 <- run_simulations(n_sim, W, n_years = 4)
run3 <- run_simulations(n_sim, W, n_years = 8)

# save.image(file = "simulations/results_negbinom.RData")
# load("simulations/preliminar_results_bell.RData")

# Analizar resultados -----------------------------------------------------

### fixed

truth_df <- tibble(
  parameter = c("beta1", "beta2"),
  true = c(1.5, -2.5)
)

F1 <- run1$fixed %>% mutate(n_year = 2)
F2 <- run2$fixed %>% mutate(n_year = 4)
F3 <- run3$fixed %>% mutate(n_year = 8)

bind_rows(F1, F2, F3) %>%
  pivot_longer(
    cols = c(beta1, beta2),
    names_to = "parameter",
    values_to = "estimate"
  ) %>%
  mutate(n_year = paste(n_year, "years")) %>% 
  ggplot(aes(x = model, y = estimate, fill = model)) +
  geom_boxplot() +
  facet_grid(parameter ~ n_year, scales = "free_y") +
  geom_hline(data = truth_df,
             aes(yintercept = true),
             linetype = "dashed",
             linewidth = 0.8,
             col = "red") +
  labs(title = "Estimator Precision Across Models and Sample Sizes", 
       x = "", y = "Estimated Value") +
  theme_bw() +
  theme(legend.position = "none")

# Crear el resumen
bind_rows(F1, F2, F3) %>%
  group_by(n_year, model) %>%
  summarise(
    mean_beta1 = mean(beta1),
    sd_beta1   = sd(beta1),
    mean_beta2 = mean(beta2),
    sd_beta2   = sd(beta2),
    .groups = "drop"
  )

### fitted.values

R1 <- run1$fitted.values %>% 
  group_by(model) %>% 
  summarise(across(c(mbe, nrmse, rae, rmsle, rse), mean)) %>% 
  mutate(tabla = "2 years")

R2 <- run2$fitted.values %>% 
  group_by(model) %>% 
  summarise(across(c(mbe, nrmse, rae, rmsle, rse), mean)) %>% 
  mutate(tabla = "4 years")

R3 <- run3$fitted.values %>% 
  group_by(model) %>% 
  summarise(across(c(mbe, nrmse, rae, rmsle, rse), mean)) %>% 
  mutate(tabla = "8 years")

# --- Modelo específico ---

bind_rows(R1, R2, R3) %>% 
  filter(model == "Bell") %>% 
  select(tabla, everything())

bind_rows(R1, R2, R3) %>% 
  filter(model == "Poisson") %>% 
  select(tabla, everything())

bind_rows(R1, R2, R3) %>% 
  filter(model == "NegBin") %>% 
  select(tabla, everything())

# Sim ---------------------------------------------------------------------

library(igraph)
library(ggraph)

am_path <- 'outputs/micro_map.graph'
W <- as.matrix(inla.graph2matrix(inla.read.graph(am_path)))

data_sim <- simulate_bell(
  W = W,
  n_months  = 12,
  n_years   = 1,
  beta      = c(1.5, -2.5),
  sd_month  = 0.05,
  n_sim = 1
)

g <- graph_from_adjacency_matrix(W, mode = "undirected")

map_df <- data_sim %>%
  filter(year == 1, month == 10) %>%
  arrange(region)

g <- set_vertex_attr(g, "mu", value = exp(map_df$eta))

ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(color = mu), size = 5) +
  scale_color_viridis_c() +
  theme_void()
