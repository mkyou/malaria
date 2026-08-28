library(MASS)

simulate_RW1 <- function (n_sample, sd_esp) {
  x <- numeric(n_sample)
  x[1] <- 0
  
  for (t in 2:n_sample) {
    eps <- rnorm(1, mean = 0, sd = sd_esp)
    x[t] <- x[t - 1] + eps
  }
  
  return(x)
}

simulate_bym2 <- function(W, sigma = 1, rho = 0.9, eps = 1e-6) {
  n <- nrow(W)
  D <- diag(rowSums(W))
  Q <- D - W
  
  Q_reg <- Q + eps * diag(n)
  
  Q_inv <- MASS::ginv(Q)
  
  c <- exp(mean(log(diag(Q_inv))))
  
  phi_raw <- as.numeric(MASS::mvrnorm(1, rep(0,n), Sigma = Q_inv))
  phi_star <- phi_raw / sqrt(c)
  
  theta <- rnorm(n)
  
  S <- sigma * ( sqrt(rho) * phi_star + sqrt(1 - rho) * theta )
  
  list(
    S = S,
    phi_star = phi_star,
    theta = theta,
    Q = Q,
    c = c
  )
}

rztpois <- function(n, lambda) {
  y <- rpois(n, lambda)
  while (any(y == 0)) {
    y[y == 0] <- rpois(sum(y == 0), lambda)
  }
  return(y)
}

rbell <- function(n, theta) {
  if (theta <= 0) stop("theta must be > 0")
  
  lambda_N <- exp(theta) - 1
  N <- rpois(n, lambda_N)
  
  Y <- numeric(n)
  for (i in seq_len(n)) {
    if (N[i] > 0) {
      Y[i] <- sum(rztpois(N[i], theta))
    } else {
      Y[i] <- 0
    }
  }
  return(Y)
}
