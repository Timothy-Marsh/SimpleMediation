# Simulate an ARX based mediation system with the variance of each step being drawn from an AR model

arx_simulation_NCVar <- function(sample_length, alpha = 0.5, beta = c(0.2,0.8), eta = c(0.3,0.4,0.5), initials = c(0,0,0)){
  
  X <- initials[1]
  M <- initials[2]
  Y <- initials[3]
  
  # adding a burn-in period of 10% of the desired sample length
  sim_length <- sample_length * 1.1
  
  garch_alpha <- 0.2
  
  errors <- c(1)
  errors2 <- c(1)
  errors3 <- c(1)
  
  innov <- abs(arima.sim(model = list(ar = 0.4), n = sim_length))
  innov2 <- abs(arima.sim(model = list(ar = 0.4), n = sim_length))
  innov3 <- abs(arima.sim(model = list(ar = 0.4), n = sim_length))
  
  for (i in seq(2,sim_length)) {
    # innov <- garch_alpha * errors[i-1] + 1
    # innov2 <- garch_alpha * errors[i-1] + 1
    # innov3 <- garch_alpha * errors[i-1] + 1
    # 
    # errors[i] <- rnorm(1,0,innov)
    # errors2[i] <- rnorm(1,0,innov2)
    # errors3[i] <- rnorm(1,0,innov3)
    
    # innov[i] <- garch_alpha * innov[i-1] + 1
    # innov2[i] <- garch_alpha * innov2[i-1] + 1
    # innov3[i] <- garch_alpha * innov3[i-1] + 1
    # 
    # errors[i] <- rnorm(1,0,innov[i])
    # errors2[i] <- rnorm(1,0,innov2[i])
    # errors3[i] <- rnorm(1,0,innov3[i])
    
    errors[i] <- rnorm(1,0,innov[i])
    errors2[i] <- rnorm(1,0,innov2[i])
    errors3[i] <- rnorm(1,0,innov3[i])
  }
  
  for (i in seq(2,sim_length)) {
    X[i] <- alpha * X[i-1] + errors[i]
    
    M[i] <- beta[1] * M[i-1] + X[i-1] * beta[2] + errors2[i]
    
    Y[i] <- eta[1] * Y[i-1] + eta[2] * M[i-1] + eta[3] * X[i-1] + errors3[i]
  }
  
  list(X = tail(X, sample_length), M = tail(M, sample_length), Y = tail(Y, sample_length))
}