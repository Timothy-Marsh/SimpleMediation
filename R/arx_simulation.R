# Simulate an ARX process

arx_simulation <- function(sample_length, alpha = 0.5, beta = c(0.2,0.8), eta = c(0.3,0.4,0.5), initials = c(0,0,0)){
  
  X <- initials[1]
  M <- initials[2]
  Y <- initials[3]
  
  # adding a burn-in period of 10% of the desired sample length
  sim_length <- sample_length * 1.1
  
  for (i in seq(2,sim_length)) {
    X[i] <- alpha * X[i-1] + rnorm(1)
    
    M[i] <- beta[1] * M[i-1] + X[i-1] * beta[2] + rnorm(1)
    
    Y[i] <- eta[1] * Y[i-1] + eta[2] * M[i-1] + eta[3] * X[i-1] + rnorm(1)
  }
  
  list(X = tail(X, sample_length), M = tail(M, sample_length), Y = tail(Y, sample_length))
}

res <- arx_simulation(1000)
X <- res$X
M <- res$M
Y <- res$Y

arima(X, order = c(1,0,0))
arima(M[-1], order = c(1,0,0), xreg = X[-1000])
xregs <- data.frame(M[-1000],X[-1000])
arima(Y[-1], order = c(1,0,0), xreg = xregs)
