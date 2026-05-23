# A function that runs mediation on ARX models and returns the covariance vs the expected theoretical covariance
arx_mediation <- function(X, M, Y, params = list(xp = 1, mp = 1, yp = 1)){
  n <- length(X)
  xp <- params$xp
  mp <- params$mp
  yp <- params$yp
  
  # create models
  Xt <- arima(X, order = c(xp,0,0))
  
  Mt <- arima(M, order = c(mp,0,0), xreg = X[-n])
  
  Xy <- X[-c(n-1,n)]
  My <- M[-c(n-1)]
  xreg <- data.frame(My,Xy)
  
  Yt <- arima(Y, order = c(yp,0,0), xreg = xreg)
  
  # extract the coefficients
  alpha <- Xt$coef
  beta <- Mt$coef
  eta <- Yt$coef
  
  # calculate the interpretations
  selfMediated <- alpha[1] * eta[4]
  crossMediated <- eta[1] * eta[4]
  indirect <- eta[3] * beta[3]
  total <- selfMediated + crossMediated + indirect
  
  # we now want to calculate their acf functions
  # Xacf <- acf(Xt)
  # Macf <- acf(Mt)
  # Yacf <- acf(Yt)
  
  # comparing theoretical covariances to observed covariances
  CovXM <- cov(X[-1],M)
  CovXM_theory <- var(X) * ((beta[3]^2)/((1-alpha[1]^2)*(1-beta[3]^2)))
  diffXM <- abs(CovXM - CovXM_theory)
  
  CovXY <- cov(X[-c(1,2)], Y)
  CovXY_theory <- var(X) * ((eta[4]^2)/((1-alpha[1]^2)*(1-eta[4]^2)))
  diffXY <- abs(CovXY- CovXY_theory)
  
  CovMY <- cov(M[-1], Y)
  CovMY_theory <- var(X) * ((eta[4]^2 * beta[3]^2)/((1-beta[3]^2)*(1-eta[4]^2))) + var(M) * ((eta[3]^2)/((1-beta[3]^2)*(1-eta[3]^2)))
  diffMY <- abs(CovMY - CovMY_theory)
  
  list(XM = c(CovXM, CovXM_theory[[1]], diffXM[[1]]), XY = c(CovXY, CovXY_theory[[1]], diffXY[[1]]), MY = c(CovMY, CovMY_theory[[1]],diffMY[[1]]))
}

# testing :P
X <- arima.sim(list( ar = 0.5), 102)
M <- arima.sim(list(ar = 0.6), 101) + 0.2 * X[-102]
Y <- arima.sim(list(ar = 0.4), 100) + 0.3 * M[-101] + 0.5 * X[c(-101,-102)]

xp <- 1
mp <- 1
yp <- 1

# A function that simulates mutliple iterations of the previous function
arx_mediation_summary <- function(iterations = 1000){
  resultsXM <- data.frame(observed = NA, theoretical = NA, difference = NA)
  resultsXY <- data.frame(observed = NA, theoretical = NA, difference = NA)
  resultsMY <- data.frame(observed = NA, theoretical = NA, difference = NA)
  
  for (i in 1:iterations) {
    #n <- runif(1, min = 1000, max = 1000)
    n <- 100
    X <- arima.sim(list(ar = runif(1,0.001,1)), n)
    M <- arima.sim(list(ar = runif(1,0.001,1)), (n-1)) + runif(1,0.001,1) * X[-n]
    Y <- arima.sim(list(ar = runif(1,0.001,1)), (n-2)) + runif(1,0.001,1) * M[-(n-1)] + runif(1,0.001,1) * X[-c(n-1,n)]
    
    result <- arx_mediation(X,M,Y,params = list(xp = 1, mp = 1, yp = 1))

    resultsXM[i,] <- result$XM
    resultsXY[i,] <- result$XY
    resultsMY[i,] <- result$MY
  }
  
  list(XM = resultsXM, XY = resultsXY, MY = resultsMY)

}

