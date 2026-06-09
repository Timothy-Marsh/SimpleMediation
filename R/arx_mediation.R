# A function that runs mediation on ARX models and returns the covariance vs the expected theoretical covariance
arx_mediation <- function(X, M, Y, params = list(xp = 1, mp = 1, yp = 1)){
  n <- length(X)
  xp <- params$xp
  mp <- params$mp
  yp <- params$yp
  
  # create models
  Xt <- arima(X, order = c(xp,0,0))
  
  Mt <- arima(M, order = c(mp,0,0), xreg = X)
  
  # Xy <- X[-c(n-1,n)]
  # My <- M[-c(n-1)]
  xreg <- data.frame(M,X)
  
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
  # remove NA's
  CovXM <- cov(X[-1],M[-1])
  # find covariance according to our theory
  CovXM_theory <- var(X) * ((beta[3]*alpha[1])/((1-alpha[1]^2)*(1-(beta[3]*alpha[1]) )))
  #CovXM_theory <- var(X) * ((beta[3]^2)/((1-alpha[1]^2)*(1-(beta[3]^2) )))
  #CovXM_theory <- var(X) * ((beta[3]^3)/((1-alpha[1]^2)^2*(1-(beta[1]^2) )*(1-alpha[1]^2)*(1-beta[1]^2)))
  diffXM <- abs(CovXM - CovXM_theory)
  diffXMpercent <- (diffXM/CovXM)*100
  
  CovXY <- cov(X[-c(1,2)], Y[-c(1,2)])
  CovXY_theory <- var(X) * ((eta[4]^2)/((1-alpha[1]^2)*(1-eta[4]^2)))
  diffXY <- abs(CovXY- CovXY_theory)
  diffXYpercent <- (diffXY/CovXY)*100
  
  CovMY <- cov(M[-c(1,2)], Y[-c(1,2)])
  CovMY_theory <- var(X) * ((eta[4]^2 * beta[3]^2)/((1-beta[3]^2)*(1-eta[4]^2))) + var(M[-1]) * ((eta[3]^2)/((1-beta[3]^2)*(1-eta[3]^2)))
  diffMY <- abs(CovMY - CovMY_theory)
  diffMYpercent <- (diffMY/CovMY)*100
  
  list(XM = c(CovXM, CovXM_theory[[1]], diffXM[[1]],diffXMpercent), XY = c(CovXY, CovXY_theory[[1]], diffXY[[1]],diffXYpercent), MY = c(CovMY, CovMY_theory[[1]],diffMY[[1]],diffMYpercent))
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
  resultsXM <- data.frame(observed = NA, theoretical = NA, difference = NA, percentDiff =  NA)
  resultsXY <- data.frame(observed = NA, theoretical = NA, difference = NA, percentDiff =  NA)
  resultsMY <- data.frame(observed = NA, theoretical = NA, difference = NA, percentDiff =  NA)
  
  for (i in 1:iterations) {
    #n <- runif(1, min = 1000, max = 1000)
    # n <- 100
    # X <- arima.sim(list(ar = runif(1,0.001,1)), n)
    # M <- arima.sim(list(ar = runif(1,0.001,1)), (n-1)) + runif(1,0.001,1) * X[-n]
    # Y <- arima.sim(list(ar = runif(1,0.001,1)), (n-2)) + runif(1,0.001,1) * M[-(n-1)] + runif(1,0.001,1) * X[-c(n-1,n)]
    
    n <- 10
    alpha <- 0.5
    beta <- c(0.2,0.3)
    eta <- c(0.4,0.5,0.6)
    X <- arima.sim(list(ar = alpha), n)
    M <- arima.sim(list(ar = beta[1]), n-1) + beta[2] * X[-n]
    Y <- arima.sim(list(ar = eta[1]), (n-2)) + eta[2] * M[-(n-1)] + eta[3] * X[-c(n-1,n)]
    
    M <- append(NA, M)
    Y <- append(c(NA,NA), Y)
    
    example_data <- head(data.frame(X,M,Y))
    
    result <- arx_mediation(X,M,Y,params = list(xp = 1, mp = 1, yp = 1))

    resultsXM[i,] <- result$XM
    resultsXY[i,] <- result$XY
    resultsMY[i,] <- result$MY
  }
  
  list(XM = resultsXM, XY = resultsXY, MY = resultsMY, example_data)

}

