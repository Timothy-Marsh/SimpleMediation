# A function that runs mediation on ARX models and returns the covariance vs the expected theoretical covariance
arx_mediation <- function(X, M, Y, params = list(xp = 1, mp = 1, yp = 1), theoreticals = list(XM_theory = 1,XY_theory=1,MY_theory=1)){
  n <- length(X)
  xp <- params$xp
  mp <- params$mp
  yp <- params$yp
  
  XM_theory <- theoreticals$XM_theory
  XY_theory <- theoreticals$XY_theory
  MY_theory <- theoreticals$MY_theory
  
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
  
  # to estimate the covariances we will need the sigmas:
  sigmaX <- var(X) * (1-alpha[1]^2)
  sigmaM <- var(M)
  
  # comparing theoretical covariances to observed covariances
  # remove NA's
  CovXM <- cov(X[-1],M[-1])
  # find covariance according to our theory
  CovXM_plug <- ((beta[3]^2)/((1-alpha[1]^2)^2 * (1-beta[1]^2)) ) * sigmaX
  
  CovXY <- cov(X[-c(1,2)], Y[-c(1,2)])
  CovXY_plug <- ((beta[3]^2*eta[3]^2 + eta[4]^2 *(1-beta[1]^2))/((1-alpha[1]^2)^2*(1-beta[1]^2)*(1-eta[1]^2))) * sigmaX
  
  CovMY <- cov(M[-c(1,2)], Y[-c(1,2)])
  CovMY_plug <- sigmaX * (eta[3]^2)/((1-beta[1]^2)^2 * (1-eta[1]^2)) + ((eta[3]^2 * beta[3]^4)+(eta[4]^2 * (1-beta[1]^2) * beta[3]^2))/((1-alpha^2)^2 * (1-beta[1]^2)^2 * (1-eta[1]^2))
  
  list(XM = c(CovXM, CovXM_plug[[1]], XM_theory), XY = c(CovXY, CovXY_plug[[1]], XY_theory), MY = c(CovMY, CovMY_plug[[1]],MY_theory))
}

# # testing :P
# X <- arima.sim(list( ar = 0.5), 102)
# m <- arima.sim(list(ar = 0.6), 101) + 0.2 * X[-102]
# 
# y <- arima.sim(list(ar = 0.4), 100) + 0.3 * m[-101] + 0.5 * X[c(-101,-102)]
# M <- append(NA,m)
# Y <- append(c(NA,NA),y)
# 
# xp <- 1
# mp <- 1
# yp <- 1

# A function that simulates mutliple iterations of the previous function
arx_mediation_summary <- function(iterations = 1000, length = 1000){
  resultsXM <- data.frame(observed = NA, plug_in = NA, theoretical = NA)
  resultsXY <- data.frame(observed = NA, plug_in = NA, theoretical = NA)
  resultsMY <- data.frame(observed = NA, plug_in = NA, theoretical = NA)
  
  n <- length
  alpha <- 0.5
  beta <- c(0.2,0.3)
  eta <- c(0.4,0.5,0.6)
  
  XM_theory <- (beta[2]^2)/((1-alpha^2)^2 * (1-beta[1]^2))
  XY_theory <- ((eta[2]^2 * beta[2]^2)+(eta[3]^2 * (1-beta[1]^2)))/((1-alpha^2)^2 * (1-beta[1]^2) * (1-eta[1]^2))
  MY_theory <- (eta[2]^2)/((1-beta[1]^2)^2 * (1-eta[1]^2)) + ((eta[2]^2 * beta[2]^4)+(eta[3]^2 * (1-beta[1]^2) * beta[2]^2))/((1-alpha^2)^2 * (1-beta[1]^2)^2 * (1-eta[1]^2))
  
  for (i in 1:iterations) {
    X <- arima.sim(list(ar = alpha), n, sd = 1)
    M <- arima.sim(list(ar = beta[1]), n-1, sd = 1) + beta[2] * X[-n]
    Y <- arima.sim(list(ar = eta[1]), (n-2), sd = 1) + eta[2] * M[-(n-1)] + eta[3] * X[-c(n-1,n)]
    
    M <- append(NA, M)
    Y <- append(c(NA,NA), Y)
    
    example_data <- head(data.frame(X,M,Y))
    
    result <- arx_mediation(X,M,Y,params = list(xp = 1, mp = 1, yp = 1), theoreticals = list(XM_theory = XM_theory,XY_theory=XY_theory,MY_theory=MY_theory))

    resultsXM[i,] <- result$XM
    resultsXY[i,] <- result$XY
    resultsMY[i,] <- result$MY
  }
  
  list(XM = resultsXM, XY = resultsXY, MY = resultsMY, example_data)
}

# Usage
# first focusing on cov(X,M)
#results <- arx_mediation_summary(iterations = 1000, length = 1000)
arx_mediation_results <- function(results){
  XM <- results[[1]]
  XY <- results[[2]]
  YM <- results[[3]]
  
  percent_diff <- (XM$observed - XM$theoretical)/XM$theoretical
  hist(percent_diff)
  
  hist(results[[1]]$observed - results[[1]]$theoretical, main = "Cov(X,M) errors")
  hist(results[[2]]$observed - results[[2]]$theoretical, main = "Cov(X,Y) errors")
  hist(results[[3]]$observed - results[[3]]$theoretical, main = "Cov(M,Y) errors")
  
  hist((results[[1]]$observed - results[[1]]$theoretical)/results[[1]]$theoretical, main = "Cov(X,M) errors diff percentage")
  hist((results[[2]]$observed - results[[2]]$theoretical)/results[[2]]$theoretical, main = "Cov(X,Y) errors diff percentage")
  hist((results[[3]]$observed - results[[3]]$theoretical)/results[[3]]$theoretical, main = "Cov(M,Y) errors diff percentage")
}
