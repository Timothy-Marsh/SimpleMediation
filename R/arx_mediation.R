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
  
  list(XM = c(CovXM, CovXM_plug[[1]], XM_theory), XY = c(CovXY, CovXY_plug[[1]], XY_theory), 
       MY = c(CovMY, CovMY_plug[[1]],MY_theory), paramsX = c(alpha[[1]], alpha[[2]]), paramsM = c(beta[[1]], beta[[3]], beta[[2]]), paramsY = c(eta[[1]], eta[[3]], eta[[4]], eta[[2]]))
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
arx_mediation_summary <- function(iterations = 1000, length = 1000, alpha = 0.5, beta = c(0.2,0.8)){
  resultsXM <- data.frame(observed = NA, plug_in = NA, theoretical = NA)
  resultsXY <- data.frame(observed = NA, plug_in = NA, theoretical = NA)
  resultsMY <- data.frame(observed = NA, plug_in = NA, theoretical = NA)
  params_x <- data.frame(alpha = NA, intercept = NA)
  params_m <- data.frame(beta0 = NA, beta1 = NA, intercept = NA)
  params_y <- data.frame(eta0 = NA, eta1 = NA, eta2 = NA, intercept = NA)
  
  n <- length
  #alpha <- 0.5
  #beta <- c(0.2,0.8)
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
    params_x[i,] <- result$paramsX
    params_m[i,] <- result$paramsM
    params_y[i,] <- result$paramsY
  }
  
  list(XM = resultsXM, XY = resultsXY, MY = resultsMY, params_x = params_x, params_m = params_m, params_y = params_y, example_data)
}

# collate the results
arx_mediation_results <- function(results){
  XM <- results[[1]]
  XY <- results[[2]]
  MY <- results[[3]]
  
  percent_diff <- (XM$observed - XM$theoretical)/XM$theoretical
  hist(percent_diff)
  
  hist((XM$observed - XM$theoretical)/XM$theoretical, main = "Cov(X,M) errors diff percentage")
  hist((XY$observed - XY$theoretical)/XY$theoretical, main = "Cov(X,Y) errors diff percentage")
  hist((MY$observed - MY$theoretical)/MY$theoretical, main = "Cov(M,Y) errors diff percentage")
  
  summary <- list(XM = list(observed = c(mean(XM$observed),sd(XM$observed),quantile(XM$observed, c(0.025,0.9725))), plug_in = c(mean(XM$plug_in),sd(XM$plug_in)), theory = mean(XM$theoretical)),
                  XY = list(observed = c(mean(XY$observed),sd(XY$observed),quantile(XY$observed, c(0.025,0.9725))), plug_in = c(mean(XY$plug_in),sd(XY$plug_in)), theory = mean(XY$theoretical)),
                  MY = list(observed = c(mean(MY$observed),sd(MY$observed),quantile(MY$observed, c(0.025,0.9725))), plug_in = c(mean(MY$plug_in),sd(MY$plug_in)), theory = mean(MY$theoretical)))
  
  summary
}
# Usage:
#results <- arx_mediation_summary(iterations = 1000, length = 1000)
#arx_mediation_results(results)


# `testing_vars` gives the estimated value of parameters for different true values to compare how they change
# Usage:
# vals <- seq(0.1,0.9,0.1)
# testing_vars(vals)
testing_vars <- function(vals){
  n <- length(vals)
  means <- data.frame(beta0 = NA, beta1 = NA, intercept = NA, true_beta0 = NA, true_beta1 = NA)
  j <- 0
  
  for (i in vals) {
    j <- j + 1
    u <- 0
    for (k in vals) {
      u <- u + 1
      m <- 9*(j-1) + u
      results <- arx_mediation_summary(iterations = 100, length = 1000, beta = c(i,k))
      newmeans <- colMeans(results$params_m)
      #print(c(i,j))
      #print(colMeans(results$params_m))
      means[m,1] <- newmeans[[1]]
      means[m,2] <- newmeans[[2]]
      means[m,3] <- newmeans[[3]]
      means[m,4] <- i
      means[m,5] <- k
      #print(means)
    }
  }
  
  means
}

testing_processing <- function(means){
  beta0_diff <- means[,1] - means[,4]
  beta1_diff <- means[,2] - means[,5]
  
  output <- data.frame(beta0_diff, beta1_diff)
  output
}

# this function uses ggplot2 to plot the estimates of the parameters and compare them to the true values
# usage:
# results <- arx_mediation_summary(iterations = 1000, length = 1000)
# testing_plotting(results)
testing_plotting <- function(results){
  library(ggplot2)
  
  data_alpha <- data.frame(alpha = results$params_x$alpha)
  
  alphas <- ggplot(data_alpha, aes(x="", y=alpha)) + 
              geom_violin() + 
              #geom_boxplot() +
              geom_segment(aes(x = 0.5, xend = 1.5, y = 0.5, yend = 0.5),
                           color = 'red',
                           linewidth = 1) +
              labs(title = "Violin plot of Alpha estimates",
                   y = "alpha")
  
  n <- length(results$params_m$beta0)
  data_beta <- data.frame(var = c(rep("beta_0", n), rep("beta_1",n)), results = c(results$params_m$beta0,results$params_m$beta1))
  betas <- ggplot(data_beta, aes(x=var, y=results)) + 
              geom_violin() + 
              #geom_boxplot() +
              geom_segment(aes(x = 0.5, xend = 1.5, y = 0.2, yend = 0.2),
                           color = 'red',
                           linewidth = 1) +
              geom_segment(aes(x = 1.5, xend = 2.5, y = 0.8, yend = 0.8),
                           color = 'red',
                           linewidth = 1) +
              labs(title = "Violin plot of Beta estimates",
                   y = "Beta")
  
  data_eta <- data.frame(var = c(rep("eta_0", n), rep("eta_1",n), rep("eta_2",n)), results = c(results$params_y$eta0,results$params_y$eta1, results$params_y$eta2))
  etas <- ggplot(data_eta, aes(x=var, y=results)) + 
            geom_violin() + 
            #geom_boxplot() +
            geom_segment(aes(x = 0.5, xend = 1.5, y = 0.4, yend = 0.4),
                         color = 'red',
                         linewidth = 1) +
            geom_segment(aes(x = 1.5, xend = 2.5, y = 0.5, yend = 0.5),
                         color = 'red',
                         linewidth = 1) +
            geom_segment(aes(x = 2.5, xend = 3.5, y = 0.6, yend = 0.6),
                         color = 'red',
                         linewidth = 1) +
            labs(title = "Violin plot of Eta estimates",
                 y = "Eta")
  
  list(alpha <- alphas, beta <- betas, eta <- etas)
}