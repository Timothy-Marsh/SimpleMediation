#' Perform bootstrapping on the residuals of a time series process and return the specified number of bootstrap replicates of the time series. Currently does it for 3 time series, but does not for causal relationships between them.
#'
#' @param data A data frame containing vectors of time series data
#' @param p A vector of values for the degree of the ar model for the X,M,Y
#' @param q A vector of values for the degree of the ma model for the X,M,Y, defaults to 0
#' @param R An integer for the number of bootstrap replicates used
#'
#' @return A named list which includes an estimate of the time series, and the bootstrap results
#'
#' @export
#'
#' @examples 
#' x <- rnorm(100)
#' m <- rnorm(100)
#' y <- rnorm(100)
#' data <- data.frame(x,m,y)
#' ts_mediation_boot(data, p = c(1,1,1), R = 50)

ts_mediation_boot <- function(data, p, q = c(0,0,0), R) {
  # add something here to check the type of data given and do different things with it, not necessarily needed depending on what other functions are created
  
  Xmod <- arima(data[,1], order = c(p[1], 0, q[1]))
  Mmod <- arima(data[,2], order = c(p[2], 0, q[2]))
  Ymod <- arima(data[,3], order = c(p[3], 0, q[3]))
  mods <- list(Xmod, Mmod, Ymod)
  
  # initialize variables to store aspects of the time series
  mod_intercept <- c()
  resids <- list(x = NA, m = NA, y = NA)
  coefs <- list(x = NA, m = NA, y = NA)
  ts_mean <- c()
  
  for (i in c(1:3)) {
    # find and centre the residuals of the time series models
    resids[[i]] <- residuals(mods[[i]]) - mean(residuals(mods[[i]]))
    # create variables for the mean of the time series and the coefficients of the model
    mod_intercept[i] <- mods[[i]]$coef[p[i] + 1]
    coefs[[i]] <- mods[[i]]$coef[1:p[i]]
    ts_mean[i] <- mean(data[,i])
  }
  
  # the bootstrapping
  n <- length(data[,1])
  x <- seq(1:n)
  # samps is a data frame with R rows indicating which observations are used in each bootstrap replicate
  samps <- data.frame(matrix(nrow = n, ncol = R))
  for (i in seq(1:R)) {
    samps[, i] <- sample(x, n, replace = TRUE)
  }
  
  boot_resids <- list()
  for (i in 1:R) {
    boot_resids[[i]] <- data.frame(x = resids$x[samps[[i]]], m = resids$m[samps[[i]]], y = resids$y[samps[[i]]])
  }

  # initializing the data frame
  boot_reps <- list()
  
  # since AR models depend on previous entries, we initialize it to be the initial observations
  for (i in seq(1:R)) {
    boot_reps[[i]] <- data
  }
  
  # a series of loops to build the R bootstrap replicates of the time series itself
  for (k in c(1:3)) {
    for (i in seq(1:R)) {
      for (j in ((p[k] + 1):n)) {
        boot_reps[[i]][j,k] <- ts_mean[k] + sum(coefs[[k]] * rev(boot_reps[[i]][(j - 1 - (p[k] - 1)):(j - 1),k])) + boot_resids[[i]][j, k]
      }
    }
  }
  
  # # do mediation on the time series
  # direct <- c()
  # indirect <- c()
  # for (i in seq(1:R)) {
  #   tempData <- boot_reps[[i]]
  #   respMod <- arima(tempData$samp_M, order = c(p[2],0,q[2]), xreg = tempData$samp_X)
  #   fullMod <- arima(tempData$samp_Y, order = c(p[3],0,q[3]), xreg = data.frame(x = tempData$samp_X, m = tempData$samp_M))
  #   
  #   indirect[i] <- respMod$coef[3] * fullMod$coef[4]
  #   direct[i] <- fullMod$coef[3]
  # }
  # return a named list with the model and the boostrap replications
  list(model = mods, bootReps = boot_reps)
}
