#' Summarize the results of `arx_boot` to get estimates of the covariances
#'
#' @param boots A list of data frames, each one being a bootstrap replicate of the data
#'
#' @return 
#'
#' @examples
#' boots <- arx_boot(500)
#' arx_summary(boots)

arx_summary <- function(boots, params = list(alpha = 0.5, beta = c(0.2,0.8), eta = c(0.3,0.4,0.5))){
  n <- length(boots)
  m <- length(boots[[1]]$X)
  
  # initializations
  cov_XM <- c()
  cov_XY <- c()
  cov_MY <- c()
  
  alpha <- c()
  beta <- matrix(nrow = n, ncol = 2)
  eta <- matrix(nrow = n, ncol = 3)
  
  # collect parameters
  for (i in seq(1,n)) {
    mod_X <- arima(boots[[i]]$X, order = c(1,0,0))
    mod_M <- arima(boots[[i]]$M[-1], order = c(1,0,0), xreg = boots[[i]]$X[-m])
    mod_Y <- arima(boots[[i]]$Y[-1], order = c(1,0,0), xreg = data.frame(boots[[i]]$M[-m], boots[[i]]$X[-m]))
    
    alpha[i] <- mod_X$coef[[1]]
    beta[i,] <- c(mod_M$coef[[1]], mod_M$coef[[3]])
    eta[i,] <- c(mod_Y$coef[[1]], mod_Y$coef[[3]], mod_Y$coef[[4]])
    
    cov_XM[i] <- cov(boots[[i]]$X,boots[[i]]$M)
    cov_XY[i] <- cov(boots[[i]]$X,boots[[i]]$Y)
    cov_MY[i] <- cov(boots[[i]]$M,boots[[i]]$Y)
  }
  
  params_x <- list(alpha = alpha)
  params_m <- list(beta1 = beta[,1], beta2 = beta[,2])
  params_y <- list(eta1 = eta[,1], eta2 = eta[,2], eta3 = eta[,3])
  
  list(covariances = list(XM = cov_XM, XY = cov_XY, MY = cov_MY), params_x = params_x, params_m = params_m, params_y = params_y, true_params = params)
}

# boots <- arx_boot(500)
# summary <- arx_summary(boots)
# arx_summary_effects(summary)
arx_summary_effects <- function(summary_arx){
  indirect <- summary_arx$params_m$beta2 * summary_arx$params_y$eta2
  direct <- summary_arx$params_y$eta3
  
  data.frame(indirect, direct)
}