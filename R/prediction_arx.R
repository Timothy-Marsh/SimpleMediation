#' Use a prediction based method along with ARX models to find the direct and indirect effects of a mediation system
#'
#' @param boot_reps An integer indicating the number of bootstrap replications to do
#' @param params A named list indicating the true parameters to use for the simulated data
#'
#' @return A list with each entry being one bootstrap replicate of the simulated data
#'
#' @export
#'
#' @examples
#' boots <- arx_boot(500)
#' prediction_arx(boots[[1]])
#' 
#' or
#' sims <- arx_simulation(100)
#' dat <- data.frame(sims$X, sims$M, sims$Y)
#' prediction_arx(dat)
#' 
prediction_arx <- function(data, p = 10){
  # construct models
  mod_x <- arima(data[,1], order = c(1,0,0))
  mod_M <- arima(data[,2], order = c(1,0,0), xreg = data[,1])
  mod_Y <- arima(data[,3], order = c(1,0,0), xreg = data.frame(data[,1], data[,2]))
  
  # construct test points
  segment_length <- (max(data[,1])-min(data[,1]))/(p-1)
  x_range <- seq(min(data[,1]), max(data[,1]), segment_length)
  
  m_range <- c()
  for (i in 1:length(x_range)) {
    m_range[i] <- predict(mod_M, newxreg = x_range[i])$pred[1]
  }
  
  y_range <- c()
  for (i in 1:length(x_range)) {
    y_range[i] <- predict(mod_Y, newxreg = data.frame(x_range[i], m_range[i]))$pred[1]
  }
  
  # construct the data frames to use for each estimate
  N <- length(data[,1])
  baseline_x <- data[(N-1),1]
  baseline_m <- data[(N-1),2]
  baseline_y <- data[(N-1),3]
  
  change_x <- data.frame(rep(baseline_m,p), x_range)
  change_m <- data.frame(m_range, rep(baseline_x,p))
  baseline <- data.frame(baseline_m, baseline_x)
  
  baseline_prediction <- predict(mod_Y, newdata = baseline_y, newxreg = baseline)$pred[1]
  
  # create predictions for each quantity
  # Natural Indirect Effect (NIE)
  NIE <- c()
  
  # self-mediated direct effect
  SMDE <- c()
  
  # cross-mediated direct effect
  CMDE <- c()
  
  for (i in 1:length(x_range)) {
    NIE[i] <- predict(mod_Y, newdata = baseline_y, newxreg = change_m[i,])$pred[1] - baseline_prediction
    
    SMDE[i] <- predict(mod_Y, newdata = baseline_y, newxreg = change_x[i,])$pred[1] - baseline_prediction
    
    CMDE[i] <- predict(mod_Y, newdata = y_range[i], newxreg = baseline)$pred[1] - baseline_prediction
  }
  
  # find standardized differences
  indirect_effect <- (NIE[2:length(x_range)] - NIE[1:(length(x_range)-1)])/segment_length
  self_mediated_effect <- (SMDE[2:length(x_range)] - SMDE[1:(length(x_range)-1)])/segment_length
  cross_mediated_effect <- (CMDE[2:length(x_range)] - CMDE[1:(length(x_range)-1)])/segment_length
  
  # output a list of vectors
  list(indirect_effect = indirect_effect, self_mediated_effect = self_mediated_effect, cross_mediated_effect = cross_mediated_effect)
  
  # can find mechanistic estimates to compare to?
  #direct_effect <- mod_Y$coef
}

# pred_arx_sum(boots)
pred_arx_sum <- function(boots){
  n <- length(boots)
  out <- data.frame(matrix(nrow = n, ncol = 2))
  for (i in 1:n) {
    out[i,] <- prediction_arx(boots[[i]])
  }
  
  out
  
}