#' Use a prediction based method along with random forest models to find the direct and indirect effects of a mediation system
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
#' prediction_rf(boots[[1]])
#' 
#' or
#' sims <- arx_simulation(100)
#' dat <- data.frame(sims$X, sims$M, sims$Y)
#' prediction_rf(dat)
#' 
prediction_rf <- function(data, p = 10){
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package \"randomForest\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }
  
  # construct models
  N <- length(data[,1])
  data = data.frame(x = data[,1][-1], m = data[,2][-1], y = data[,3][-1], xlag = data[,1][-N], mlag = data[,2][-N], ylag = data[,3][-N])
  mod_x <- randomForest::randomForest(x ~ xlag, data = data)
  mod_M <- randomForest::randomForest(m ~ mlag + xlag, data = data)
  mod_Y <- randomForest::randomForest(y ~ ylag + mlag + xlag, data = data)
  
  # construct test points
  segment_length <- (max(data[,1])-min(data[,1]))/(p-1)
  x_range_initial <- seq(min(data[,1]), max(data[,1]), segment_length)
  
  x_range <- c()
  j <- length(x_range_initial)
  for (i in 1:j) {
    x_range[i] <- predict(mod_x, newdata = data.frame(xlag = x_range_initial[i]))
  }
  
  m_range <- c()
  for (i in 1:length(x_range)) {
    m_range[i] <- predict(mod_M, newdata = data.frame(xlag = x_range_initial[i], mlag = data$m[(N-1)]))
  }
  
  y_range <- c()
  for (i in 1:length(x_range)) {
    y_range[i] <- predict(mod_Y, newdata = data.frame(xlag = x_range_initial[i], mlag = m_range[i], ylag = data$y[(N-1)]))
  }
  
  # construct the data frames to use for each estimate
  baseline_x <- tail(data$x,1)
  baseline_m <- tail(data$m,1)
  baseline_y <- tail(data$y,1)
  
  change_x <- data.frame(mlag = rep(baseline_m,p), xlag = x_range, ylag = rep(baseline_y,p))
  change_m <- data.frame(mlag = m_range, xlag = rep(baseline_x,p), ylag = rep(baseline_y,p))
  change_y <- data.frame(mlag = rep(baseline_m,p), xlag = rep(baseline_x,p), ylag = y_range)
  baseline <- data.frame(mlag = baseline_m, xlag = baseline_x, ylag = baseline_y)
  
  baseline_prediction <- predict(mod_Y, newdata = baseline)
  
  # create predictions for each quantity
  # Natural Indirect Effect (NIE)
  NIE <- c()
  
  # self-mediated direct effect
  SMDE <- c()
  
  # cross-mediated direct effect
  CMDE <- c()
  
  for (i in 1:length(x_range)) {
    NIE[i] <- predict(mod_Y, newdata = change_m[i,]) - baseline_prediction
    
    SMDE[i] <- predict(mod_Y, newdata = change_x[i,]) - baseline_prediction
    
    CMDE[i] <- predict(mod_Y, newdata = change_y[i,]) - baseline_prediction
  }
  
  # find standardized differences
  indirect_effect <- (NIE[2:length(x_range)] - NIE[1:(length(x_range)-1)])/segment_length
  self_mediated_effect <- (SMDE[2:length(x_range)] - SMDE[1:(length(x_range)-1)])/segment_length
  cross_mediated_effect <- (CMDE[2:length(x_range)] - CMDE[1:(length(x_range)-1)])/segment_length
  
  # output a list of vectors
  list(indirect_effect = indirect_effect, self_mediated_effect = self_mediated_effect, cross_mediated_effect = cross_mediated_effect)
}