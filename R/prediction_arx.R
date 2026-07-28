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
  mod_M <- arima(data[,2], order = c(1,0,0), xreg = data[,1])
  mod_Y <- arima(data[,3], order = c(1,0,0), xreg = data.frame(data[,1], data[,2]))
  
  direct_effect <- mod_Y$coef
  
  x_range <- seq(min(data[,1]), max(data[,1]), (max(data[,1])-min(data[,1]))/p)
  m_range <- seq(min(data[,2]), max(data[,2]), (max(data[,2])-min(data[,2]))/p)
  
  total_change <- data.frame(m_range,x_range)
  indirect_change <- data.frame(m_range, rep(mean(data[,1]),(p+1)))
  direct_change <- data.frame(rep(mean(data[,2]),(p+1)), x_range)
  baseline <- data.frame(rep(mean(data[,2]),(p+1)), rep(mean(data[,1]),(p+1)))
  
  TC <- c()
  IC <- c()
  DC <- c()
  NC <- c()
  
  for (i in 1:length(x_range)) {
    TC[i] <- predict(mod_Y, newxreg = total_change[i,])$pred[1]
    
    IC[i] <- predict(mod_Y, newxreg = indirect_change[i,])$pred[1]
    
    DC[i] <- predict(mod_Y, newxreg = direct_change[i,])$pred[1]
    
    NC[i] <- predict(mod_Y, newxreg = baseline[i,])$pred[1]
  }
  
  changes <- data.frame(all_change = TC, m_change = IC, x_change = DC, baseline = NC)
  
  direct <- changes$x_change - changes$baseline
  #direct2 <- changes$m_change - changes$all_change
  indirect <- changes$m_change - changes$baseline
  #indirect2 <- changes$x_change - changes$all_change
  #data.frame(direct = direct, direct2 = direct2, indirect = indirect, indirect2 = indirect2)
  
  direct_summary <- diff(direct)/((max(data[,1])-min(data[,1]))/p)
  indirect_summary <- diff(indirect) / ((max(data[,2])-min(data[,2]))/p)
  
  c(indirect_summary[1], direct_summary[1])
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