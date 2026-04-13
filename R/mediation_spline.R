#' A helper function to run a single iteration of spline mediation, which will then be used to perform bootstrapping on
single_spline <- function(df, samps) {
  use_samp <- df[samps, ]
  x <- use_samp[, 1]
  m <- use_samp[, 2]
  y <- use_samp[, 3]
  
  knots <- 5
  
  #basisSplines <- mgcv::s(x, bs = "bs", k = 5)
  
  #mediator_model <- lm(m ~ basisSplines, data = use_samp)
  
  # create the models and get the needed coefficients from them
  mediator_model <- mgcv::gam(m ~ s(x, bs = "bs", k = knots), family = gaussian(), data = use_samp)
  
  phi <- mediator_model$coefficients
  
  response_model <- mgcv::gam(y ~ s(x, bs = "bs", k = knots), family = gaussian(), data = use_samp)
  
  full_model <- mgcv::gam(y ~ s(x, bs = "bs", k = knots) + m)
  
  beta <- full_model$coefficients["m"]
  
  theta <- full_model$coefficients[3:(knots+1)]
  
  # need to get the smooth values used in the gams to calculate the direct and indirect effects. <- these should be the basis splines used I think
  
  # this gives a matrix of the splines estimated at each point
  med_bs <- predict(mediator_model, type = "lpmatrix")
  
  resp_bs <- predict(response_model, type = "lpmatrix")
  
  full_bs <- predict(full_model, type = "lpmatrix")
  # ntoe that resp_bs and full_bs give the same basis splines for each point!
  
  # to get an estimate for the direct effect
  
  direct_effect <- sum(theta * colMeans(resp_bs)[2:5])
  
  indirect_effect <- sum(phi[2:5] * beta * colMeans(resp_bs)[2:5])
  
  total_effect <- direct_effect + indirect_effect
  
  return(c(total_effect, direct_effect, indirect_effect))
}

#' Perform a spline mediation on variables X, M, and Y
#' 
#' @param df A data frame with columns X, M, and Y
#'
#' @return Values for the total effect, direct effect, the indirect effect calculated in two different ways, and standard error for each of these values.
#'   Also returns a covariance matrix for those values.
#' @export
#'
#' @examples
# x <- c(1,2,3,4,5,6,7)
# m <- c(1,2.3,4.2,4.9,5.6,6,6.7)
# y <- c(3.4,5.4,6.5,7.9,8,10,11)
# df <- data.frame(x,m,y)
#' mediation_spline(df)
mediation_spline <- function(df) {
  bootstrap_results <- boot::boot(data = df, statistic = single_spline, R = 500)
  
  bootstrap_covariance <- cov(na.omit(bootstrap_results$t))
  rownames(bootstrap_covariance) <- c("Total Effect", "Direct Effect", "Indirect Effect")
  colnames(bootstrap_covariance) <- c("Total Effect", "Direct Effect", "Indirect Effect")
  
  list(bootstrap_results, bootstrap_covariance)
}
