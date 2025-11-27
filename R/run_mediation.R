#' Run a simple linear Mediation
#'
#' @param df a vector of observations
#' @param samps The sample indices, as required by boot::boot()
#'
#' @return An estimate of the total, direct, and two estimates of the indirect effect in that order
#' @export
#'
#' @examples
#' x <- c(1,2,3,4,5,6,7)
#' m <- c(1,2.3,4.2,4.9,5.6,6,6.7)
#' y <- c(3.4,5.4,6.5,7.9,8,10,11)
#' df <- data.frame(x,m,y)
#' run_mediation(df)
run_mediation <- function(df, samps) {
  use_samp <- df[samps, ]
  x <- use_samp[, 1]
  m <- use_samp[, 2]
  y <- use_samp[, 3]
  mediator_model <- stats::lm(m ~ x)
  a <- mediator_model$coefficients[[2]]
  response_model <- stats::lm(y ~ x)
  c <- response_model$coefficients[[2]]
  full_model <- stats::lm(y ~ m + x)
  b <- full_model$coefficients[[2]]
  direct_effect <- full_model$coefficients[[3]]

  indirect_effect <- a * b
  indirect_effect_v2 <- c - direct_effect
  total_effect <- indirect_effect + direct_effect

  return(c(total_effect, direct_effect, indirect_effect, indirect_effect_v2))
}
