#' Run a simple Mediation
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
  n <- nrow(df)

  use_samp <- df[samps,]
  x <- use_samp[,1]
  m <- use_samp[,2]
  y <- use_samp[,3]
  MX.mod <- stats::lm(m ~ x)
  a <- MX.mod$coefficients[[2]]
  YX.mod <- stats::lm(y ~ x)
  c <- YX.mod$coefficients[[2]]
  YMX.mod <- stats::lm(y ~ m + x)
  b <- YMX.mod$coefficients[[2]]
  c_prime <- YMX.mod$coefficients[[3]]

  indirect_effect <- a*b
  indirect_effect_v2 <- c - c_prime
  direct_effect <- c_prime
  total_effect <- indirect_effect + direct_effect

  return(c(total_effect, direct_effect, indirect_effect, indirect_effect_v2))
}
