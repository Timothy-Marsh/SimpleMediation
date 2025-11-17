#' Perform bootstrapping on the residuals of a time series process
#'
#' @param x residuals from a time series process
#' @param samps indices of samples
#'
#' @return ?
#'
#' @export
#'
#' @examples
#' x <- residuals(ts_object)
#' run_boot_ts(x, samps)

run_boot_ts <- function(x, R) {
  
  n <- length(x)
  
  samps <- data.frame(matrix(nrow = n, ncol = R))

  for (i in seq(1:R)) {
    samps[,i] <- sample(x, n, replace = TRUE)
  }

  samps
  # to be returned, the mean of the process and the the prediction
  #c(mean(boot_samps), predict(boot_samps, n.ahead = 5))

}
