#' Perform bootstrapping on the residuals of a time series process
#'
#' @param x residuals from a time series process
#' @param R number of bootstrap replicates
#'
#' @return A data frame of bootstrap replicates of the residuals
#'
#' @export
#'
#' @examples
#' x <- residuals(ts_object)
#' run_boot_ts(x, R)

run_boot_ts <- function(x, R) {
  
  n <- length(x)
  
  samps <- data.frame(matrix(nrow = n, ncol = R))

  for (i in seq(1:R)) {
    samps[,i] <- sample(x, n, replace = TRUE)
  }

  samps
}
