#' Perform bootstrapping on the residuals of a time series process and return the specified number of bootstrap replicates of the time series
#'
#' @param x A vectors of time series data
#' @param n An integer indicating the degree of AR model given
#' @param R An integer for the number of bootstrap replicates used
#'
#' @return An estimate of the time series, and the bootstrap results
#'
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' ts_simple_resid_boot(x, 2, 50)

ts_simple_resid_boot <- function(x, n, R) {
  # add something here to check the type of data given and do different things with it

  ts_model <- arima(x, order = c(n,0,0))

  # find and centre the residuals of the time series model
  resids <- residuals(ts_model)
  resids <- resids - mean(resids)

  # create variables for the mean of the time series and the coefficients of the model
  mod_intercept <- ts_model$coef[n+1]
  coefs <- ts_model$coef[1:n]
  coefs
  ts_mean <- mean(x)

  # call a function to bootstrap the residuals
  boot_resids <- run_boot_ts(resids, R)

  # initializing the data frame
  boot_reps <- data.frame(matrix(nrow = length(x), ncol = R))

  # since AR models depend on previous entries, we initialize it to be the initial observations
  for (i in seq(1:R)) {
    boot_reps[,i] <- x
  }

  # a series of loops to build the R bootstrap replicates of the time series itself
  for (i in seq(1:R)) {
    for (j in ((n+1):length(x))) {
      boot_reps[j,i] <- ts_mean + sum(coefs * rev(boot_reps[,i][(j-n-1):(j-1)])) + boot_resids[j,i]
    }
  }

  boot_reps
}
