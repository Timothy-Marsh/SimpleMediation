#' Perform bootstrapping on the residuals of a time series process
#'
#' @param df A data frame with a value changing over time
#'
#' @return An estimate of the time series, and the bootstrap results
#'
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' ts_simple_resid_boot(x)

ts_simple_resid_boot <- function(df) {
  # add something here to check the type of data given and do different things with it
  x <- df

  ts_model <- auto.arima(x)

  check_res <- checkresiduals(ts_model)

  resids <- residuals(ts_model)
  resids <- resids - mean(resids)

  coefs <- ts_model$coef

  #boot function to run on that model
  #tsboot(resids, statistic = run_boot_ts, R = 500, sim = "model")
  boot_resids <- boot::boot(data = resids, statistic = run_boot_ts, R = 500)

}
