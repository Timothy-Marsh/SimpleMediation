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
  x <- df[,1]

  ts_model <- auto.arima(x)

  check_res <- checkresiduals(ts_model)

  resids <- residuals(ts_model)

  #boot function to run on that model
  boot::tsboot(resids, statistic = run_boot_ts, R = 500, sim = "model")

}
