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
  time <- df[,1]
  x <- df[,2]

  #function to estimate the time series (and identify what type of time series it is)
  ts_model <- auto.arima(x)

  resids <- checkresiduals(ts_model)

  ts_plot <- plot(ts_model)

  #boot function to run on that model


}
