#' Perform bootstrapping on the residuals of a time series process
#'
#' @param df A data frame with a value changing over time
#' @param n An integer indicating the degree of AR model given
#'
#' @return An estimate of the time series, and the bootstrap results
#'
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' ts_simple_resid_boot(x)

ts_simple_resid_boot <- function(df, n) {
  # add something here to check the type of data given and do different things with it
  x <- df

  ts_model <- arima(x, order = c(n,0,0))

  # interesting data but unnecessary for our purposes here I think?
  #  Would it be useful to have some kind of output like this for the hospital project?
  #check_res <- checkresiduals(ts_model)

  # find and centre the residuals of the time series model
  resids <- residuals(ts_model)
  resids <- resids - mean(resids)

  coefs <- ts_model$coef
  
  ts_mean <- mean(x)

  #boot function to run on that model
  #tsboot(resids, statistic = run_boot_ts, R = 500, sim = "model")
  #boot_resids <- boot::boot(data = resids, statistic = run_boot_ts, R = 500)
  R <- 500
  boot_resids <- run_boot_ts(resids, R)
  
  #final bootstrapped iteration is something like this
  
  # this should be a data frame? which each column as a bootstrap replicate of values in the AR series
  #ts_booted <- ts_mean + coefs %*% x + boot_resids$t
  
  # initializing the data frame
  boot_reps <- data.frame(matrix(nrow = length(x), ncol = R))
  # a series of loops to build the R bootstrap replicates of the time series itself
  for (i in seq(1:R)) {
    for (j in seq(1:length(x))) {
      boot_reps[i,j] <- ts_mean + coefs[1] * boot_reps[i,j-1] + boot_resids[i,j]
    }
  }
  
  ts_booted

}
