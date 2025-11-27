#' Perform regression on lagged data
#'
#' @param df A data frame an x and y variable
#' @param n An integer indicating what degree of lag to use
#'
#' @return A linear model of y on x and n lagged x terms
#'
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' y <- runif(100)
#' df <- data.frame(x,y)
#' lags(df,3)
lags <- function(formula, df) {
  # can use the rlang package
  library(rlang)

  lhs <- f_lhs(formula)
  rhs <- f_rhs(formula)

  # n is the number of lags desired
  n <- f_rhs(formula)[[3]]

  # this works to detect if the left hand side value is in the data frame!
  if (as.character(lhs) %in% colnames(df)) {
    lhs_data <- df[, which(colnames(df) == lhs)]
  } else {
    paste("Variable", lhs, "not found")
  }

  if (as.character(rhs[[2]]) %in% colnames(df)) {
    rhs_data <- df[, which(colnames(df) == rhs[[2]])]
  } else {
    paste("Variable", rhs, "not found")
  }


  library(dplyr)

  lagged_vals <- data.frame(matrix(nrow = length(lhs_data), ncol = n))

  for (i in seq(1:n)) {
    lagged_vals[, i] <- lag(rhs_data, i)
  }

  lagged_vals$lhs_data <- lhs_data
  lagged_vals$rhs_data <- rhs_data

  lagged_vals <- na.omit(lagged_vals)

  lagged_model <- lm(lhs_data ~ ., data = lagged_vals)
  lagged_model
}