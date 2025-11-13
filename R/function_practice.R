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
lags <- function(df, n) {
  library(dplyr)
  #probably put this as a function (then can call the function for X and Y)
  if ("x" %in% colnames(df)) {
    x <- df$x
  } else if ("X" %in% colnames(df)) {
    x <- df$X
  } else{
    x <- df[,1]
  }
  
  if ("y" %in% colnames(df)) {
    y <- df$y
  } else if ("Y" %in% colnames(df)) {
    y <- df$Y
  } else{
    y <- df[,2]
  }
  
  lagged_vals <- data.frame(matrix(nrow = length(y), ncol = n))
  
  for (i in seq(1:n)) {
    lagged_vals[,i] <- lag(x,i)
  }
  
  lagged_vals$y <- y
  lagged_vals$x <- x
  
  lagged_vals <- na.omit(lagged_vals)
  
  lagged_model <- lm(y ~ ., data = lagged_vals)
  lagged_model
}