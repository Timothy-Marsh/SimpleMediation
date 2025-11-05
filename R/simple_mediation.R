#' Perform simple linear mediation on variables X, M, and Y
#'
#' @param df A data frame with columns X, M, and Y
#'
#' @return Values for the total effect, direct effect, the indirect effect calculated in two different ways, and standard error for each of these values.
#'   Also returns a covariance matrix for those values.
#' @export
#'
#' @examples
#' x <- c(1,2,3,4,5,6,7)
#' m <- c(1,2.3,4.2,4.9,5.6,6,6.7)
#' y <- c(3.4,5.4,6.5,7.9,8,10,11)
#' df <- data.frame(x,m,y)
#' simple_mediation(df)
simple_mediation <- function(df) {
 # Structure this as calling a function that runs the mediation and bootstrapping that
 
 bootstrap_results <- boot::boot(data = df, statistic = run_mediation, R = 500)
 
 # possibly separate this out as its own function?
 bootstrap_covariance <- cov(na.omit(bootstrap_results$t))
 rownames(bootstrap_covariance) <- c("Total Effect", "Direct Effect", "Indirect Effect", "Indirect Effect")
 colnames(bootstrap_covariance) <- c("Total Effect", "Direct Effect", "Indirect Effect", "Indirect Effect")
 
 list(bootstrap_results,bootstrap_covariance)
}
