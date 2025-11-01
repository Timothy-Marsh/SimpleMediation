#' Performs simple mediation
#'
#' @param df A data frame with columns X, M, and Y
#'
#' @return Values for the total effect, direct effect, the indirect effect calculated in two different ways, and standard error for each of these values.
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
 #
 # idk what function to use for the bootstrap atm so the following is a placeholder structure
  boot::boot(data = df, statistic = run_mediation, R = 500)

}
