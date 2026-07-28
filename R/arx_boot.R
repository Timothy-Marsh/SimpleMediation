#' Run the bootstrap algorithm on simulated ARX data to get an estimate for the covariance between variables and parameters.
#'
#' @param boot_reps An integer indicating the number of bootstrap replications to do
#' @param params A named list indicating the true parameters to use for the simulated data
#'
#' @return A list with each entry being one bootstrap replicate of the simulated data
#'
#' @export
#'
#' @examples
#' arx_boot(500)

arx_boot <- function(boot_reps = 100, sim_length = 1000, params = list(alpha = 0.5, beta = c(0.2,0.8), eta = c(0.3,0.4,0.5))){
  
  #simulate the data
  data <- arx_simulation(sim_length, alpha = params$alpha, beta = params$beta, eta = params$eta, initials = c(0,0,0))
  
  # get bootstrap replicates of the data
  
    # data prep
  data <- data.frame(X = data$X, M = data$M, Y = data$Y)
  
    # using `arx_mediation_boot`
  boots <- arx_mediation_boot(data, p = c(1,1,1), R = boot_reps)
  
  boots$bootReps
}