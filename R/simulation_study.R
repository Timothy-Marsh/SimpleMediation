#' 
#'
#' @param parameters A list of lists of the parameters in the mediation system
#'
#' @return 
#'
#' @export
#'
#' @examples
#' parameters <- list(list(alpha = 0.5, beta = c(0.2,0.8), eta = c(0.3,0.4,0.5)))

# A simulation study should choose several sets of true parameters, 
# simulate repeatedly from each set of parameters, 
# then compare the distribution of estimates from the simulation to the true parameters used for simulation.

# What exactly are we simulating here?
# - Demonstrating that our methods of estimating the functions and parameters are accurate
# - Start with several arrays of parameters
# - For each of these sets of parameters simulate data


simulation_study <- function(parameters = list(list(alpha = 0.5, beta = c(0.2,0.8), eta = c(0.3,0.4,0.5)),
                                               list(alpha = 0.1, beta = c(0.1,0.1), eta = c(0.1,0.1,0.1)),
                                               list(alpha = 0.3, beta = c(0.3,0.4), eta = c(0.3,0.4,0.5)),
                                               list(alpha = 0.7, beta = c(0.6,0.4), eta = c(0.7,0.6,0.9))), sample_length = 500, boot_reps = 100){
  n <- length(parameters)
  
  plots <- list()
  
  # for each set of parameters we need to run the algorithm
  for (i in 1:n) {
    # simulate data and get bootstrap replicates
    data <- arx_boot(boot_reps = boot_reps, sim_length = sample_length, params = parameters[[i]])
    
    # on each bootstrap replicate calculate the estimates for the parameters
    estimates <- arx_summary(data, params = parameters[[i]])
    
    # produce violin plots to overlay the results
    plots[[i]] <- arx_plotting(estimates)
    
  }
  
  plots
  # ouput those charts for every set of parameters
}