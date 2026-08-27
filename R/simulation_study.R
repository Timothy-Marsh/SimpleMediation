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


simulation_study <- function(parameters = list(list(alpha = 0.3, beta = c(0.2,0.4), eta = c(0.3,0.2,0.3)),
                                               list(alpha = 0.1, beta = c(0.1,0.1), eta = c(0.1,0.1,0.1)),
                                               list(alpha = 0.7, beta = c(0.2,0.3), eta = c(0.1,0.3,0.2)),
                                               list(alpha = 0.8, beta = c(0.4,0.3), eta = c(0.7,0.145,0.1)),
                                               list(alpha = 0.3, beta = c(0.3,0.4), eta = c(0.3,0.4,0.5)),
                                               list(alpha = 0.7, beta = c(0.6,0.4), eta = c(0.7,0.6,0.9)),
                                               list(alpha = 0.6, beta = c(0.6,0.6), eta = c(0.6,0.6,0.6)),
                                               list(alpha = 0.5, beta = c(0.2,0.8), eta = c(0.3,0.4,0.5))), sample_length = 500, sample_reps = 100){
  n <- length(parameters)
  
  plots <- list()
  
  # for each set of parameters we need to run the algorithm
  for (i in 1:n) {
    # simulate data and get simulation replicates
    data <- list()
    predictions <- list()
      for (j in 1:sample_reps) {
        data[[j]] <- arx_simulation(sample_length, alpha = parameters[[i]]$alpha, beta = parameters[[i]]$beta, eta = parameters[[i]]$eta, initials = c(0,0,0))
        
        predictions[[j]] <- prediction_arx(data.frame(data[[j]]$X, data[[j]]$M, data[[j]]$Y))
        }
    
    
    # on each simulation replicate calculate the estimates for the parameters
    estimates <- arx_summary(data, params = parameters[[i]])
    
    # produce violin plots to overlay the results
    plots[[i]] <- list(arx_plotting(estimates),prediction_plot_helper(predictions, parameters[[i]]))
    
    
  }
  
  plots
  # ouput those charts for every set of parameters
}


prediction_plot_helper <- function(predictions, true_params){
  w <- length(predictions)
  
  predictions_df <- data.frame(indirect_effect = NA, self_mediated_effect = NA, cross_mediated_effect = NA, theory_NIE = NA, theory_SMDE = NA, theory_CMDE = NA)  
  
  for (i in 1:w) {
    predictions_df[i,1] <- predictions[[i]]$indirect_effect[1]
    predictions_df[i,2] <- predictions[[i]]$self_mediated_effect[1]
    predictions_df[i,3] <- predictions[[i]]$cross_mediated_effect[1]
    predictions_df[i,4] <- predictions[[i]]$theory_NIE
    predictions_df[i,5] <- predictions[[i]]$theory_SMDE
    predictions_df[i,6] <- predictions[[i]]$theory_CMDE
  }
  
  predict_df <- data.frame(results = c(predictions_df[,1],predictions_df[,4], predictions_df[,2], predictions_df[,5],predictions_df[,3],predictions_df[,6]),
                           indicator = c(rep("Prediction NIE",w), rep("AR NIE",w), rep("Prediction SMDE",w), rep("AR SMDE",w),rep("Prediction CMDE",w), rep("AR CMDE",w)))

  ggplot(data = predict_df, aes(x = factor(indicator, levels = c("Prediction NIE","AR NIE","Prediction SMDE","AR SMDE","Prediction CMDE","AR CMDE")), y = results))+
    geom_violin() + 
    labs(y = "", x = "Effect", title = "Prediction Based Effects vs AR Theoretical Effects", 
         subtitle = bquote(alpha == .(true_params$alpha) ~ ", " ~ beta[0] == .(true_params$beta[1])~ ", " ~ beta[1] == .(true_params$beta[2])~ ", " ~ eta[0] == .(true_params$eta[1])~ ", " ~ eta[1] == .(true_params$eta[2])~ ", " ~ eta[2] == .(true_params$eta[3])) )
  }