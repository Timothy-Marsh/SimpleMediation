#' Plot the results of `arx_summary`, giving a violin plot for the parameters
#'
#' @param results A named list of lists, each one being a property of the data
#'
#' @return Three plots, one for each variable
#'
#' @export
#'
#' @examples
#' boots <- arx_boot(500, params = list(alpha = 0.2, beta = c(0.1,0.5), eta = c(0.2,0.3,0.6)))
#' boots2 <- arx_summary(boots, params = list(alpha = 0.2, beta = c(0.1,0.5), eta = c(0.2,0.3,0.6)))
#' arx_plotting(boots2)

arx_plotting <- function(results){
  library(ggplot2)
  
  if (is.null(results$true_params$alpha)) {
    data_alpha <- data.frame(alpha = results$params_x$alpha)
    
    alphas <- ggplot(data_alpha, aes(x="", y=alpha)) + 
      geom_violin() + 
      labs(title = "Violin plot of Alpha estimates",
           y = "alpha")
    
    n <- length(results$params_m$beta1)
    data_beta <- data.frame(var = c(rep("beta_1", n), rep("beta_2",n)), results = c(results$params_m$beta1,results$params_m$beta2))
    betas <- ggplot(data_beta, aes(x=var, y=results)) + 
      geom_violin() + 
      labs(title = "Violin plot of Beta estimates",
           y = "Beta")
    
    data_eta <- data.frame(var = c(rep("eta_1", n), rep("eta_2",n), rep("eta_3",n)), results = c(results$params_y$eta1,results$params_y$eta2, results$params_y$eta3))
    etas <- ggplot(data_eta, aes(x=var, y=results)) + 
      geom_violin() + 
      labs(title = "Violin plot of Eta estimates",
           y = "Eta")
    
    return(list(alpha <- alphas, beta <- betas, eta <- etas))
  }
  
  alpha_true <- results$true_params$alpha
  beta_true <- results$true_params$beta
  eta_true <- results$true_params$eta
  
  data_alpha <- data.frame(alpha = results$params_x$alpha)
  
  alphas <- ggplot(data_alpha, aes(x="", y=alpha)) + 
    geom_violin() + 
    #geom_boxplot() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = alpha_true, yend = alpha_true),
                 color = 'red',
                 linewidth = 1) +
    labs(title = "Violin plot of Alpha estimates",
         y = "alpha")
  
  n <- length(results$params_m$beta1)
  data_beta <- data.frame(var = c(rep("beta_1", n), rep("beta_2",n)), results = c(results$params_m$beta1,results$params_m$beta2))
  betas <- ggplot(data_beta, aes(x=var, y=results)) + 
    geom_violin() + 
    #geom_boxplot() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = beta_true[1], yend = beta_true[1]),
                 color = 'red',
                 linewidth = 1) +
    geom_segment(aes(x = 1.5, xend = 2.5, y = beta_true[2], yend = beta_true[2]),
                 color = 'red',
                 linewidth = 1) +
    labs(title = "Violin plot of Beta estimates",
         y = "Beta")
  
  data_eta <- data.frame(var = c(rep("eta_1", n), rep("eta_2",n), rep("eta_3",n)), results = c(results$params_y$eta1,results$params_y$eta2, results$params_y$eta3))
  etas <- ggplot(data_eta, aes(x=var, y=results)) + 
    geom_violin() + 
    #geom_boxplot() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = eta_true[1], yend = eta_true[1]),
                 color = 'red',
                 linewidth = 1) +
    geom_segment(aes(x = 1.5, xend = 2.5, y = eta_true[2], yend = eta_true[2]),
                 color = 'red',
                 linewidth = 1) +
    geom_segment(aes(x = 2.5, xend = 3.5, y = eta_true[3], yend = eta_true[3]),
                 color = 'red',
                 linewidth = 1) +
    labs(title = "Violin plot of Eta estimates",
         y = "Eta")
  
  true_XM <- (beta_true[2]^2)/((1-alpha_true^2)^2 * (1-beta_true[1]^2))
  true_XY <- ((eta_true[2]^2 * beta_true[2]^2)+(eta_true[3]^2 * (1-beta_true[1]^2)))/((1-alpha_true^2)^2 * (1-beta_true[1]^2) * (1-eta_true[1]^2))
  true_MY <- (eta_true[2]^2)/((1-beta_true[1]^2)^2 * (1-eta_true[1]^2)) + ((eta_true[2]^2 * beta_true[2]^4)+(eta_true[3]^2 * (1-beta_true[1]^2) * beta_true[2]^2))/((1-alpha_true^2)^2 * (1-beta_true[1]^2)^2 * (1-eta_true[1]^2))
    
  data_XM <- data.frame(results = results$covariances$XM, x = rep("Cov(X,M)", n))
  cov_XM <- ggplot(data_XM, aes(y = results, x = x)) + 
    geom_violin() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = true_XM, yend = true_XM),
                 color = 'red',
                 linewidth = 1)
  
  data_XY <- data.frame(results = results$covariances$XY, x = rep("Cov(X,Y)", n))
  cov_XY <- ggplot(data_XY, aes(y = results, x = x)) + 
    geom_violin() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = true_XY, yend = true_XY),
                 color = 'red',
                 linewidth = 1)
  
  data_MY <- data.frame(results = results$covariances$MY, x = rep("Cov(M,Y)", n))
  cov_MY <- ggplot(data_MY, aes(y = results, x = x)) + 
    geom_violin()  +
    geom_segment(aes(x = 0.5, xend = 1.5, y = true_MY, yend = true_MY),
                 color = 'red',
                 linewidth = 1)
  
  list(alpha <- alphas, beta <- betas, eta <- etas, cov_XM, cov_XY, cov_MY)
}