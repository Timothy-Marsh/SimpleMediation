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
  library(patchwork)
  
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
  
  indirect_true <- eta_true[2] * beta_true[2]
  SMDE_true <- eta_true[3] * alpha_true
  CMDE_true <- eta_true[1] * eta_true[3]
  
  data_alpha <- data.frame(alpha = results$params_x$alpha)
  
  alphas <- ggplot(data_alpha, aes(x="", y=alpha)) + 
    geom_violin() + 
    #geom_boxplot() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = alpha_true, yend = alpha_true),
                 color = 'red',
                 linewidth = 1) +
    labs( y = "alpha",x = "")
  
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
    labs( y = "Beta", x = "")
  
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
    labs(y = "Eta",x = "")
  
  true_XM <- (beta_true[2]*alpha_true)/((1-alpha_true^2) * (1-(beta_true[1]*alpha_true)))
  true_XY <- ((eta_true[2]^2 * beta_true[2] * eta_true[3])+(eta_true[3]*alpha_true * (1-(beta_true[1]*alpha_true)))) / ((1-alpha_true^2) * (1-(beta_true[1]*alpha_true)) * (1-(eta_true[1]*alpha_true)))
  true_MY <- (eta_true[2]*beta_true[1])/((1-beta_true[1]^2) * (1-(eta_true[1]*beta_true[1]))) + ((beta_true[1]*(beta_true[2]^2)*eta_true[2] + beta_true[2]*eta_true[3]*(1-(beta_true[1]^2))) / ((1-(alpha_true^2))*(1-(beta_true[1]^2))*(1-(beta_true[1]*eta_true[1])))) + ((alpha_true*beta_true[1]*beta_true[2]*eta_true[3]+alpha_true*beta_true[2]*eta_true[1]*eta_true[3]+alpha_true*(beta_true[2]^2)*eta_true[2]-2*(alpha_true^2)*beta_true[1]*beta_true[2]*eta_true[1]*eta_true[3]) / ((1-(alpha_true^2)) * (1-(beta_true[1]*eta_true[1])) * (1-(alpha_true*beta_true[1])) * (1-(alpha_true*eta_true[1]))))
    
  data_XM <- data.frame(results = results$covariances$XM, x = rep("Cov(X,M)", n))
  cov_XM <- ggplot(data_XM, aes(y = results, x = x)) + 
    geom_violin() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = true_XM, yend = true_XM),
                 color = 'red',
                 linewidth = 1)+
    labs(x="",y="")
  
  data_XY <- data.frame(results = results$covariances$XY, x = rep("Cov(X,Y)", n))
  cov_XY <- ggplot(data_XY, aes(y = results, x = x)) + 
    geom_violin() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = true_XY, yend = true_XY),
                 color = 'red',
                 linewidth = 1)+
    labs(x="",y="")
  
  data_MY <- data.frame(results = results$covariances$MY, x = rep("Cov(M,Y)", n))
  cov_MY <- ggplot(data_MY, aes(y = results, x = x)) + 
    geom_violin()  +
    geom_segment(aes(x = 0.5, xend = 1.5, y = true_MY, yend = true_MY),
                 color = 'red',
                 linewidth = 1)+
    labs(x="",y="")
  
  param_estimates <- data.frame(alpha = results$params_x$alpha, beta1 = results$params_m$beta1, beta2 = results$params_m$beta2, eta1 = results$params_y$eta1, eta2 = results$params_y$eta2, eta3 = results$params_y$eta3)
  cov_matrix <- cov(param_estimates)
  cov_df <- as.data.frame(as.table(cov_matrix))
  names(cov_df) <- c("Var1", "Var2", "Covariance")
  
  cov_plot <- ggplot(cov_df, aes(x = Var1, y = Var2, fill = Covariance)) + 
                geom_tile(color = "white") +
                scale_fill_gradient2(
                  low = "blue", mid = "white", high = "red", midpoint = 0,
                  name = "Covariance"
                ) +
                theme_minimal(base_size = 14) +
                theme(
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                  panel.grid = element_blank()
                ) +
                coord_fixed() +
                labs(title = "Covariance Matrix Heatmap", y ="", x="",
                     subtitle =bquote(alpha == .(alpha_true) ~ ", " ~ beta[0] == .(beta_true[1])~ ", " ~ beta[1] == .(beta_true[2])~ ", " ~ eta[0] == .(eta_true[1])~ ", " ~ eta[1] == .(eta_true[2])~ ", " ~ eta[2] == .(eta_true[3])))
  
  indirect_obs <- results$params_y$eta2 * results$params_m$beta2
  SMDE_obs <- results$params_y$eta3 * results$params_x$alpha
  CMDE_obs <- results$params_y$eta1 * results$params_y$eta3
  
  effects_df <- data.frame(results = c(indirect_obs, SMDE_obs, CMDE_obs), Effects = c(rep("Indirect", length(indirect_obs)), rep("SMDE", length(indirect_obs)), rep("CMDE",length(indirect_obs))))
  effects_plot <- ggplot(effects_df, aes(x = factor(Effects, levels = c("Indirect", "SMDE", "CMDE")), y = results)) +
    geom_violin() +
    geom_segment(aes(x = 0.5, xend = 1.5, y = indirect_true, yend = indirect_true,color = "Theoretical Effect"),
                 linewidth = 1) + 
    geom_segment(aes(x = 1.5, xend = 2.5, y = SMDE_true, yend = SMDE_true,color = "Theoretical Effect"),
                 linewidth = 1) + 
    geom_segment(aes(x = 2.5, xend = 3.5, y = CMDE_true, yend = CMDE_true, color = "Theoretical Effect"),
                 linewidth = 1) +
    labs(title = "Observed Effects from AR based Method", y = "", x = "",
         subtitle = bquote(alpha == .(alpha_true) ~ ", " ~ beta[0] == .(beta_true[1])~ ", " ~ beta[1] == .(beta_true[2])~ ", " ~ eta[0] == .(eta_true[1])~ ", " ~ eta[1] == .(eta_true[2])~ ", " ~ eta[2] == .(eta_true[3]))) +
    scale_color_manual(name = NULL, values = c("Theoretical Effect" = 'red'))
  
  covs <- cov_XM+cov_XY+cov_MY + plot_annotation(title = "Observed covariances with Theoretical Value",
                                                 subtitle = bquote(alpha == .(alpha_true) ~ ", " ~ beta[0] == .(beta_true[1])~ ", " ~ beta[1] == .(beta_true[2])~ ", " ~ eta[0] == .(eta_true[1])~ ", " ~ eta[1] == .(eta_true[2])~ ", " ~ eta[2] == .(eta_true[3])))
  params <- alphas + betas + etas + plot_annotation(title = "Observed Parameters with Actual Values",
                                                    subtitle = bquote(alpha == .(alpha_true) ~ ", " ~ beta[0] == .(beta_true[1])~ ", " ~ beta[1] == .(beta_true[2])~ ", " ~ eta[0] == .(eta_true[1])~ ", " ~ eta[1] == .(eta_true[2])~ ", " ~ eta[2] == .(eta_true[3])))
  
  list(params <- params, covs <- covs, cov_plot, effects_plot)
}