#' @description 
#' Script to plot estimates and CIs of fixed effects from 
#' multilevel functional regression with functional outcome.
#' 
#' The code is heavily based on plotting code published with Cui et al. (2021).  
#' 
#' @references
#' Erjia Cui, Andrew Leroux, Ekaterina Smirnova, Ciprian M. Crainiceanu (2021) 
#' Fast Univariate Inference for Longitudinal Functional Models, 
#' Journal of Computational and Graphical Statistics, 
#' DOI: 10.1080/10618600.2021.1950006
#' 
#' @author 
#' Marcos Matabuena, Marta Karas

rm(list = ls())
library(here)
library(tidyverse)
library(cowplot)

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 


# Function to plot 
plot.FUI <- function(r, fit_tmp, name = NULL){
  var_name <- c("Intercept", "Gender (1=male, 0=female)", "Race (1=HIIT, 0=CR)")
  decimal <- c(0,0,0)
  beta.hat.plt <- data.frame(s = seq(1, ncol(fit_tmp$betaHat), length.out = ncol(fit_tmp$betaHat)), 
                             beta = fit_tmp$betaHat[r,],
                             lower = fit_tmp$betaHat[r,] - 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             upper = fit_tmp$betaHat[r,] + 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             lower.joint = fit_tmp$betaHat[r,] - fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             upper.joint = fit_tmp$betaHat[r,] + fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])))
  p.CI <- ggplot() +
    theme_bw(base_size = 12) + 
    theme(legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
          panel.grid.major = element_line(size = 0.2),
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          strip.background = element_rect(fill = alpha('white', 0.1), color = NA),
          strip.text = element_text(angle = 0, hjust = 0),
          plot.title = element_text(hjust = 0.5, face = "bold")) + 
    geom_ribbon(aes(x = s, ymax = upper.joint, ymin = lower.joint), data = beta.hat.plt, fill = "gray30", alpha = 0.2) +
    geom_ribbon(aes(x = s, ymax = upper, ymin = lower), data = beta.hat.plt, fill = "gray10", alpha = 0.4) +
    geom_line(aes(x = s, y = beta, color = "Estimate"), data = beta.hat.plt, alpha = 1, lty = 5) +
    scale_colour_manual(name="", values=c("Estimate"="blue3")) +
    scale_y_continuous(labels=function(x) sprintf(paste0("%.", decimal[r], "f"), x)) +
    scale_x_continuous(breaks = c(1, 24, 47, 70, 93))
  
  if(r == 1){
    p.CI <- p.CI + labs(x = "Stride phase", y = expression(beta[0](s)), title = var_name[r]) +
      theme(legend.title=element_blank(),
            legend.position = c(0.05, 1),
            legend.justification = c("left", "top"),
            legend.box.just = "right",
            legend.margin = margin(0, 0, 0, 0),
            legend.background = element_rect(fill=alpha('white', 0)))
  }else{
    p.CI <- p.CI + labs(x = "Stride phase", y = bquote(paste(beta[.(r-1)], "(s)")), 
                        title = var_name[r]) +
      theme(legend.position = "none") +
      geom_hline(yintercept=0, color = "black", lwd=0.5, linetype = "dotted")
  }
  if(!is.null(name)){
    p.CI <- p.CI + labs(title = name)
  }
  return(p.CI)
}


axis_unq <- c("x", "y", "z")
for (axis_tmp in axis_unq){
  message(paste0("axis = ", axis_tmp))
  # pull precomputed model object
  fit_fname <- paste0("fit_result_lfosr3s_", axis_tmp, ".rds")
  fit_fpath <- file.path(here(), "results_objects", fit_fname)
  fit_tmp <- readRDS(fit_fpath)
  # generate and combine plots
  plt_list <- list()
  for(prdtr in 1:nrow(fit_tmp$betaHat)){
    plt_list[[prdtr]] <- plot.FUI(r = prdtr, fit_tmp = fit_tmp)
  }
  plt <- plot_grid(plotlist = plt_list, nrow = 1, align = "hv", byrow = FALSE)
  # save plot
  plt_fname <- paste0("fixed_effect_lfosr3s_", axis_tmp, ".jpeg")
  plt_fpath <- file.path(here(), "results_figures", plt_fname)
  save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 3.4)
}


