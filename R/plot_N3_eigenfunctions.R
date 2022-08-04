#' @description 
#' Script to plot scores from N3 models across the 3 levels: 
#' 
#' level 1 -- 19 individuals 
#' level 2 -- (19 individuals x 2 races) 
#' level 3 -- (19 individuals x 2 races x 20 strides) 
#' 
#' @author 
#' Marcos Matabuena, Marta Karas


rm(list = ls())
library(here)
library(tidyverse)
library(cowplot)
library(ggsci)
select <- dplyr::select
filter <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 


# ------------------------------------------------------------------------------
#  Pull eigenfunctions data   --------------------------------------------------
# ------------------------------------------------------------------------------

axis_vec <- c("x", "y", "z")
hierarch_level_vec <- c(1,2,3)

plt_df_all <- data.frame()
# iterate over 3 separate models for measurement axes
for (i in 1 : 3){ # i <- 1
  axis_tmp <-  axis_vec[i]
  print(paste0("axis_tmp = ", axis_tmp))
  # read pre-computed model
  fit_label_tmp <- paste0("HT1post_HT2post_", axis_tmp)
  fname_tmp <- paste0("fit_result_N3_", fit_label_tmp, ".rds")
  fpath_tmp <- file.path(here(), "results_objects", fname_tmp)
  fit_obj <- readRDS(fpath_tmp)
  # pull data 
  eigenfunc_val_vec = c(
    fit_obj$results$phi[[1]][, 1],
    fit_obj$results$phi[[1]][, 2],
    fit_obj$results$phi[[2]][, 1],
    fit_obj$results$phi[[2]][, 2],
    fit_obj$results$phi[[3]][, 1],
    fit_obj$results$phi[[3]][, 2]
  )
  plt_df_i <- data.frame(
    x = rep(1 : 100, 6),
    eigenfunc_val = eigenfunc_val_vec,
    axis = rep(axis_tmp, 6 * 100),
    hierarch_level = c(rep(1, 200), rep(2, 200), rep(3, 200)),
    phi_idx = c(rep(1, 100), rep(2, 100), rep(1, 100), rep(2, 100), rep(1, 100), rep(2, 100))
  )
  plt_df_all <- rbind(plt_df_all, plt_df_i)
}

head(plt_df_all)


# ------------------------------------------------------------------------------
#  Plot eigenfunctions data   --------------------------------------------------
# ------------------------------------------------------------------------------

lines_col <- c("blue", "red", "green")
y_limits = c(-1, 1) * abs(max(plt_df_all$eigenfunc_val))
text_y <- y_limits[2] + 0.2 * y_limits[2]
y_limits <- c(y_limits[1], text_y)
text_x <- min(plt_df_all$x) + diff(range(plt_df_all$x)) * 0.1

plt_list <- list()
for (i in 1 : 3){
  for (j in 1 : 3){ # i <- 1; j <- 1
    print(paste0("i = ", i, ", j = ", j))
    axis_i <-  axis_vec[i]
    hierarch_level_j <-  hierarch_level_vec[j]
    # pull data specific to current iteration
    plt_df_ij <- 
      plt_df_all %>% 
      filter(axis == axis_i, hierarch_level == hierarch_level_j)
    plt_df_ij$phi_idx_fct <- factor( plt_df_ij$phi_idx, levels = c(1,2), labels = paste0("Eigenfun. ", c(1,2)))
    # plot
    plt_ij <- 
      ggplot(plt_df_ij, aes(x = x, y = eigenfunc_val, group = phi_idx_fct, linetype = phi_idx_fct)) + 
      geom_line(color = lines_col[i]) + 
      theme_bw(base_size = 9) + 
      theme(legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
            panel.grid.major = element_line(size = 0.2),
            # panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "grey70", fill = NA, size = 0.3),
            legend.position = "none",
            strip.background = element_rect(fill = alpha('white', 0.1), color = NA),
            strip.text = element_text(angle = 0, hjust = 0),
            plot.title = element_text(face = "bold") 
      ) + 
      scale_y_continuous(limits = y_limits) + 
      labs(#title = title = "",
           x = "Stride % cycle, t", 
           y = "Value",
           linetype = ""
      ) + 
      annotate(geom="text", x = text_x, y = text_y, label = paste0("Level ", hierarch_level_j), size = 3, fontface =2)
    if (hierarch_level_j == 1){
      plt_ij <- 
        plt_ij + 
        labs(title = paste0("Measurement axis: ", axis_i))
    }
    if (hierarch_level_j == 3){
      plt_ij <- 
        plt_ij + 
        theme(legend.position = c(0.7, 0.35))
    }
    plt_list[[length(plt_list) + 1]] <- plt_ij
  }
}


# ------------------------------------------------------------------------------
#  PLOT: combine and save   ----------------------------------------------------
# ------------------------------------------------------------------------------

plt_all <- plot_grid(plotlist = plt_list, ncol = 3, align = "hv", byrow = TRUE)
# plt_all

path_tmp <- file.path(here(), "results_figures", paste0("n3_eigenfunctions_level_all.jpeg"))
save_plot(filename = path_tmp, plot = plt_all, base_width = (3 * 2.2), base_height = (3 * 2))





