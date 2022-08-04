#' @description 
#' Script to plot variability explained from N3 models across the 3 levels: 
#' 
#' level 1 -- 19 individuals  
#' level 2 -- (19 individuals x 2 races) 
#' level 3 -- (19 individuals x 2 races x 20 strides) 
#' 
#' @author 
#' Marcos Matabuena, Marta Karas

rm(list = ls())
library(tidyverse)
library(cowplot)
library(here)
# library(ggsci)

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 

comb_tmp <-  c("HT1 post", "HT2 post")
axis_unq <- c("x", "y", "z")



# ------------------------------------------------------------------------------
# pull all variance explained values into one data frame 
# ------------------------------------------------------------------------------

# vectors to store the results
cumvarexp_vec      <- numeric()
cumvarexp_idx_vec  <- numeric()
axis_vec           <- numeric()
hierarch_level_vec <- numeric()

for (i in 1 : 3){ # i <- 1
  axis_tmp <-  axis_unq[i]
  # axis-specific
  print(paste0("axis_tmp = ", axis_tmp))
  # read pre-computed model
  fit_label_tmp <- paste0(gsub(" ", "", comb_tmp[1]), "_", gsub(" ", "", comb_tmp[2]), "_", axis_tmp)
  fname_tmp <- paste0("fit_result_N3_", fit_label_tmp, ".rds")
  fpath_tmp <- file.path(here(), "results_objects", fname_tmp)
  fit_obj <- readRDS(fpath_tmp)
  # get variance explained
  lambda_i <- fit_obj$results$lambda
  # variance explained -- level 1
  varexp_lvl1 = lambda_i[[1]] / sum(lambda_i[[1]])
  varexp_lvl2 = lambda_i[[2]] / sum(lambda_i[[2]])
  varexp_lvl3 = lambda_i[[3]] / sum(lambda_i[[3]])
  # cumulative variance explained -- level 1, level 2, level 3
  cumvarexp_lvl1 = cumsum(varexp_lvl1)
  cumvarexp_lvl2 = cumsum(varexp_lvl2)
  cumvarexp_lvl3 = cumsum(varexp_lvl3)
  idx_n <- length(cumvarexp_lvl1)
  # append data
  cumvarexp_vec      <- c(cumvarexp_vec, cumvarexp_lvl1, cumvarexp_lvl2, cumvarexp_lvl3)
  cumvarexp_idx_vec  <- c(cumvarexp_idx_vec, 1 : idx_n, 1 : idx_n, 1 : idx_n)
  axis_vec           <- c(axis_vec, rep(axis_tmp, 3 * idx_n))
  hierarch_level_vec <- c(hierarch_level_vec, rep(1, idx_n), rep(2, idx_n), rep(3, idx_n))
}

# make a data frame
plt_df <- data.frame(
  cumvarexp = cumvarexp_vec,
  cumvarexp_idx = cumvarexp_idx_vec,
  axis = axis_vec,
  hierarch_level = hierarch_level_vec
)


# ------------------------------------------------------------------------------
# make four plots 
# ------------------------------------------------------------------------------

hierarch_level <- c(1,2,3)

idx_max <- 10
plt_df %>% 
  filter(cumvarexp_idx <= idx_max) %>%
  pull(cumvarexp) %>% 
  min()

plt_list <- list()
for (i in 1 : 3){ # i <- 3
  hierarch_level_i <- hierarch_level[i]
  plt_df_i <- plt_df %>% filter(hierarch_level == hierarch_level_i)
  text_x <- 2
  text_y <- 1.05
  plt_i <- 
    ggplot(plt_df_i, aes(x = cumvarexp_idx, y = cumvarexp, color = axis, group = axis)) + 
    geom_line(alpha = 0.7, size = 0.5) + 
    geom_point(alpha = 0.9, size = 2) + 
    scale_color_manual(breaks = c("x", "y", "z"),
                       values = c("blue", "red", "green"))  + 
    labs(x = "FPC index",  y = "Cumulative variance explained", color = "Measurement\naxis: ") + 
    theme_bw(base_size = 14) + 
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
    scale_x_continuous(breaks = seq(1, idx_max), limits = c(1, idx_max), expand = expansion(add = c(0.8, 0.8))) + 
    scale_y_continuous(breaks = seq(0.5, 1, by = 0.1), limits = c(0.49, NA)) + 
    annotate(geom="text", x = text_x, y = text_y, label = paste0("Level ", hierarch_level_i), size = 4.5, fontface =2)
  if (i == 3){
    plt_i <- 
      plt_i + 
      theme(legend.position = c(0.75, 0.25))
  }
  plt_list[[length(plt_list) + 1]] <- plt_i
}

plt_all <- plot_grid(plotlist = plt_list, ncol = 3, align = "hv", byrow = FALSE)
plt_all

path_tmp <- file.path(here(), "results_figures", paste0("n3_variance_explained_level_all.jpeg"))
save_plot(filename = path_tmp, plot = plt_all, base_width = 11, base_height = 3.8)






