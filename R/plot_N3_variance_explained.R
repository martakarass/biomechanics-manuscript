#' @description 
#' Script to plot variability explaines
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

comb_tmp <-  c("HT1 post", "HT2 post")
axis_unq <- c("x", "y", "z")

# ------------------------------------------------------------------------------
#  PLOT: Level 1   -------------------------------------------------------------
# ------------------------------------------------------------------------------

plot_list <- list()

for (i in 1 : 3){
  # i <- 1
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
  plt_df <- data.frame(
    x = seq(1, 20, by = 1),
    varexp_lvl1 = lambda_i[[1]] / sum(lambda_i[[1]]),
    varexp_lvl2 = lambda_i[[2]] / sum(lambda_i[[2]]),
    varexp_lvl3 = lambda_i[[3]] / sum(lambda_i[[3]])
  )
  plt_df_l <- 
    plt_df %>%
    pivot_longer(cols = -x) %>%
    mutate(name_fct = factor(name, levels = paste0("varexp_lvl", 1 : 3), labels = paste0("Level ", 1 : 3)))
  
  plt <- 
    ggplot(plt_df_l, aes(x = x, y = value)) + 
    geom_point() + 
    facet_wrap(~ name_fct) + 
    scale_y_continuous(limits = c(0, 1)) + 
    scale_x_continuous(breaks = seq(1, 20, by = 3)) + 
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
    labs(title = paste0("Measurement axis: ", axis_tmp),
         x = "Number of the principal component",
         y = "Proportion of variance explained"
    ) 
  plt
  
  
}


plt_all <- plot_grid(plotlist = plot_list, ncol = 3, align = "hv", byrow = FALSE)
plt_all

path_tmp <- file.path(here(), "results_figures", paste0("scores_level_1.jpeg"))
save_plot(filename = path_tmp, plot = plt_all, base_width = (3 * 2.2), base_height = (1 * 2.2))




# ------------------------------------------------------------------------------
#  PLOT: Level 2   -------------------------------------------------------------
# ------------------------------------------------------------------------------

plot_list <- list()

for (i in 1 : 3){
  # i <- 1
  axis_tmp <-  axis_unq[i]
  
  # axis-specific 
  print(paste0("axis_tmp = ", axis_tmp))
  
  # read pre-computed model
  fit_label_tmp <- paste0(gsub(" ", "", comb_tmp[1]), "_", gsub(" ", "", comb_tmp[2]), "_", axis_tmp)
  fname_tmp <- paste0("fit_result_N3_", fit_label_tmp, ".rds")
  fpath_tmp <- file.path(here(), "results_objects", fname_tmp)
  fit_obj <- readRDS(fpath_tmp)
  
  # get scores
  sc_mat <- fit_obj$results$xi$psu
  sc_df <- as.data.frame(sc_mat)
  colnames(sc_df) <- paste0("Id", 1 : 19)
  sc_df$score_num <- rep(paste0("score", 1 : 20), 2)
  sc_df$visit <- rep(c(1,2), each = 20)
  plt_df <- 
    sc_df %>% 
    pivot_longer(cols = -c(score_num, visit), names_to = "SubjId") %>%
    filter(score_num %in% paste0("score", 1 : 2)) %>%
    pivot_wider(values_from = value, names_from = score_num) %>%
    inner_join(demog, by = "SubjId") %>%
    mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels),
           SubjId_fct = factor(SubjId, levels = SubjId_levels)) 
  # plt_df
  
  plt <- 
    ggplot(plt_df, aes(x = score1, y = score2, color = SubjId_fct)) + 
    geom_text(aes(label = as.character(gender_fct)), fontface = "bold") + 
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
    labs(title = paste0("Measurement axis: ", axis_tmp),
         x = "Score 1", 
         y = "Score 2",
         shape = "Gender"
    ) 
  # plt

  plot_list[[length(plot_list) + 1]] <- plt
}


plt_all <- plot_grid(plotlist = plot_list, ncol = 3, align = "hv", byrow = FALSE)
plt_all

path_tmp <- file.path(here(), "results_figures", paste0("scores_level_2.jpeg"))
save_plot(filename = path_tmp, plot = plt_all, base_width = (3 * 2.2), base_height = (1 * 2.2))




# ------------------------------------------------------------------------------
#  PLOT: Level 3   -------------------------------------------------------------
# ------------------------------------------------------------------------------

plot_list <- list()

for (i in 1 : 3){
  # i <- 1
  axis_tmp <-  axis_unq[i]
  
  # axis-specific 
  print(paste0("axis_tmp = ", axis_tmp))
  
  # read pre-computed model
  fit_label_tmp <- paste0(gsub(" ", "", comb_tmp[1]), "_", gsub(" ", "", comb_tmp[2]), "_", axis_tmp)
  fname_tmp <- paste0("fit_result_N3_", fit_label_tmp, ".rds")
  fpath_tmp <- file.path(here(), "results_objects", fname_tmp)
  fit_obj <- readRDS(fpath_tmp)
  
  # get scores
  sc_mat <- fit_obj$results$xi$psw
  sc_df <- as.data.frame(sc_mat)
  colnames(sc_df) <- paste0("Id", 1 : 19)

  # attempt 1
  sc_df$score_num <- rep(paste0("score", 1 : 20), 2 * 20)
  sc_df$visit <- rep(c(1,2), each = 400)
  sc_df$stride <- rep(c(1:40), each = 20)
  
  # attempt 2
  # sc_df$score_num <- rep(paste0("score", 1 : 20), 2 * 20)
  # sc_df$visit <- rep(c(1,2), each = 400)
  # sc_df$stride <- rep(c(1:40), each = 20)
  # 
  # plt_df <- 
  #   sc_df %>% 
  #   filter(score_num %in% paste0("score", 1 : 2)) 

  plt_df <- 
    sc_df %>% 
    pivot_longer(cols = -c(score_num, visit, stride), names_to = "SubjId") %>%
    filter(score_num %in% paste0("score", 1 : 2)) %>%
    pivot_wider(values_from = value, names_from = score_num) %>%
    inner_join(demog, by = "SubjId") %>%
    mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels),
           SubjId_fct = factor(SubjId, levels = SubjId_levels)) 
  # plt_df
  
  plt <- 
    ggplot(plt_df, aes(x = score1, y = score2, color = SubjId_fct)) + 
    geom_text(aes(label = as.character(gender_fct))) + 
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
    labs(title = paste0("Measurement axis: ", axis_tmp),
         x = "Score 1", 
         y = "Score 2",
         shape = "Gender"
    ) 
  plt
  
  plot_list[[length(plot_list) + 1]] <- plt
}


plt_all <- plot_grid(plotlist = plot_list, ncol = 3, align = "hv", byrow = FALSE)
plt_all

path_tmp <- file.path(here(), "results_figures", paste0("scores_level_3.jpeg"))
save_plot(filename = path_tmp, plot = plt_all, base_width = (3 * 2.2), base_height = (1 * 2.2))





