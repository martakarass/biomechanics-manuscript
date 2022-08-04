#' @description 
#' Script to plot scores from N3 models across the 3 levels: 
#' 
#' In N3 models estimation, we used: 
#' N1 = 20
#' N2 = 20
#' N3 = 20
#' 
#' level 1 -- 19 individuals => [N1, 19] = [20, 19] scores matrix 
#' level 2 -- (19 individuals x 2 races) => [N2 * 2, 19] = [40, 19] scores matrix (rows order: L2 for visit 1, L2 for visit 2)
#' level 3 -- (19 individuals x 2 races x 20 strides) => [N3 * 2 * 20, 19] = [800, 19] scores matrix 
#' 
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
#  READ DATA   -----------------------------------------------------------------
# ------------------------------------------------------------------------------

# read demog data 
demog_path <- file.path(here(), "data", "demog_anonym.rds")
demog <- readRDS(demog_path) 

comb_tmp <-  c("HT1 post", "HT2 post")
axis_unq <- c("x", "y", "z")

# scores number to be used
psx_k <- 2

# factor variable levels
score_name_levels <- paste0("score_", 1 : psx_k)
score_name_labels <- paste0("Score ", 1 : psx_k)
gender_levels <- c("female", "male")
gender_labels <- c("F", "M")
SubjId_levels <- sort(unique(demog$SubjId))



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
  
  # get scores
  sc_mat <- fit_obj$results$xi$psx
  sc_df <- as.data.frame(sc_mat)
  colnames(sc_df) <- paste0("Id", 1 : 19)
  # sc_df$score_num <- rep(paste0("score", 1 : 20), 2)
  # sc_df$visit <- rep(c(1,2), each = 20)
  sc_df$score_num <- paste0("score", 1 : 20)
  plt_df <- 
    sc_df %>% 
    pivot_longer(cols = -c(score_num), names_to = "SubjId") %>%
    filter(score_num %in% paste0("score", 1 : 2)) %>%
    pivot_wider(values_from = value, names_from = score_num) %>%
    inner_join(demog, by = "SubjId") %>%
    mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels),
           SubjId_fct = factor(SubjId, levels = SubjId_levels)) 
  # plt_df
  
  text_y <- max(plt_df$score2) + diff(range(plt_df$score2)) * 0.2
  text_x <- min(plt_df$score1) + diff(range(plt_df$score1)) * 0.1
  plt <- 
    ggplot(plt_df, aes(x = score1, y = score2, color = SubjId_fct)) + 
    geom_text(aes(label = as.character(gender_fct)), fontface = "bold") + 
    theme_bw(base_size = 9) + 
    theme(legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
          panel.grid.major = element_line(size = 0.2),
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey70", fill = NA, size = 0.3),
          strip.background = element_rect(fill = alpha('white', 0.1), color = NA),
          strip.text = element_text(angle = 0, hjust = 0),
          plot.title = element_text(face = "bold") ,
          legend.position="none"
    ) + 
    labs(title = paste0("Measurement axis: ", axis_tmp),
         x = "Score 1", 
         y = "Score 2",
         shape = "Gender"
    ) + 
    annotate(geom="text", x = text_x, y = text_y, label = "Level 1", size = 3, fontface =2)

  plot_list[[length(plot_list) + 1]] <- plt
}


# ------------------------------------------------------------------------------
#  PLOT: Level 2   -------------------------------------------------------------
# ------------------------------------------------------------------------------

# plot_list <- list()

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
  
  text_y <- max(plt_df$score2) + diff(range(plt_df$score2)) * 0.2
  text_x <- min(plt_df$score1) + diff(range(plt_df$score1)) * 0.1
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
    labs(#title = title = "",
         x = "Score 1", 
         y = "Score 2",
         shape = "Gender"
    ) + 
    annotate(geom="text", x = text_x, y = text_y, label = "Level 2", size = 3, fontface =2)
  # plt

  plot_list[[length(plot_list) + 1]] <- plt
}



# ------------------------------------------------------------------------------
#  PLOT: Level 3   -------------------------------------------------------------
# ------------------------------------------------------------------------------

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

  plt_df <- 
    sc_df %>% 
    pivot_longer(cols = -c(score_num, visit, stride), names_to = "SubjId") %>%
    filter(score_num %in% paste0("score", 1 : 2)) %>%
    pivot_wider(values_from = value, names_from = score_num) %>%
    inner_join(demog, by = "SubjId") %>%
    mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels),
           SubjId_fct = factor(SubjId, levels = SubjId_levels)) 
  # plt_df
  
  text_y <- max(plt_df$score2) + diff(range(plt_df$score2)) * 0.2
  text_x <- min(plt_df$score1) + diff(range(plt_df$score1)) * 0.1
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
    labs(#title = "",
         x = "Score 1", 
         y = "Score 2",
         shape = "Gender"
    ) + 
    annotate(geom="text", x = text_x, y = text_y, label = "Level 3", size = 3, fontface =2)
  # plt
  
  plot_list[[length(plot_list) + 1]] <- plt
}


# ------------------------------------------------------------------------------
#  PLOT: combine and save   ----------------------------------------------------
# ------------------------------------------------------------------------------

plt_all <- plot_grid(plotlist = plot_list, ncol = 3, align = "hv", byrow = FALSE)
path_tmp <- file.path(here(), "results_figures", paste0("n3_scores_level_all.jpeg"))
save_plot(filename = path_tmp, plot = plt_all, base_width = (3 * 2.2), base_height = (3 * 2.2))





