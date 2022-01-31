#' @description 
#' Script to plot scores from N3 models across the 3 levels: 
#' 
#' In N3 models estimation, we used: 
#' L1 = 20
#' L2 = 20
#' L3 = 20
#' 
#' level 1 -- 19 individuals => [L1, 19] = [20, 19] scores matrix 
#' level 2 -- (19 individuals x 2 races) => [L2 * 2, 19] = [40, 19] scores matrix 
#' level 3 -- (19 individuals x 2 races x 20 strides) => [L3 * 2 * 20, 19] = [800, 19] scores matrix 
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
demog <- select(demog, c(SubjIdx, Gender))


# ------------------------------------------------------------------------------
#  PLOT: Level 1   -------------------------------------------------------------
# ------------------------------------------------------------------------------

comb_tmp <-  c("HT1 post", "HT2 post")
axis_unq <- c("x", "y", "z")

# scores number to be used
psx_k <- 2

# factor variable levels
score_name_levels <- paste0("score_", 1 : psx_k)
score_name_labels <- paste0("Score ", 1 : psx_k)
gender_levels <- c("female", "male")
gender_labels <- c("F", "M")

axis_tmp <- "z"

# axis-specific 
print(paste0("axis_tmp = ", axis_tmp))

# read pre-computed model
fit_label_tmp <- paste0(gsub(" ", "", comb_tmp[1]), "_", gsub(" ", "", comb_tmp[2]), "_", axis_tmp)
fname_tmp <- paste0("fit_result_N3_", fit_label_tmp, ".rds")
fpath_tmp <- file.path(here(), "results_objects", fname_tmp)
fit_obj <- readRDS(fpath_tmp)

# get scores
dim(fit_obj$results$xi$psx)
dim(fit_obj$results$xi$psu)
dim(fit_obj$results$xi$psw)

L1 = Q = 19
L2 = J = 2
L3 = 20
I_index = rep(1 : L1, each = L2 * L3)
IJ_index = rep(1 : (L1 * L2), each = L3)

N1= 20
N2= 20
N3= 20

1:N1
(N1+1):(N1+N2*L2)
(N1+N2+1):(N1+N2+L2*L3*N1)

ps_X[,i] = ps_i[1:N1,]
ps_U[,i] = ps_i[(N1+1):(N1+N2*L2),]
ps_W[,i] = ps_i[(N1+N2+1):(N1+N2+L2*L3*N1),]



scores_k_mat <- fit_obj$results$xi$psx[1 : psx_k, ]



scores_k_df <- data.frame(t(scores_k_mat))
names(scores_k_df) <- paste0("score_", 1 : psx_k)
dat_df <- cbind(scores_k_df, demog_diffs_df)





for (axis_tmp in axis_unq){ # axis_tmp <- "z"

  # axis-specific 
  print(paste0("axis_tmp = ", axis_tmp))
  # read pre-computed model
  fit_label_tmp <- paste0(gsub(" ", "", comb_tmp[1]), "_", gsub(" ", "", comb_tmp[2]), "_", axis_tmp)
  fname_tmp <- paste0("fit_result_N3_", fit_label_tmp, ".rds")
  fpath_tmp <- file.path(here(), "results_objects", fname_tmp)
  fit_obj <- readRDS(fpath_tmp)
  # get scores
  scores_k_mat <- fit_obj$results$xi$psx[1 : psx_k, ]
  scores_k_df <- data.frame(t(scores_k_mat))
  names(scores_k_df) <- paste0("score_", 1 : psx_k)
  dat_df <- cbind(scores_k_df, demog_diffs_df)
  # create plot df 
  plt_df <- dat_df %>% 
    pivot_longer(cols = starts_with("score_"), names_to = "score_name", values_to = "score_value") %>% 
    pivot_longer(cols = starts_with("diff_"), names_to = "demog_var_name", values_to = "demog_var_value") %>%
    mutate(score_name_fct = factor(score_name, levels = score_name_levels, labels = score_name_labels)) %>%
    mutate(demog_var_name_fct = factor(demog_var_name, levels = demog_var_levels, labels = demog_var_labels)) %>%
    mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels)) 
  
  plot_list <- list()
  for (demog_var_tmp in demog_var_labels){
    for (score_name_tmp in score_name_labels){ # demog_var_tmp <- demog_var_labels[1]; score_name_tmp <- score_name_labels[1]
      # plot data subset
      plt_df_sub <- plt_df %>% filter(demog_var_name_fct == demog_var_tmp, score_name_fct == score_name_tmp)
      plt_df_sub_corr <- 
        plt_df_sub %>% 
        group_by(gender_fct) %>% 
        summarise(corr = cor(score_value, demog_var_value)) %>%
        arrange(gender_fct)
      corr_vec <- plt_df_sub_corr %>% pull(corr)
      geom_text_label = paste0("corr F = ", round(corr_vec[1], 2), ", corr M = ", round(corr_vec[2], 2))
      geom_text_x <- min(plt_df_sub$score_value) + 0 * diff(range(plt_df_sub$score_value))
      geom_text_y <- min(plt_df_sub$demog_var_value) + 1.1 * diff(range(plt_df_sub$demog_var_value))
      # plot
      plt <- 
        ggplot(plt_df_sub, aes(x = score_value, y = demog_var_value, color = gender_fct)) + 
        geom_point(size = 1) + 
        theme_bw(base_size = 9) + 
        geom_smooth(method = 'lm', formula = y ~ x, size = 0.5, se = FALSE) + 
        labs(
            x = score_name_tmp,
             y = demog_var_tmp,
             color = "Gender:"
        ) + 
        theme(legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
              panel.grid.major = element_line(size = 0.2),
              # panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "grey70", fill = NA, size = 0.3),
              legend.position = "bottom",
              strip.background = element_rect(fill = alpha('white', 0.1), color = NA),
              strip.text = element_text(angle = 0, hjust = 0),
              plot.title = element_text(face = "bold")
        ) + 
        annotate("text", x = geom_text_x, y = geom_text_y, label = geom_text_label, 
                 hjust = 0, size = 2.3) + 
        theme(plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) + 
        scale_color_jama()
      if (length(plot_list) == 0){
        plt <- 
          plt + 
          labs(title = paste0("Measurement axis: ", axis_tmp)) 
      }
      ## add legend 
      if (length(plot_list) == 5){
        legend_x <- min(plt_df_sub$score_value) + 0.5 * diff(range(plt_df_sub$score_value))
        legend_y <- min(plt_df_sub$demog_var_value) + 0.15 * diff(range(plt_df_sub$demog_var_value))
        plt <- plt + 
          theme(legend.position = c(0.8, 0.3),
                # legend.title = element_blank(),
                legend.background = element_rect(fill=alpha('white', 0.6), color = NA))  
      } else {
        plt <- plt + theme(legend.position = "none")
      }
      # append plot
      plot_list[[length(plot_list) + 1]] <- plt
    }
  }

  plt_all <- plot_grid(plotlist = plot_list, ncol = 3, align = "hv", byrow = FALSE)
  plt_all
  
  path_tmp <- file.path(here(), "results_figures", paste0("demog_corr_scores_by_gender_",fit_label_tmp, ".jpeg"))
  save_plot(filename = path_tmp, plot = plt_all, base_width = (3 * 2.2), base_height = (2 * 1.8))
  
}

