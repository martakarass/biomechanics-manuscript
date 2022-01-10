#' @description 
#' Script to compute correlation of N3 scores with the strength data.
#' 
#' @author 
#' Marcos Matabuena, Marta Karas

rm(list = ls())
library(here)
library(tidyverse)
select <- dplyr::select
filter <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 


# ------------------------------------------------------------------------------
# READ DATA  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read strength data 
strength_path <- file.path(here(), "data", "strength_anonym.rds")
strength <- readRDS(strength_path)


# ------------------------------------------------------------------------------
# GET CORRELATIONS   -----------------------------------------------------------
# ------------------------------------------------------------------------------

# compute variables of interest (End - Start differences) for strength data
strength_diffs_df <- 
  strength %>%
  mutate(diff_HT_KE   = KEstrengthHiitEnd - KEstrengthHiitStart) %>%
  mutate(diff_HT_HABD = HABDstrengthHiitEnd - HABDstrengthHiitStart) %>%
  mutate(diff_HT_HADD = HADDstrengthHiitEnd - HADDstrengthHiitStart) %>%
  select(SubjIdx, Gender, starts_with("diff"))

comb_tmp <-  c("HT1 post", "HT2 post")
axis_unq <- c("x", "y", "z")

# df levels
score_name_levels <- paste0("score_", 1 : psx_k)
score_name_labels <- paste0("Score ", 1 : psx_k)
strength_var_levels <- c("diff_HT_KE", "diff_HT_HABD", "diff_HT_HADD")
# strength_var_labels <- paste0("(End-Start) strength:\n", c("Knee Extension", "Hip Abduction", "Hip Adduction"))
strength_var_labels <- c("Knee Extension", "Hip Abduction", "Hip Adduction")
gender_levels <- c("female", "male")
gender_labels <- c("F", "M")
# scores number to be used
psx_k <- 3 

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
  dat_df <- cbind(scores_k_df, strength_diffs_df)
  # create plot df 
  plt_df <- dat_df %>% 
    pivot_longer(cols = starts_with("score_"), names_to = "score_name", values_to = "score_value") %>% 
    pivot_longer(cols = starts_with("diff_"), names_to = "strength_var_name", values_to = "strength_var_value") %>%
    mutate(score_name_fct = factor(score_name, levels = score_name_levels, labels = score_name_labels)) %>%
    mutate(strength_var_name_fct = factor(strength_var_name, levels = strength_var_levels, labels = strength_var_labels)) %>%
    mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels)) 
  
  plot_list <- list()
  for (strength_var_tmp in strength_var_labels){
    for (score_name_tmp in score_name_labels){ # strength_var_tmp <- strength_var_labels[1]; score_name_tmp <- score_name_labels[1]
      # plot data subset
      plt_df_sub <- plt_df %>% filter(strength_var_name_fct == strength_var_tmp, score_name_fct == score_name_tmp)
      plt_df_sub_corr <- plt_df_sub %>% group_by(gender_fct) %>% summarise(corr = cor(score_value, strength_var_value))
      # plot
      plt <- 
        ggplot(plt_df_sub, aes(x = score_value, y = strength_var_value, color = gender_fct)) + 
        geom_point(size = 1) + 
        theme_bw(base_size = 10) + 
        geom_smooth(method = 'lm', formula = y ~ x, size = 0.5, se = FALSE) + 
        # facet_grid(strength_var_name_fct ~ score_name_fct, scales = "free") + 
        labs(x = "Score value", 
             y = "(End-Start) strength", 
             color = "Gender:"
             # title = paste0("Measurement axis: ", axis_tmp)) 
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
        ) 
      
    }
  }
    
  
    # create plot df 
    plt_df <- dat_df %>% 
      pivot_longer(cols = starts_with("score_"), names_to = "score_name", values_to = "score_value") %>% 
      pivot_longer(cols = starts_with("diff_"), names_to = "strength_var_name", values_to = "strength_var_value") %>%
      mutate(score_name_fct = factor(score_name, levels = score_name_levels, labels = score_name_labels)) %>%
      mutate(strength_var_name_fct = factor(strength_var_name, levels = strength_var_levels, labels = strength_var_labels)) %>%
      mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels)) 
    
    # # plot 1 
    # plt_df_sub <- plt_df %>% filter(score_name %in% paste0("score_", 1 : 3))
    # plt <- 
    #   ggplot(plt_df_sub, aes(x = score_value, y = strength_var_value)) + 
    #   geom_point() + 
    #   theme_bw(base_size = 12) + 
    #   geom_smooth(method = 'lm', formula = y ~ x) + 
    #   facet_grid(strength_var_name_fct ~ score_name_fct, scales = "free") 
    # path_tmp <- file.path(here(), "results_figures", paste0("strength_corr_scores_",fit_label_tmp, ".jpeg"))
    # ggsave(filename = path_tmp, plot = plt, width = 18, height = 14, units = "cm")
    
    # plot 2 -- with split by gender 
    plt <- 
      ggplot(plt_df, aes(x = score_value, y = strength_var_value, color = gender_fct)) + 
      geom_point(size = 1) + 
      theme_bw(base_size = 10) + 
      geom_smooth(method = 'lm', formula = y ~ x, size = 0.5) + 
      facet_grid(strength_var_name_fct ~ score_name_fct, scales = "free") + 
      labs(x = "Score value", y = "(End-Start) strength", color = "Gender:") + 
      theme(legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
            panel.grid.major = element_line(size = 0.2),
            # panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # panel.border = element_blank(),
            legend.position = "bottom",
            strip.background = element_rect(fill = alpha('white', 0.1), color = NA),
            strip.text = element_text(angle = 0, hjust = 0)
      ) 
    
    plt <- 
      ggplot(plt_df, aes(x = score_value, y = strength_var_value, color = gender_fct)) + 
      geom_point(size = 1) + 
      theme_bw(base_size = 10) + 
      geom_smooth(method = 'lm', formula = y ~ x, size = 0.5, se = FALSE) + 
      facet_grid(strength_var_name_fct ~ score_name_fct, scales = "free") + 
      labs(x = "Score value", 
           y = "(End-Start) strength", 
           color = "Gender:"
           # title = paste0("Measurement axis: ", axis_tmp)) 
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
      ) 
    path_tmp <- file.path(here(), "results_figures", paste0("strength_corr_scores_by_gender_",fit_label_tmp, ".jpeg")) 
    ggsave(filename = path_tmp, plot = plt, width = 18, height = 12, units = "cm")
  }
}
