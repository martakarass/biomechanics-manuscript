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
for (axis_tmp in axis_unq){ # axis_tmp <- "z"
  print(paste0("axis_tmp = ", axis_tmp))
  
  fit_label_tmp <- paste0(gsub(" ", "", comb_tmp[1]), "_", gsub(" ", "", comb_tmp[2]), "_", axis_tmp)
  fname_tmp <- paste0("fit_result_N3_", fit_label_tmp, ".rds")
  fpath_tmp <- file.path(here(), "results_objects", fname_tmp)
  fit_obj <- readRDS(fpath_tmp)
  
  # get scores 
  psx_k <- 10
  scores_k_mat <- fit_obj$results$xi$psx[1 : psx_k, ]
  scores_k_df <- data.frame(t(scores_k_mat))
  names(scores_k_df) <- paste0("score_", 1 : psx_k)
  # combine with strenghts data variables
  dat_df <- cbind(scores_k_df, strength_diffs_df)
  
  # create plot df 
  plt_df <- dat_df %>% pivot_longer(cols = starts_with("score_"), names_to = "score_name", values_to = "score_value")
  plt_df <- plt_df %>% pivot_longer(cols = starts_with("diff_"), names_to = "strength_var_name", values_to = "strength_var_value")

  # plot 1 
  plt_df_sub <- plt_df %>% filter(score_name %in% paste0("score_", 1 : 3))
  plt <- 
    ggplot(plt_df_sub, aes(x = score_value, y = strength_var_value)) + 
    geom_point() + 
    theme_bw(base_size = 12) + 
    geom_smooth(method='lm', formula = y ~ x) + 
    facet_grid(strength_var_name ~ score_name, scales = "free") 
  path_tmp <- file.path(here(), "results_figures", paste0("strength_corr_scores_",fit_label_tmp, ".jpeg"))
  ggsave(filename = path_tmp, plot = plt, width = 14, height = 14, units = "cm")
  
  # plot 2 -- with split by gender 
  plt_df_sub <- plt_df %>% filter(score_name %in% paste0("score_", 1 : 3))
  plt <- 
    ggplot(plt_df_sub, aes(x = score_value, y = strength_var_value, color = Gender)) + 
    geom_point() + 
    theme_bw(base_size = 12) + 
    geom_smooth(method='lm', formula = y ~ x) + 
    facet_grid(strength_var_name ~ score_name, scales = "free") 
  path_tmp <- file.path(here(), "results_figures", paste0("strength_corr_scores_by_gender_",fit_label_tmp, ".jpeg"))
  ggsave(filename = path_tmp, plot = plt, width = 14, height = 14, units = "cm")
}