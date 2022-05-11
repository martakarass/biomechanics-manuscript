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
library(stargazer)
select <- dplyr::select
filter <- dplyr::filter
separate <- tidyr::separate

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
#  TABLE: Level 1   -------------------------------------------------------------
# ------------------------------------------------------------------------------

df_all <- data.frame()

for (i in 1 : 3){
  # i <- 2
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
  sc_df$score_num <- paste0("score", 1 : 20)
  tbl_df <- 
    sc_df %>% 
    pivot_longer(cols = -c(score_num), names_to = "SubjId") %>%
    filter(score_num %in% paste0("score", 1 : 2)) %>%
    pivot_wider(values_from = value, names_from = score_num) %>%
    inner_join(demog, by = "SubjId") %>%
    mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels),
           SubjId_fct = factor(SubjId, levels = SubjId_levels)) %>%
    select(
      score1, score2, Gender
    )
  tbl_df$axis <- axis_tmp
  tbl_df$level <- "Level 1"
  
  df_all <- rbind(df_all, tbl_df)
}


# ------------------------------------------------------------------------------
#  TABLE: Level 2   -------------------------------------------------------------
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
  sc_mat <- fit_obj$results$xi$psu
  sc_df <- as.data.frame(sc_mat)
  colnames(sc_df) <- paste0("Id", 1 : 19)
  sc_df$score_num <- rep(paste0("score", 1 : 20), 2)
  sc_df$visit <- rep(c(1,2), each = 20)
  tbl_df <- 
    sc_df %>% 
    pivot_longer(cols = -c(score_num, visit), names_to = "SubjId") %>%
    filter(score_num %in% paste0("score", 1 : 2)) %>%
    pivot_wider(values_from = value, names_from = score_num) %>%
    inner_join(demog, by = "SubjId") %>%
    mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels),
           SubjId_fct = factor(SubjId, levels = SubjId_levels)) %>%
    select(
      score1, score2, Gender
    )
  tbl_df$axis <- axis_tmp
  tbl_df$level <- "Level 2"
  
  df_all <- rbind(df_all, tbl_df)
}


# ------------------------------------------------------------------------------
#  TABLE: Level 3   -------------------------------------------------------------
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

  tbl_df <- 
    sc_df %>% 
    pivot_longer(cols = -c(score_num, visit, stride), names_to = "SubjId") %>%
    filter(score_num %in% paste0("score", 1 : 2)) %>%
    pivot_wider(values_from = value, names_from = score_num) %>%
    inner_join(demog, by = "SubjId") %>%
    mutate(gender_fct = factor(Gender, levels = gender_levels, labels = gender_labels),
           SubjId_fct = factor(SubjId, levels = SubjId_levels)) %>%
    select(
      score1, score2, Gender
    )
  tbl_df$axis <- axis_tmp
  tbl_df$level <- "Level 3"
  
  df_all <- rbind(df_all, tbl_df)
}


# ------------------------------------------------------------------------------
#  TABLE: combine, aggregate and format  ---------------------------------------
# ------------------------------------------------------------------------------

dim(df_all)

df_all_agg <- 
  df_all %>%
  group_by(level, axis, Gender) %>%
  summarise(
    cnt = n(),
    score1_mean = mean(score1),
    score1_sd = sd(score1),
    score2_mean = mean(score2),
    score2_sd = sd(score2),
  ) %>%
  mutate(
    score1_sd_of_mean = score1_sd/sqrt(cnt),
    score2_sd_of_mean = score2_sd/sqrt(cnt),
    score1_ci_lo = score1_mean - 1.96 * score1_sd_of_mean,
    score1_ci_up = score1_mean + 1.96 * score1_sd_of_mean,
    score2_ci_lo = score2_mean - 1.96 * score2_sd_of_mean,
    score2_ci_up = score2_mean + 1.96 * score2_sd_of_mean,
    # score1_f = paste0(sprintf(score1_mean, fmt = '%#.1f'), " (", sprintf(score1_sd_of_mean, fmt = '%#.1f'), ")"),
    # score2_f = paste0(sprintf(score2_mean, fmt = '%#.1f'), " (", sprintf(score2_sd_of_mean, fmt = '%#.1f'), ")")
    score1_f = paste0(sprintf(score1_mean, fmt = '%#.1f'), " [", sprintf(score1_ci_lo, fmt = '%#.1f'), ", ", sprintf(score1_ci_up, fmt = '%#.1f'), "]"),
    score2_f = paste0(sprintf(score2_mean, fmt = '%#.1f'), " [", sprintf(score2_ci_lo, fmt = '%#.1f'), ", ", sprintf(score2_ci_up, fmt = '%#.1f'), "]")
  ) %>%
  select(
    level, axis, Gender,
    score1_f,
    score2_f
  )

df_all_agg_sc1 <- 
  df_all_agg %>%
  select(-score2_f) %>% 
  pivot_wider(names_from = Gender, values_from = score1_f) %>%
  mutate(score = "Score 1")
df_all_agg_sc2 <- 
  df_all_agg %>%
  select(-score1_f) %>% 
  pivot_wider(names_from = Gender, values_from = score2_f) %>%
  mutate(score = "Score 2")

df_all_agg_scs <-
  df_all_agg_sc1 %>% 
  rbind(df_all_agg_sc2) %>%
  select(axis, level, score, everything()) %>%
  arrange(axis, level, score) %>%
  as.data.frame()
df_all_agg_scs
  
stargazer(df_all_agg_scs, summary = FALSE)