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
library(tidyverse)

# [MK]: replace with yours
project_dir <- "/Users/martakaras/Dropbox/_PROJECTS/biomechanics-manuscript/"


# ------------------------------------------------------------------------------
#  READ FITTED OBJECT   --------------------------------------------------------
# ------------------------------------------------------------------------------

#' The below code reads exmplary data fit returned from the function threenest()

fname_tmp <- "fit_result_N3_HT1post_HT2post_x.rds"
fpath_tmp <- file.path(project_dir, "results_objects", fname_tmp)
fit_obj <- readRDS(fpath_tmp)
fit_obj
str(fit_obj)


# ------------------------------------------------------------------------------
#  TO RETRIEVE VARIANLE EXPLAINED   -----------------------------------------------------------------
# ------------------------------------------------------------------------------

lambda_i <- fit_obj$results$lambda

# variance explained -- level 1 
varexp_lvl1 = lambda_i[[1]] / sum(lambda_i[[1]])

# variance explained -- level 2
varexp_lvl2 = lambda_i[[2]] / sum(lambda_i[[2]])

# variance explained -- level 3
varexp_lvl3 = lambda_i[[3]] / sum(lambda_i[[3]])

# [MK]: Marcos, please double check if you agree the above is correct


# ------------------------------------------------------------------------------
#  TO RETRIEVE FIRST FEW EIGENFUNCTIONS ----------------------------------------
# ------------------------------------------------------------------------------

# [MK]: Marcos, please advise. 
# If helpful, the file 
# /Users/martakaras/Dropbox/_PROJECTS/biomechanics-manuscript/R/plot_N3_scores.R
# contains the code to pull the scores 





