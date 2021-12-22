
#' @description 
#' Script to plot functional coefficients from FOSR with covariates model
#' 
#' @author 
#' Marcos Matabuena, Marta Karas

rm(list = ls())
library(here)
library(readr)
library(fda.usc)
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



# path_tmp <- file.path(here(), "results_figures", "raw_data_per_subject.jpeg") 
# ggsave(filename = path_tmp, plot = plt, width = 20, height = 24, units = "cm")
# 
# 
# 
