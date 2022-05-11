#' @description 
#' Script to estimate 
#' multilevel functional regression models with functional outcome.
#' 
#' @author 
#' Marcos Matabuena, Marta Karas

rm(list = ls())
library(here)
library(fda.usc)
library(tidyverse)
library(gridExtra)
select <- dplyr::select
filter <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 

source(file.path(here(), "R", "lfos3s.R"))


# ------------------------------------------------------------------------------
# READ DATA  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read raw data 
datosindividualesKnee_Xstride2 <- readRDS(file.path(here(), "data", "Knee_Xstride_anonym.rds"))
datosindividualesKnee_Ystride2 <- readRDS(file.path(here(), "data", "Knee_Ystride_anonym.rds"))
datosindividualesKnee_Zstride2 <- readRDS(file.path(here(), "data", "Knee_Zstride_anonym.rds"))

# read demog data 
demog_path <- file.path(here(), "data", "demog_anonym.rds")
demog <- readRDS(demog_path)


# ------------------------------------------------------------------------------
# FIT FOSR  --------------------------------------------------------------------
# ------------------------------------------------------------------------------

# prepare data 
# add info about axis (x,y,z) to each data set 
datosindividualesKnee_Xstride2$axis <- "x" 
datosindividualesKnee_Ystride2$axis <- "y" 
datosindividualesKnee_Zstride2$axis <- "z" 
dat_df <- rbind(
  datosindividualesKnee_Xstride2,
  datosindividualesKnee_Ystride2,
  datosindividualesKnee_Zstride2
)
# filter to keep CR1 post HT1 post only
dat_df <- filter(dat_df, Race %in% c("CR1 post", "HT1 post"))

table(dat_df$Race)

# format data frame
demog_sub <- demog %>% select(SubjIdx, Gender)
RaceType_levels <- c("CR", "HT")
Gender_levels   <- c("female", "male")
SubjIdx_levels  <- 1 : 19
dat_df <- 
  dat_df %>% 
  left_join(demog_sub, by = "SubjIdx") %>%
  separate(Race, sep = " ", into = c("RaceType", "RacePart"), remove = FALSE) %>%
  mutate(RaceType = substr(RaceType, 0, 2)) %>%
  mutate(RaceType_fct = factor(RaceType, levels = RaceType_levels)) %>%
  mutate(Gender_fct = factor(Gender, levels = Gender_levels)) %>%
  mutate(SubjIdx_fct = factor(SubjIdx, levels = SubjIdx_levels))

dat_df %>% group_by(RaceType_fct, SubjIdx_fct) %>% summarise(cnt = n()) %>% as.data.frame()

# fit for each axis separately 
axis_unq <- sort(unique(dat_df$axis))
for (axis_tmp in axis_unq){ # axis_tmp <- "z"
  message(paste0("axis = ", axis_tmp))
  # make data frame with data specific to this loop iteration
  dat_df_tmp    <- dat_df %>% filter(axis == axis_tmp)     
  fit_Y_tmp     <- dat_df_tmp %>% select(starts_with("V")) %>% as.matrix()
  fit_dat_tmp   <- dat_df_tmp %>% select(-starts_with("V"))
  fit_dat_tmp$Y <- fit_Y_tmp
  # fit model
  t1 <- Sys.time()
  fit_tmp <- lfosr3s(formula = Y ~ Gender_fct + RaceType_fct + (1 | SubjIdx_fct),
                     data = fit_dat_tmp,
                     var = TRUE, analytic = TRUE, parallel = FALSE, silent = TRUE,
                     knots_manual = 5)
  t2 <- Sys.time()
  print(t2-t1)
  # save fit object
  fit_fname <- paste0("fit_result_lfosr3s_simpler_model_", axis_tmp, ".rds")
  fit_fpath <- file.path(here(), "results_objects", fit_fname)
  saveRDS(fit_tmp, fit_fpath)
}
