#' @description 
#' Script to calculate intraclass correlation coefficient. 
#' Case N2
#' 
#' @references 
#' Shou, H, Eloyan, A, Lee, S, Zipunnikov, V, Crainiceanu, AN, Nebel, NB, Caffo, 
#' B, Lindquist, MA, Crainiceanu, CM (2013). Quantifying the reliability of image 
#' replication studies: the image intraclass correlation coefficient (I2C2). 
#' Cogn Affect Behav Neurosci, 13, 4:714-24.
#' 
#' @author 
#' Marcos Matabuena, Marta Karas


rm(list = ls())
library(here)
library(tidyverse)
library(I2C2)
library(data.table)
select   <- dplyr::select
filter   <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 


# ------------------------------------------------------------------------------
# READ DATA  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read raw data 
dat_Knee_Xstride2 <- readRDS(file.path(here(), "data", "Knee_Xstride_anonym.rds")) 
dat_Knee_Ystride2 <- readRDS(file.path(here(), "data", "Knee_Ystride_anonym.rds")) 
dat_Knee_Zstride2 <- readRDS(file.path(here(), "data", "Knee_Zstride_anonym.rds")) 

# combine data
dat_Knee_Xstride2 <- dat_Knee_Xstride2 %>% mutate(axis = "x")
dat_Knee_Ystride2 <- dat_Knee_Ystride2 %>% mutate(axis = "y")
dat_Knee_Zstride2 <- dat_Knee_Zstride2 %>% mutate(axis = "z")
dat_all <- rbind(
  dat_Knee_Xstride2,
  dat_Knee_Ystride2,
  dat_Knee_Zstride2
)


# ------------------------------------------------------------------------------
# CALCULATE ICC, N2  -----------------------------------------------------------
# ------------------------------------------------------------------------------

# define vector of unique race values 
race_unq <- sort(unique(dat_all$Race))
axis_unq <- sort(unique(dat_all$axis))

table(dat_all$Race)
# race_tmp <- "CR1 pre"
# axis_tmp <- "x"

I2C2_out_df_ALL <- data.frame()
for (race_tmp in race_unq){
  for (axis_tmp in axis_unq){
    print(paste0("race = ", race_tmp, ", axis = ", axis_tmp))
    # subset of gait data set (specific race, specific measurement axis)
    dat_tmp   <- dat_all %>% filter(Race == race_tmp, axis == axis_tmp)
    # pull measurements columns only
    dat_tmp_y <- dat_tmp %>% select(starts_with("V")) %>% as.matrix()
    # compute  Intraclass Correlation Coefficient
    I2C2_out  <- I2C2(y = dat_tmp_y, id = dat_tmp$SubjIdx, visit = dat_tmp$Step)
    # convert to data frame 
    I2C2_out_df <- as.data.frame(I2C2_out[1:3])
    I2C2_out_df$Race = race_tmp
    I2C2_out_df$Axis = axis_tmp
    # I2C2_out_df$MovementPhase = "stride"
    # I2C2_out_df$MeasurementLocation = "knee"
    # append results    
    I2C2_out_df_ALL <- rbind(I2C2_out_df_ALL, I2C2_out_df)
  }
}

# save to file -- RDS 
path_tmp <- file.path(here(), "results_tables", "ICC_N2.rds") 
saveRDS(object = I2C2_out_df_ALL, file = path_tmp)

# save to file -- CSV 
path_tmp <- file.path(here(), "results_tables", "ICC_N2.csv") 
fwrite(I2C2_out_df_ALL, file = path_tmp)
