
#' @description 
#' Script to anonymize demog data (original, non-anonymized file is stored locally). 
#' Save anonymized data as separate file in the project directory (publicly available). 

rm(list = ls())
library(here)
library(data.table)
library(tidyverse)

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 


# read force data 
dat_demog_path <-  "/Users/martakaras/Documents/data_biomechanics/fuerza.csv"
dat_demog <- as.data.frame(fread(dat_demog_path))

# subset data, rename columns
dat_demog <- select(dat_demog, -V1)
names(dat_demog)[2] <- "SubjId"
names(dat_demog)[3] <- "SubjIdx"
dat_demog <- dat_demog[, c(1,2,3)]

head(dat_demog)
str(dat_demog)

# save to file 
dat_demog_f_path <- file.path(here(), "data", "demog_anonym.rds")
saveRDS(dat_demog, dat_demog_f_path)

message(paste0("demog data anonymized and saved: ", dat_demog_f_path))



