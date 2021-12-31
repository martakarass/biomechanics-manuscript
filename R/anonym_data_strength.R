
#' @description 
#' Script to anonymize strength data (original, non-anonymized file is stored locally). 
#' Save anonymized data as separate file in the project directory (publicly available). 

rm(list = ls())
library(here)
library(data.table)
library(tidyverse)

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 


# read force data 
dat_strength_path <-  "/Users/martakaras/Downloads/New_files/fuerza.csv"
# dat_strength <- read_delim(dat_strength_path, delim = ";", escape_double = FALSE, col_names = TRUE,
#                      locale = locale(decimal_mark = ","), na = "NA", trim_ws = TRUE)
dat_strength <- as.data.frame(fread(dat_strength_path))

# remove redundant columns
dat_strength <- select(dat_strength, -V1)
# rename columns
names(dat_strength)[2] <- "SubjId"
names(dat_strength)[3] <- "SubjIdx"

head(dat_strength)
str(dat_strength)

# save to file 
dat_strength_f_path <- file.path(here(), "data", "strength_anonym.rds")
saveRDS(dat_strength, dat_strength_f_path)

message(paste0("Strength data anonymized and saved: ", dat_strength_f_path))



