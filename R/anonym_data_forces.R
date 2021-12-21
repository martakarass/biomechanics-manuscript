
#' @description 
#' Script to anonymize forces data (original, non-anonymized file is stored locally). 
#' Save anonymized data as separate file in the project directory (publicly available). 

rm(list = ls())
library(tidyverse)
library(here)
library(readr)
library(digest)
library(data.table)

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 


# read force data 
fuerza_path <-  "/Users/martakaras/Dropbox/BIOMECHANICSNEW/fuerza.csv"
fuerza <- read_delim(fuerza_path, delim = ";", escape_double = FALSE, col_names = TRUE,
                     locale = locale(decimal_mark = ","), na = "NA", trim_ws = TRUE)
fuerza = as.data.frame(fuerza)
head(fuerza)

# arrange rows by subj id number 
fuerza = fuerza[order(fuerza[,2]),]
fuerza[, 3] = 1 : 19
str(fuerza)
head(fuerza)

# rename columns
names(fuerza)[c(1,2,3)] <- c("sex", "subj_id", "subj_idx")

# use hash function to anonymize IDs 
fuerza$subj_id <- sapply(fuerza$subj_id, digest, algo = 'xxhash32')
head(fuerza)

# save to file 
fuerza <- as_tibble(fuerza)
fuerza_f_path <- file.path(here(), "data", "forces_anonym.csv")
# saveRDS(fuerza, fuerza_f_path)
fwrite(fuerza, fuerza_f_path)

message(paste0("Forces data anonymized and saved: ", fuerza_f_path))



