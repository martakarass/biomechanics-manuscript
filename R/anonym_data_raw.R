
#' @description 
#' Script to anonymize raw data (original, non-anonymized files are stored locally). 
#' Save anonymized data as separate file in the project directory (publicly available). 

rm(list = ls())
library(tidyverse)
library(here)
library(readr)
library(digest)
library(tools)

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 

# define local path to non-anonymized data 
fnames <- c(
  "Knee_Xstride.RDS",
  "Knee_Ystride.RDS",
  "Knee_Zstride.RDS",
  "Knee_Xswing.RDS",
  "Knee_Yswing.RDS",
  "Knee_Zswing.RDS"
)
fdir <- "/Users/martakaras/Dropbox/BIOMECHANICSNEW"

# anonymize and save 
for (i in 1 : length(fnames)){ # i <- 1
  
  # read non-anonymized data from local path
  f_orig_name <- fnames[i]
  f_orig_path <- file.path(fdir, f_orig_name)
  dat <- readRDS(f_orig_path)
  rm(f_orig_path)
  
  # anonymize values in "archivo" column 
  dat$archivo <- sapply(dat$archivo, function(x){
    x_initials_anonym <- digest(substr(x, 1, 2), algo = 'xxhash32') 
    paste0(x_initials_anonym, substr(x, 3, nchar(x)))
  })
  
  # save anonymized file 
  f_anonym_name <- paste0(file_path_sans_ext(f_orig_name), "_anonym.", file_ext(f_orig_name))
  f_anonym_path <- file.path(here(), "data", f_anonym_name)
  saveRDS(dat, f_anonym_path)
  message(paste0(f_orig_name, " anonymized and saved: ", f_anonym_path))
  
  # print first few and last rows
  print(rbind(head(dat[, c(100:103)], 3),
              tail(dat[, c(100:103)], 3)))
}


# ------------------------------------------------------------------------------
# checks for original data 

i <- 1
# read non-anonymized data from local path
f_orig_name <- fnames[i]
f_orig_path <- file.path(fdir, f_orig_name)
dat <- readRDS(f_orig_path)

archivo_unique <- sort(unique(dat$archivo))
initl_unique <- sort(unique(substr(archivo_unique, 1, 2)))
initl_unique_anonym <- sapply(initl_unique, digest, algo = 'xxhash32')
df_test_raw <- data.frame(initials_raw = initl_unique, subj_id_raw = initl_unique_anonym)

# # read force data 
fuerza_path <-  "/Users/martakaras/Dropbox/BIOMECHANICSNEW/fuerza.csv"
fuerza <- read_delim(fuerza_path, delim = ";", escape_double = FALSE, col_names = TRUE,
                     locale = locale(decimal_mark = ","), na = "NA", trim_ws = TRUE)
fuerza = as.data.frame(fuerza)
fuerza = fuerza[order(fuerza[,2]),]
fuerza = fuerza[, c(1,2,3)]
fuerza[, 3] = 1 : 19
names(fuerza)[c(1,2,3)] <- c("sex", "initials_forces", "subj_idx_forces")
fuerza$subj_id_forces <- sapply(fuerza$initials_forces, digest, algo = 'xxhash32')

df_test_comb <- 
  df_test_raw %>% 
  full_join(fuerza, by = c("subj_id_raw" = "subj_id_forces"))
df_test_comb





