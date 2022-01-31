
#' @description 
#' Script to anonymize raw data (original, non-anonymized files are stored locally). 
#' Save anonymized data as separate file in the project directory (publicly available). 

rm(list = ls())
library(tidyverse)
library(here)
library(tools)

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 

# define local path to non-anonymized data 
fnames <- c(
  "Knee_Xstride.RDS",
  "Knee_Ystride.RDS",
  "Knee_Zstride.RDS"
)
fdir <-  "/Users/martakaras/Documents/data_biomechanics"

# define map between: 
# (a) initials derived from "archivo" column  
# (b) SubjId
# (c) SubjIdx
i <- 1
f_orig_name <- fnames[i]
f_orig_path <- file.path(fdir, f_orig_name)
dat <- readRDS(f_orig_path)
race_type <- unique(dat$carrera)[1]
archivo_unique <- unique(dat[dat$carrera == race_type, ]$archivo)
archivo_unique_initials <- substr(archivo_unique, 1, 2)
map_initials_SubjId <- data.frame(
  initials = archivo_unique_initials,
  SubjId = paste0("Id", 1 : length(archivo_unique_initials)),
  SubjIdx = 1 : length(archivo_unique_initials),
  stringsAsFactors = FALSE
  )
rm(f_orig_path, dat)
map_initials_SubjId

# anonymize and save 
for (i in 1 : length(fnames)){ # i <- 1
  
  print(paste0("Processing i = ", i))  
  
  # read non-anonymized data from local path
  f_orig_name <- fnames[i]
  f_orig_path <- file.path(fdir, f_orig_name)
  dat <- readRDS(f_orig_path)
  rm(f_orig_path)
  
  # Define initials column
  dat$initials <- substr(dat$archivo, 1, 2)
  
  # checks for ID matches 
  carrera_unq <- unique(dat$carrera)
  for (race_type_tmp in carrera_unq){ # race_type <- carrera_unq[1]
    initials_tmp <- unique(dat[dat$carrera == race_type_tmp, ]$initials)
    test1_passed <- all(initials_tmp  == archivo_unique_initials)
    # message(paste0("race_type_tmp ", race_type_tmp, ": ", test1_passed))
    if (!test1_passed){
      stop(paste0("!test1_passed for ", race_type, " for i=", i, " f_orig_name=", f_orig_name))
    }
  }
  
  # join to add SubjId, SubjIdx
  # remove columns 
  # rename columns
  dat <-
    dat %>% 
    left_join(map_initials_SubjId, by = "initials") %>%
    select(-c(archivo, initials)) %>%
    rename(Step = paso, Race = carrera) %>%
    mutate(Race = tolower(Race)) %>%
    mutate(Race = gsub("ctr", "cr", Race)) %>%
    mutate(Race = gsub(" 2", "2", Race)) %>%
    mutate(Race = gsub("-", " ", Race)) %>%
    mutate(Race = gsub("cr", "CR", Race)) %>%
    mutate(Race = gsub("ht", "HT", Race)) %>%
    filter(Race %in% c("HT1 post", "HT2 post", "CR1 post"))
  
  # save anonymized file
  f_anonym_name <- paste0(file_path_sans_ext(f_orig_name), "_anonym.", tolower(file_ext(f_orig_name)))
  f_anonym_path <- file.path(here(), "data", f_anonym_name)
  saveRDS(dat, f_anonym_path)
  message(paste0(f_orig_name, " anonymized and saved: ", f_anonym_path))

  # # print first few and last rows
  # print(rbind(head(dat[, c(100:103)], 3),
  #             tail(dat[, c(100:103)], 3)))
}
