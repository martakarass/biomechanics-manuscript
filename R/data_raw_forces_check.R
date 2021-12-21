
rm(list = ls())
library(tidyverse)

# replace with your path
project_dir <- "/Users/martakaras/Dropbox/BIOMECHANICSNEW"

# read raw data 
dat_raw_name <- "Knee_Xstride.RDS"
f_orig_path <- file.path(project_dir, dat_raw_name)
dat_raw <- readRDS(f_orig_path)
head(dat_raw[, c(100:103)])
# create "subj_id" from archivo data 
archivo_unique <- unique(dat_raw$archivo)
initl_unique <- unique(substr(archivo_unique, 1, 2))
df_test_raw <- data.frame(initials_raw = initl_unique)

# read forces data 
fuerza_path <-  "/Users/martakaras/Dropbox/BIOMECHANICSNEW/fuerza.csv"
fuerza <- read_delim(fuerza_path, delim = ";", escape_double = FALSE, col_names = TRUE,
                     locale = locale(decimal_mark = ","), na = "NA", trim_ws = TRUE)
fuerza = as.data.frame(fuerza)
fuerza = fuerza[order(fuerza[,2]),]
fuerza[, 3] = 1 : 19
df_test_fuerza <- fuerza[c(2,3)]
names(df_test_fuerza) <- c("initials_fuerza", "subj_idx_fuerza")

# combine the two (outer join)
df_test_comb <- 
  df_test_raw %>% 
  full_join(df_test_fuerza, by = c("initials_raw" = "initials_fuerza"), keep = TRUE)
df_test_comb
#    initials_raw initials_fuerza subj_idx_fuerza
# 1            AR              AR               1
# 2            CC              CC               2
# 3            CW              CW               3
# 4            DH              DH               4
# 5            DW              DW               5
# 6            HX              HX               6
# 7            JD              JD               7
# 8            JD              JD               8
# 9            JH              JH               9
# 10           JR              JR              10
# 11           JW              JW              11
# 12           KM              KM              12
# 13           KX              KX              13
# 14           LB              LB              14
# 15           LL              LL              15
# 16           MD            <NA>              NA
# 17           MX            <NA>              NA
# 18           PD              PD              17
# 19           RR            <NA>              NA
# 20           RW              RW              19
# 21         <NA>              MI              16
# 22         <NA>              RB              18