#' @description 
#' Script to calculate intraclass correlation coefficient. 
#' Case N3
#' 
#' @references 
#' Di, C., Crainiceanu, C.M. and Jank, W.S. (2014), Multilevel sparse functional 
#' principal component analysis. STAT, 3: 126-143. https://doi.org/10.1002/sta4.50
#' 
#' @author 
#' Marcos Matabuena, Marta Karas


rm(list = ls())
library(here)
# library(readr)
# library(fda.usc)
library(tidyverse)
library(I2C2)
library(data.table)
select   <- dplyr::select
filter   <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 

source(file.path(here(), "R", "threenest.R"))


# ------------------------------------------------------------------------------
# READ DATA  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read raw data 
# stride
datosindividualesKnee_Xstride2 <- readRDS(file.path(here(), "data", "Knee_Xstride_anonym.rds"))
datosindividualesKnee_Ystride2 <- readRDS(file.path(here(), "data", "Knee_Ystride_anonym.rds"))
datosindividualesKnee_Zstride2 <- readRDS(file.path(here(), "data", "Knee_Zstride_anonym.rds"))
# mutate
datosindividualesKnee_Xstride2 <- datosindividualesKnee_Xstride2 %>% mutate(axis = "x")
datosindividualesKnee_Ystride2 <- datosindividualesKnee_Ystride2 %>% mutate(axis = "y")
datosindividualesKnee_Zstride2 <- datosindividualesKnee_Zstride2 %>% mutate(axis = "z")

# combine data
dat_all <- rbind(
  datosindividualesKnee_Xstride2,
  datosindividualesKnee_Ystride2,
  datosindividualesKnee_Zstride2
)


# ------------------------------------------------------------------------------
# CALCULATE ICC, N3  -----------------------------------------------------------
# ------------------------------------------------------------------------------

#' X_{i,j,k} i individual, j test pre and post k measured number
#' 
#' L1 -- number of units at hierarchy level 1 (here: number of individuals)
#' L2 -- number of units at hierarchy level 2 (here: number of races per individual)
#' L3 -- number of units at hierarchy level 3 (here: number of steps per race)

L1 = Q = 19
L2 = J = 2
L3 = 20

table(dat_all$Race)
# CR1 post  CR1 pre CR2 post  CR2 pre HT1 post  HT1 pre HT2 post  HT2 pre 
# 1140     1140     1140     1140     1140     1140     1140     1140 

race_comb_list <- list(
  c("HT1 pre", "HT2 pre"),
  c("HT1 post", "HT2 post"),
  c("CR1 pre", "CR2 pre"),
  c("CR1 post", "CR2 post")
)
axis_unq <- sort(unique(dat_all$axis))

out_df_all <- data.frame()
for (comb_tmp in race_comb_list){
  for (axis_tmp in axis_unq){ # comb_tmp <- c("HT1 post", "HT2 post"); axis_tmp <- "z"
    
    # define races combination label
    fit_label_tmp <- paste0(gsub(" ", "", comb_tmp[1]), "_", gsub(" ", "", comb_tmp[2]), "_", axis_tmp)
    message(paste0("fit_label_tmp = ", fit_label_tmp))
   
     #' define Y argument of threenest() 
    #' - Y is a list 
    #' - it has length equal to number of units at level 1 (equal to L1)
    #' - each element of list Y is a matrix that has (L2 * L3) rows (each row is one 
    #'   functional observation (each element of list Y contains data from one unit
    #'   of level 1 only)
    #' - within an element of list Y, ordering of level 2 units is kept (e.g, 
    #'   all observations from one level 2 unit first, then all observations
    #'   from another level 2 unit, etc... )
    obj_Y_tmp <- list()
    for (L1_ii in 1 : L1){ # L1_ii <- 1
      obj_Y_tmp_ii <- 
        dat_all %>% 
        # filter data to keep observations from specific Race combination only
        filter(Race %in% comb_tmp) %>%
        # filter data to keep observations from specific axis only
        filter(axis == axis_tmp) %>%
        # filter data to keep observations from specific subject only 
        filter(SubjIdx == L1_ii) %>%
        # arrange to assure that within an element of list Y, ordering of level 2 units is kept
        arrange(Race) %>%
        # keep measurements only, convert to matrix
        select(starts_with("V")) %>%
        as.matrix()
      # append data to list 
      obj_Y_tmp[[L1_ii]] <- obj_Y_tmp_ii
    }
    
    # prepare table with current loop iteration results
    out_df_tmp <- data.frame(Race1 = comb_tmp[1], Race2 = comb_tmp[2], Axis = axis_tmp) 

    # fit N3 model
    tryCatch(
      {
        fit_out <-  threenest(Y = obj_Y_tmp, L1 = L1, L2 = L2, L3 = L3, smooth.y = TRUE, alg.y = 'smoothspline')
        # pull ICC
        out_df_tmp$ICC <- fit_out$ICC
        # save the model object
        fname_tmp <- paste0("fit_result_N3_", fit_label_tmp, ".rds")
        fpath_tmp <- file.path(here(), "results_objects", fname_tmp)
        saveRDS(object = fit_out, file = fpath_tmp)
      },
      error = function(cond) {
        out_df_tmp$ICC <- NA
      }
    )
   
    # append ICC info 
    out_df_all <- rbind(out_df_all, out_df_tmp)
    
    # print the info 
    print(out_df_tmp)
    rm(fit_out)
  }
}


# save ICC summary table to file 

# save to file -- RDS 
path_tmp <- file.path(here(), "results_tables", "ICC_N3.rds") 
saveRDS(object = out_df_all, file = path_tmp)

# save to file -- CSV 
path_tmp <- file.path(here(), "results_tables", "ICC_N3.csv") 
fwrite(out_df_all, file = path_tmp)