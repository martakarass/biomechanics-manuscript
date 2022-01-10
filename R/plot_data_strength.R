
#' @description 
#' Script to plot data with strength measurements
#' 
#' @author 
#' Marcos Matabuena, Marta Karas

rm(list = ls())
library(here)
library(tidyverse)
library(ggrepel)
select   <- dplyr::select
filter   <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 


# ------------------------------------------------------------------------------
# READ DATA  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read strength data (anonymized)
strength_path <- file.path(here(), "data", "strength_anonym.rds")
strength <- readRDS(strength_path)
str(strength)


# ------------------------------------------------------------------------------
# PLOT FORCES, VER 1  ----------------------------------------------------------
# ------------------------------------------------------------------------------

grafico = function(x, SubjIdx_vec, variable = "Variable"){
  x=apply(x,2, as.numeric)
  n= dim(x)[1]
  p= dim(x)[2]
  minimo= min(x,na.rm = TRUE)
  maximo= max(x,na.rm = TRUE)
  xaux= c(1,2,3,4)
  yaux= x[1,]
  plot(xaux,yaux,ylim= c(minimo, maximo),type = "l", main = variable)
  text(mean(xaux), mean(yaux,na.rm=TRUE), labels=SubjIdx_vec[1])
  for(i in 2:n){
    yaux= x[i,]
    lines(xaux,yaux)
    text(mean(xaux), mean(yaux,na.rm=TRUE), labels=SubjIdx_vec[i])
  }
}

# HABDStrength : hip abduction 
# HADDStrength : Hip Adduction
# HEStrength : Hip Extension 
# HFStrength : Hip Flexion 
# KFStrength : Knee Flexion 
# KE Strength; Knee Extension 
HABD = strength %>% select(starts_with("HABDstrength"))
HADD = strength %>% select(starts_with("HADDstrength")) 
HER  = strength %>% select(starts_with("HERstrength")) 
HIR  = strength %>% select(starts_with("HIRstrength")) 
HF   = strength %>% select(starts_with("HFstrength")) 
KE   = strength %>% select(starts_with("KEstrength")) 
KF   = strength %>% select(starts_with("KFstrength")) 
HE   = strength %>% select(starts_with("HEstrength")) 
# HABD = strength[,c(4,5,20,21)]
# HADD=strength[,c(6,7,22,23)]
# HER= strength[,c(8,9,24,25)]
# HIR= strength[,c(10,11,26,27)]
# HF=  strength[,c(12,13,28,29)]
# KE=  strength[,c(14,15,30,31)]
# KF=  strength[,c(16,17,32,33)]
# HE=  strength[,c(18,19,34,35)] 

# x <- HABD; variable = "HABD"; SubjIdx_vec = strength[, 3]
SubjIdx_vec <- strength %>% pull(SubjIdx)
grafico(HABD, SubjIdx_vec = SubjIdx_vec, variable = "HABD")
grafico(HADD, SubjIdx_vec = SubjIdx_vec, variable = "HADD")
grafico(HER, SubjIdx_vec = SubjIdx_vec, variable = "HER")
grafico(HIR, SubjIdx_vec = SubjIdx_vec, variable = "HIR")
grafico(HF, SubjIdx_vec = SubjIdx_vec, variable = "HF")
grafico(KE, SubjIdx_vec = SubjIdx_vec, variable = "KE")
grafico(KF, SubjIdx_vec = SubjIdx_vec, variable = "KF")
grafico(HE, SubjIdx_vec = SubjIdx_vec, variable = "HE")

# path_tmp <- file.path(here(), "results_figures", "raw_data_per_subject.jpeg") 
# ggsave(filename = path_tmp, plot = plt, width = 20, height = 24, units = "cm")


# ------------------------------------------------------------------------------
# PLOT FORCES, VER 2  ----------------------------------------------------------
# ------------------------------------------------------------------------------

var_name_levels <- c(
  'HABD',
  'HADD',
  'HER',
  'HIR',
  'HF',
  'KE',
  'KF',
  'HE'
)

#' @TODO label HER, HIR properly 
var_name_labels <- c(
  'Hip Abduction',
  'Hip Adduction',
  'HER', # ?? 
  'HIR', # ?? 
  'Hip Flexion',
  'Knee Extension',
  'Knee Flexion',
  'Hip Extension'
)
SubjIdx_levels <- sort(unique(strength$SubjIdx))
SubjIdx_labels <- paste0(SubjIdx_levels)
session_part_levels <- c("Start", "End")
session_type_levels <- c("HIIT", "MICR")

# make plot data frame
plt_df <- 
  strength %>% 
  group_by(SubjId) %>% 
  filter(n() == 1) %>%
  ungroup()
plt_df_long <- 
  plt_df %>% 
  pivot_longer(cols = contains("strength")) %>%
  separate(name, into = c("var_name", "part"), sep = "strength", remove = FALSE) %>%
  mutate(session_type = part, 
         session_part = part,
         session_type = ifelse(grepl("Cr", session_type), "MICR", "HIIT"),
         session_part = ifelse(grepl("Start", session_part), "Start", "End")) %>%
  select(-part) %>%
  mutate(var_name_fct = factor(var_name, levels = var_name_levels, labels = var_name_labels)) %>%
  mutate(SubjIdx_fct = factor(SubjIdx, levels = SubjIdx_levels, labels = SubjIdx_labels)) %>%
  mutate(session_part_fct = factor(session_part, levels = session_part_levels)) %>%
  mutate(session_type_fct = factor(session_type, levels = session_type_levels))

# generate plot for each variable name 
var_name_unq <- sort(unique(plt_df_long$var_name))
for (i in 1 : length(var_name_unq)) { #i <- 1
  # make subset of data for a particular variable name 
  plt_df_long_i <- plt_df_long %>% filter(var_name == var_name_unq[i])
  var_name_fct_i <- plt_df_long_i$var_name_fct[1]
  print(paste0(i, "-th plot: ", var_name_fct_i))
  plt <- 
    ggplot(plt_df_long_i, aes(x = session_part_fct, y = value, group = SubjId)) + 
    geom_line(alpha = 0.5) + 
    geom_point(color = "grey30") + 
    # https://ggrepel.slowkow.com/articles/examples.html
    geom_text_repel(
      data = plt_df_long_i %>% filter(session_part == "Start"),
      aes(label = SubjIdx_fct),
      nudge_x = -0.07, nudge_y = 0,
      size = 2,
      hjust = 0,
      segment.color	= "grey",
      segment.size	= 0.3
    ) +
    facet_wrap(~ session_type_fct, ncol = 2) + 
    labs(x = "", y = paste0("Strength: ", var_name_fct_i)) + 
    theme_bw(base_size = 14) + 
    theme(legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
          panel.grid.major = element_line(size = 0.2),
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill = alpha('white', 0.1), color = NA),
          strip.text = element_text(angle = 0, hjust = 0)
    ) + 
    scale_x_discrete(expand = c(0.28, 0)) 
  # plt
  # save plot to file  
  path_tmp <- file.path(here(), "results_figures", paste0("strength_data_per_variable_", var_name_unq[i], ".jpeg"))
  ggsave(filename = path_tmp, plot = plt, width = 14, height = 10, units = "cm")
}



