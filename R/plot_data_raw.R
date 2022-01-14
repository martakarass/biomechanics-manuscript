
#' @description 
#' Script to plot raw data (angle profiles recorded along 20 strides in two HIIT sessions)
#' 
#' @author 
#' Marcos Matabuena, Marta Karas

rm(list = ls())
library(here)
library(fda.usc)
library(tidyverse)
select <- dplyr::select
filter <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 


# ------------------------------------------------------------------------------
# READ DATA  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read raw data 
datosindividualesKnee_Xstride2 <- readRDS(file.path(here(), "data", "Knee_Xstride_anonym.rds"))
datosindividualesKnee_Ystride2 <- readRDS(file.path(here(), "data", "Knee_Ystride_anonym.rds"))
datosindividualesKnee_Zstride2 <- readRDS(file.path(here(), "data", "Knee_Zstride_anonym.rds"))


# ------------------------------------------------------------------------------
# PLOT FUNCTIONAL OBSERVATONS, GROUPED    --------------------------------------
# ------------------------------------------------------------------------------

# prepare plot data 
# add info about dimension (x,y,z) to each data set 
datosindividualesKnee_Xstride2$dimens <- "x" 
datosindividualesKnee_Ystride2$dimens <- "y" 
datosindividualesKnee_Zstride2$dimens <- "z" 
plt_df <- rbind(
  datosindividualesKnee_Xstride2,
  datosindividualesKnee_Ystride2,
  datosindividualesKnee_Zstride2
)

# define list of race combinations
comb_tmp <- c("HT1 post", "HT2 post")

# define races combination label specific to current loop iteration
plot_label_tmp <- paste0(gsub(" ", "", comb_tmp[1]), "_", gsub(" ", "", comb_tmp[2]))
message(paste0("plot_label_tmp = ", plot_label_tmp))

# define Race levels
# Race_sub_levels <- c("HT1 post", "HT2 post")
Race_sub_levels <- comb_tmp
Race_sub_labels <- c("1", "2")
# define SubjIdx levels
SubjIdx_levels <- sort(unique(plt_df$SubjIdx))
SubjIdx_labels <- paste0("ID ", SubjIdx_levels)

# prepare plot frame
plt_df_long <- 
  plt_df %>%
  filter(Race %in% Race_sub_levels) %>%
  mutate(Race_fct = factor(Race, levels = Race_sub_levels, labels = Race_sub_labels)) %>%
  pivot_longer(cols = starts_with("V")) %>%
  mutate(
    name = gsub("V", "", name),
    name = as.numeric(name),
    phase = (name - min(name))/(max(name) - min(name)),
    phase = phase * 100,
    obs_id = paste0(SubjId, "_", Race, "_", dimens, "_", Step),
    obs_id = gsub(" ", "_", obs_id),
    SubjIdx_fct = factor(SubjIdx, levels = SubjIdx_levels, labels = SubjIdx_labels)
    ) %>%
  as.data.frame()

# generate plot 
plt <- 
  ggplot(plt_df_long, aes(x = phase, y = value, color = dimens, group = obs_id)) + 
  geom_line(alpha = 0.3, size = 0.5) + 
  facet_wrap(~ SubjIdx_fct, ncol = 4) + 
  scale_color_manual(breaks = c("x", "y", "z"),
                     values = c("blue", "red", "green"))  + 
  labs(x = "Stride % cycle, t",  y = "Knee location (t)", color = "Measurement axis: ") + 
  theme_bw(base_size = 14) + 
  theme(legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
        panel.grid.major = element_line(size = 0.2),  
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = alpha('white', 0.1), color = NA),
        strip.text = element_text(angle = 0, hjust = 0)
        ) 
# plt

# save plot to file
fname_tmp <- paste0("raw_data_per_subject_", plot_label_tmp, ".jpeg")
fpath_tmp <- file.path(here(), "results_figures", fname_tmp) 
ggsave(filename = fpath_tmp, plot = plt, width = 20, height = 22, units = "cm")



