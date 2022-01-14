
#' @description 
#' Script to plot data with strength measurements
#' 
#' @author 
#' Marcos Matabuena, Marta Karas

rm(list = ls())
library(here)
library(tidyverse)
library(ggrepel)
library(cowplot)
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
# PLOT FORCES  -----------------------------------------------------------------
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

var_name_labels <- c(
  'Hip abduction',
  'Hip adduction',
  'HER', 
  'HIR',  
  'Hip flexion',
  'Knee extension',
  'Knee flexion',
  'Hip extension'
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
plot_list <- list()
var_name_unq <- c("KE", "HABD", "HADD")
for (i in 1 : length(var_name_unq)) { # i <- 1
  # make subset of data for a particular variable name 
  plt_df_long_i <- 
    plt_df_long %>% 
    filter(var_name == var_name_unq[i]) %>%
    filter(session_type == "HIIT")
  var_name_fct_i <- plt_df_long_i$var_name_fct[1]
  print(paste0(i, "-th plot: ", var_name_fct_i))
  # text label
  geom_text_label = paste0(var_name_fct_i)
  geom_text_x <- plt_df_long_i$session_part_fct[1] 
  geom_text_y <- min(plt_df_long_i$value) + 1.1 * diff(range(plt_df_long_i$value))
  y_breaks <- seq(0, max(plt_df_long_i$value), by = 50)
  y_breaks <- y_breaks[y_breaks > min(plt_df_long_i$value)]
  # y_breaks <- seq(50, 400, by = 50)
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
    annotate("text", x = geom_text_x, y = geom_text_y, label = geom_text_label, 
             hjust = 0, size = 4) + 
    # facet_wrap(~ session_type_fct, ncol = 2) + 
    labs(x = "Time", y = paste0("Strength")) + 
    theme_bw(base_size = 12) + 
    theme(legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
          panel.grid.major = element_line(size = 0.2),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill = alpha('white', 0.1), color = NA),
          strip.text = element_text(angle = 0, hjust = 0)
    ) + 
    scale_x_discrete(expand = c(0.28, 0)) + 
    scale_y_continuous(breaks = y_breaks) 
  plot_list[[length(plot_list) + 1]] <- plt
  # # save plot to file  
  # path_tmp <- file.path(here(), "results_figures", paste0("strength_data_per_variable_", var_name_unq[i], ".jpeg"))
  # ggsave(filename = path_tmp, plot = plt, width = 14, height = 10, units = "cm")
}

# save the plot
plt_all <- plot_grid(plotlist = plot_list, ncol = 3, align = "hv", byrow = FALSE)
# plt_all

path_tmp <- file.path(here(), "results_figures", paste0("strength_data_per_variable", ".jpeg"))
# ggsave(filename = path_tmp, plot = plt, width = 18, height = 12, units = "cm")
save_plot(filename = path_tmp, plot = plt_all, base_width = 10, base_height = 4)



