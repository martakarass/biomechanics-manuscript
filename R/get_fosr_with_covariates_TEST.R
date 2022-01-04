#' @description 
#' Script to estimate MFDA models with covariate
#' 
#' @author 
#' Marcos Matabuena

rm(list = ls())
library(here)
library(fda.usc)
library(tidyverse)
library(gridExtra)
select <- dplyr::select
filter <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 

source(file.path(here(), "R", "lfos3s.R"))


# ------------------------------------------------------------------------------
# READ DATA  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read raw data 
# stride
datosindividualesKnee_Xstride2 <- readRDS(file.path(here(), "data", "Knee_Xstride_anonym.rds"))
datosindividualesKnee_Ystride2 <- readRDS(file.path(here(), "data", "Knee_Ystride_anonym.rds"))
datosindividualesKnee_Zstride2 <- readRDS(file.path(here(), "data", "Knee_Zstride_anonym.rds"))
# swing 

# look up some variable stats
dim(datosindividualesKnee_Zstride2)
table(datosindividualesKnee_Zstride2$Step)
table(datosindividualesKnee_Zstride2$SubjIdx)

# read strength data (anonymized)
strength_path <- file.path(here(), "data", "strength_anonym.rds")
strength <- readRDS(strength_path)
head(strength)


# ------------------------------------------------------------------------------
# FIT FOSR, VER 2  -------------------------------------------------------------
# ------------------------------------------------------------------------------

# prepare data 
# add info about axis (x,y,z) to each data set 
datosindividualesKnee_Xstride2$axis <- "x" 
datosindividualesKnee_Ystride2$axis <- "y" 
datosindividualesKnee_Zstride2$axis <- "z" 
plt_df <- rbind(
  datosindividualesKnee_Xstride2,
  datosindividualesKnee_Ystride2,
  datosindividualesKnee_Zstride2
)

# add gender info 
strength_sub <- strength %>% select(SubjIdx, Gender)
plt_df <- plt_df %>% left_join(strength_sub, by = "SubjIdx")

# define RaceType levels (1st is the reference)
RaceType_levels <- c("CR", "HT")
# define Gender levels
Gender_levels <- c("female", "male")
# SubjIdx  levels
SubjIdx_levels <- 1 : 19

# add RaceType, RacePart info 
plt_df <- 
  plt_df %>% 
  separate(Race, sep = " ", into = c("RaceType", "RacePart"), remove = FALSE) %>%
  mutate(RaceIdx = substr(RaceType, 3, 4)) %>%
  mutate(RaceType = substr(RaceType, 0, 2)) %>%
  mutate(RaceType_fct = factor(RaceType, levels = RaceType_levels)) %>%
  mutate(Gender_fct = factor(Gender, levels = Gender_levels)) %>%
  mutate(SubjIdx_fct = factor(SubjIdx, levels = SubjIdx_levels))
head(plt_df)
dim(plt_df)


# define unique values of fit 
axis_unq <- sort(unique(plt_df$axis))
axis_tmp <- "z"
Race_sub_tmp <- c("HT1 post", "HT2 post", "CR1 pre", "CR1 post")
# Race_sub_tmp <- c("HT2 post")

fit_df_tmp    <- plt_df %>% filter(axis == axis_tmp, Race %in% Race_sub_tmp)
fit_Y_tmp     <- fit_df_tmp %>% select(starts_with("V")) %>% as.matrix()
fit_dat_tmp   <- fit_df_tmp %>% select(-starts_with("V"))
fit_dat_tmp$Y <- fit_Y_tmp

dim(fit_dat_tmp)
dim(fit_dat_tmp$Y)
table(fit_dat_tmp$Race)
table(fit_dat_tmp$RaceType_fct)

t1 <- Sys.time()
fit_tmp <- lfosr3s(formula = Y ~ Gender_fct  + RaceType_fct + (1 | SubjIdx_fct),
                   data = fit_dat_tmp,
                   var = TRUE, analytic = TRUE, parallel = FALSE, silent = TRUE,
                   analytic_knots = 10)
t2 <- Sys.time()
t2-t1
# Time difference of 18.03444 secs
plot(diag(fit_tmp$betaHat.var[,,1]), type = "l")
plot(diag(fit_tmp$betaHat.var[,,2]), type = "l")
plot(diag(fit_tmp$betaHat.var[,,3]), type = "l")






## Figure 4
plot.FUI <- function(r, name = NULL){
  var_name <- c("Intercept", "Gender (male=1, female=0)", "Race (HT=1, CR=0)")
  decimal <- c(2,2,2,2,3)
  beta.hat.plt <- data.frame(s = seq(1, ncol(fit_tmp$betaHat), length.out = ncol(fit_tmp$betaHat)), 
                             beta = fit_tmp$betaHat[r,],
                             lower = fit_tmp$betaHat[r,] - 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             upper = fit_tmp$betaHat[r,] + 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             lower.joint = fit_tmp$betaHat[r,] - fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             upper.joint = fit_tmp$betaHat[r,] + fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])))
  p.CI <- ggplot() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    geom_ribbon(aes(x = s, ymax = upper.joint, ymin = lower.joint), data = beta.hat.plt, fill = "gray30", alpha = 0.2) +
    geom_ribbon(aes(x = s, ymax = upper, ymin = lower), data = beta.hat.plt, fill = "gray10", alpha = 0.4) +
    geom_line(aes(x = s, y = beta, color = "Estimate"), data = beta.hat.plt, alpha = 1, lty = 5) +
    scale_colour_manual(name="", values=c("Estimate"="blue3")) 
  # scale_y_continuous(labels=function(x) sprintf(paste0("%.", decimal[r], "f"), x)) +
  # scale_x_continuous(breaks = c(1, 24, 47, 70, 93))
  
  if(r == 1){
    p.CI <- p.CI + labs(x = "Stride phase", y = expression(beta[0](s)), title = var_name[r]) +
      theme(legend.title=element_blank(),
            legend.position = c(0.15, 0.99),
            legend.justification = c("left", "top"),
            legend.box.just = "right",
            legend.margin = margin(0, 0, 0, 0),
            legend.background = element_rect(fill=alpha('white', 0)))
  }else{
    p.CI <- p.CI + labs(x = "Stride phase", y = bquote(paste(beta[.(r-1)], "(s)")), 
                        title = var_name[r]) +
      theme(legend.position = "none") +
      geom_hline(yintercept=0, color = "black", lwd=0.5, linetype = "dotted")
  }
  if(!is.null(name)){
    p.CI <- p.CI + labs(title = name)
  }
  return(p.CI)
}

plot.f4 <- list()
for(prdtr in 1:nrow(fit_tmp$betaHat)){
  plot.f4[[prdtr]] <- plot.FUI(r = prdtr)
}

do.call("grid.arrange", c(plot.f4, nrow = 1))





