
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
# stride
datosindividualesKnee_Xstride2 <- readRDS(file.path(here(), "data", "Knee_Xstride_anonym.rds"))
datosindividualesKnee_Ystride2 <- readRDS(file.path(here(), "data", "Knee_Ystride_anonym.rds"))
datosindividualesKnee_Zstride2 <- readRDS(file.path(here(), "data", "Knee_Zstride_anonym.rds"))
# swing 
datosindividualesKnee_Xswing2 <- readRDS(file.path(here(), "data", "Knee_Xswing_anonym.rds"))  
datosindividualesKnee_Yswing2 <- readRDS(file.path(here(), "data", "Knee_Yswing_anonym.rds")) 
datosindividualesKnee_Zswing2 <- readRDS(file.path(here(), "data", "Knee_Zswing_anonym.rds"))  

# look up some variable stats
dim(datosindividualesKnee_Zswing2)
table(datosindividualesKnee_Zswing2$Step)
table(datosindividualesKnee_Zswing2$SubjId)
table(datosindividualesKnee_Zswing2$Race)
datosindividualesKnee_Xstride2[, 100:104]


# ------------------------------------------------------------------------------
# PLOT FUNCTIONAL OBSERVATIONS, ALL AT ONCE  -----------------------------------
# ------------------------------------------------------------------------------

# Create a functional data object of class fdata
# X dimension
fda1X = fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$Race == "CR1 pre",  1:100])
fda2X = fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$Race == "CR1 post", 1:100])
fda6X = fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$Race == "HT1 post", 1:100])
fda8X = fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$Race == "HT2 post", 1:100])
# Y dimension
fda1Y = fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$Race == "CR1 pre",  1:100])
fda2Y = fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$Race == "CR1 post", 1:100])
fda6Y = fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$Race == "HT1 Post", 1:100])
fda8Y = fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$Race == "HT2 post", 1:100])
# Z dimension
fda1Z = fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race == "CR1 pre",  1:100])
fda2Z = fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race == "CR1 post", 1:100])
fda6Z = fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race == "HT1 post", 1:100])
fda8Z = fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race == "HT2 post", 1:100])

# look up some data 
dim(fda1X)
dim(fda2Y)
dim(fda6Z)

# plot some data 
# X dimension
plot(fda1X, main="CR1 Pre")
plot(fda2X, main="CR1 Post")
plot(fda6X, main="HIT1 Post")
plot(fda8X, main="HIT2 Post")
# Y dimension
plot(fda1Y, main="CR1 Pre")
plot(fda2Y, main="CR1 Post")
plot(fda6Y, main="HIT1 Post")
plot(fda8Y, main="HIT2 Post")


# ------------------------------------------------------------------------------
# PLOT FUNCTIONAL OBSERVATONS, GROUPED, VER 1   --------------------------------
# ------------------------------------------------------------------------------

# PLOT PARTICIPANT DATA KNEE Z HT1 AND HIT2
nombres  = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="HT1 post",]$SubjId)
nombres2 = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="HT2 post",]$SubjId)
aux  = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="HT1 post",]
aux2 = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="HT2 post",]
lista = 1:19
lista = as.list(lista)

# list with FUNCTIONAL INFORMATION OF  Knee_Zstride FUNCTIONAL PATTERNS on HT1 AND HT2 POST 
# EACH ELEMENT OF THE LISK, ONE PARTICIPANT
for(i in 1:19){
  auxnombres1= fdata(aux[aux$SubjId==nombres[i],1:100], argvals = seq(0,1,length=100))
  auxnombres2= fdata(aux2[aux2$SubjId==nombres2[i],1:100], argvals = seq(0,1,length=100))
  plot(auxnombres1, main=nombres[i], ylim= c(-80,80))  
  lines(auxnombres2)  
  sel= rbind(aux[aux$SubjId==nombres[i],1:100],aux2[aux2$SubjId==nombres2[i],1:100])
  lista[[i]]= as.matrix(sel)
}


# plot.new()
# path_tmp <- file.path(here(), "results_figures", "KNEEZ1nuevo.pdf")
# pdf(path_tmp)
par(mfrow=c(1,2))

plot(fdata(lista[[1]]), col="Red", ylim= c(-17,70),main="Knee Z", xlab = "% cycle, t", ylab="Angle(t)")
lines(fdata(lista[[2]]),col="Blue")
lines(fdata(lista[[3]]),col="Green")
legend("topleft", legend= c("1","2","3"),col=c("Red","Blue","Green"),lty=rep(1,3), horiz= TRUE, cex=0.5)

plot(fdata(lista[[4]]), col="Red", ylim= c(-17,70),main="Knee Z", xlab = "% cycle, t", ylab="Angle(t)")
lines(fdata(lista[[5]]),col="Blue")
lines(fdata(lista[[6]]),col="Green")
legend("topleft", legend= c("4","5","6"),col=c("Red","Blue","Green"),lty=rep(1,3), horiz= TRUE, cex=0.5)

# dev.off()


# plot.new()
# path_tmp <- file.path(here(), "results_figures", "KNEEZ2nuevo.pdf")
# pdf(path_tmp)
par(mfrow=c(1,2))

plot(fdata(lista[[7]]), col="Red", ylim= c(-17,70),main="Knee Z", xlab = "% cycle, t", ylab="Angle(t)")
lines(fdata(lista[[8]]),col="Blue")
lines(fdata(lista[[9]]),col="Green")
legend("topleft", legend= c("7","8","9"),col=c("Red","Blue","Green"),lty=rep(1,3), horiz= TRUE, cex=0.5)

plot(fdata(lista[[10]]), col="Red", ylim= c(-17,70),main="Knee Z", xlab = "% cycle, t", ylab="Angle(t)")
lines(fdata(lista[[11]]),col="Blue")
lines(fdata(lista[[12]]),col="Green")
legend("topleft", legend= c("10","11","12"),col=c("Red","Blue","Green"),lty=rep(1,3), horiz= TRUE, cex=0.5)

# dev.off()


# plot.new()
# path_tmp <- file.path(here(), "results_figures", "KNEEZ3nuevo.pdf")
# pdf(path_tmp)
par(mfrow=c(1,2))

plot(fdata(lista[[13]]), col="Red", ylim= c(-17,70),main="Knee Z", xlab = "% cycle, t", ylab="Angle(t)")
lines(fdata(lista[[14]]),col="Blue")
lines(fdata(lista[[15]]),col="Green")
legend("topleft", legend= c("13","14","15"),col=c("Red","Blue","Green"),lty=rep(1,3), horiz= TRUE, cex=0.5)

plot(fdata(lista[[16]]), col="Red", ylim= c(-17,70),main="Knee Z", xlab = "% cycle, t", ylab="Angle(t)")
lines(fdata(lista[[17]]),col="Blue")
lines(fdata(lista[[18]]),col="Green")
lines(fdata(lista[[18]]),col="Green")
lines(fdata(lista[[19]]),col="Orange")
legend("topleft", legend= c("16","17","18","19"),col=c("Red","Blue","Green", "Orange"),lty=rep(1,4), horiz= TRUE, cex=0.5)

# dev.off()

# plot.new()
par(mfrow=c(1,1))


# ------------------------------------------------------------------------------
# PLOT FUNCTIONAL OBSERVATONS, GROUPED, VER 2   --------------------------------
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
race_comb_list <- list(
  c("HT1 post", "HT2 post")
  # c("HT1 pre", "HT2 pre"),
  # c("CR1 pre", "CR2 pre"),
  # c("CR1 post", "CR2 post")
)

for (comb_tmp in race_comb_list){ # comb_tmp <- c("HT1 post", "HT2 post")
  
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
    labs(x = "% cycle, t",  y = "Angle(t)", color = "Measurement: ") + 
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
  ggsave(filename = fpath_tmp, plot = plt, width = 20, height = 24, units = "cm")
}


