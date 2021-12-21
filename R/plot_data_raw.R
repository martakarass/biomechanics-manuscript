
#' @description 
#' Script to plot raw data (angle profiles recorded along 20 strides in two HIIT sessions)

rm(list = ls())
library(here)
library(readr)
library(fda.usc)
library(tidyverse)
select <- dplyr::select
filter <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 

# read file with ggplot theme config (the same for the whole project)
source(file.path(here(), "R", "config_figures.R"))


# ------------------------------------------------------------------------------
# READ DATA  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read raw data 
# stride
datosindividualesKnee_Xstride2 <- readRDS(file.path(here(), "data", "Knee_Xstride_anonym.RDS"))
datosindividualesKnee_Ystride2 <- readRDS(file.path(here(), "data", "Knee_Ystride_anonym.RDS"))
datosindividualesKnee_Zstride2 <- readRDS(file.path(here(), "data", "Knee_Zstride_anonym.RDS"))
# swing 
datosindividualesKnee_Xswing2 <- readRDS(file.path(here(), "data", "Knee_Xswing_anonym.RDS"))  
datosindividualesKnee_Yswing2 <- readRDS(file.path(here(), "data", "Knee_Yswing_anonym.RDS")) 
datosindividualesKnee_Zswing2 <- readRDS(file.path(here(), "data", "Knee_Zswing_anonym.RDS"))  

# look up some variable stats
dim(datosindividualesKnee_Zswing2)
table(datosindividualesKnee_Zswing2$paso)
table(datosindividualesKnee_Zswing2$archivo)
table(datosindividualesKnee_Zswing2$carrera)
datosindividualesKnee_Xstride2[, c(101, 102, 103)]

# read force data (anonymized)
# strength_path <- file.path(here(), "data", "strength_anonym.csv")
# strength <- read_csv(strength_path)
strength_path <- file.path(here(), "data", "strength_anonym.rds")
strength <- readRDS(strength_path)
# head(strength)
# dim(strength)


# ------------------------------------------------------------------------------
# PLOT FUNCTIONAL OBSERVATIONS, ALL AT ONCE  -----------------------------------
# ------------------------------------------------------------------------------

# Create a functional data object of class fdata
# X dimension
fda1X = fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$carrera=="CR1 Pre",  1:100])
fda2X = fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$carrera=="CR1 POST", 1:100])
fda6X = fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$carrera=="HT1 Post", 1:100])
fda8X = fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$carrera=="HT 2 Post",1:100])
# Y dimension
fda1Y = fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$carrera=="CR1 Pre",1:100])
fda2Y = fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$carrera=="CR1 POST",1:100])
fda6Y = fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$carrera=="HT1 Post",1:100])
fda8Y = fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$carrera=="HT 2 Post",1:100])
# Z dimension
fda1Z = fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="CR1 Pre",1:100])
fda2Z = fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="CR1 POST",1:100])
fda6Z = fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT1 Post",1:100])
fda8Z = fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT 2 Post",1:100])

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
# PLOT STRENGTH INFORMATION  ---------------------------------------------------
# ------------------------------------------------------------------------------

# FUNCTION TO PLOT STRENGHT INFORMATION
grafico= function(x, nombres = strength$subj_idx, variable="Variable"){
  x=apply(x,2, as.numeric)
  n= dim(x)[1]
  p= dim(x)[2]
  minimo= min(x,na.rm = TRUE)
  maximo= max(x,na.rm = TRUE)
  xaux= c(1,2,3,4)
  yaux= x[1,]
  plot(xaux,yaux,ylim= c(minimo, maximo),type = "l",main= variable)
  text(mean(xaux), mean(yaux,na.rm=TRUE), labels=nombres[1])
  for(i in 2:n){
    yaux= x[i,]
    lines(xaux,yaux)
    text(mean(xaux), mean(yaux,na.rm=TRUE), labels=nombres[i])
  }
}

# HABDStrength : hip abduction 
# HADDStrength : Hip Adduction
# HEStrength : Hip Extension 
# HFStrength : Hip Flexion 
# KFStrength : Knee Flexion 
# KE Strength; Knee Extension 
HABD=strength[,c(4,5,20,21)]
HADD=strength[,c(6,7,22,23)]
HER= strength[,c(8,9,24,25)]
HIR= strength[,c(10,11,26,27)]
HF=  strength[,c(12,13,28,29)]
KE=  strength[,c(14,15,30,31)]
KF=  strength[,c(16,17,32,33)]
HE=  strength[,c(18,19,34,35)] 

# x <- HABD; variable = "HABD"; nombres = strength$subj_idx
grafico(HABD, variable = "HABD")
grafico(HADD, variable = "HADD")
grafico(HER, variable = "HER")
grafico(HIR, variable = "HIR")
grafico(HF, variable = "HF")
grafico(KE, variable = "KE")
grafico(KF, variable = "KF")
grafico(HE, variable = "HE")


# ------------------------------------------------------------------------------
# PLOT FUNCTIONAL OBSERVATONS, GROUPED, VER 1   --------------------------------
# ------------------------------------------------------------------------------

# PLOT PARTICIPANT DATA KNEE Z HT1 AND HIT2
nombres= unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT1 Post",]$archivo)
nombres2= unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT 2 Post",]$archivo)
aux= datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT1 Post",]
aux2= datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT 2 Post",]
lista= 1:19
lista= as.list(lista)

# list with FUNCTIONAL INFORMATION OF  Knee_Zstride FUNCTIONAL PATTERNS on HT1 AND HT2 POST 
# EACH ELEMENT OF THE LISK, ONE PARTICIPANT
for(i in 1:19){
  auxnombres1= fdata(aux[aux$archivo==nombres[i],1:100], argvals = seq(0,1,length=100))
  auxnombres2= fdata(aux2[aux2$archivo==nombres2[i],1:100], argvals = seq(0,1,length=100))
  plot(auxnombres1, main=nombres[i], ylim= c(-80,80))  
  lines(auxnombres2)  
  sel= rbind(aux[aux$archivo==nombres[i],1:100],aux2[aux2$archivo==nombres2[i],1:100])
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


# ------------------------------------------------------------------------------
# PLOT FUNCTIONAL OBSERVATONS, GROUPED, VER 2   --------------------------------
# ------------------------------------------------------------------------------

# get subject hash -- subject ID mapping from strength df
strength_sub <- strength %>% select(subj_id, subj_idx)

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

# define carrera levels
carrera_sub_levels <- c("HT1 Post", "HT 2 Post")
carrera_sub_labels <- c("1", "2")
subj_idx_levels <- sort(unique(strength_sub$subj_idx))
subj_idx_labels <- paste0("ID ", subj_idx_levels)

# mutate, format to long 
plt_df_long <- 
  plt_df %>%
  filter(carrera %in% carrera_sub_levels) %>%
  mutate(carrera_fct = factor(carrera, levels = carrera_sub_levels, labels = carrera_sub_labels)) %>%
  separate(archivo, into = "subj_id", sep = "_", extra = "drop") %>%
  left_join(strength_sub, by = "subj_id")  %>%
  filter(!is.na(subj_idx)) %>%
  pivot_longer(cols = starts_with("V")) %>%
  mutate(
    name = gsub("V", "", name),
    name = as.numeric(name),
    phase = (name - min(name))/(max(name) - min(name)),
    obs_id = paste0(subj_id, "_", carrera, "_", dimens, "_", paso),
    obs_id = gsub(" ", "_", obs_id),
    subj_idx_fct = factor(subj_idx, levels = subj_idx_levels, labels = subj_idx_labels)
    ) %>%
  as.data.frame()
head(plt_df_long)

# generate plot 
plt <- 
  ggplot(plt_df_long, aes(x = phase, y = value, color = dimens, group = obs_id)) + 
  geom_line() + 
  facet_wrap(~ subj_idx_fct, ncol = 4) + 
  labs(x = "Stride phase", 
       y = "value")
plt

