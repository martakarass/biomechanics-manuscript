#' @description 
#' Script to estimate MFDA models with covariate
#' 
#' @author 
#' Marcos Matabuena

rm(list = ls())
library(here)
library(readr)
library(fda.usc)
library(tidyverse)
library(I2C2)
select <- dplyr::select
filter <- dplyr::filter
separate <- tidyr::separate

# open the project in RStudio via clicking biomechanics-manuscript.Rproj
# to have here() pointing to the project directory path 
here() 

source(file.path(here(), "R", "threenest.R"))
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
datosindividualesKnee_Xswing2 <- readRDS(file.path(here(), "data", "Knee_Xswing_anonym.rds"))  
datosindividualesKnee_Yswing2 <- readRDS(file.path(here(), "data", "Knee_Yswing_anonym.rds")) 
datosindividualesKnee_Zswing2 <- readRDS(file.path(here(), "data", "Knee_Zswing_anonym.rds"))  

# look up some variable stats
dim(datosindividualesKnee_Zswing2)
table(datosindividualesKnee_Zswing2$paso)
table(datosindividualesKnee_Zswing2$archivo)
table(datosindividualesKnee_Zswing2$carrera)

# read strength data (anonymized)
strength_path <- file.path(here(), "data", "strength_anonym.rds")
strength <- readRDS(strength_path)


# ------------------------------------------------------------------------------
# READ DATA  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

aux  = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT1 Post",]
aux2 = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT 2 Post",]
aux3 = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="CR1 Pre",]
aux4 = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="CR1 POST",]

nombres  = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT1 Post",]$archivo)
nombres2 = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT 2 Post",]$archivo)
nombres3 = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="CR1 Pre",]$archivo)
nombres4 = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="CR1 POST",]$archivo)

lista = 1:19
lista = as.list(lista)
for(i in 1:19){
  auxnombres1= fdata(aux[aux$archivo==nombres[i],1:100], argvals = seq(0,1,length=100))
  auxnombres2= fdata(aux2[aux2$archivo==nombres2[i],1:100], argvals = seq(0,1,length=100))
  auxnombres3= fdata(aux3[aux3$archivo==nombres3[i],1:100], argvals = seq(0,1,length=100))
  auxnombres4= fdata(aux4[aux4$archivo==nombres4[i],1:100], argvals = seq(0,1,length=100))
  sel = rbind(
    aux[aux$archivo==nombres[i],1:100],
    aux2[aux2$archivo==nombres2[i],1:100],
    aux3[aux3$archivo==nombres3[i],1:100],
    aux4[aux4$archivo==nombres4[i],1:100])
  lista[[i]] = as.matrix(sel)
}


sexo = strength %>% pull(1)
sexo = ifelse(sexo == "male", 1, 0)

# comparision HIT AND CT introducing race and sex as a covariates
matriz = matrix(0, nrow= 19*40*2, ncol=104)
contar =1
for(i in 1:19){
  matriz[contar:(contar+79),1]= rep(i,40)
  matriz[contar:(contar+79),2]=rep(sexo[i],40)
  matriz[contar:(contar+79),3]=c(rep(1,40), rep(2,40))
  matriz[contar:(contar+79),4:103]= lista[[i]] 
  contar= contar+80
}

datas= matriz
datas= as.data.frame(datas)
colnames(datas)[1]="id"
colnames(datas)[2]="Gender"
colnames(datas)[3]="Race"
datas$Y= datas[,4:104]
str(datas)
table(datas$id)
table(datas$Gender)
table(datas$Race)

datas %>% group_by(id, Gender, Race) %>% summarise(cnt = n())
# A tibble: 38 × 4
# Groups:   id, Gender [19]
#      id Gender  Race   cnt
#   <dbl>  <dbl> <dbl> <int>
# 1     1      1     1    40
# 2     1      1     2    40
# 3     2      0     1    40
# 4     2      0     2    40
# 5     3      0     1    40
# 6     3      0     2    40
# 7     4      1     1    40
# 8     4      1     2    40
# 9     5      1     1    40
# 10     5      1     2    40


# t1 <- Sys.time()
# fit_lfosr3s_analytic <- 
#   lfosr3s(formula = Y ~ Gender + Race + (1 | id), data = datas,
#           var = TRUE, analytic = TRUE, parallel = FALSE, silent = FALSE)
# t2 <- Sys.time()
# t2 - t1
# # Time difference of 22.52151 secs
# saveRDS(fit_lfosr3s_analytic, file = file.path(here(), "results_objects", "fit_lfosr3s_analytic.rds"))


# t1 <- Sys.time()
# fit_lfosr3s_boot <- 
#   lfosr3s(formula = Y ~ Gender + Race + (1 | id), data = datas,
#           var = TRUE, analytic = FALSE, parallel = TRUE, silent = FALSE)
# t2 <- Sys.time()
# t2 - t1
# # Time difference of 27.01422 secs
# saveRDS(fit_lfosr3s_boot, file = file.path(here(), "results_objects", "fit_lfosr3s_boot.rds"))


fit_lfosr3s <- readRDS(file.path(here(), "results_objects", "fit_lfosr3s_analytic.rds"))
str(fit_lfosr3s)

plot(fdata(fit_lfosr3s$betaHat[1,]),main="Intercept")
plot(fdata(fit_lfosr3s$betaHat[2,]),main="Gender")
plot(fdata(fit_lfosr3s$betaHat[3,]),main="Race")

str(fit_lfosr3s$betaHat.var)


