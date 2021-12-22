#' @description 
#' Script to calculate ICC
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

fda1X= fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$carrera=="CR1 Pre",1:100])
fda2X= fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$carrera=="CR1 POST",1:100])
fda6X= fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$carrera=="HT1 Post",1:100])
fda8X= fdata(datosindividualesKnee_Xstride2[datosindividualesKnee_Xstride2$carrera=="HT 2 Post",1:100])

fda1Y= fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$carrera=="CR1 Pre",1:100])
fda2Y= fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$carrera=="CR1 POST",1:100])
fda6Y= fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$carrera=="HT1 Post",1:100])
fda8Y= fdata(datosindividualesKnee_Ystride2[datosindividualesKnee_Ystride2$carrera=="HT 2 Post",1:100])

fda1Z= fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="CR1 Pre",1:100])
fda2Z= fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="CR1 POST",1:100])
fda6Z= fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT1 Post",1:100])
fda8Z= fdata(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT 2 Post",1:100])


# ------------------------------------------------------------------------------
# CALCULATE ICC, N2  -----------------------------------------------------------
# ------------------------------------------------------------------------------

# Create data structure
id = c()
for(i in 1 : 19){
  id = c(id, rep(i, 20))
}
visita= rep(1:20, 19)

# knee x cr1 
res_fda1X <- I2C2(fda1X$data, id, visita)
res_fda1X
# $lambda
# [1] 0.9757536
# 
# $Kx
# [1] 4477.896
# 
# $Ku
# [1] 111.2706

# knne x cr2 
res_fda2X <- I2C2(fda2X$data, id, visita)
res_fda2X
# $lambda
# [1] 0.9789654
# 
# $Kx
# [1] 5000.555
# 
# $Ku
# [1] 107.4447

# PRE-POST 
res_fda6X = I2C2(fda6X$data, id, visita)
res_fda6X
# $lambda
# [1] 0.9597422
# 
# $Kx
# [1] 3112.946
# 
# $Ku
# [1] 130.5772

res_fda8X = I2C2(fda8X$data, id, visita)
res_fda8X
# $lambda
# [1] 0.9777353
# 
# $Kx
# [1] 5947.229
# 
# $Ku
# [1] 135.4285


# ------------------------------------------------------------------------------
# CALCULATE ICC, N3, DI BIOMETRICS 2014 (N3) with 2 HIT RUNs   -----------------
# ------------------------------------------------------------------------------

# X_{i,j,k} i individual, j test pre and post k measured number
Q=19
L1=19
J=L2=2
L3=20

I_index=rep(1:L1,each=L2*L3)
n=length(I_index)
IJ_index=rep(1:(L1*L2),each=L3)

I=max(I_index)
n_I0=table(I_index)
IJ=max(IJ_index)
n_IJ=table(IJ_index)


I_index=rep(1:L1,each=L2*L3)
n=length(I_index)
IJ_index=rep(1:(L1*L2),each=L3)

I=max(I_index)
n_I0=table(I_index)
IJ=max(IJ_index)
n_IJ=table(IJ_index)

nombres= unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT1 Post",]$archivo)
nombres2= unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT 2 Post",]$archivo)

aux= datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT1 Post",]
aux2= datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$carrera=="HT 2 Post",]

lista = 1:19
lista = as.list(lista)
for(i in 1:19){
  auxnombres1= fdata(aux[aux$archivo==nombres[i],1:100], argvals = seq(0,1,length=100))
  auxnombres2= fdata(aux2[aux2$archivo==nombres2[i],1:100], argvals = seq(0,1,length=100))
  sel= rbind(aux[aux$archivo==nombres[i],1:100],aux2[aux2$archivo==nombres2[i],1:100])
  lista[[i]]= as.matrix(sel)
}

# t1 <- Sys.time()
# Y= lista
# fit.result<-try(threenest(Y=lista,L1=L1,L2=L2,L3=L3,smooth.y=TRUE,alg.y='smoothspline',Q=Q))
# t2 <- Sys.time()
# t2 - t1
# saveRDS(fit.result, file = file.path(here(), "results_objects", "fit_result_N3_Di2014.rds"))
# # Time difference of 54.75834 secs
fit.result <- readRDS(file.path(here(), "results_objects", "fit_result_N3_Di2014.rds"))
str(fit.result)


