#' @description 
#' Script to estimate MFDA models with covariate
#' 
#' @author 
#' Marcos Matabuena

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
# FIT FOSR, VER 1  -------------------------------------------------------------
# ------------------------------------------------------------------------------

aux  = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="HT1 post",]
aux2 = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="HT2 post",]
aux3 = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="CR1 pre",]
aux4 = datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="CR1 post",]

nombres  = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="HT1 post",]$SubjIdx)
nombres2 = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="HT2 post",]$SubjIdx)
nombres3 = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="CR1 pre",]$SubjIdx)
nombres4 = unique(datosindividualesKnee_Zstride2[datosindividualesKnee_Zstride2$Race=="CR1 post",]$SubjIdx)

all(nombres == nombres2)
all(nombres == nombres3)
all(nombres == nombres4)

lista = 1:19
lista = as.list(lista)
for(i in 1:19){ # i <- 1
  # auxnombres1= fdata(aux[aux$SubjIdx==nombres[i],1:100], argvals = seq(0,1,length=100))
  # auxnombres2= fdata(aux2[aux2$SubjIdx==nombres2[i],1:100], argvals = seq(0,1,length=100))
  # auxnombres3= fdata(aux3[aux3$SubjIdx==nombres3[i],1:100], argvals = seq(0,1,length=100))
  # auxnombres4= fdata(aux4[aux4$SubjIdx==nombres4[i],1:100], argvals = seq(0,1,length=100))
  sel= rbind(aux[aux$SubjIdx==nombres[i],1:100],
             aux2[aux2$SubjIdx==nombres2[i],1:100],
             aux3[aux3$SubjIdx==nombres3[i],1:100],
             aux4[aux4$SubjIdx==nombres4[i],1:100])
  lista[[i]]= as.matrix(sel)
  if (!(nrow(lista[[i]]) == 80)) stop("!(nrow(ista[[i]]) == 80)")
}
dim(lista[[1]])


# comparison HIIT AND CT introducing race and sex as a covariates
gender = strength$Gender
gender = ifelse(gender=="male", 1, 0)

matriz = matrix(0, nrow= 19*40*2, ncol=104)
contar = 1
for(i in 1:19){  # i <- 1
  matriz[contar:(contar+79),1]= rep(i,40)
  matriz[contar:(contar+79),2]= rep(gender[i],40)
  # matriz[contar:(contar+79),3]= c(rep(1,40), rep(2,40)) ## FIXED HERE!!
  # HT == 0, CR == 1
  matriz[contar:(contar+79),3]= c(rep(0,40), rep(1,40))
  matriz[contar:(contar+79),4:103] = lista[[i]] 
  contar= contar+80
}

datas= matriz
datas= as.data.frame(datas)
colnames(datas)[1]="id"
colnames(datas)[2]="Gender"
colnames(datas)[3]="Race"
# datas$Y= datas[,4:104]    ## FIXED HERE!!
datas$Y= datas[,4:103]
dim(datas$Y)
# [1] 1520  100

t1 <- Sys.time()
fit_lfosr3s <- lfosr3s(formula = Y ~ Gender + Race + (1 | id), data = datas, 
                       var = TRUE, analytic = TRUE, parallel = FALSE, silent = FALSE)
t2 <- Sys.time()
t2-t1

plot(fdata(fit_lfosr3s$betaHat[1,]), main="Intercept")
plot(fdata(fit_lfosr3s$betaHat[2,]), main="Gender")
plot(fdata(fit_lfosr3s$betaHat[3,]), main="Race")
print(fit_lfosr3s$betaHat)
str(fit_lfosr3s$betaHat.var)


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

# define RaceType levels
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
  # [former]
  mutate(gender = ifelse(Gender == "male", 1, 0)) %>%
  mutate(race = ifelse(RaceType == "CR", 1, 0)) 
  # [mine]
  # mutate(RaceType_fct = factor(RaceType, levels = RaceType_levels)) %>%
  # mutate(Gender_fct = factor(Gender, levels = Gender_levels)) %>%
  # mutate(SubjIdx_fct = factor(SubjIdx, levels = SubjIdx_levels))
head(plt_df)


# define unique values of fit 
axis_unq <- sort(unique(plt_df$axis))
axis_tmp <- "z"
Race_sub_tmp <- c("HT1 post", "HT2 post", "CR1 pre", "CR1 post")

fit_df_tmp    <- plt_df %>% filter(axis == axis_tmp, Race %in% Race_sub_tmp)
fit_Y_tmp     <- fit_df_tmp %>% select(starts_with("V")) %>% as.matrix()
fit_dat_tmp   <- fit_df_tmp %>% select(-starts_with("V"))
fit_dat_tmp$Y <- fit_Y_tmp

dim(fit_dat_tmp)
dim(fit_dat_tmp$Y)

# t1 <- Sys.time()
# fit_tmp <- lfosr3s(formula = Y ~ Gender_fct + RaceType_fct + (1 | SubjIdx_fct), 
#                    data = fit_dat_tmp, 
#                    var = TRUE, analytic = TRUE, parallel = FALSE, silent = FALSE)
# t2 <- Sys.time()
# t2-t1

t1 <- Sys.time()
fit_tmp <- lfosr3s(formula = Y ~ gender + race + (1 | SubjIdx),
                   data = fit_dat_tmp,
                   var = TRUE, analytic = TRUE, parallel = FALSE, silent = FALSE)
t2 <- Sys.time()
t2-t1


plot(fdata(fit_tmp$betaHat[1,]), main="Intercept")
plot(fdata(fit_tmp$betaHat[2,]), main="Gender (male=1, female=0)")
plot(fdata(fit_tmp$betaHat[3,]), main="Race (CR=1, HT=0)")
print(fit_tmp$betaHat)
str(fit_tmp$betaHat.var)


fit_tmp <- lfosr3s(formula = Y ~ gender + race + (1 | SubjIdx),
                   data = fit_dat_tmp,
                   var = TRUE, analytic = TRUE, parallel = FALSE, silent = FALSE)
fit_tmp$betaHat.var
# use bootstrap methodology 
# boot CI 












