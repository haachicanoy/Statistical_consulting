# Statistical consulting 1: Multivariate mixed models
# H. Achicanoy & M. López
# 2015

setwd('C:/Users/Harold/Documents')

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

data <- read_csv('iniData.csv')
str(data)
data$tramo <- factor(data$tramo)
data$periodo <- factor(data$periodo)

library(lme4)
library(nlme)

# My first mixed model
# Using lme4 package
fit <- lmer(formula=ancho_banca~caudal_medio+caudal_maximo+caudal_50+caudal_banca+velocidad+ancho_superficial+diametro_promedio+dev_granulometrica+coef_uniformidad+carga_media+(1|tramo)+(1|periodo),data=data,REML=T)
summary(fit)




# Waiting for Mikey additions
