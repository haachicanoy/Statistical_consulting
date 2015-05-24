# Statistical consulting 1: Multivariate mixed models
# H. Achicanoy & M. López
# 2015

options(warn=-1)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

data <- read_csv('iniData.csv')
data$tramo <- factor(data$tramo)
data$periodo <- factor(data$periodo)
str(data)

pairs(data[,-c(1:2)])
cor(data[,-c(1:2)],use='complete.obs')

## Variables de respuesta: morfologicas
# 1. Pendiente del cauce
# 2. Ancho a banca llena del cauce
# 3. Índice de sinuosidad
# 4. Amplitud de meandros
# 5. Longitud de onda de meandros
# 6. Tasa de movilidad
# 7. Tasa de movilidad máxima

## Variables explicativas: Hidrológicas e hidráulicas
# 1. Caudal medio multianual
# 2. Caudal máximo historico
# 3. Caudal para 50 años de periodo de retorno
# 4. Caudal de banca llena
# 5. Velocidad para el caudal de banca llena
# 6. Ancho superficial del flujo para el caudal de banca llena

# Variables explicativas: Sedimentológicas
# 1. Diametro promedio del material del lecho
# 2. Desviación típica granulométrica
# 3. Coeficiente de uniformidad
# 4. Carga media anual de material del lecho

lm.fit <- lm()

library(nlme)

# My first mixed model
# Using lme4 package
fit <- lmer(formula=ancho_banca~caudal_medio+caudal_maximo+caudal_50+caudal_banca+velocidad+ancho_superficial+diametro_promedio+dev_granulometrica+coef_uniformidad+carga_media+(1|tramo)+(1|periodo),data=data,REML=T)
summary(fit)
