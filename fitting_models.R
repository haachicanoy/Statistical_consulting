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

# Pearson correlation matrix
write.csv(round(cor(data[,-c(1:2)],use='complete.obs',method='pearson'),2),'correlation_matrix_pearson.csv',row.names=T)
# Spearman correlation matrix
write.csv(round(cor(data[,-c(1:2)],use='complete.obs',method='spearman'),2),'correlation_matrix_spearman.csv',row.names=T)

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

# Correlaciones entre variables por decada
cor.by.period <- lapply(as.numeric(levels(data$periodo)),function(i)
{
  data.by.period <- data %>% filter(periodo==i)
  data.by.period <- data.by.period[,-c(1:2)]
  cor.mat <- cor(data.by.period,method='spearman',use='pairwise.complete.obs')
  cor.mat <- as.data.frame(cor.mat)
  cor.mat <- round(cor.mat,2)
  cor.mat$varName <- rownames(cor.mat)
  rownames(cor.mat) <- 1:nrow(cor.mat)
  cor.mat$periodo <- i
  return(cor.mat)
})
cor.by.period <- Reduce(function(...) rbind(..., deparse.level=1), cor.by.period)
rownames(cor.by.period) <- 1:nrow(cor.by.period)
write.csv(cor.by.period,'correlations_by_period.csv',row.names=FALSE)

# Correlaciones entre variables por tramo
cor.by.tramo <- lapply(as.numeric(levels(data$tramo)),function(i)
{
  data.by.tramo <- data %>% filter(periodo==i)
  data.by.tramo <- data.by.tramo[,-c(1:2)]
  cor.mat <- cor(data.by.tramo,method='spearman',use='pairwise.complete.obs')
  cor.mat <- as.data.frame(cor.mat)
  cor.mat <- round(cor.mat,2)
  cor.mat$varName <- rownames(cor.mat)
  rownames(cor.mat) <- 1:nrow(cor.mat)
  cor.mat$tramo <- i
  return(cor.mat)
})
cor.by.tramo <- Reduce(function(...) rbind(..., deparse.level=1), cor.by.tramo)
rownames(cor.by.tramo) <- 1:nrow(cor.by.tramo)
write.csv(cor.by.tramo,'correlations_by_tramo.csv',row.names=FALSE)

# Regresiones lineales múltiples por variable de respuesta
resList <- names(data)[3:9]
lm.fit.all <- lapply(1:length(resList),function(i)
{
  label <- paste('lm.fit <- lm(',resList[i],'~ tramo + periodo + caudal_medio + caudal_maximo + caudal_50 + caudal_banca + velocidad + ancho_superficial + diametro_promedio + dev_granulometrica + coef_uniformidad + carga_media, data = data)')
  eval(parse(text=label)); rm(label)
  return(lm.fit)
})

# Regresiones lineales múltiples por mínimos cuadrados generalizados
# incluyendo una componente autorregresiva de orden 1 por tramo
resList <- names(data)[3:9]
lm.fit.all.time <- lapply(1:length(resList),function(i)
{
  library(nlme)
  label <- paste('lm.fit <- gls(',resList[i],'~ tramo + periodo + caudal_medio + caudal_maximo + caudal_50 + caudal_banca + velocidad + ancho_superficial + diametro_promedio + dev_granulometrica + coef_uniformidad + carga_media, data = data, na.action = na.omit, correlation = corAR1(form = ~periodo|tramo))')
  eval(parse(text=label)); rm(label)
  return(lm.fit)
})

# Modelos lineales mixtos considerando todas las variables como efectos fijos
# y usando el tiempo como factor aleatorio
library(nlme)
resList <- names(data)[3:9]
lmm.fit.all <- lapply(1:length(resList),function(i)
{
  label <- paste('lmm.fit <- lme(',resList[i],'~ tramo + caudal_medio + caudal_maximo + caudal_50 + caudal_banca + velocidad + ancho_superficial + diametro_promedio + dev_granulometrica + coef_uniformidad + carga_media, na.action = na.omit, random = ~1 | periodo, data=data)')
  eval(parse(text=label)); rm(label)
  return(lmm.fit)
})

# Modelos lineales mixtos considerando todas las variables como efectos fijos
# incluyendo una pendiente aleatoria para cada variable que cambia en función del tiempo
resList <- names(data)[3:9]
lmm.fit.all.random <- lapply(1:length(resList),function(i)
{
  label <- paste('lmm.fit <- lme(',resList[i],'~ tramo + caudal_medio + caudal_maximo + caudal_50 + caudal_banca + velocidad + ancho_superficial + diametro_promedio + dev_granulometrica + coef_uniformidad + carga_media, random = ~tramo + caudal_medio + caudal_maximo + caudal_50 + caudal_banca + velocidad + ancho_superficial + diametro_promedio + dev_granulometrica + coef_uniformidad + carga_media | periodo, data=data, na.action = na.omit)')
  eval(parse(text=label)); rm(label)
  return(lmm.fit)
})

# Modelos lineales mixtos considerando todas las variables como efectos fijos
# incluyendo una componente autorregresiva de orden 1 por tramo (En prueba!)
resList <- names(data)[3:9]
lmm.fit.all.ar1 <- lapply(1:length(resList),function(i)
{
  label <- paste('lmm.fit <- lme(',resList[i],'~ tramo + caudal_medio + caudal_maximo + caudal_50 + caudal_banca + velocidad + ancho_superficial + diametro_promedio + dev_granulometrica + coef_uniformidad + carga_media, random = ~tramo + caudal_medio + caudal_maximo + caudal_50 + caudal_banca + velocidad + ancho_superficial + diametro_promedio + dev_granulometrica + coef_uniformidad + carga_media | periodo, correlation = corAR1(form = ~periodo), data=data, na.action = na.omit)')
  eval(parse(text=label)); rm(label)
  return(lmm.fit)
})

library(lme4)
?glmer() # Modelos lineales mixtos generalizados








