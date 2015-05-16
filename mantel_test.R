# Mantel test
# H. Achicanoy
# 2015

options(warn=-1)
library(ade4)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

data <- read.csv('iniData.csv')
coords <- read.csv('trans_coordinates.csv')
coords <- coords[,c('tramo','Lon','Lat')]

# Se calcula el test de Mantel por decada para cada una de las variables del dataset

points.dist <- dist(cbind(coords$Lon,coords$Lat))

periods <- sort(unique(data$tramo))
lapply(1:length(periods), function(i)
{
  data_by_period <- data %>% filter(periodo==periods[i])
  data_by_period <- data_by_period[,-c(1:2)]
  varList <- names(data_by_period)
  
  calc.dist <- unlist(lapply(1:length(varList), function(j)
  {
    var.dist <- dist(data_by_period[,varList[j]])
    
    if(sum(is.na(var.dist))==15){
      cat('It is not possible calculate Mantel test\n')
      mantel.res <- NA
    } else {
      mantel.res <- mantel.rtest(points.dist, var.dist, nrepet=10000)
      mantel.res <- mantel.res$pvalue
    }
  }))
  
})










