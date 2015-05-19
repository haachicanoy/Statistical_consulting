# Spatial autocorrelation
# H. Achicanoy & M. López
# 2015

# --------------------------------------------------------------------------- #
# Moran index in order to check spatial autocorrelation
# --------------------------------------------------------------------------- #

options(warn=-1)
library(ape)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

data <- read.csv('iniData.csv')
coords <- read.csv("trans_coordinates.csv")
coords <- coords[,c('tramo','Lon','Lat')]

# Calculate distance between coordinates
points.dist <- dist(cbind(coords$Lon,coords$Lat))
points.dist.inv <- 1/points.dist
points.dist.inv <- as.matrix(points.dist.inv)

# Periods of reference
periods <- sort(unique(data$tramo))

# Calculate Mantel test for check spatial autocorrelation using 10.000 rep
moran.final <- lapply(1:length(periods), function(i)
{
  cat('Processing decade:',periods[i],'\n')
  data_by_period <- data %>% filter(periodo==periods[i])
  data_by_period <- data_by_period[,-c(1:2)]
  varList <- names(data_by_period)
  
  calc.dist <- unlist(lapply(1:length(varList), function(j)
  {
    var.vect <- as.vector(data_by_period[,varList[j]])
    moran.res <- tryCatch(expr={ moran.res <- Moran.I(x=var.vect, weight=points.dist.inv); moran.res <- moran.res$p.value},
                           error=function(e){ cat("Error in variable:",varList[j],"\n"); moran.res <- NA; return(moran.res) })
  }))
  calc.dist <- t(calc.dist)
  return(calc.dist)
})

moran.final <- Reduce(function(...) rbind(..., deparse.level=1), moran.final)
moran.final <- t(moran.final)
colnames(moran.final) <- paste('Decada',periods)
rownames(moran.final) <- names(data)[-c(1:2)]
write.csv(moran.final, './spatial_autocorrelation/moran_results.csv', row.names=T)

g <- gc(); rm(list=ls())

moran.final <- read.csv('./spatial_autocorrelation/moran_results.csv')

library(ggplot2)
library(reshape)

y <- melt(moran.final)
p <- ggplot(y, aes(x=variable,y=X))
p <- p + geom_tile(aes(fill=value))
p <- p + scale_fill_gradient(low='darkblue', high='white', limits=c(0,1))
p <- p + xlab("") + ylab("")
p <- p + labs(fill='p-value')
p

# --------------------------------------------------------------------------- #
# Mantel test in order to check spatial autocorrelation
# --------------------------------------------------------------------------- #

g <- gc(); rm(list=ls())

options(warn=-1)
library(ade4)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

data <- read.csv('iniData.csv') # Read complete dataset
coords <- read.csv('trans_coordinates.csv') # Read coordinates
coords <- coords[,c('tramo','Lon','Lat')]

# Calculate distance between coordinates
points.dist <- dist(cbind(coords$Lon,coords$Lat))

# Periods of reference
periods <- sort(unique(data$tramo))

# Calculate Mantel test for check spatial autocorrelation using 10.000 rep
mantel.final <- lapply(1:length(periods), function(i)
{
  cat('Processing decade:',periods[i],'\n')
  data_by_period <- data %>% filter(periodo==periods[i])
  data_by_period <- data_by_period[,-c(1:2)]
  varList <- names(data_by_period)
  
  calc.dist <- unlist(lapply(1:length(varList), function(j)
  {
    var.dist <- dist(data_by_period[,varList[j]])
    mantel.res <- tryCatch(expr={ mantel.res <- mantel.rtest(points.dist, var.dist, nrepet=10000); mantel.res <- mantel.res$pvalue },
                           error=function(e){ cat("Error in variable:",varList[j],"\n"); mantel.res <- NA; return(mantel.res) })
  }))
  calc.dist <- t(calc.dist)
  return(calc.dist)
})

mantel.final <- Reduce(function(...) rbind(..., deparse.level=1), mantel.final)
mantel.final <- t(mantel.final)
colnames(mantel.final) <- paste('Decada',periods)
rownames(mantel.final) <- names(data)[-c(1:2)]
write.csv(mantel.final, './spatial_autocorrelation/mantel_results.csv', row.names=T)

g <- gc(); rm(list=ls())

mantel.final <- read.csv('./spatial_autocorrelation/mantel_results.csv')

library(ggplot2)
library(reshape)

y <- melt(mantel.final)
p <- ggplot(y, aes(x=variable,y=X))
p <- p + geom_tile(aes(fill=value))
p <- p + scale_fill_gradient(low='darkblue', high='white', limits=c(0,1))
p <- p + xlab("") + ylab("")
p <- p + labs(fill='p-value')
p
