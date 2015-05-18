# Mantel test
# H. Achicanoy & M. López
# 2015

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
