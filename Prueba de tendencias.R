# --------------------------------------------------------------------------- #
# Test de Mann Kendall para probar tendencias temporales
# --------------------------------------------------------------------------- #

options(warn=-1)
library(ape)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

require("Kendall")

data <- read.csv('iniData.csv')

# tramo de referencia
tramos <- sort(unique(data$periodo))

# Calculo del test de MannKendall
kendall.final <- lapply(1:length(tramos), function(i)
{
  cat('Processing tramo:',tramos[i],'\n')
  data_by_tramo <- subset(data, tramo==tramos[i]) 
  data_by_tramo <- data_by_tramo[,-c(1:2)]
  varList <- names(data_by_tramo)
  
  calc.dist <- unlist(lapply(1:length(varList), function(j)
  {
    
      var.vect <- as.vector(data_by_tramo[,varList[j]])
    kendall.res <- tryCatch(expr={ kendall.res <- MannKendall(var.vect); kendall.res <- kendall.res[2]},
                          error=function(e){ cat("Error in variable:","\n"); kendall.res <- NA; return(kendall.res) })
  }))
  calc.dist <- t(calc.dist)
  return(calc.dist)
})

kendall.final <- Reduce(function(...) rbind(..., deparse.level=1), kendall.final)
kendall.final <- t(kendall.final)
colnames(kendall.final) <- paste('Tramo',tramos)
rownames(kendall.final) <- names(data)[-c(1:2)]

kendall.final = round(kendall.final,4)
kendall.final

write.csv(kendall.final, './test_tendencias/kendall_results.csv', row.names=T)

g <- gc(); rm(list=ls())

kendall.final <- read.csv('./test_tendencias/kendall_results.csv')

library(ggplot2)
library(reshape)

y <- melt(kendall.final)
p <- ggplot(y, aes(x=variable,y=X))
p <- p + geom_tile(aes(fill=value))
p <- p + scale_fill_gradient(low='darkblue', high='white', limits=c(0,1))
p <- p + xlab("") + ylab("")
p <- p + labs(fill='p-value')
p = p + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
p = p + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
p = p + theme(axis.text.x = element_text(size=20, colour=rgb(0,0,0)))
p = p + theme(axis.text.y = element_text(size=20, colour = rgb(0,0,0)))
p = p + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
p = p + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))
p



