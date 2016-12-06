# Cacao analysis
# H. Achicanoy, 2016

# R options
options(warn = -1)
options(scipen = 999)

# Load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readxl))
suppressMessages(library(ggplot2))
suppressMessages(library(agricolae))

setwd('/Users/haachicanoy/Statistical_consulting/_cacao_analysis/Data') # Mac OS
setwd('C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_cacao_analysis/Data') # Windows

# ================================================================== #
# Objective 1
# ================================================================== #

cotiledon <- readxl::read_excel('cambios_cotiledon.xlsx', sheet = 1)

### GENERAL DIFFERENCES

### Wilcoxon-test for all clones using bootstrapping scheme in order to estimate significative differences before and after fermentation
# pValList <- unlist(lapply(1:5000, function(x){
#   
#   bootSam <- sample(x = 1:length(which(cotiledon$Variable=='Contenido de aceite')), size = length(which(cotiledon$Variable=='Contenido de aceite')), replace = TRUE)
#   results <- wilcox.test(x = cotiledon$Fresco[bootSam], y = cotiledon$Fermentado[bootSam], mu = 0, alternative = 'two.sided', exact = TRUE)
#   # results <- t.test(x = cotiledon$Fresco[bootSam], y = cotiledon$Fermentado[bootSam], mu = 0, alternative = 'two.sided', paired = TRUE)
#   pVal <- results$p.value
#   return(pVal)
#   
# }))
# round(quantile(pValList, probs = c(0.05, 0.95)), 5)
# 
# png('./Results/contenido_aceite_general_wilcoxon.png', width = 5, height = 5, units = 'in', res = 300)
# hist(pValList, xlab = 'Wilcoxon-test p-value', main = 'p-value bootstrap distribution ', probability = FALSE)
# dev.off()

unique(cotiledon$Variable)

lngth <- length(which(cotiledon$Variable=='Actividad antioxidante'))
gg <- ggplot(cotiledon[cotiledon$Variable=='Actividad antioxidante',], aes(x = 1:lngth, y = Fresco, colour = 'black')) + geom_point()
gg <- gg + geom_point(data = cotiledon[cotiledon$Variable=='Actividad antioxidante',], aes(x = 1:lngth, y = Fermentado, colour='red'))
gg <- gg + xlab('ClonID') + ylab('Actividad antioxidante') + theme_bw() + scale_colour_manual(name = 'Etapa', values = 1:2, breaks = c("black", "red"), labels = c("Fresco", "Fermentado"))
ggsave(filename = './Results/act_antioxidanteDescriptive.png', plot = gg, width = 6, height = 5, units = 'in')

### Wilcoxon-test for all clones using resampling scheme in order to estimate significative differences before and after fermentation
pValList2 <- unlist(lapply(1:10000, function(x){
  
  sub_data <- cotiledon[cotiledon$Variable=='Actividad antioxidante',]; rownames(sub_data) <- 1:nrow(sub_data)
  n <- nrow(sub_data) # Contenido de aceite
  bootSam <- sample(x = 1:n, size = .8*n, replace = FALSE)
  results <- wilcox.test(x = sub_data$Fresco[bootSam], y = sub_data$Fermentado[bootSam], mu = 0, alternative = 'two.sided', exact = FALSE)
  # results <- t.test(x = cotiledon$Fresco[bootSam], y = cotiledon$Fermentado[bootSam], mu = 0, alternative = 'two.sided', paired = TRUE)
  pVal <- results$p.value
  return(pVal)
  
}))
round(quantile(pValList2, probs = c(0.05, 0.95)), 5)

png('./Results/act_antioxidante_general_wilcoxon.png', width = 5, height = 5, units = 'in', res = 300)
hist(pValList2, xlab = 'Wilcoxon-test p-value', main = 'p-value resampling distribution ', probability = FALSE)
dev.off()

### DIFFERENCES THROUGH ZONE

cotiledon[cotiledon$Variable=='Fenoles totales',] %>% group_by(Zona) %>% summarise(median(Perdida)) # Contenido de aceite
gg <- ggplot(cotiledon[cotiledon$Variable=='Actividad antioxidante',], aes(x = as.factor(Zona), y = Perdida))  # Contenido de aceite
gg <- gg + geom_jitter(width = 0.25) + xlab('Zona') + ylab('Diferencia en la actividad antioxidante')
gg <- gg + theme_bw()
ggsave(filename = './Results/act_antioxidante_zonaDescriptive.png', plot = gg, width = 5, height = 5, units = 'in')

### Original data
agricolae::kruskal(y = cotiledon$Perdida[cotiledon$Variable=='Fenoles totales'], trt = cotiledon$Zona[cotiledon$Variable=='Fenoles totales'], alpha = 0.05, console = TRUE) # Contenido de aceite

### Kruskal-Wallis test by zone using resampling scheme in order to evaluate significative differences before and after fermentation
kruskalpVal <- unlist(lapply(1:10000, function(x){
  
  sub_data <- cotiledon[cotiledon$Variable=='Actividad antioxidante',]; rownames(sub_data) <- 1:nrow(sub_data)
  n <- nrow(sub_data) # Contenido de aceite
  bootSam <- sample(x = 1:n, size = .8*n, replace = FALSE)
  results <- agricolae::kruskal(y = sub_data$Perdida[bootSam], trt = sub_data$Zona[bootSam], alpha = 0.05, console = FALSE)
  # results <- t.test(x = cotiledon$Fresco[bootSam], y = cotiledon$Fermentado[bootSam], mu = 0, alternative = 'two.sided', paired = TRUE)
  pVal <- results$statistics$p.chisq
  return(pVal)
  
}))
round(quantile(kruskalpVal, probs = c(0.05, 0.95)), 5)

png('./Results/act_antioxidante_zona_kruskal.png', width = 5, height = 5, units = 'in', res = 300)
hist(kruskalpVal, xlab = 'Kruskal-Wallis test p-value', main = 'p-value resampling distribution', probability = FALSE)
dev.off()

### DIFFERENCES THROUGH CLONES

cotiledon[cotiledon$Variable=='?cido palm?tico (C16:0)',] %>% group_by(ClonID, Zona) %>% summarise(median(Perdida)) # Contenido de aceite
gg <- ggplot(cotiledon[cotiledon$Variable=='Actividad antioxidante',], aes(x = Perdida, y = as.factor(ClonID), colour = as.factor(ClonID))) + geom_point() + xlab('Diferencia en actividad antioxidante') + ylab('Clon') + facet_wrap(~Zona) + theme_bw() # Contenido de aceite
gg <- gg + labs(colour = "Clon")
ggsave(filename = './Results/act_antioxidante_zona_clonDescriptive.png', plot = gg, width = 8, height = 5, units = 'in')

### Original data
cotiledon_complete <- cotiledon %>% filter(ClonID %in% c('CCN 51', 'FEAR 5', 'FLE 3', 'FSA 12', 'FSV 41', 'ICS 95', 'SCC 55'))
agricolae::durbin.test(judge = cotiledon_complete$Zona, evaluation = cotiledon_complete$Perdida, trt = cotiledon_complete$ClonID, alpha = 0.05, console = TRUE)

# ================================================================== #
# Objective 2
# ================================================================== #

all_data <- read.csv('Base_completa.csv')
summary(all_data)
str(all_data)
rownames(all_data) <- paste(all_data$Ambiente, '-', all_data$Clon, '-', all_data$Repeticion, sep = '')

# Indice mazorca vs Indice grano
plot(x = all_data$indiceMazorca, y = all_data$indiceGrano, pch = 20, xlab = 'Indice de mazorca', ylab = 'Indice de grano')
plot(x = all_data$acPalmitico, y = all_data$acEstearico, pch = 20, xlab = 'Acido palmitico', ylab = 'Acido estearico')

pairs(all_data[,-c(1:11)])

suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(corrplot))
suppressMessages(library(plsdepot))
suppressMessages(library(viridis))
suppressMessages(library(gplots))

## Exploring correlations

corMat <- cor(all_data[,-c(1:11)], method = 'spearman')
diag(corMat) <- NA
my_palette <- colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(299)

png('./Results/spearman_correlation.png', width = 6, height = 6, units = 'in', res = 300)
heatmap.2(corMat, col=my_palette, density.info="none", trace="none", dendrogram="column", key.title='', key.xlab='Spearman correlation', margins=c(9,9))
dev.off()

hist(corMat)

all_data[,-c(1:11)] %>% correlate(method='spearman') %>% network_plot(min_cor = .1)

# Omiting soils variables
pca2 <- FactoMineR::PCA(X = all_data[,-c(1:11)], scale.unit = TRUE, graph = FALSE)

# Quality of representation
png('./Results/quality_representation.png', width = 8, height = 6, units = 'in', res = 300)
par(mfrow=c(1,3))
corrplot(pca2$var$cos2[,1:3], is.corr=FALSE) # Representation quality of each variable
corrplot(pca2$var$contrib[,1:3], is.corr=FALSE) # Contribution of each variable to dimension
corrplot(pca2$var$cor[,1:3], method="ellipse", is.corr=TRUE) # Correlation of each variable to dimension
dev.off()

par(mfrow=c(1,2))
corrplot(pca2$ind$cos2[,1:3], is.corr=FALSE) # Representation quality of each variable
corrplot(pca2$ind$contrib[,1:3], is.corr=FALSE) # Contribution of each variable to dimension

write.csv(pca2$ind$cos2[,1:3], file = 'cos2_individuals.csv', row.names = TRUE)

# Individuals description
indMat <- cbind(all_data[,c("Ambiente", "Clon", "Repeticion")], pca2$ind$contrib[,1:3])
indMat$Dim.1 <- (indMat$Dim.1 - min(indMat$Dim.1))/(max(indMat$Dim.1)-min(indMat$Dim.1))
indMat$Dim.2 <- (indMat$Dim.2 - min(indMat$Dim.2))/(max(indMat$Dim.2)-min(indMat$Dim.2))
indMat$Dim.3 <- (indMat$Dim.3 - min(indMat$Dim.3))/(max(indMat$Dim.3)-min(indMat$Dim.3))
indMat <- indMat %>% gather('PC', 'Value', 4:ncol(indMat))

gg <- ggplot(data=indMat, aes(x = Value, y = Clon)) + geom_point() + geom_point(aes(x = Value, y = Clon, colour = Value > 0.5)) + facet_grid(PC ~ Ambiente)
gg <- gg + scale_colour_manual(name = 'Clones importantes', values = setNames(c('red', 'black'), c(T, F)))
gg <- gg + theme_bw() + geom_vline(xintercept = 0.5, color='red') + xlab('Contribuci?n')
ggsave(filename = './Results/ind_cosines.png', plot = gg, width = 10, height = 8, units = 'in')

# Explained variance
gg <- fviz_eig(pca2, addlabels = TRUE, hjust = -0.3) + theme_bw()
ggsave(filename = './Results/pca_eigenVal.png', plot = gg, width = 6.5, height = 6, units = 'in')

# Variables map
gg <- fviz_pca_var(pca2, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_bw()
ggsave(filename = './Results/pca_variables_1_2.png', plot = gg, width = 6.5, height = 6, units = 'in')

gg <- fviz_pca_var(pca2, axes = c(1, 3), col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_bw()
ggsave(filename = './Results/pca_variables_1_3.png', plot = gg, width = 6.5, height = 6, units = 'in')

gg <- fviz_pca_var(pca2, axes = c(2, 3), col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_bw()
ggsave(filename = './Results/pca_variables_2_3.png', plot = gg, width = 6.5, height = 6, units = 'in')

# Individuals factor map
fviz_pca_ind(pca2, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_bw()

# Biplot
gg <- fviz_pca_biplot(pca2, axes = c(1, 2),  label="var", habillage=all_data$Ambiente,
                      addEllipses=FALSE, ellipse.level=0.95) + theme_bw()
ggsave(filename = './Results/pca_1_2_biplot.png', plot = gg, width = 6.5, height = 6, units = 'in')

gg <- fviz_pca_biplot(pca2, axes = c(1, 3),  label="var", habillage=all_data$Ambiente,
                      addEllipses=FALSE, ellipse.level=0.95) + theme_bw()
ggsave(filename = './Results/pca_1_3_biplot.png', plot = gg, width = 6.5, height = 6, units = 'in')

gg <- fviz_pca_biplot(pca2, axes = c(2, 3),  label="var", habillage=all_data$Ambiente,
                      addEllipses=FALSE, ellipse.level=0.95) + theme_bw()
ggsave(filename = './Results/pca_2_3_biplot.png', plot = gg, width = 6.5, height = 6, units = 'in')

# ================================================================== #
# Objective 3
# ================================================================== #

# Load data
cVol <- readxl::read_excel('compuestos_volatiles.xlsx', sheet = 1)
str(cVol)
rNames  <- paste(cVol$Ambiente, '-', cVol$Clon, sep = '')
cVol$ID <- cVol$Ambiente <- cVol$Clon <- NULL

# Omit constant variables, sd = 0
sdList <- apply(X = cVol, MARGIN = 2, FUN = sd)
cVol <- cVol[, -as.numeric(which(sdList==0))]
rownames(cVol) <- rNames; rm(rNames)

# Convert all variables to factor
cVol <- as.data.frame(cVol)
for(i in 1:ncol(cVol)){ cVol[,i] <- as.factor(cVol[,i]) }; rm(i)

library(FactoMineR)
library(factoextra)
library(corrplot)
library(plsdepot)
library(corrplot)
library(viridis)
library(gplots)
library(ClustOfVar)

## Exploring associations: chi-square test
p.chisq = matrix(0, nrow=ncol(cVol), ncol=ncol(cVol), byrow=T)
for(i in 1:ncol(cVol)){
  for(j in 1:ncol(cVol)){
    p.chisq[i, j] = round(chisq.test(cVol[,i], cVol[,j])$p.value, 3)
  }
}; rm(i); rm(j)

diag(p.chisq) <- NA
colnames(p.chisq) <- colnames(cVol)
rownames(p.chisq) <- colnames(cVol)
p.chisq <- as.matrix(p.chisq)
color_scale <- colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(300)
heatmap.2(p.chisq, Rowv=NULL, dendrogram="column", col=color_scale, linecol=NULL, tracecol=NULL, density.info="density", denscol="blue", margins=c(10,10))

# Run a Multiple Correspondence Analysis
mca1 <- FactoMineR::MCA(X = cVol, graph = FALSE, ncp = 8)

# Explained variance
gg <- fviz_eig(mca1, addlabels = TRUE, hjust = -0.3) + theme_bw()
ggsave(filename = './Results/mca_eigenVal.png', plot = gg, width = 6.5, height = 6, units = 'in')

# Quality of representation
png('./Results/mca_quality_representation.png', width = 8, height = 6, units = 'in', res = 300)
corrplot(mca1$var$cos2[,1:7], is.corr=FALSE) # Representation quality of each variable
dev.off()

write.csv(mca1$var$cos2[,1:8], file = 'MCA_cos2_variables.csv', row.names = TRUE)
write.csv(mca1$ind$cos2[,1:8], file = 'MCA_cos2_individuals.csv', row.names = TRUE)
write.csv(mca1$ind$contrib[,1:8], file = 'MCA_contrib_individuals.csv', row.names = TRUE)

# Individuals factor map
gg <- fviz_mca_ind(mca1, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_bw()
ggsave(filename = './Results/mca_individuals_1_2.png', plot = gg, width = 6.5, height = 6, units = 'in')

# Variables map
gg <- fviz_mca_var(mca1, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_bw()
ggsave(filename = './Results/mca_variables_1_2.png', plot = gg, width = 6.5, height = 6, units = 'in')

# Biplot
cVol2 <- readxl::read_excel('compuestos_volatiles.xlsx', sheet = 1)
gg <- fviz_mca_biplot(mca1, axes = c(1, 2),  label="all", habillage=cVol2$Ambiente,
                      addEllipses=FALSE, ellipse.level=0.95) + theme_bw()
ggsave(filename = './Results/mca_1_2_biplot.png', plot = gg, width = 6.5, height = 6, units = 'in')

# Individuals description
indMat <- cbind(cVol2[,c("Ambiente", "Clon")], mca1$ind$contrib[,1:8])
colnames(indMat)[3:ncol(indMat)] <- gsub(pattern = " ", replacement = ".", x = colnames(indMat)[3:ncol(indMat)])
for(i in 1:8){
  label <- paste("indMat$Dim.", i, " <- (indMat$Dim.", i, " - min(indMat$Dim.", i, "))/(max(indMat$Dim.", i, ")-min(indMat$Dim.", i, "))", sep="")
  eval(parse(text=label)); rm(label)
}; rm(i)
write.csv(indMat, file = 'MCA_contrib_standarized_individuals.csv', row.names = TRUE)
indMat <- indMat %>% gather('PC', 'Value', 4:ncol(indMat))

# Cluster analysis for individuals
hcpc1 <- FactoMineR::HCPC(mca1, nb.clust = -1)
hcpc1$desc.var
hcpc1$desc.ind

# Cluster analysis for variables

cVol_clus <- hclustvar(X.quali = cVol)
plot(cVol_clus)
rect.hclust(cVol_clus, k=3, border='red')
plot(cutree(cVol_clus, k = 3))
stability(cVol_clus, B=10, graph = TRUE)

cVol_clus_cutted <- cutreevar(obj = cVol_clus, k = 2, matsim = TRUE)
cVol_clus_cutted$cluster

# ================================================================== #
# Objective 4
# ================================================================== #

all_data <- read.csv('Base_completa.csv')
summary(all_data)
str(all_data)
rownames(all_data) <- paste(all_data$Ambiente, '-', all_data$Clon, '-', all_data$Repeticion, sep = '')

all_data$harvest_date <- NA
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'CCN 51'] <- '04/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'CCN 51'] <- '12/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'CCN 51'] <- '18/09/2013'
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'FEAR 5'] <- '05/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'FEAR 5'] <- '12/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'FEAR 5'] <- '18/09/2013'
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'FEC 2'] <- '04/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'FEC 2'] <- '12/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'FEC 2'] <- '18/09/2013'
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'FLE 3'] <- '04/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'FLE 3'] <- '22/08/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'FLE 3'] <- '18/09/2013'





as.Date(x = '04/07/2013', format = '%d/%m/%Y')
# Processing climate info

#### Arauca
## Rain
rain <- read.csv('./Climate/arauca/PROCESS/03_SERIES_DAILY_With_Holes/RAIN_to.csv')
rain <- as.numeric(rain[rain$year==2013 & rain$month %in% 7:8,] %>% summarise(sum(X36025010, na.rm = TRUE)))
## Rhum
rhum <- read.csv('./Climate/arauca/PROCESS/03_SERIES_DAILY_With_Holes/RHUM_to.csv')
rhum <- as.numeric(rhum[rhum$year==2013 & rhum$month %in% 7:8,] %>% summarise(mean(X36025010, na.rm = TRUE)))

arauca <- data.frame(Ambiente='Arauca', Rain=rain, RHum=rhum)

#### Huila
## Rain
rain <- read.csv('./Climate/huila/PROCESS/03_SERIES_DAILY_With_Holes/RAIN_to.csv')
rain <- as.numeric(rain[rain$year==2013 & rain$month %in% 7:8,] %>% summarise(sum(X21045010, na.rm = TRUE)))
## Rhum
rhum <- read.csv('./Climate/huila/PROCESS/03_SERIES_DAILY_With_Holes/RHUM_to.csv')
rhum <- as.numeric(rhum[rhum$year==2013 & rhum$month %in% 7:8,] %>% summarise(mean(X21045010, na.rm = TRUE)))
## Tmax
tmax <- read.csv('./Climate/huila/PROCESS/03_SERIES_DAILY_With_Holes/TMAX_to.csv')
tmax <- as.numeric(tmax[tmax$year==2013 & tmax$month %in% 7:8,] %>% summarise(mean(X21045010, na.rm = TRUE)))
## Tmin
tmin <- read.csv('./Climate/huila/PROCESS/03_SERIES_DAILY_With_Holes/TMIN_to.csv')
tmin <- as.numeric(tmin[tmin$year==2013 & tmin$month %in% 7:8,] %>% summarise(mean(X21045010, na.rm = TRUE)))

huila <- data.frame(Ambiente='Huila', Rain=rain, RHum=rhum, TMax=tmax, TMin=tmin)

#### Santander
rain <- read.csv('./Climate/santander/PROCESS/03_SERIES_DAILY_With_Holes/RAIN_to.csv')
rain <- as.numeric(rain[rain$year==2013 & rain$month %in% 7:8,] %>% summarise(sum(X23145020, na.rm = TRUE)))
## Rhum
rhum <- read.csv('./Climate/santander/PROCESS/03_SERIES_DAILY_With_Holes/RHUM_to.csv')
rhum <- as.numeric(rhum[rhum$year==2013 & rhum$month %in% 7:8,] %>% summarise(mean(X23145020, na.rm = TRUE)))
## Tmax
tmax <- read.csv('./Climate/santander/PROCESS/03_SERIES_DAILY_With_Holes/TMAX_to.csv')
tmax <- as.numeric(tmax[tmax$year==2013 & tmax$month %in% 7:8,] %>% summarise(mean(X23145020, na.rm = TRUE)))

santander <- data.frame(Ambiente='Santander', Rain=rain, RHum=rhum, TMax=tmax)

library(plyr)
climate <- rbind.fill(arauca, huila, santander)

all_data <- inner_join(all_data, climate, by='Ambiente')
climateList <- c("Rain", "RHum", "TMax", "TMin")
fisicoquimicos <- c('porcAceite', 'acPalmitico', 'acEstearico', 'acOleico', 'acLinoleico', 'rT_C')
funcionales <- c("fenolesTot", "actAntxd", "Catequina", "Epicatequina")
fqMat <- as.data.frame(cor(all_data[, c(fisicoquimicos, climateList)], use = 'pairwise.complete.obs', method = 'spearman'))
fnMat <- as.data.frame(cor(all_data[, c(funcionales, climateList)], use = 'pairwise.complete.obs', method = 'spearman'))

fqMat$rowVar <- rownames(fqMat)
fqMat2 <- fqMat %>% gather(colVar, spCorrelation, porcAceite:TMin)
fqMat2 <- fqMat2[which(fqMat2$rowVar %in% climateList),]
fqMat2 <- fqMat2[-which(fqMat2$rowVar %in% climateList & fqMat2$colVar %in% climateList),]

fnMat$rowVar <- rownames(fnMat)
fnMat2 <- fnMat %>% gather(colVar, spCorrelation, fenolesTot:TMin)
fnMat2 <- fnMat2[which(fnMat2$rowVar %in% climateList),]
fnMat2 <- fnMat2[-which(fnMat2$rowVar %in% climateList & fnMat2$colVar %in% climateList),]

fqMat2$Grupo <- 'Caracteristicas fisicoquimicas'
fnMat2$Grupo <- 'Caracteristicas funcionales'

allMat <- rbind(fqMat2, fnMat2)

library(ggplot2)
library(viridis)
library(ggthemes)
library(RColorBrewer)
gg <- ggplot(allMat, aes(x=rowVar, y=colVar, fill=spCorrelation))
gg <- gg + geom_tile(color="white", size=0.3) # "white"
gg <- gg + scale_fill_viridis(name="Spearman correlation", limits=c(-1, 1)) # option = 'magma'
gg <- gg + coord_equal()
gg <- gg + labs(x=NULL, y=NULL, title="Influencia genotipo-ambiente")
# gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0, size=17))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=10, angle=90))
gg <- gg + theme(axis.text.y=element_text(size=10))
gg <- gg + theme(legend.title=element_text(size=11, face='bold'))
gg <- gg + theme(legend.text=element_text(size=10))
gg <- gg + xlab('Variables de clima') + ylab('Caracteristicas fisicoquimicas y funcionales')
# gg <- gg + geom_hline(yintercept = 6.5, color='red')
ggsave(filename='./Results/heatmap_clima_caract.png', plot=gg, width=6, height=5, units='in')

library(randomForest)

all_data <- read.csv('Base_completa.csv')
summary(all_data)
str(all_data)
rownames(all_data) <- paste(all_data$Ambiente, '-', all_data$Clon, '-', all_data$Repeticion, sep = '')

rfFit <- randomForest(indiceMazorca~., data = all_data[,12:ncol(all_data)])
plot(rfFit, lty=1)
varImpPlot(rfFit)
partialPlot(rfFit, all_data[,12:ncol(all_data)], indiceGrano, ylim=c(10, 25))

