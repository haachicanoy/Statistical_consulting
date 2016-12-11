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
suppressMessages(library(plyr))
suppressMessages(library(broom))

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
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'FSA 12'] <- '05/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'FSA 12'] <- '22/08/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'FSA 12'] <- '18/09/2013'
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'FSV 41'] <- '05/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'FSV 41'] <- '22/08/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'FSV 41'] <- '18/09/2013'
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'ICS 95'] <- '05/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'ICS 95'] <- '22/08/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'ICS 95'] <- '18/09/2013'
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'SCC 23'] <- '05/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'SCC 23'] <- '12/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'SCC 23'] <- '18/09/2013'
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'SCC 55'] <- '05/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'SCC 55'] <- '22/08/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'SCC 55'] <- '18/09/2013'
all_data$harvest_date[all_data$Ambiente == 'Santander' & all_data$Clon == 'SCC 80'] <- '05/07/2013'
all_data$harvest_date[all_data$Ambiente == 'Arauca' & all_data$Clon == 'SCC 80'] <- '22/08/2013'
all_data$harvest_date[all_data$Ambiente == 'Huila' & all_data$Clon == 'SCC 80'] <- '18/09/2013'

all_data$harvest_date <- as.Date(x = all_data$harvest_date, format = '%d/%m/%Y')
all_data$sowing_date <- all_data$harvest_date - 180

# Processing climate info

#### Arauca
rain <- read.csv('./Climate/arauca/PROCESS/03_SERIES_DAILY_With_Holes/RAIN_to.csv')
rhum <- read.csv('./Climate/arauca/PROCESS/03_SERIES_DAILY_With_Holes/RHUM_to.csv')
climaArauca <- data.frame(Ambiente='Arauca',
                          FECHA=paste(rain$year, '-',
                                                         ifelse(test = nchar(rain$month)==1, yes = paste('0', rain$month, sep = ''), no = rain$month), '-',
                                                         ifelse(test = nchar(rain$day)==1, yes = paste('0', rain$day, sep = ''), no = rain$day), sep = ''),
                          RAIN=rain$X36025010, RHUM=rhum$X36025010)
climaArauca$FECHA <- as.Date(climaArauca$FECHA, format='%Y-%m-%d')

#### Huila
rain <- read.csv('./Climate/huila/PROCESS/03_SERIES_DAILY_With_Holes/RAIN_to.csv')
rhum <- read.csv('./Climate/huila/PROCESS/03_SERIES_DAILY_With_Holes/RHUM_to.csv')
tmax <- read.csv('./Climate/huila/PROCESS/03_SERIES_DAILY_With_Holes/TMAX_to.csv')
tmin <- read.csv('./Climate/huila/PROCESS/03_SERIES_DAILY_With_Holes/TMIN_to.csv')
climaHuila <- data.frame(Ambiente='Huila',
                         FECHA=paste(rain$year, '-',
                                     ifelse(test = nchar(rain$month)==1, yes = paste('0', rain$month, sep = ''), no = rain$month), '-',
                                     ifelse(test = nchar(rain$day)==1, yes = paste('0', rain$day, sep = ''), no = rain$day), sep = ''),
                         RAIN=rain$X21045010, RHUM=rhum$X21045010, TMAX=tmax$X21045010, TMIN=tmin$X21045010)
climaHuila$FECHA <- as.Date(x = climaHuila$FECHA, format = '%Y-%m-%d')

#### Santander
rain <- read.csv('./Climate/santander/PROCESS/03_SERIES_DAILY_With_Holes/RAIN_to.csv')
rhum <- read.csv('./Climate/santander/PROCESS/03_SERIES_DAILY_With_Holes/RHUM_to.csv')
tmax <- read.csv('./Climate/santander/PROCESS/03_SERIES_DAILY_With_Holes/TMAX_to.csv')
climaSantander <- data.frame(Ambiente='Santander',
                             FECHA=paste(rain$year, '-',
                                         ifelse(test = nchar(rain$month)==1, yes = paste('0', rain$month, sep = ''), no = rain$month), '-',
                                         ifelse(test = nchar(rain$day)==1, yes = paste('0', rain$day, sep = ''), no = rain$day), sep = ''),
                             RAIN=rain$X23145020, RHUM=rhum$X23145020, TMAX=tmax$X23145020)
climaSantander$FECHA <- as.Date(x = climaSantander$FECHA, format = '%Y-%m-%d')
rm(rain, rhum, tmax, tmin)

source('../Scripts/climFunctions.R')

### Arauca
# Define climate indicator
climVar <- c("sum(RAIN, na.rm=TRUE)", "mean(RHUM, na.rm=TRUE)")
# Define indicator's name
namFun  <- c("P_accu", "RH_avg")
periodBase     <- 180      # Duracion total ciclo productivo      Default values: periodBase  <- 120
FaseCultivo    <- c("ALL") # Nombre corto por etapas de cultivo   Default values: FaseCultivo <- c("VEG","REP","LLEN")
diasPorFase    <- c(180)   # Dias de cada etapa                   Default values: diasPorFase <- <- c(48,41,31)
namFec         <- c("sowing_date","harvest_date") # Nombres de la fecha de siembra       Default values: namFec <- c("fecha_siembra","fecha_cosecha")
aArauca <- climIndicatorsGenerator(climVar=climVar, namFun=namFun, Fase=FaseCultivo,
                                  periodcul=periodBase, diasFase=diasPorFase, cosechBase=all_data[all_data$Ambiente=='Arauca',],
                                  namFecha=namFec, climBase=climaArauca, onePhase=TRUE)

### Huila
# Define climate indicator
climVar <- c("mean(TMAX, na.rm=TRUE)", "mean(TMIN, na.rm=TRUE)", "mean((TMAX+TMIN)/2, na.rm=TRUE)", "mean(TMAX-TMIN, na.rm=TRUE)",
             "sum(RAIN, na.rm=TRUE)", "mean(RHUM, na.rm=TRUE)")
# Define indicator's name
namFun  <- c("TX_avg", "TM_avg", "T_avg", "Diurnal_Range_avg",
             "P_accu", "RH_avg")
periodBase     <- 180      # Duracion total ciclo productivo      Default values: periodBase  <- 120
FaseCultivo    <- c("ALL") # Nombre corto por etapas de cultivo   Default values: FaseCultivo <- c("VEG","REP","LLEN")
diasPorFase    <- c(180)   # Dias de cada etapa                   Default values: diasPorFase <- <- c(48,41,31)
namFec         <- c("sowing_date","harvest_date") # Nombres de la fecha de siembra       Default values: namFec <- c("fecha_siembra","fecha_cosecha")
aHuila <- climIndicatorsGenerator(climVar=climVar, namFun=namFun, Fase=FaseCultivo,
                                  periodcul=periodBase, diasFase=diasPorFase, cosechBase=all_data[all_data$Ambiente=='Huila',],
                                  namFecha=namFec, climBase=climaHuila, onePhase=TRUE)

### Santander
# Define climate indicator
climVar <- c("mean(TMAX, na.rm=TRUE)", "sum(RAIN, na.rm=TRUE)", "mean(RHUM, na.rm=TRUE)")
# Define indicator's name
namFun  <- c("TX_avg", "P_accu", "RH_avg")
periodBase     <- 180      # Duracion total ciclo productivo      Default values: periodBase  <- 120
FaseCultivo    <- c("ALL") # Nombre corto por etapas de cultivo   Default values: FaseCultivo <- c("VEG","REP","LLEN")
diasPorFase    <- c(180)   # Dias de cada etapa                   Default values: diasPorFase <- <- c(48,41,31)
namFec         <- c("sowing_date","harvest_date") # Nombres de la fecha de siembra       Default values: namFec <- c("fecha_siembra","fecha_cosecha")
aSantander <- climIndicatorsGenerator(climVar=climVar, namFun=namFun, Fase=FaseCultivo,
                                  periodcul=periodBase, diasFase=diasPorFase, cosechBase=all_data[all_data$Ambiente=='Santander',],
                                  namFecha=namFec, climBase=climaSantander, onePhase=TRUE)

aArauca$Stage_1 <- NULL
aHuila$Stage_1 <- NULL
aSantander$Stage_1 <- NULL

arauca <- cbind(all_data[all_data$Ambiente == 'Arauca',], aArauca)
huila <- cbind(all_data[all_data$Ambiente == 'Huila',], aHuila)
santander <- cbind(all_data[all_data$Ambiente == 'Santander',], aSantander)
all_data <- rbind.fill(arauca, huila, santander)
rm(arauca, huila, santander, aArauca, aHuila, aSantander) # Delete auxiliar databases
rm(climaArauca, climaHuila, climaSantander) # Delete climate databases
rm(climVar, diasPorFase, FaseCultivo, namFec, namFun, periodBase) # Delete parameters
rm(calculoIndicador, climIndicatorsGenerator, unifDatos) # Delete functions

# Response variables
fqDF <- data.frame(as.matrix(expand.grid(c('porcAceite', 'acPalmitico', 'acEstearico', 'acOleico', 'acLinoleico', 'rT_C'), c('P_accu_ALL', 'RH_avg_ALL'))))
fnDF <- data.frame(as.matrix(expand.grid(c("fenolesTot", "actAntxd", "Catequina", "Epicatequina"), c('P_accu_ALL', 'RH_avg_ALL'))))
names(fqDF) <- c('y', 'x')
names(fnDF) <- c('y', 'x')

fqDF$y <- as.character(fqDF$y)
fqDF$x <- as.character(fqDF$x)
fqDF$yDesc <- rep(c('Porcentaje de aceite (%)', 'Contenido de acido palmítico', 'Contenido de acido esteárico',
                'Contenido de acido oleico', 'Contenido de acido linoleico', 'Relación Tauromina/Cafeína'), 2)
fqDF$xDesc <- c(rep('Precipitación acumulada (mm)', 6), rep('Humedad relativa promedio (%)', 6))
fnDF$y <- as.character(fnDF$y)
fnDF$x <- as.character(fnDF$x)
fnDF$yDesc <- rep(c('Fenoles totales', 'Actividad antioxidante', 'Contenido de catequina', 'Contenido de epicatequina'), 2)
fnDF$xDesc <- c(rep('Precipitación acumulada (mm)', 4), rep('Humedad relativa promedio (%)', 4))

# Variables fisicoquimicas
lapply(1:nrow(fqDF), function(i){
  
  eval(parse(text=paste('bootlm_aug <- all_data %>% bootstrap(500) %>% do(augment(lm(', fqDF[i, 1], ' ~ ', fqDF[i, 2], ', .)))', sep = '')))
  eval(parse(text=paste('gg <- ggplot(bootlm_aug, aes(', fqDF[i, 2], ', ', fqDF[i, 1], ')) + geom_point() + geom_line(aes(y=.fitted, group=replicate), colour = "cadetblue3", alpha=.1) + theme_bw() + xlab(fqDF[i, 4]) + ylab(fqDF[i, 3])', sep = '')))
  gg <- gg + theme(axis.title.x = element_text(size = 14),
                   axis.title.y = element_text(size = 14),
                   axis.text = element_text(size = 12))
  ggsave(filename = paste('./Results/lmRegression_', fqDF[i, 2], '_vs_', fqDF[i, 1], '.png', sep = ''), plot = gg, width = 6, height = 6, units = 'in')
  
})

bootrlm <- all_data %>% bootstrap(1000) %>% do(tidy(MASS::rlm(acPalmitico ~ RH_avg_ALL, .)))
alpha <- .05
bootrlm %>% group_by(term) %>% summarize(low=quantile(estimate, alpha / 2), high=quantile(estimate, 1 - alpha / 2))
ggplot(bootrlm, aes(estimate)) + geom_histogram(binwidth=2) + facet_wrap(~ term, scales="free")

# Variables funcionales
lapply(1:nrow(fnDF), function(i){
  
  eval(parse(text=paste('bootlm_aug <- all_data %>% bootstrap(500) %>% do(augment(lm(', fnDF[i, 1], ' ~ ', fnDF[i, 2], ', .)))', sep = '')))
  eval(parse(text=paste('gg <- ggplot(bootlm_aug, aes(', fnDF[i, 2], ', ', fnDF[i, 1], ')) + geom_point() + geom_line(aes(y=.fitted, group=replicate), colour = "cadetblue3", alpha=.1) + theme_bw() + xlab(fnDF[i, 4]) + ylab(fnDF[i, 3])', sep = '')))
  gg <- gg + theme(axis.title.x = element_text(size = 14),
                   axis.title.y = element_text(size = 14),
                   axis.text = element_text(size = 12))
  ggsave(filename = paste('./Results/lmRegression_', fnDF[i, 2], '_vs_', fnDF[i, 1], '.png', sep = ''), plot = gg, width = 6, height = 6, units = 'in')
  
})

rm(fnDF, fqDF, fisicoquimicos, funcionales, gg, i, independent, te, bootlm_aug)

##############################################################
# Broom functionality
bootlm_aug <- all_data %>% bootstrap(500) %>%
  do(augment(lm(acPalmitico ~ RH_avg_ALL, .)))

ggplot(bootlm_aug, aes(RH_avg_ALL, acPalmitico)) + geom_point() +
  geom_line(aes(y=.fitted, group=replicate), colour = 'cadetblue3', alpha=.1) + theme_bw() +
  xlab('Humedad relativa (%)') + ylab('Contenido de acido palmítico')

###############
bootlm <- all_data %>% bootstrap(500) %>%
  do(tidy(lm(acPalmitico ~ RH_avg_ALL, .)))
alpha = .05
bootlm %>% group_by(term) %>% summarize(low=quantile(estimate, alpha / 2),
                                         high=quantile(estimate, 1 - alpha / 2))
ggplot(bootlm, aes(estimate)) + geom_histogram(binwidth=2) + facet_wrap(~ term, scales="free")
###############

smoothspline_aug <- all_data %>% bootstrap(100) %>%
  do(augment(smooth.spline(.$RH_avg_ALL, .$porcAceite, df=4), .))

ggplot(smoothspline_aug, aes(RH_avg_ALL, porcAceite)) + geom_point() +
  geom_line(aes(y=.fitted, group=replicate), alpha=.2)
##############################################################

fisicoquimicos <- c('porcAceite', 'acPalmitico', 'acEstearico', 'acOleico', 'acLinoleico', 'rT_C')
funcionales <- c("fenolesTot", "actAntxd", "Catequina", "Epicatequina")
climateList <- c('P_accu_ALL', 'RH_avg_ALL', 'TX_avg_ALL', 'TM_avg_ALL', 'T_avg_ALL', 'Diurnal_Range_avg_ALL')

fqMat <- as.data.frame(cor(all_data[, c(fisicoquimicos, climateList)], use = 'pairwise.complete.obs', method = 'spearman'))
fnMat <- as.data.frame(cor(all_data[, c(funcionales, climateList)], use = 'pairwise.complete.obs', method = 'spearman'))

fqMat$rowVar <- rownames(fqMat)
fqMat2 <- fqMat %>% gather(colVar, spCorrelation, porcAceite:Diurnal_Range_avg_ALL)
fqMat2 <- fqMat2[which(fqMat2$rowVar %in% climateList),]
fqMat2 <- fqMat2[-which(fqMat2$rowVar %in% climateList & fqMat2$colVar %in% climateList),]

fnMat$rowVar <- rownames(fnMat)
fnMat2 <- fnMat %>% gather(colVar, spCorrelation, fenolesTot:Diurnal_Range_avg_ALL)
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
ggsave(filename='./Results/heatmap_clima_caract2.png', plot=gg, width=6, height=5, units='in')

library(randomForest)

all_data <- read.csv('Base_completa.csv')
summary(all_data)
str(all_data)
rownames(all_data) <- paste(all_data$Ambiente, '-', all_data$Clon, '-', all_data$Repeticion, sep = '')

rfFit <- randomForest(indiceMazorca~., data = all_data[,12:ncol(all_data)])
plot(rfFit, lty=1)
varImpPlot(rfFit)
partialPlot(rfFit, all_data[,12:ncol(all_data)], indiceGrano, ylim=c(10, 25))

