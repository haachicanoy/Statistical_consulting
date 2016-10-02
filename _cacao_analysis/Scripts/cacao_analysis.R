# Cacao analysis
# H. Achicanoy, 2016

# setwd('/Users/haachicanoy/Documents/Asesorias/Liliana\ Moreno/Data')
options(scipen = 999); options(warn = -1)
setwd('C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_cacao_analysis/Data')

# ================================================================== #
# Objective 1
# ================================================================== #

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(agricolae)

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

cotiledon[cotiledon$Variable=='Ácido palmítico (C16:0)',] %>% group_by(ClonID, Zona) %>% summarise(median(Perdida)) # Contenido de aceite
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

library(FactoMineR)
library(factoextra)
library(corrplot)
library(plsdepot)
library(corrplot)
library(viridis)
library(gplots)

## Exploring correlations

corMat <- cor(all_data[,-c(1:11)], method = 'spearman')
diag(corMat) <- NA
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

png('./Results/spearman_correlation.png', width = 6, height = 6, units = 'in', res = 300)
heatmap.2(corMat, col=viridis, density.info="none", trace="none", dendrogram="column", key.title='', key.xlab='Spearman correlation', margins=c(9,9))
dev.off()

############### Using all variables
# pca1 <- FactoMineR::PCA(X = all_data[,-c(1,2,11)], scale.unit = TRUE, graph = FALSE)
# 
# # Eigen values
# fviz_eig(pca1, addlabels = TRUE, hjust = -0.3) + theme_bw()
# 
# # Variables map
# fviz_pca_var(pca1, col.var="cos2") +
#   scale_color_gradient2(low="white", mid="blue", 
#                         high="red", midpoint=0.5) + theme_bw()
# 
# # Individuals factor map
# fviz_pca_ind(pca1, col.ind="cos2") +
#   scale_color_gradient2(low="white", mid="blue", 
#                         high="red", midpoint=0.50) + theme_bw()

############### Omiting soils variables
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

View(pca2$ind$cos2[,1:3])

# Eigen values
fviz_eig(pca2, addlabels = TRUE, hjust = -0.3) + theme_bw()

# Variables map
gg <- fviz_pca_var(pca2, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_bw()
ggsave(filename = './Results/pca_variables.png', plot = gg, width = 6.5, height = 6, units = 'in')


# Individuals factor map
fviz_pca_ind(pca2, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_bw()

# Biplot
gg <- fviz_pca_biplot(pca2,  label="var", habillage=all_data$Ambiente,
                      addEllipses=FALSE, ellipse.level=0.95) + theme_bw()
ggsave(filename = './Results/pca_biplot.png', plot = gg, width = 6.5, height = 6, units = 'in')

############### Omiting soils variables and other indices
pca3 <- FactoMineR::PCA(X = all_data[,-c(1:13, 16, 21)], scale.unit = TRUE, graph = FALSE)

# Eigen values
fviz_eig(pca3, addlabels = TRUE, hjust = -0.3) + theme_bw()

# Variables map
fviz_pca_var(pca3, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_bw()

# Individuals factor map
fviz_pca_ind(pca3, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_bw()

# Biplot
fviz_pca_biplot(pca3,  label="var", habillage=all_data$Ambiente,
                addEllipses=FALSE, ellipse.level=0.95) +
  theme_bw()

# ================================================================== #
# Objective 3
# ================================================================== #

# ================================================================== #
# Objective 4
# ================================================================== #

library(randomForest)

all_data <- read.csv('Base_completa.csv')
summary(all_data)
str(all_data)
rownames(all_data) <- paste(all_data$Ambiente, '-', all_data$Clon, '-', all_data$Repeticion, sep = '')

rfFit <- randomForest(indiceMazorca~., data = all_data[,12:ncol(all_data)])
plot(rfFit, lty=1)
varImpPlot(rfFit)
partialPlot(rfFit, all_data[,12:ncol(all_data)], Epicatequina)

