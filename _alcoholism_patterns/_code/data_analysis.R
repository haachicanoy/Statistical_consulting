
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Load data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
options(warn=-1)
library(foreign)
all_data <- read.spss(file='C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_alcoholism_patterns/_data/DatosNuevos2Work.sav', use.value.labels=TRUE, to.data.frame=TRUE, use.missings=TRUE)
names(all_data)

levels(all_data$Edad_categorizada) <- c('Hasta 17 años', '18 o más años')
levels(all_data$Carrera)[7:8] <- c("Diseño industrial", "Diseño medios interactivos")
levels(all_data$Pregunta1)[5] <- '4 o más veces a la semana'
all_data$Pregunta1 <- factor(all_data$Pregunta1, ordered=TRUE)
all_data$Pregunta2 <- as.character(all_data$Pregunta2)
# all_data$Pregunta2[which(is.na(match(all_data$Pregunta2, 0:4)))] <- 1
# all_data$Pregunta2[which(all_data$Pregunta2==0)] <- '1 o 2'
# all_data$Pregunta2[which(all_data$Pregunta2==1)] <- '3 o 4'
# all_data$Pregunta2[which(all_data$Pregunta2==2)] <- '5 o 6'
# all_data$Pregunta2[which(all_data$Pregunta2==3)] <- 'De 7 a 9'
all_data$Pregunta2[which(all_data$Pregunta2=='10 o mÃ¡s')] <- '10 o más'
all_data$Pregunta2 <- factor(all_data$Pregunta2, levels=c('1 o 2', '3 o 4', '5 o 6', 'De 7 a 9', '10 o más'), ordered=TRUE)
all_data$Pregunta3 <- factor(all_data$Pregunta3, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
# all_data$Pregunta4 <- as.character(all_data$Pregunta4)
# all_data$Pregunta4[which(is.na(match(all_data$Pregunta4, 0:4)))] <- 0
# all_data$Pregunta4[which(all_data$Pregunta4=='0')] <- 'Nunca'
# all_data$Pregunta4[which(all_data$Pregunta4=='1')] <- 'Menos de una vez al mes'
# all_data$Pregunta4[which(all_data$Pregunta4=='2')] <- 'Mensualmente'
# all_data$Pregunta4[which(all_data$Pregunta4=='3')] <- 'Semanalmente'
# all_data$Pregunta4[which(all_data$Pregunta4=='4')] <- 'A diario o casi a diario'
all_data$Pregunta4 <- factor(all_data$Pregunta4, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta5 <- factor(all_data$Pregunta5, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta6 <- factor(all_data$Pregunta6, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta7 <- factor(all_data$Pregunta7, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta8 <- factor(all_data$Pregunta8, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
levels(all_data$Pregunta9) <- c('No', 'Sí, pero no en el curso del último año', 'Sí, el último año')
levels(all_data$Pregunta10) <- c('No', 'Sí, pero no en el curso del último año', 'Sí, el último año')

# Percent of women with AUDIT > 7
sum(all_data$PAudit[all_data$Genero=='Mujer']>7, na.rm=TRUE)/length(all_data$PAudit[all_data$Genero=='Mujer'])

# Percent of women with AUDIT > 8
sum(all_data$PAudit[all_data$Genero=='Hombre']>8, na.rm=TRUE)/length(all_data$PAudit[all_data$Genero=='Hombre'])

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Multiple correspondence analysis
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
library(FactoMineR)

all_data2 <- all_data[,c("Genero", "Edad", "Carrera", "Facultad", "PAudit", "Pregunta1", "Pregunta2", "Pregunta3", "Pregunta4", "Pregunta5", "Pregunta6", "Pregunta7", "Pregunta8", "Pregunta9", "Pregunta10")]
mca_patt <- MCA(X=all_data2, quanti.sup=c(2,5), quali.sup=c(1,3,4), graph=TRUE)

library(ggplot2)
library(factoextra)

fviz_screeplot(mca_patt)
fviz_mca_biplot(mca_patt)
fviz_mca_biplot(mca_patt) + theme_bw()
fviz_mca_var(mca_patt)
fviz_mca_var(mca_patt, col.var="black", shape.var = 15)
library("corrplot")
corrplot(mca_patt$var$contrib, is.corr = FALSE)
fviz_contrib(mca_patt, choice = "var", axes = 1)
fviz_mca_var(mca_patt, col.var = "contrib")
fviz_mca_var(mca_patt, col.var="contrib") + scale_color_gradient2(low="white", mid="blue", high="red", midpoint=2) + theme_bw()
fviz_mca_var(mca_patt, alpha.var="contrib") + theme_bw()
corrplot(mca_patt$var$cos2, is.corr=FALSE)
fviz_mca_ind(mca_patt)
fviz_mca_ind(mca_patt, col.ind="contrib") + scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.85) + theme_bw()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Principal component analysis for mixed data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(PCAmixdata)

pcamix_patt <- PCAmix(X.quanti=all_data2[,c('Edad','PAudit')],
                      X.quali=all_data2[,c('Genero', paste('Pregunta', 1:10, sep=''))],
                      rename.level=TRUE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Descriptive analysis
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

wilcox.test(PAudit~Genero, data=all_data)

boxplot(PAudit~Genero, data=all_data)
ggplot(data=all_data, aes(x=PAudit, y=..density.., colour=Genero)) + geom_density()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Correlation analysis
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Chi-square test
options(warn=-1)
p.chisq = matrix(0, nrow=ncol(all_data), ncol=ncol(all_data), byrow=T)
for(i in 1:ncol(all_data)){
  for(j in 1:ncol(all_data)){
    p.chisq[i,j] = round(chisq.test(all_data[,i], all_data[,j])$p.value, 3)
  }
}; rm(i); rm(j)
diag(p.chisq) = NA
colnames(p.chisq) = colnames(all_data)
rownames(p.chisq) = colnames(all_data)

# heatmap(p.chisq)
library(gplots)
color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
plot.new()
heatmap.2(p.chisq,
          main="Independence test",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=NULL,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(5,5))

# Heterogeneous correlation
library(polycor)
x <- hetcor(all_data, use='pairwise.complete.obs', ML=FALSE, std.err=FALSE)
x <- x$correlations
library(gplots)
library(corrplot)
cex.before <- par("cex")
par(cex = 0.4)
corrplot(x, method="ell", type="upper", tl.pos="no", cl.cex=1.1, tl.cex=1.1, order="FPC") # order="hclust"
corrplot(x, method="number", type="lower", col='black', tl.cex=1.1, add=TRUE, tl.pos="d", order="FPC")
par(cex = cex.before)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Multivariate analyisis
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(FactoMineR)
library(factoextra)
library(ggplot2)

all_data2 <- all_data
all_data2 <- as.data.frame(apply(X=all_data2, MARGIN=2, FUN=as.factor))

all_data_mca <- MCA(all_data2, graph=FALSE)

# Variability explained by each factor
fviz_screeplot(all_data_mca, addlabels=TRUE) + theme_bw()

# Graph of variables
fviz_mca_var(all_data_mca, col.var="steelblue") + theme_bw()

# Control variable colors using their contributions
# Use gradient color
fviz_mca_var(all_data_mca, axes=c(1, 2), col.var="contrib") + scale_color_gradient2(low="white", mid="blue", high="red", midpoint=96) + theme_bw()

# Variable contributions on axis 1
fviz_contrib(all_data_mca, choice="var", axes=1)

# Variable contributions on axes 1 + 2
fviz_contrib(all_data_mca, choice="var", axes=1:2)

# Graph of individuals
fviz_mca_ind(all_data_mca, geom="text")

# Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# Use gradient color
fviz_mca_ind(all_data_mca, col.ind="cos2", geom="point") +  scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0.6) + theme_bw()

# Biplot of individuals and variables
fviz_mca_biplot(all_data_mca, geom="point") + theme_bw()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Cluster analysis
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(cluster)
mydm <- daisy(all_data)
data_cluster <- agnes(mydm, method="weighted")
plot(data_cluster, which.plot=2, hang=-1)
rect.hclust(data_cluster, k=10, border='red')

test <- rect.hclust(data_cluster, k=5)

si1 <- silhouette(cutree(data_cluster, k=3), daisy(all_data))
plot(si1, nmax=120, cex.names=0.5)
abline(v=0.5, col=2)

summary(data_cluster)


sqr_edist <- function(x, y) {
  sum((x-y)^2)
}

wss.cluster <- function(clustermat) {
  c0 <- apply(clustermat, 2, FUN=mean)
  sum(apply(clustermat, 1, FUN=function(row){sqr_edist(row,c0)}))
}

wss.total <- function(dmatrix, labels) {
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels==i))
  wsstot
}

totss <- function(dmatrix) {
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}

totss <- function(dmatrix) {
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}

ch_criterion <- function(dmatrix, kmax, method="kmeans") {
  if(!(method %in% c("kmeans", "hclust"))) {
    stop("method must be one of c('kmeans', 'hclust')")
  }
  npts <- dim(dmatrix)[1] # number of rows.
  totss <- totss(dmatrix)
  wss <- numeric(kmax)
  crit <- numeric(kmax)
  wss[1] <- (npts-1)*sum(apply(dmatrix, 2, var))
  for(k in 2:kmax) {
    if(method=="kmeans") {
      clustering<-kmeans(dmatrix, k, nstart=10, iter.max=100)
      wss[k] <- clustering$tot.withinss
    }else { # hclust
      d <- dist(dmatrix, method="euclidean")
      pfit <- hclust(d, method="ward")
      labels <- cutree(pfit, k=k)
      wss[k] <- wss.total(dmatrix, labels)
    }
  }
  bss <- totss - wss
  crit.num <- bss/(0:(kmax-1))
  crit.denom <- wss/(npts - 1:kmax)
  list(crit = crit.num/crit.denom, wss = wss, totss = totss)
}

pmatrix <- as.matrix(mydm)

library(reshape2)
clustcrit <- ch_criterion(pmatrix, 10, method="hclust") # clustcrit <- ch_criterion(pmatrix, 10, method="hclust")
critframe <- data.frame(k=1:10, ch=scale(clustcrit$crit),
                        wss=scale(clustcrit$wss))
critframe <- melt(critframe, id.vars=c("k"),
                  variable.name="measure",
                  value.name="score")
library(ggplot2)
ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)

# Run models

dataSet <- read.csv2('C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_alcoholism_patterns/_data/Data_checked/plantilla audits RV ABRIL 5.csv')
str(dataSet)

write.csv(dataSet, 'C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_alcoholism_patterns/_data/Data_checked/verified_alcoholism_survey.csv', row.names=FALSE)

dataSet$PAudit <- as.numeric(as.character(dataSet$PAudit))

options(warn=-1)
if(!require(gtools)){install.packages('gtools');library(gtools)} else{library(gtools)}
if(!require(gridBase)){install.packages('gridBase');library(gridBase)} else{library(gridBase)}
if(!require(relaimpo)){install.packages('relaimpo');library(relaimpo)} else{library(relaimpo)}
if(!require(caret)){install.packages('caret');library(caret)} else{library(caret)}
if(!require(party)){install.packages('party');library(party)} else{library(party)}
if(!require(randomForest)){install.packages('randomForest');library(randomForest)} else{library(randomForest)}
if(!require(snowfall)){install.packages('snowfall');library(snowfall)} else{library(snowfall)}
if(!require(earth)){install.packages('earth');library(earth)} else{library(earth)}
if(!require(reshape)){install.packages('reshape');library(reshape)} else{library(reshape)}
if(!require(agricolae)){install.packages('agricolae');library(agricolae)} else{library(agricolae)}
if(!require(stringr)){install.packages('stringr');library(stringr)} else{library(stringr)}
if(!require(readxl)){install.packages('readxl');library(readxl)} else{library(readxl)}
if(!require(raster)){install.packages('raster');library(raster)} else{library(raster)}
if(!require(rgdal)){install.packages('rgdal');library(rgdal)} else{library(rgdal)}
if(!require(maptools)){install.packages('maptools');library(maptools)} else{library(maptools)}
if(!require(sp)){install.packages('sp');library(sp)} else{library(sp)}
if(!require(dismo)){install.packages('dismo');library(dismo)} else{library(dismo)}
if(!require(gbm)){install.packages('gbm');library(gbm)} else{library(gbm)}
if(!require(cowplot)){install.packages('cowplot');library(cowplot)} else{library(cowplot)}
if(!require(gridExtra)){install.packages('gridExtra');library(gridExtra)} else{library(gridExtra)}
if(!require(ggplot2)){install.packages('ggplot2');library(ggplot2)} else{library(ggplot2)}

dataSet <- dataSet[complete.cases(dataSet),]; rownames(dataSet) <- 1:nrow(dataSet)
dataSet <- dataSet[,-1]

# ----------------------------------------------------------------------------------------------------------------- #
# Select variables to analyse from database
# ----------------------------------------------------------------------------------------------------------------- #

dataSet <- data.frame(dataSet[,1:(ncol(dataSet)-1)],
                      splitVar=rep('All', nrow(dataSet)), # In case of exists variety variable doesn't run this line and use that variable like segmentation variable
                      PAudit=dataSet[,ncol(dataSet)])

inputs  <- 1:13  # inputs columns
segme   <- 14    # split column; In case of exists variety variable USE IT HERE
output  <- 15    # output column

namsDataSet <- names(dataSet)

# ----------------------------------------------------------------------------------------------------------------- #
# Creating the split factors (in case of exists more than 1 variety run models for each variety)
# ----------------------------------------------------------------------------------------------------------------- #

wkDir <- 'C:/Users/haachicanoy/Documents/GitHub/RAW_REGRESSION_MODELS_BIGDATA'; setwd(wkDir)

load('All-Functions-AEPS_BD.RData')
contVariety <- table(dataSet[,segme])
variety0    <- names(sort(contVariety[contVariety>=30]))
if(length(variety0)==1){variety = variety0 }else{variety = factor(c(variety0,"All"))}
variety <- 'All' # Omit this line in case of exists more than 1 variety

wkDir <- 'C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_alcoholism_patterns/_results'
runID <- paste(wkDir, '/fitModel', sep='')
if(!dir.exists(runID)){cat('Creating run directory\n'); dir.create(runID)} else {cat('Run directory exists\n')}
setwd(runID)

# ----------------------------------------------------------------------------------------------------------------- #
# Creating folders
# ----------------------------------------------------------------------------------------------------------------- #

dirFol <- getwd()
createFolders(dirFol, variety)

# ----------------------------------------------------------------------------------------------------------------- #
# Descriptive analysis
# ----------------------------------------------------------------------------------------------------------------- #

descriptiveGraphics(variety="All", dataSet=dataSet, inputs=inputs, segme=segme, output=output, smooth=TRUE,
                    ylabel="AUDIT score", smoothInd=NULL, ghrp="box", res=NA, sztxt=15, szlbls=15,
                    colbox="skyblue", colpts="greenyellow", colsmot="red", szpts=4, szdts=1.5)

# ----------------------------------------------------------------------------------------------------------------- #
# DataSets ProcesosF; parallelize processes usign caret R package
# ----------------------------------------------------------------------------------------------------------------- #

dataSetProces(variety, dataSet, segme, corRed="caret")























# Análisis con datos revisados

library(readxl)

all_data <- read_excel(path='C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_alcoholism_patterns/_data/Data_checked/verified_alcoholism_survey.xlsx', sheet='verified_survey_r', na='NA')

all_data <- all_data[all_data$Consecutivo!=294,]
rownames(all_data) <- 1:nrow(all_data)

all_data$Pregunta_1 <- factor(all_data$Pregunta_1, levels=c('Nunca', 'Una o menos veces al mes', 'De 2 a 4 veces al mes', 'De 2 a 3 veces a la semana', '4 o más veces a la semana'), ordered=TRUE)
all_data$Pregunta_2 <- factor(all_data$Pregunta_2, levels=c('1 o 2', '3 o 4', '5 o 6', 'De 7 a 9', '10 o más'), ordered=TRUE)
all_data$Pregunta_3 <- factor(all_data$Pregunta_3, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta_4 <- factor(all_data$Pregunta_4, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta_5 <- factor(all_data$Pregunta_5, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta_6 <- factor(all_data$Pregunta_6, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta_7 <- factor(all_data$Pregunta_7, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta_8 <- factor(all_data$Pregunta_8, levels=c('Nunca', 'Menos de una vez al mes', 'Mensualmente', 'Semanalmente', 'A diario o casi a diario'), ordered=TRUE)
all_data$Pregunta_9 <- factor(all_data$Pregunta_9, levels=c('No', 'Sí, pero no en el curso del último año', 'Sí, el último año'), ordered=FALSE)
all_data$Pregunta_10 <- factor(all_data$Pregunta_10, levels=c('No', 'Sí, pero no en el curso del último año', 'Sí, el último año'), ordered=FALSE)

# Chi-square test

options(warn=-1)
p.chisq = matrix(0, nrow=ncol(all_data[,paste('Pregunta_', 1:10, sep='')]), ncol=ncol(all_data[,paste('Pregunta_', 1:10, sep='')]), byrow=T)
for(i in 1:ncol(all_data[,paste('Pregunta_', 1:10, sep='')])){
  for(j in 1:ncol(all_data[,paste('Pregunta_', 1:10, sep='')])){
    p.chisq[i,j] = round(chisq.test(all_data[,paste('Pregunta_', 1:10, sep='')][,i], all_data[,paste('Pregunta_', 1:10, sep='')][,j])$p.value, 3)
  }
}; rm(i); rm(j)
diag(p.chisq) = NA
colnames(p.chisq) = colnames(all_data[,paste('Pregunta_', 1:10, sep='')])
rownames(p.chisq) = colnames(all_data[,paste('Pregunta_', 1:10, sep='')])
View(p.chisq)

# Descriptive statistics

hist(all_data$Edad)
hist(all_data$PAudit)
boxplot(all_data$PAudit ~ all_data$Pregunta_10)
plot(all_data$Edad, all_data$PAudit)

boxplot(all_data$Edad ~ all_data$Pregunta_10)

# Estadisticas descriptivas edad
# summary(all_data$Edad)
# Estadísticas descriptivas Puntaje AUDIT
# summary(all_data$PAudit)

# Estadísticas descriptivas del puntaje AUDIT por género
# summary(all_data$PAudit[all_data$Genero=='Hombre'])
# summary(all_data$PAudit[all_data$Genero=='Mujer'])

# Estadísticas descriptivas del puntaje AUDIT por género en el grupo de menores de edad
# summary(all_data$PAudit[all_data$Genero=='Hombre' & all_data$Edad<=17])
# summary(all_data$PAudit[all_data$Genero=='Mujer' & all_data$Edad<=17])

# Porcentaje de estudiantes por carrera
# round(sort(table(all_data$Carrera)/nrow(all_data) * 100, decreasing=TRUE), 2)

# Porcentaje de menores de edad en la muestra
sum(as.numeric(na.omit(all_data$Edad < 18)))/length(as.numeric(na.omit(all_data$Edad)))
# Porcentaje de hombres menores de edad
sum(as.numeric(na.omit(all_data$Edad < 18 & all_data$Genero=='Hombre')))/sum(as.numeric(na.omit(all_data$Edad < 18)))
# Porcentaje de mujeres menores de edad
sum(as.numeric(na.omit(all_data$Edad < 18 & all_data$Genero=='Mujer')))/sum(as.numeric(na.omit(all_data$Edad < 18)))

# Porcentaje de estudiantes con puntaje AUDIT de bajo riesgo
sum(as.numeric(na.omit(all_data$PAudit <= 7)))/length(as.numeric(na.omit(all_data$PAudit)))
# Porcentaje de estudiantes con puntaje AUDIT de alto riesgo
sum(as.numeric(na.omit(all_data$PAudit >= 8)))/length(as.numeric(na.omit(all_data$PAudit)))

# Porcentaje de hombres con puntaje AUDIT >= 8
sum(as.numeric(na.omit(all_data$Genero == 'Hombre' & all_data$PAudit >=8)))/sum(as.numeric(na.omit(all_data$Genero == 'Hombre')))

# Porcentaje de mujeres con puntaje AUDIT >= 7
sum(as.numeric(na.omit(all_data$Genero == 'Mujer' & all_data$PAudit >=8)))/sum(as.numeric(na.omit(all_data$Genero == 'Mujer')))

# Porcentaje de hombres menores de edad con puntaje AUDIT >= 8
sum(as.numeric(na.omit(all_data$Edad < 18 & all_data$Genero == 'Hombre' & all_data$PAudit >=8)))/sum(as.numeric(na.omit(all_data$Edad < 18 & all_data$Genero == 'Hombre')))
# Porcentaje de mujeres menores de edad con puntaje AUDIT >= 7
sum(as.numeric(na.omit(all_data$Edad < 18 & all_data$Genero == 'Mujer' & all_data$PAudit >=7)))/sum(as.numeric(na.omit(all_data$Edad < 18 & all_data$Genero == 'Mujer')))

# Problema pasado y presente
table(all_data$Edad<18, all_data$Pregunta_9)
table(all_data$Edad<18, all_data$Pregunta_10)

# Carreras vs Puntajes AUDIT altos
# table(all_data$Carrera, all_data$PAudit>=7)
table(all_data$Carrera, all_data$PAudit>=8 & all_data$Genero=='Hombre')
table(all_data$Carrera, all_data$PAudit>=7 & all_data$Genero=='Mujer')

# Pregunta 9
table(all_data$Pregunta_9=='Sí, pero no en el curso del último año', all_data$Genero=='Hombre' & all_data$Edad<18)
table(all_data$Pregunta_9=='Sí, pero no en el curso del último año', all_data$Genero=='Mujer' & all_data$Edad<18)

table(all_data$Pregunta_9=='Sí, el último año', all_data$Genero=='Hombre' & all_data$Edad<18)
table(all_data$Pregunta_9=='Sí, el último año', all_data$Genero=='Mujer' & all_data$Edad<18)

# Pregunta 10
table(all_data$Pregunta_10=='Sí, pero no en el curso del último año', all_data$Genero=='Hombre' & all_data$Edad<18)
table(all_data$Pregunta_10=='Sí, pero no en el curso del último año', all_data$Genero=='Mujer' & all_data$Edad<18)

table(all_data$Pregunta_10=='Sí, el último año', all_data$Genero=='Hombre' & all_data$Edad<18)
table(all_data$Pregunta_10=='Sí, el último año', all_data$Genero=='Mujer' & all_data$Edad<18)

# Consumo de riesgo de alcohol
round(table(all_data$Pregunta_1)/sum(table(all_data$Pregunta_1)),3) * 100
round(table(all_data$Pregunta_2)/sum(table(all_data$Pregunta_2)),3) * 100
round(table(all_data$Pregunta_3)/sum(table(all_data$Pregunta_3)),3) * 100

# Dependencia
round(table(all_data$Pregunta_4)/sum(table(all_data$Pregunta_4)),3) * 100
round(table(all_data$Pregunta_5)/sum(table(all_data$Pregunta_5)),3) * 100
round(table(all_data$Pregunta_6)/sum(table(all_data$Pregunta_6)),3) * 100

# Consumo perjudicial de alcohol
round(table(all_data$Pregunta_7)/sum(table(all_data$Pregunta_7)),3) * 100
round(table(all_data$Pregunta_8)/sum(table(all_data$Pregunta_8)),3) * 100
round(table(all_data$Pregunta_9)/sum(table(all_data$Pregunta_9)),3) * 100
round(table(all_data$Pregunta_10)/sum(table(all_data$Pregunta_10)),3) * 100
