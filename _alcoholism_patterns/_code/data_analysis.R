
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

library(FactoMineR)

all_data2 <- all_data[,c("Genero", "Edad", "Carrera", "Facultad", "PAudit",
                         "Pregunta1", "Pregunta2", "Pregunta3", "Pregunta4", "Pregunta5", "Pregunta6", "Pregunta7", "Pregunta8", "Pregunta9", "Pregunta10")]
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





