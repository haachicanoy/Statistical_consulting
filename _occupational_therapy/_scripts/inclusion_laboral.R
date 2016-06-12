# Estudio sobre inclusión laboral en población con discapacidad
# H. Achicanoy
# 2016

library(readxl)
inDir <- 'C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_data/_modified_version'
all_data <- read_excel(paste(inDir, '/all_data_final_coded.xlsx', sep=''), sheet=1)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# ANALISIS DESCRIPTIVO
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(ggplot2)
# Boxplot Sexo vs Edad
gg <- ggplot(all_data, aes(x=Sexo, y=Edad, fill=Sexo)) + geom_boxplot()
gg <- gg + theme_bw() + ylab('Edad (años)') + xlab('Género')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + theme(legend.position="none")
ggsave(filename='C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/bxpl_sexo_edad.pdf', plot=gg, width=7, height=7, units='in')

# Histograma Edad
mean(all_data$Edad)
sd(all_data$Edad)

pdf('C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/hist_edad.pdf', height=7, width=7)
par(mar=c(5, 4, 2, 2) + 0.1)
hist(all_data$Edad, xlab='Edad (años)', ylab='Densidad', prob=TRUE, main='', col='forestgreen')
box()
dev.off()

# Grafico de barras educacion vs condicion de discapacidad
all_data$Educacion_formal <- factor(all_data$Educacion_formal, levels=c('Ninguno','Primaria','Bachillerato','Técnico','Tecnología','Profesional'), ordered=TRUE)
gg <- ggplot(all_data, aes(Educacion_formal, fill=Grupo)) + geom_bar() + theme_bw()
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_fill_discrete(name="Grupo estudiado",
                               breaks=c("Discapacidad_cognitiva", "Discapacidad_sensorial_auditiva", "Sin_discapacidad"),
                               labels=c("Discapacidad cognitiva", "Discapacidad sensorial/auditiva", "Sin discapacidad"))
gg <- gg + xlab('Grado más alto de educación formal alcanzado') + ylab('Número de individuos')
ggsave(filename='C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/barplot_eduGrupo.pdf', plot=gg, width=9, height=7, units='in')

ggplot(all_data, aes(Educacion_formal)) + geom_bar() + facet_wrap(~ Grupo) + theme_bw()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# ANALISIS DE CORRELACION
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

all_data$Educacion_formal <- factor(all_data$Educacion_formal, levels=c('Ninguno','Primaria','Bachillerato','Técnico','Tecnología','Profesional'), ordered=TRUE)
all_data$Ingreso_hogar <- factor(all_data$Ingreso_hogar, levels=c('Menos de un salario mínimo','Un salario mínimo','Entre 2 y 3 salarios mínimos','Entre 4 y 5 salarios mínimos','Más de 5 salarios mínimos'), ordered=TRUE)
all_data$Nivel_discapacidad <- factor(all_data$Nivel_discapacidad, levels=c('Ninguno','Leve','Moderado','Severo','Profundo'), ordered=TRUE)

### Factores ambientales
mtch_fa <- grep(pattern='^FA', colnames(all_data))
for(i in 1:length(mtch_fa)){
  all_data[,mtch_fa[i]] <- factor(all_data[,mtch_fa[i]], levels=c('No aplica',
                                                                  'No hay barrera','Barrera ligera','Barrera moderada','Barrera grave','Barrera completa',
                                                                  'No hay facilitador','Facilitador ligero','Facilitador moderado','Facilitador grave','Facilitador completo'), ordered=TRUE)
}; rm(i)

### Apoyo y servicios
mtch_ap <- grep(pattern='^APS', colnames(all_data))
for(i in 1:length(mtch_ap)){
  all_data[,mtch_ap[i]] <- factor(all_data[,mtch_ap[i]], levels=c('No','Si, ninguno','Si, alguno pero no suficiente','Si, suficiente'), ordered=TRUE)
}; rm(i)

### Calidad de vida
mtch_cv <- grep(pattern='^CV', colnames(all_data))
odd_numbers <- unlist(lapply(1:length(mtch_cv), function(i){
  sst <- strsplit(colnames(all_data)[mtch_cv][i], "")[[1]]
  out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])
  out <- as.numeric(out[3])
  is.even <- function(x) x %% 2 == 0
  out <- is.even(out)
  return(out)
}))

# Importancia
mtch_cv_imp <- mtch_cv[!odd_numbers]
for(i in 1:length(mtch_cv_imp)){
  all_data[,mtch_cv_imp[i]] <- factor(all_data[,mtch_cv_imp[i]], levels=c('Poco importante','Algo importante','Medianamente importante','Muy importante','Crucialmente importante'), ordered=TRUE)
}; rm(i)

mtch_cv_sat <- mtch_cv[odd_numbers]
# Satisfaccion
for(i in 1:length(mtch_cv_sat)){
  all_data[,mtch_cv_sat[i]] <- factor(all_data[,mtch_cv_sat[i]], levels=c('Muy insatisfecho','Insatisfecho','Neutral','Satisfecho','Muy satisfecho'), ordered=TRUE)
}; rm(i)

# str(all_data)

### Test Chi-cuadrado de independencia

# Chi-square test for original data
options(warn=-1)
p.chisq = matrix(0, nrow=ncol(all_data), ncol=ncol(all_data), byrow=T)
for(i in 1:ncol(all_data)){
  for(j in 1:ncol(all_data)){
    p.chisq[i,j] = round(chisq.test(all_data[,i],all_data[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq) = NA
colnames(p.chisq) = colnames(all_data)
rownames(p.chisq) = colnames(all_data)

library(gplots)
color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
pdf('C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/chi_test.pdf', height=7, width=7)
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
dev.off()

sum(as.numeric(p.chisq[upper.tri(p.chisq)])<0.05)/length(as.numeric(p.chisq[upper.tri(p.chisq)]))

write.csv(p.chisq,"C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/chisq_matrix.csv", row.names=T)
rm(p.chisq, color_scale)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# FRECUENCIAS: APOYO Y SERVICIOS
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Apoyo y servicios
apser <- all_data[,mtch_ap]
apser <- lapply(1:ncol(apser), function(i){
  z <- as.data.frame(t(as.numeric(table(apser[,i]))))
  rownames(z) <- colnames(apser)[i]
  colnames(z) <- levels(apser[,1])
  return(z)
})
apser <- do.call(rbind, apser)
apser$Variable <- rownames(apser)

library(dplyr)
library(tidyr)

apser <- apser %>% gather(Level, Count, -Variable)
apser$Level <- factor(apser$Level, levels=c('No','Si, ninguno','Si, alguno pero no suficiente','Si, suficiente'), ordered=TRUE)

library(ggplot2)
library(viridis)
library(ggthemes)
gg <- ggplot(apser, aes(x=Variable, y=Level, fill=Count))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="# Casos")
gg <- gg + coord_equal()
gg <- gg + labs(x=NULL, y=NULL, title="Apoyo y servicios")
# gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=5))
gg <- gg + theme(axis.text.y=element_text(size=7))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=7))
ggsave(filename='C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/freqApoyoServicios.pdf', plot=gg, width=10, height=5, units='in')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# FRECUENCIAS: FACTORES AMBIENTALES
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

fambt <- all_data[,mtch_fa]
fambt <- lapply(1:ncol(fambt), function(i){
  z <- as.data.frame(t(as.numeric(table(fambt[,i]))))
  rownames(z) <- colnames(fambt)[i]
  colnames(z) <- levels(fambt[,1])
  return(z)
})
fambt <- do.call(rbind, fambt)
fambt$Variable <- rownames(fambt)

library(dplyr)
library(tidyr)

fambt <- fambt %>% gather(Level, Count, -Variable)
fambt$Level <- factor(fambt$Level, levels=c('No aplica',
                                            'No hay barrera','Barrera ligera','Barrera moderada','Barrera grave','Barrera completa',
                                            'No hay facilitador','Facilitador ligero','Facilitador moderado','Facilitador grave','Facilitador completo'), ordered=TRUE)

library(ggplot2)
library(viridis)
library(ggthemes)
gg <- ggplot(fambt, aes(x=Variable, y=Level, fill=Count))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="# Casos")
gg <- gg + coord_equal()
gg <- gg + labs(x=NULL, y=NULL, title="Factores ambientales")
# gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=5))
gg <- gg + theme(axis.text.y=element_text(size=7))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=7))
ggsave(filename='C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/freqFactoresAmbientales.pdf', plot=gg, width=10, height=5, units='in')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# FRECUENCIAS: CALIDAD DE VIDA importancia
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

cvimp <- all_data[,mtch_cv_imp]
cvimp <- lapply(1:ncol(cvimp), function(i){
  z <- as.data.frame(t(as.numeric(table(cvimp[,i]))))
  rownames(z) <- colnames(cvimp)[i]
  colnames(z) <- levels(cvimp[,1])
  return(z)
})
cvimp <- do.call(rbind, cvimp)
cvimp$Variable <- rownames(cvimp)

library(dplyr)
library(tidyr)

cvimp <- cvimp %>% gather(Level, Count, -Variable)
cvimp$Level <- factor(cvimp$Level, levels=c('Poco importante','Algo importante','Medianamente importante','Muy importante','Crucialmente importante'), ordered=TRUE)

library(ggplot2)
library(viridis)
library(ggthemes)
gg <- ggplot(cvimp, aes(x=Variable, y=Level, fill=Count))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="# Casos")
gg <- gg + coord_equal()
gg <- gg + labs(x=NULL, y=NULL, title="Calidad de vida - importancia")
# gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=5))
gg <- gg + theme(axis.text.y=element_text(size=7))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=7))
ggsave(filename='C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/freqCalidadVidaImportancia.pdf', plot=gg, width=10, height=5, units='in')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# FRECUENCIAS: CALIDAD DE VIDA satisfacción
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

cvsat <- all_data[,mtch_cv_sat]
cvsat <- lapply(1:ncol(cvsat), function(i){
  z <- as.data.frame(t(as.numeric(table(cvsat[,i]))))
  rownames(z) <- colnames(cvsat)[i]
  colnames(z) <- levels(cvsat[,1])
  return(z)
})
cvsat <- do.call(rbind, cvsat)
cvsat$Variable <- rownames(cvsat)

library(dplyr)
library(tidyr)

cvsat <- cvsat %>% gather(Level, Count, -Variable)
cvsat$Level <- factor(cvsat$Level, levels=c('Muy insatisfecho','Insatisfecho','Neutral','Satisfecho','Muy satisfecho'), ordered=TRUE)

library(ggplot2)
library(viridis)
library(ggthemes)
gg <- ggplot(cvsat, aes(x=Variable, y=Level, fill=Count))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="# Casos")
gg <- gg + coord_equal()
gg <- gg + labs(x=NULL, y=NULL, title="Calidad de vida - satisfacción")
# gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=5))
gg <- gg + theme(axis.text.y=element_text(size=7))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=7))
ggsave(filename='C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/freqCalidadVidaSatisfaccion.pdf', plot=gg, width=10, height=5, units='in')
