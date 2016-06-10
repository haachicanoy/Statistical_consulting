# Estudio sobre inclusión laboral en población con discapacidad
# H. Achicanoy
# 2016

library(readxl)
inDir <- 'C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_data/_modified_version'
all_data <- read_excel(paste(inDir, '/all_data_final_coded.xlsx', sep=''), sheet=1)

# ANALISIS DESCRIPTIVO

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


# ANALISIS DE CORRELACION

all_data$Ingreso_hogar <- factor(all_data$Ingreso_hogar, levels=c('Menos de un salario mínimo','Un salario mínimo','Entre 2 y 3 salarios mínimos','Entre 4 y 5 salarios mínimos','Más de 5 salarios mínimos'), ordered=TRUE)
all_data$Nivel_discapacidad <- factor(all_data$Nivel_discapacidad, levels=c('Ninguno','Leve','Moderado','Severo','Profundo'), ordered=TRUE)

### Factores ambientales
mtch_fa <- grep(pattern='^FA', colnames(all_data))
for(i in 1:length(mtch_fa)){
  all_data[,mtch_fa[i]] <- factor(all_data[,mtch_fa[i]], levels=c('No aplica',
                                                                  'No hay barrera','Barrera ligera','Barrera moderada','Barrera grave','Barrera completa',
                                                                  'No hay facilitador','Facilitador ligero','Facilitador moderado','Facilitador grave','Facilitador completo'), ordered=TRUE)
}; rm(i, mtch_fa)

### Apoyo y servicios
mtch_ap <- grep(pattern='^APS', colnames(all_data))
for(i in 1:length(mtch_ap)){
  all_data[,mtch_ap[i]] <- factor(all_data[,mtch_ap[i]], levels=c('No','Si, ninguno','Si, alguno pero no suficiente','Si, suficiente'), ordered=TRUE)
}; rm(i, mtch_ap)

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
}; rm(i, mtch_cv_imp)

mtch_cv_sat <- mtch_cv[odd_numbers]
# Satisfaccion
for(i in 1:length(mtch_cv_sat)){
  all_data[,mtch_cv_sat[i]] <- factor(all_data[,mtch_cv_sat[i]], levels=c('Muy insatisfecho','Insatisfecho','Neutral','Satisfecho','Muy satisfecho'), ordered=TRUE)
}; rm(i, mtch_cv_sat)

# str(all_data)

# Test Chi-cuadrado de independencia

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


# CUANTIFICACION OPTIMA

res <- homals(galo, active = c(rep(TRUE, 4), FALSE), sets = list(c(1,3,4),2,5))
res$low.rank # Quantified variables



all_data <- all_data[complete.cases(all_data),]

library(plspm)

ambiente          <- c(0,0,0,0)
apoyos_servicios  <- c(0,0,0,0)
calidad_vida      <- c(0,0,0,0)
inclusion_laboral <- c(1,1,1,0)
ot_path           <- rbind(ambiente, apoyos_servicios, calidad_vida, inclusion_laboral)
colnames(ot_path) <- rownames(ot_path)
rm(ambiente, apoyos_servicios, calidad_vida, inclusion_laboral)

innerplot(ot_path)

rus_blocks = list(
  c("Pregunta_1", "Pregunta_2", "Pregunta_3"),
  c("Pregunta_4", "Pregunta_5", "Pregunta_6", "Pregunta_7"),
  c("Pregunta_8", "Pregunta_9", "Pregunta_10"),
  c("PAudit"))

rus_modes = rep("A", 4)

# scaling
rus_scaling3 <- list(rep('ordinal',3),
                     rep('ordinal',4),
                     c('ordinal', 'nominal', 'nominal'),
                     c('numeric'))

alch_pls = plspm(all_data, alch_path, rus_blocks, modes=rus_modes, scaling=rus_scaling3, scheme="CENTROID", plscomp=c(1,1,1,1), tol=0.0000001)

# load plspm
library(plspm)
# load offense dataset
data(offense)
# let's take a peek
head(offense)

# path matrix
n1 = c(0, 0, 0, 0, 0)
n2 = c(0, 0, 0, 0, 0)
n3 = c(0, 0, 0, 0, 0)
n4 = c(0, 1, 1, 0, 0)
n5 = c(1, 0, 0, 1, 0)
nfl_path = rbind(n1, n2, n3, n4, n5)

