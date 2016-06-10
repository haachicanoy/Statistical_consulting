# Estudio sobre inclusión laboral en población con discapacidad
# H. Achicanoy
# 2016

library(readxl)
inDir <- 'C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_data/_modified_version'
all_data <- read_excel(paste(inDir, '/all_data.xlsx', sep=''), sheet=1)

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
all_data$Educación_formal <- factor(all_data$Educacion_formal, levels=c('Ninguno','Primaria','Bachillerato','Técnico','Tecnología','Profesional'), ordered=TRUE)
gg <- ggplot(all_data, aes(Educación_formal, fill=Grupo)) + geom_bar() + theme_bw()
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

ggplot(all_data, aes(Educación_formal)) + geom_bar() + facet_wrap(~ Grupo) + theme_bw()

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

