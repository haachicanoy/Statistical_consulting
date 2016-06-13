# Estudio sobre inclusión laboral en población con discapacidad
# H. Achicanoy
# 2016

library(readxl)
inDir <- 'C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_data/_modified_version'
all_data <- read_excel(paste(inDir, '/all_data_final_coded.xlsx', sep=''), sheet=1)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# DEFINICIÓN DE ESCALAS DE MEDICIÓN
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

all_data$Educacion_formal <- factor(all_data$Educacion_formal, levels=c('Ninguno','Primaria','Bachillerato','Técnico','Tecnología','Profesional'), ordered=TRUE)
all_data$Ingreso_hogar <- factor(all_data$Ingreso_hogar, levels=c('Menos de un salario mínimo','Un salario mínimo','Entre 2 y 3 salarios mínimos','Entre 4 y 5 salarios mínimos','Más de 5 salarios mínimos'), ordered=TRUE)
all_data$Nivel_discapacidad <- factor(all_data$Nivel_discapacidad, levels=c('Ninguno','Leve','Moderado','Severo','Profundo'), ordered=TRUE)

### Factores ambientales
mtch_fa <- grep(pattern='^FA', colnames(all_data))
for(i in 1:length(mtch_fa)){
  mtch <- which(all_data[,mtch_fa[i]]=='No aplica')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='No aplica')] <- paste('00_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='No aplica')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='Barrera completa')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Barrera completa')] <- paste('01_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Barrera completa')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='No hay facilitador')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='No hay facilitador')] <- paste('02_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='No hay facilitador')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='Barrera grave')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Barrera grave')] <- paste('03_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Barrera grave')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='Facilitador ligero')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Facilitador ligero')] <- paste('04_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Facilitador ligero')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='Barrera moderada')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Barrera moderada')] <- paste('05_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Barrera moderada')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='Facilitador moderado')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Facilitador moderado')] <- paste('06_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Facilitador moderado')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='Barrera ligera')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Barrera ligera')] <- paste('07_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Barrera ligera')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='Facilitador grave')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Facilitador grave')] <- paste('08_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Facilitador grave')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='No hay barrera')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='No hay barrera')] <- paste('09_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='No hay barrera')], sep='')
  }
  mtch <- which(all_data[,mtch_fa[i]]=='Facilitador completo')
  if(length(mtch)>0){
    all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Facilitador completo')] <- paste('10_', all_data[,mtch_fa[i]][which(all_data[,mtch_fa[i]]=='Facilitador completo')], sep='')
  }
  
  all_data[,mtch_fa[i]] <- factor(all_data[,mtch_fa[i]], levels=c('00_No aplica','01_Barrera completa','02_No hay facilitador','03_Barrera grave','04_Facilitador ligero','05_Barrera moderada',
                                                                  '06_Facilitador moderado','07_Barrera ligera','08_Facilitador grave','09_No hay barrera','10_Facilitador completo'), ordered=TRUE)
}; rm(i,mtch)

### Apoyo y servicios
mtch_ap <- grep(pattern='^APS', colnames(all_data))
for(i in 1:length(mtch_ap)){
  mtch <- which(all_data[,mtch_ap[i]]=='No')
  if(length(mtch)>0){
    all_data[,mtch_ap[i]][which(all_data[,mtch_ap[i]]=='No')] <- paste('00_', all_data[,mtch_ap[i]][which(all_data[,mtch_ap[i]]=='No')], sep='')
  }
  mtch <- which(all_data[,mtch_ap[i]]=='Si, ninguno')
  if(length(mtch)>0){
    all_data[,mtch_ap[i]][which(all_data[,mtch_ap[i]]=='Si, ninguno')] <- paste('01_', all_data[,mtch_ap[i]][which(all_data[,mtch_ap[i]]=='Si, ninguno')], sep='')
  }
  mtch <- which(all_data[,mtch_ap[i]]=='Si, alguno pero no suficiente')
  if(length(mtch)>0){
    all_data[,mtch_ap[i]][which(all_data[,mtch_ap[i]]=='Si, alguno pero no suficiente')] <- paste('02_', all_data[,mtch_ap[i]][which(all_data[,mtch_ap[i]]=='Si, alguno pero no suficiente')], sep='')
  }
  mtch <- which(all_data[,mtch_ap[i]]=='Si, suficiente')
  if(length(mtch)>0){
    all_data[,mtch_ap[i]][which(all_data[,mtch_ap[i]]=='Si, suficiente')] <- paste('03_', all_data[,mtch_ap[i]][which(all_data[,mtch_ap[i]]=='Si, suficiente')], sep='')
  }
  all_data[,mtch_ap[i]] <- factor(all_data[,mtch_ap[i]], levels=c('00_No','01_Si, ninguno','02_Si, alguno pero no suficiente','03_Si, suficiente'), ordered=TRUE)
}; rm(i, mtch)

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
  mtch <- which(all_data[,mtch_cv_imp[i]]=='Poco importante')
  if(length(mtch)>0){
    all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Poco importante')] <- paste('00_', all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Poco importante')], sep='')
  }
  mtch <- which(all_data[,mtch_cv_imp[i]]=='Algo importante')
  if(length(mtch)>0){
    all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Algo importante')] <- paste('01_', all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Algo importante')], sep='')
  }
  mtch <- which(all_data[,mtch_cv_imp[i]]=='Medianamente importante')
  if(length(mtch)>0){
    all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Medianamente importante')] <- paste('02_', all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Medianamente importante')], sep='')
  }
  mtch <- which(all_data[,mtch_cv_imp[i]]=='Muy importante')
  if(length(mtch)>0){
    all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Muy importante')] <- paste('03_', all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Muy importante')], sep='')
  }
  mtch <- which(all_data[,mtch_cv_imp[i]]=='Crucialmente importante')
  if(length(mtch)>0){
    all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Crucialmente importante')] <- paste('04_', all_data[,mtch_cv_imp[i]][which(all_data[,mtch_cv_imp[i]]=='Crucialmente importante')], sep='')
  }
  all_data[,mtch_cv_imp[i]] <- factor(all_data[,mtch_cv_imp[i]], levels=c('00_Poco importante','01_Algo importante','02_Medianamente importante','03_Muy importante','04_Crucialmente importante'), ordered=TRUE)
}; rm(i,mtch)

# Satisfaccion
mtch_cv_sat <- mtch_cv[odd_numbers]
for(i in 1:length(mtch_cv_sat)){
  mtch <- which(all_data[,mtch_cv_sat[i]]=='Muy insatisfecho')
  if(length(mtch)>0){
    all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Muy insatisfecho')] <- paste('00_', all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Muy insatisfecho')], sep='')
  }
  mtch <- which(all_data[,mtch_cv_sat[i]]=='Insatisfecho')
  if(length(mtch)>0){
    all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Insatisfecho')] <- paste('01_', all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Insatisfecho')], sep='')
  }
  mtch <- which(all_data[,mtch_cv_sat[i]]=='Neutral')
  if(length(mtch)>0){
    all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Neutral')] <- paste('02_', all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Neutral')], sep='')
  }
  mtch <- which(all_data[,mtch_cv_sat[i]]=='Satisfecho')
  if(length(mtch)>0){
    all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Satisfecho')] <- paste('03_', all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Satisfecho')], sep='')
  }
  mtch <- which(all_data[,mtch_cv_sat[i]]=='Muy satisfecho')
  if(length(mtch)>0){
    all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Muy satisfecho')] <- paste('04_', all_data[,mtch_cv_sat[i]][which(all_data[,mtch_cv_sat[i]]=='Muy satisfecho')], sep='')
  }
  all_data[,mtch_cv_sat[i]] <- factor(all_data[,mtch_cv_sat[i]], levels=c('00_Muy insatisfecho','01_Insatisfecho','02_Neutral','03_Satisfecho','04_Muy satisfecho'), ordered=TRUE)
}; rm(i, mtch)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# CUANTIFICACIÓN ÓPTIMA
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(homals)

###############################################
# Factores ambientales
###############################################
faData   <- all_data[,mtch_fa]
faHomals <- homals(faData, active=rep(TRUE, ncol(faData)), level='ordinal', sets=list(1:ncol(faData)))
faQuan   <- faHomals$low.rank # Quantified variables
faDataqF <- faData

for(i in 1:ncol(faDataqF)){
  
  faDataqF[,i] <- as.character(faDataqF[,i])
  dfQuan <- as.data.frame(faQuan[[i]])
  
  for(j in 1:length(rownames(dfQuan))){
    mtch <- which(faDataqF[,i]==rownames(dfQuan)[j])
    if(length(mtch)>0){
      faDataqF[,i][which(faDataqF[,i]==rownames(dfQuan)[j])] <- dfQuan[j,1]
    }
  }
  
  faDataqF[,i] <- as.numeric(faDataqF[,i])
  
}; rm(i,j,mtch)
rm(faData, faHomals, dfQuan, faQuan)

###############################################
# Apoyo y servicios
###############################################
asData   <- all_data[,mtch_ap]
asHomals <- homals(asData, active=rep(TRUE, ncol(asData)), level='ordinal', sets=list(1:ncol(asData)))
asQuan   <- asHomals$low.rank # Quantified variables
asDataqF <- asData

for(i in 1:ncol(asDataqF)){
  
  asDataqF[,i] <- as.character(asDataqF[,i])
  dfQuan <- as.data.frame(asQuan[[i]])
  
  for(j in 1:length(rownames(dfQuan))){
    mtch <- which(asDataqF[,i]==rownames(dfQuan)[j])
    if(length(mtch)>0){
      asDataqF[,i][which(asDataqF[,i]==rownames(dfQuan)[j])] <- dfQuan[j,1]
    }
  }
  
  asDataqF[,i] <- as.numeric(asDataqF[,i])
  
}; rm(i,j,mtch)
rm(asData, asHomals, dfQuan, asQuan)

###############################################
# Calidad de vida - importancia
###############################################
cvimpData   <- all_data[,mtch_cv_imp]
cvimpHomals <- homals(cvimpData, active=rep(TRUE, ncol(cvimpData)), level='ordinal', sets=list(1:ncol(cvimpData)))
cvimpQuan   <- cvimpHomals$low.rank # Quantified variables
cvimpDataqF <- cvimpData

for(i in 1:ncol(cvimpDataqF)){
  
  cvimpDataqF[,i] <- as.character(cvimpDataqF[,i])
  dfQuan <- as.data.frame(cvimpQuan[[i]])
  
  for(j in 1:length(rownames(dfQuan))){
    mtch <- which(cvimpDataqF[,i]==rownames(dfQuan)[j])
    if(length(mtch)>0){
      cvimpDataqF[,i][which(cvimpDataqF[,i]==rownames(dfQuan)[j])] <- dfQuan[j,1]
    }
  }
  
  cvimpDataqF[,i] <- as.numeric(cvimpDataqF[,i])
  
}; rm(i,j,mtch)
rm(cvimpData, cvimpHomals, dfQuan, cvimpQuan)


###############################################
# Calidad de vida - satisfacción
###############################################
cvsatData   <- all_data[,mtch_cv_sat]
cvsatHomals <- homals(cvsatData, active=rep(TRUE, ncol(cvsatData)), level='ordinal', sets=list(1:ncol(cvsatData)))
cvsatQuan   <- cvsatHomals$low.rank # Quantified variables
cvsatDataqF <- cvsatData

for(i in 1:ncol(cvsatDataqF)){
  
  cvsatDataqF[,i] <- as.character(cvsatDataqF[,i])
  dfQuan <- as.data.frame(cvsatQuan[[i]])
  
  for(j in 1:length(rownames(dfQuan))){
    mtch <- which(cvsatDataqF[,i]==rownames(dfQuan)[j])
    if(length(mtch)>0){
      cvsatDataqF[,i][which(cvsatDataqF[,i]==rownames(dfQuan)[j])] <- dfQuan[j,1]
    }
  }
  
  cvsatDataqF[,i] <- as.numeric(cvsatDataqF[,i])
  
}; rm(i,j,mtch)
rm(cvsatData, cvsatHomals, dfQuan, cvsatQuan)

qFvar <- cbind(faDataqF, asDataqF, cvimpDataqF, cvsatDataqF)
write.csv(qFvar, 'C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/quantifiedVariables.csv', row.names=FALSE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# AJUSTE DEL MODELO PLS-PM
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(readxl)
inDir <- 'C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_data/_modified_version'
all_data <- read_excel(paste(inDir, '/all_data_final_coded.xlsx', sep=''), sheet=1)

# Variables cuantificadas
qFvar <- read.csv('C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/quantifiedVariables.csv')
naID <- grep(pattern=1, apply(X=qFvar, MARGIN=1, FUN=function(x){z <- sum(is.na(x)); return(z)}))
qFvar <- qFvar[complete.cases(qFvar),]

library(plsdepot)

### Factores ambientales
mtch_fa <- grep(pattern='^FA', colnames(qFvar))
faPCA <- nipals(qFvar[,mtch_fa])
faPCA <- faPCA$scores[,1]
boxplot(as.numeric(faPCA)~all_data$Grupo[-naID])

### Apoyo y servicios
mtch_ap <- grep(pattern='^APS', colnames(qFvar))
apPCA <- nipals(qFvar[,mtch_ap])
apPCA <- apPCA$scores[,1]
boxplot(as.numeric(apPCA)~all_data$Grupo[-naID])

### Calidad de vida
mtch_cv <- grep(pattern='^CV', colnames(qFvar))
odd_numbers <- unlist(lapply(1:length(mtch_cv), function(i){
  sst <- strsplit(colnames(qFvar)[mtch_cv][i], "")[[1]]
  out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])
  out <- as.numeric(out[3])
  is.even <- function(x) x %% 2 == 0
  out <- is.even(out)
  return(out)
}))

# Importancia
mtch_cv_imp <- mtch_cv[!odd_numbers]
cvimpPCA <- nipals(qFvar[,mtch_cv_imp])
cvimpPCA <- cvimpPCA$scores[,1]
boxplot(as.numeric(cvimpPCA)~all_data$Grupo[-naID])

# Satisfaccion
mtch_cv_sat <- mtch_cv[odd_numbers]
cvsatPCA <- nipals(qFvar[,mtch_cv_sat])
cvsatPCA <- cvsatPCA$scores[,1]
boxplot(as.numeric(cvsatPCA)~all_data$Grupo[-naID])

######################################################
# PLS-PM Calidad de vidad: importancia y satisfacción
######################################################

library(plspm)

ambiente          <- c(0,0,0,0,0)
apoyos_servicios  <- c(0,0,0,0,0)
calidad_vida_imp  <- c(0,0,0,0,0)
calidad_vida_sat  <- c(0,0,0,0,0)
inclusion_laboral <- c(1,1,1,1,0)
dscp_path        <- rbind(ambiente, apoyos_servicios, calidad_vida_imp, calidad_vida_sat, inclusion_laboral)
colnames(dscp_path) <- rownames(dscp_path)
rm(ambiente, apoyos_servicios, calidad_vida_imp, calidad_vida_sat, inclusion_laboral)

innerplot(dscp_path)

dFtwostep   <- cbind(qFvar, faPCA, apPCA, cvimpPCA, cvsatPCA)

# options(warn=-1)
# itest <- matrix(0, nrow=ncol(dFtwostep), ncol=ncol(dFtwostep), byrow=T)
# for(i in 1:ncol(dFtwostep)){
#   for(j in 1:ncol(dFtwostep)){
#     itest[i,j] <- identical(dFtwostep[,i],dFtwostep[,j])
#   }
# }; rm(i); rm(j)
# diag(itest) <- NA
# colnames(itest) <- colnames(dFtwostep)
# rownames(itest) <- colnames(dFtwostep)
# 
# itest <- as.data.frame(itest)
# itest$var <- rownames(itest)
# library(tidyr)
# itest <- itest %>% gather(var1, condition, -var)

# identical variables (delete FAEN06)
# FAEN06, FAEN08

mtch_fa_c <- colnames(qFvar)[grep(pattern='^FA', colnames(qFvar))]
mtch_fa_c <- grep(pattern='^FA', colnames(qFvar))[-which(mtch_fa_c=='FAEN06')]

# list of blocks
dscp_blocks <- list(mtch_fa_c, mtch_ap, mtch_cv_imp, mtch_cv_sat, 164:ncol(dFtwostep))

# vector modes
dscp_modes <- rep('B',5)

# apply plspm
set.seed(1235)
dscp_pls <- plspm(dFtwostep, dscp_path, dscp_blocks, modes=dscp_modes, scaled=FALSE, scheme="centroid")
plot(dscp_pls)

# coeficientes estimados
dscp_pls$inner_model

# resumen del modelo
dscp_pls$inner_summary

# bondad de ajuste
dscp_pls$gof

# cargas estimadas
plot(dscp_pls, what="loadings")

# pesos estimados
plot(dscp_pls, what = "weights")

# cargas cruzadas
library(ggplot2)
library(reshape)
# reshape crossloadings data.frame for ggplot
xloads = melt(dscp_pls$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")
# bar-charts of crossloadings by block
ggplot(data=xloads, aes(x=name, y=value, fill=block)) +
  # add horizontal reference lines
  geom_hline(yintercept=0, color="gray75") +
  geom_hline(yintercept=0.5, color="gray70", linetype=2) +
  # indicate the use of car-charts
  geom_bar(stat = 'identity', position='dodge') +
  # panel display (i.e. faceting)
  facet_wrap(block ~ LV) +
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  # add title
  ggtitle("Crossloadings")

indexes <- as.data.frame(dscp_pls$scores)
boxplot(indexes$calidad_vida_sat~all_data$Grupo[-naID])

indexes$Grupo <- all_data$Grupo[-naID]

library(tidyr)

indexes_c <- indexes %>% gather(Indice, Value, -Grupo)
indexes_c$Indice <- gsub(pattern='ambiente', replacement='Factores ambientales', indexes_c$Indice)
indexes_c$Indice <- gsub(pattern='apoyos_servicios', replacement='Apoyo y servicios', indexes_c$Indice)
indexes_c$Indice <- gsub(pattern='calidad_vida_imp', replacement='Calidad de vida-Importancia', indexes_c$Indice)
indexes_c$Indice <- gsub(pattern='calidad_vida_sat', replacement='Calidad de vida-Satisfacción', indexes_c$Indice)
indexes_c$Indice <- gsub(pattern='inclusion_laboral', replacement='Inclusión laboral', indexes_c$Indice)

library(ggplot2)
gg <- ggplot(data=indexes_c, aes(x=Grupo, y=Value)) + geom_jitter(aes(colour=Grupo), alpha=0.4, size=2)
gg <- gg + facet_wrap(~Indice, scales='free')
gg <- gg + scale_fill_brewer(palette="Spectral")
gg <- gg + theme_bw() + geom_hline(yintercept=0, colour=2) + ylab('Índice estandarizado')
gg <- gg + theme(axis.text.x=element_text(size=7, angle=10))
gg <- gg + theme(axis.text.y=element_text(size=7))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=7))
ggsave(filename='C:/Users/haachicanoy/Documents/GitHub/Statistical_consulting/_occupational_therapy/_results/indicesGrupo.pdf', plot=gg, width=9, height=7, units='in')

######################################################
# PLS-PM Calidad de vidad combinada
######################################################

# PCA calidad de vida
cvidaPCA <- nipals(qFvar[,mtch_cv])
cvidaPCA <- cvidaPCA$scores[,1]
boxplot(as.numeric(cvidaPCA)~all_data$Grupo[-naID])

library(plspm)

ambiente          <- c(0,0,0,0)
apoyos_servicios  <- c(0,0,0,0)
calidad_vida      <- c(0,0,0,0)
inclusion_laboral <- c(1,1,1,0)
dscp_path2        <- rbind(ambiente, apoyos_servicios, calidad_vida, inclusion_laboral)
colnames(dscp_path2) <- rownames(dscp_path2)
rm(ambiente, apoyos_servicios, calidad_vida, inclusion_laboral)

innerplot(dscp_path2)

dFtwostep2 <- cbind(qFvar, faPCA, apPCA, cvidaPCA)

mtch_fa_c <- colnames(qFvar)[grep(pattern='^FA', colnames(qFvar))]
mtch_fa_c <- grep(pattern='^FA', colnames(qFvar))[-which(mtch_fa_c=='FAEN06')]

# list of blocks
dscp_blocks2 <- list(mtch_fa_c, mtch_ap, mtch_cv, 164:ncol(dFtwostep2))

# vector modes
dscp_modes2 <- rep('B',4)

# apply plspm
set.seed(1235)
dscp_pls2 <- plspm(dFtwostep2, dscp_path2, dscp_blocks2, modes=dscp_modes2, scaled=TRUE, scheme="centroid")
plot(dscp_pls2)
