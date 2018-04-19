# Practicas innovadoras - Proyecto Univalle Psicologia: cargar y explorar datos
# H. Achicanoy
# 2018

# R options
options(warn = -1); options(scipen = 999)

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(foreign))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))
suppressMessages(library(psych))
suppressMessages(library(gplots))
suppressMessages(library(viridis))
suppressMessages(library(lsr))
suppressMessages(library(DescTools))
suppressMessages(library(readxl))

# ------------------------------------------------------- #
# Loading data
# ------------------------------------------------------- #
in_data <- foreign::read.spss(file = "../_data/Base GC Innovación - Cliente.sav", to.data.frame = T, use.value.labels = T) # F
names(in_data)
in_data %>% glimpse

enterprise_list <- readxl::read_excel(path = "../_data/Empresas encuestadas por listado.xlsx", sheet = 2)

# ------------------------------------------------------- #
# Fixing data
# ------------------------------------------------------- #
fix_data <- T
if(fix_data){
  
  # Q1: City
  levels(in_data$Q_1) <- c("Armenia", "Balboa-Anserma Nuevo", "Barrancabermeja",
                           "Barranquilla", "Bello", "Bogota", "Bogota", "Cali",
                           "Cartagena", "Itagui", "Manizales", "Medellin", "Medellin",
                           "Paipa", "Palmira", "Santa Marta", "Yumbo")
  
  # Q2: Foundation year
  
  # Q3: (Does not have variability)
  in_data$Q_3 <- NULL
  
  # Q4: Economic sector
  levels(in_data$Q_4) <- c("Primario", "Secundario", "Terciario")
  in_data$Q_4 <- in_data$Q_4 %>% factor(., ordered = T)
  
  # Q5: Employment type
  levels(in_data$Q_5)
  
  # Q6: Number of direct employees
  levels(in_data$Q_6) <- c("De 10-50", "De 51-100", "De 101-150", "De 150-200", "De 200-500", "Mas de 500")
  in_data$Q_6 <- in_data$Q_6 %>% factor(., ordered = T)
  
  # Q7: Number of non-direct employees
  levels(in_data$Q_7) <- c("De 10-50", "De 51-100", "De 101-150", "De 150-200", "De 200-500", "Mas de 500")
  in_data$Q_7 <- in_data$Q_7 %>% factor(., ordered = T)
  
  # Q8: Spaces for solving organizational problems (it has little variability)
  in_data$Q_8 <- NULL
  
  # Q9: 
  
  # Q89: Enterprise name
  levels(in_data$Q_89) <- c("ALIMENTOS Q SABOR", "AOXLAB", "B AHMAN Y CIA S.A.S",
                            "C.I. ISOLUX S.A.S", "CAFEXCOOP S.A", "CI BIOCOSTA",
                            "COLANTA", "COMPANIA NACIONAL DE LEVADURA LEVAPAN S.A",
                            "CONSTRUCCIONES EL CONDOR", "COSMITET", "COTECMAR",
                            "CREACIONES VANGLO S.A.S", "DICOPLAST S.A.S", "DIGITAL WARE S.A",
                            "DISTRIBUIDORA NISSA", "ELECTRICAS DE MEDELLIN INGENIERIA Y SERVICIOS",
                            "FABRICA DE PAPELES PALMIRA LIMITADA", "FABRICATO",
                            "FARID CURE AND  COMPANY  S.A.S ARROCERA DEL LITORAL",
                            "GRUPO ENEL CODENSA Y EHGESA", "GRUPO OET", "HARINERA DEL VALLE",
                            "HUNTER DOUGLAS DE COLOMBIA", "INCAUCA", "INDUMA S.C.A",
                            "INDUSTRIAS DE ENVASES S.A", "INGENIO RISARALDA S.A",
                            "INTEGRAL S.A", "ITALCOL DE OCCIDENTE S.A", "JAVIER PEREZ",
                            "LABORATORIOS NEO LTDA", "LEONARDO RODRIGUEZ", "LIDERPAN S.A",
                            "LINK DIAGNOSTICO DIGITAL", "LUZ MARINA CASAS",
                            "MADECENTRO", "MANUEL ENRIQUE BENAVIDES",
                            "MEALS MERCADEO DE ALIMENTOS DE COLOMBIA S.A.S",
                            )
}

level_count <- lapply(1:ncol(in_data), function(i){
  
  if(is.factor(in_data[,i])){
    in_data[,i] <- in_data[,i] %>% as.character
    in_data[,i] <- in_data[,i] %>% as.factor
    return(in_data[,i] %>% levels %>% length)
  } else {
    return(NA)
  }
  
}) %>% unlist

in_data[,which(level_count == 1)] %>% View

apply(X = in_data, MARGIN = 2, FUN = function(x){
  if(is.factor(x)){
    return(levels(x) %>% length)
  } else {
    return(cat("Something to check\n"))
  }
})


library(funModeling)
df_status(in_data)
plot_num(in_data)
profiling_num(in_data)
freq(in_data)
correlation_table(in_data, "Q_16")
var_rank_info(in_data, "Q_4")
cross_plot(data=in_data, input=c("Q_6", "Q_4"), target="Q_8")

apply(X = in_data, MARGIN = 2, FUN = is.double)

mca.res <- FactoMineR::MCA(X = in_data %>% select(Q_3:Q_59_O10), graph = F)
factoextra::fviz_mca_biplot(mca.res, 
                            repel = TRUE, # Avoid text overlapping (slow if many point)
                            ggtheme = theme_minimal())

pilares <- km_data %>% select(P3_1:P3_8, P5_1:P9_6, P11_1:P13_16, P22_1:P25_5)
for(j in 1:ncol(pilares)){
  pilares[,j] <- factor(pilares[,j], levels = c("Totalmente en desacuerdo",
                                                "En desacuerdo",
                                                "Ni de acuerdo ni en desacuerdo",
                                                "De acuerdo",
                                                "Totalmente de acuerdo"), ordered = T)
}; rm(j)
pilares %>% glimpse
pilares %>% str