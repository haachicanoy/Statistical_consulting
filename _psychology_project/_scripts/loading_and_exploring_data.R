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

# ------------------------------------------------------- #
# Loading data
# ------------------------------------------------------- #
in_data <- read.spss(file = "../_data/Base GC Innovación - Cliente.sav", to.data.frame = T, use.value.labels = T) # F
names(in_data)
in_data %>% glimpse

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