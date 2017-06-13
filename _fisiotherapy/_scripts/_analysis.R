# Fisiotherapy analysis
# H. Achicanoy
# 2017

# R options
options(warn = -1); options(scipen = 999)

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(modelr))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(broom))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(foreign))
suppressMessages(library(haven))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))

df <- read.spss(file = "./_data/cantera_data.sav", to.data.frame = T)
df$MES_INGRESO <- df$FECHA_LESION <- df$FECHA_RETORNO_ENTRENO <- df$NOMBRE <- df$ANTIGUEDAD <- df$INCAPACIDAD <- df$APTITUD <- df$DIAGNOSTICO <- df$LUGAR_ATENCION <- df$MES_RETORNO <- NULL
df$GRAVEDAD <- factor(x = df$GRAVEDAD, levels = c("MENOR", "LEVE", "MODERADA", "GRAVE"), ordered = T)
df$IMC_CUALI <- factor(x = df$IMC_CUALI, levels = c("BAJO PESO", "PESO NORMAL", "SOBREPESO"), ordered = T)

# First big change
# df$LATERALIDAD <- df$LADO <- df$MECANISMO <- NULL

glimpse(df)

# Explore correlations first
M <- hetcor(data = df)
corrplot(corr = M$correlations, type = "upper", is.corr = T)

# Multiple correspondence analysis
mca.res <- FactoMineR::MCA(X = df, quanti.sup = c(1, 14, 16:18, 20:21), graph = F)
# mca.res <- FactoMineR::MCA(X = df, quanti.sup = c(1, 11, 13:15, 17:18), graph = F) # Originally c(1, 14, 16:18, 20:21)

summary(mca.res, nb.dec = 3, nbelements = 10,  ncp = TRUE)

# Eigenvalues/variances
fviz_screeplot(mca.res) + theme_bw()

# Plotting variables and individuals biplot
fviz_mca_biplot(mca.res, repel = TRUE) + theme_bw()

# Coordinates of variable categories
fviz_mca_var(mca.res, repel = TRUE) + theme_bw()

# Contribution of variable categories to the dimensions
corrplot(mca.res$var$contrib, is.corr = FALSE)

# The quality of representation of variable categories
corrplot(mca.res$var$cos2, is.corr=FALSE)

# Just individuals
fviz_mca_ind(mca.res) + theme_bw()
fviz_mca_ind(mca.res, label = "none", habillage = df$CATEGORIA) + theme_bw()
fviz_mca_ind(mca.res, label = "none", habillage = df$CATEGORIA, addEllipses = TRUE, ellipse.level = 0.95) + theme_bw()

# Complex biplot
fviz_mca_biplot(mca.res, 
                habillage = df$RECAIDA, addEllipses = F,
                label = "var", shape.var = 15) +
  scale_color_brewer(palette="Dark2")+
  theme_bw()

plot(mca.res, choix = "quanti.sup")
