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
suppressMessages(library(funModeling))

# ------------------------------------------------------- #
# Loading data
# ------------------------------------------------------- #
# in_data <- foreign::read.spss(file = "../_data/Base GC Innovación - Cliente.sav", to.data.frame = T, use.value.labels = T)
in_data <- readxl::read_excel(path = "../_data/GC_innovacion.xlsx", sheet = "Labels")
names(in_data)

# ------------------------------------------------------- #
# Exclude variables without variability
# ------------------------------------------------------- #
in_data$Srvyr <- in_data$Q_3 <- in_data$Q_8 <- in_data$Q_9 <- in_data$Q_11 <- in_data$Q_13 <- NULL
in_data$Q_33 <- in_data$Q_35 <- in_data$Q_37 <- in_data$Q_39 <- in_data$Q_41 <- in_data$q_43 <- NULL
in_data$Q_45 <- in_data$Q_59_television <- in_data$Q_59_telefonia <- in_data$Q_59_teleconferencia <- NULL
in_data$Q_64 <- in_data$Q_69_otros <- in_data$Q_71 <- in_data$Q_73 <- in_data$Q_75 <- in_data$Q_82 <- in_data$Q_85 <- NULL
in_data$Q_86 <- in_data$Q_87 <- in_data$Q_88 <- NULL

in_data %>% glimpse

enterprise_list <- readxl::read_excel(path = "../_data/GC_listado.xlsx", sheet = 1)

agrep2 <- Vectorize(agrep, vectorize.args = "pattern")
in_data$Clasificacion <-  enterprise_list$LISTADO[as.numeric(agrep2(pattern = in_data$Q_89, x = enterprise_list$EMPRESA, max = 1, ignore.case = T))]
rm(enterprise_list, agrep2)

in_data <- in_data %>% base::as.data.frame()

# ------------------------------------------------------- #
# Processing categorical variables
# ------------------------------------------------------- #
for(i in 1:ncol(in_data)){
  
  if(is.character(in_data[,i])){
    
    ordinal_cont <- grep(pattern = "[0-9].", x = in_data[,i], ignore.case = T)
    if(length(ordinal_cont) > 0){
      in_data[,i] <- in_data[,i] %>% as.factor
      levels(in_data[,i]) <- levels(in_data[,i]) %>% gtools::mixedsort()
      in_data[,i] <- in_data[,i] %>% factor(., ordered = T)
    } else {
      in_data[,i] <- in_data[,i] %>% as.factor
    }
    
  }
  
}; rm(i)
in_data$Clasificacion <- in_data$Clasificacion %>% as.character() %>% as.factor()

# ------------------------------------------------------- #
# Descriptive analysis
# ------------------------------------------------------- #
df_status(in_data)
plot_num(in_data)
profiling_num(in_data)
freq(in_data)
# correlation_table(in_data, "Q_16")
# var_rank_info(in_data, "Q_4")
# cross_plot(data=in_data, input=c("Q_6", "Q_4"), target="Q_9")

# ------------------------------------------------------- #
# Correlation analysis
# ------------------------------------------------------- #
df_tmpr <- in_data[,sapply(in_data, is.factor)]

options(warn = -1)
p.chisq = matrix(0, nrow = ncol(df_tmpr), ncol = ncol(df_tmpr), byrow = T)
for(i in 1:ncol(df_tmpr)){
  for(j in 1:ncol(df_tmpr)){
    p.chisq[i,j] = round(chisq.test(df_tmpr[,i], df_tmpr[,j])$p.value, 3)
  }
}; rm(i); rm(j)

diag(p.chisq) = NA
colnames(p.chisq) = colnames(df_tmpr)
rownames(p.chisq) = colnames(df_tmpr)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
# png('./_results/chi_test.png', height = 7, width = 7, units = "in", res = 300)
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
          margins=c(11,11))
# dev.off(); rm(catVar, p.chisq, color_scale)

# ------------------------------------------------------- #
# Multivariate analysis: knowledge
# ------------------------------------------------------- #
mca_knowledge <- FactoMineR::MCA(X = in_data %>% dplyr::select(Q_19:Q_31_no_permanece), graph = T)
mca_knowledge %>% factoextra::fviz_mca_biplot(repel = TRUE, # Avoid text overlapping (slow if many point)
                                              ggtheme = theme_bw(),
                                              habillage = in_data$Q_4,
                                              addEllipses = F,
                                              geom = "point")

# ------------------------------------------------------- #
# Multivariate analysis: organization
# ------------------------------------------------------- #
mca_organization <- FactoMineR::MCA(X = in_data %>% dplyr::select(Q_32:Q_62_rutinaria), graph = T) # Q_44
mca_organization %>% factoextra::fviz_mca_biplot(repel = TRUE, # Avoid text overlapping (slow if many point)
                                                 ggtheme = theme_bw(),
                                                 habillage = in_data$Q_4,
                                                 addEllipses = F,
                                                 geom = "point")

# ------------------------------------------------------- #
# Multivariate analysis: management
# ------------------------------------------------------- #
mca_management <- FactoMineR::MCA(X = in_data %>% dplyr::select(Q_63:Q_69_redes_sociales), graph = T)
mca_management %>% factoextra::fviz_mca_biplot(repel = TRUE, # Avoid text overlapping (slow if many point)
                                               ggtheme = theme_bw(),
                                               habillage = in_data$Clasificacion,
                                               addEllipses = F,
                                               geom = "point")

# ------------------------------------------------------- #
# Multivariate analysis: technology
# ------------------------------------------------------- #
mca_technology <- FactoMineR::MCA(X = in_data %>% dplyr::select(Q_70:Q_84), graph = T) # Q_76_educacion
mca_technology %>% factoextra::fviz_mca_biplot(repel = TRUE, # Avoid text overlapping (slow if many point)
                                               ggtheme = theme_bw(),
                                               habillage = in_data$Clasificacion,
                                               addEllipses = F,
                                               geom = "point")
