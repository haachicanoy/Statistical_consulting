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
suppressMessages(library(psych))
suppressMessages(library(gplots))
suppressMessages(library(viridis))

# ----------------------------------------------------------------------------------- #
# Read and pre-process data
# ----------------------------------------------------------------------------------- #
df <- read.spss(file = "./_data/cantera_data.sav", to.data.frame = T)
df$MES_INGRESO <- df$FECHA_LESION <- df$FECHA_RETORNO_ENTRENO <- df$NOMBRE <- df$ANTIGUEDAD <- df$INCAPACIDAD <- df$APTITUD <- df$DIAGNOSTICO <- df$LUGAR_ATENCION <- df$MES_RETORNO <- NULL
df$GRAVEDAD <- factor(x = df$GRAVEDAD, levels = c("MENOR", "LEVE", "MODERADA", "GRAVE"), ordered = T)
df$IMC_CUALI <- factor(x = df$IMC_CUALI, levels = c("BAJO PESO", "PESO NORMAL", "SOBREPESO"), ordered = T)
df$CATEGORIA <- as.character(df$CATEGORIA); df$CATEGORIA <- as.factor(df$CATEGORIA)
df$MOMENTO_LESION <- as.character(df$MOMENTO_LESION); df$MOMENTO_LESION <- as.factor(df$MOMENTO_LESION)
df$AREA_CORPORAL <- as.character(df$AREA_CORPORAL); df$AREA_CORPORAL <- as.factor(df$AREA_CORPORAL)
df$TIPO_LESION <- as.character(df$TIPO_LESION); df$TIPO_LESION <- as.factor(df$TIPO_LESION)
df$TEJIDO_LESIONADO <- as.character(df$TEJIDO_LESIONADO); df$TEJIDO_LESIONADO <- as.factor(df$TEJIDO_LESIONADO)
df$MECANISMO <- as.character(df$MECANISMO); df$MECANISMO <- as.factor(df$MECANISMO)

glimpse(df)

# ----------------------------------------------------------------------------------- #
# Calculate frequencies table
# ----------------------------------------------------------------------------------- #
fqTable <- df %>%
  select(CATEGORIA, POSICION, LATERALIDAD, CIRUGIA, MOMENTO_LESION, AREA_CORPORAL, LADO, TIPO_LESION, TEJIDO_LESIONADO, MECANISMO, RECAIDA, GRAVEDAD) %>%
  gather(measure, value) %>%
  count(measure, value) %>%
  spread(measure, n) %>%
  gather(key = Variable, value = Count, AREA_CORPORAL:TIPO_LESION)
fqTable <- fqTable[complete.cases(fqTable),]; rownames(fqTable) <- 1:nrow(fqTable); colnames(fqTable)[1] <- "Category"
fqTable <- fqTable %>% dplyr::mutate(Percentage = Count/nrow(df))

# Plot it (SAVE IT!!!!)
gg <- fqTable %>% ggplot(aes(x = Category, y = Percentage*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("./_results/qualitative_frequencies.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

# ----------------------------------------------------------------------------------- #
# Plotting cuantitative variables (SAVE IT!!!)
# ----------------------------------------------------------------------------------- #
gg <- df %>% select(EDAD, DIAS_INCAPACIDAD, TALLA, PESO, IMC, HORAS_JUEGO, HORAS_ENTRENAMIENTO) %>%
  gather(Variable, Value) %>% ggplot(aes(x = Value, fill = Variable, alpha = .6)) +
  geom_density() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(face = "bold")) +
  guides(alpha = F, fill = F) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("./_results/quantitative_density.png", plot = gg, width = 22, height = 10, units = "in"); rm(gg)

# ----------------------------------------------------------------------------------- #
# Summary measures
# ----------------------------------------------------------------------------------- #
df %>% select(EDAD, DIAS_INCAPACIDAD, TALLA, PESO, IMC, HORAS_JUEGO, HORAS_ENTRENAMIENTO) %>%
  psych::describe() %>% select(mean, sd, median, min, max, range) %>% write.csv(file = "./_results/quantitative_summary.csv", row.names = T)

# ----------------------------------------------------------------------------------- #
# Explore variable relationships
# Chi-square test for cualitative data
# ----------------------------------------------------------------------------------- #
catVar <- df %>%
  select(CATEGORIA, POSICION, LATERALIDAD, CIRUGIA, MOMENTO_LESION, AREA_CORPORAL, LADO, TIPO_LESION, TEJIDO_LESIONADO, MECANISMO, RECAIDA, GRAVEDAD)

options(warn=-1)
p.chisq = matrix(0, nrow=ncol(catVar), ncol=ncol(catVar), byrow=T)
for(i in 1:ncol(catVar)){
  for(j in 1:ncol(catVar)){
    p.chisq[i,j] = round(chisq.test(catVar[,i],catVar[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq) = NA
colnames(p.chisq) = colnames(catVar)
rownames(p.chisq) = colnames(catVar)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png('./_results/chi_test.png', height = 7, width = 7, units = "in", res = 300)
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
dev.off(); rm(catVar, p.chisq, color_scale)

# ----------------------------------------------------------------------------------- #
# Explore variable relationships
# Spearman correlation for cuantitative data
# ----------------------------------------------------------------------------------- #

png('./_results/spearman_corr.png', height = 7, width = 7, units = "in", res = 300)
df %>% select(EDAD, DIAS_INCAPACIDAD, TALLA, PESO, IMC, HORAS_JUEGO, HORAS_ENTRENAMIENTO) %>%
  cor(method = "spearman") %>% corrplot(method = "ellipse", type="upper", is.corr = T, mar = c(0, 0, 3, 3))
dev.off()

# M <- hetcor(data = df)
# corrplot(corr = M$correlations, type = "upper", is.corr = T)

# ----------------------------------------------------------------------------------- #
# Factor analysis for mixed data
# ----------------------------------------------------------------------------------- #
famd.res <- FactoMineR::FAMD(base = df, graph = T)
summary(famd.res, nb.dec = 3, nbelements = 10,  ncp = TRUE)

# ----------------------------------------------------------------------------------- #
# Multiple correspondence analysis
# ----------------------------------------------------------------------------------- #

mca.res <- df %>%
  select(CATEGORIA, POSICION, LATERALIDAD, CIRUGIA, MOMENTO_LESION, AREA_CORPORAL, LADO, TIPO_LESION, TEJIDO_LESIONADO, MECANISMO, RECAIDA, GRAVEDAD, EDAD, DIAS_INCAPACIDAD, TALLA, PESO, IMC, HORAS_JUEGO, HORAS_ENTRENAMIENTO) %>%
  FactoMineR::MCA(quanti.sup = 13:19, graph = F)

# Change No. 1
# df$LATERALIDAD <- df$LADO <- df$MECANISMO <- NULL
# mca.res <- FactoMineR::MCA(X = df, quanti.sup = c(1, 11, 13:15, 17:18), graph = F) # Originally c(1, 14, 16:18, 20:21)

# Change No. 2
# df$AREA_CORPORAL <- df$TEJIDO_LESIONADO <- df$CONTRATO <- NULL
# mca.res <- FactoMineR::MCA(X = df, quanti.sup = c(1, 8, 10:12, 14:15), graph = F) # Originally c(1, 14, 16:18, 20:21)

# Change No. 3
# df2 <- df[,c("EDAD", "POSICION", "MOMENTO_LESION", "LADO", "MECANISMO", "RECAIDA", "TALLA", "PESO", "IMC", "HORAS_JUEGO", "HORAS_ENTRENAMIENTO")]
# mca.res <- FactoMineR::MCA(X = df2, quanti.sup = c(1, 7:11), graph = F)

summary(mca.res, nb.dec = 3, nbelements = 10,  ncp = TRUE)

# Eigenvalues/variances
gg <- fviz_screeplot(mca.res) + theme_bw()
ggsave("./_results/mca_eigenValues.png", plot = gg, width = 7, height = 7, units = 'in'); rm(gg)

# Coordinates of variable categories
gg <- fviz_mca_var(mca.res, repel = TRUE) + theme_bw()
ggsave("./_results/mca_qualiVar_map.png", plot = gg, width = 22, height = 10, units = 'in'); rm(gg)

# Correlation circle for quantitative variables
png('./_results/mca_quantiVar_map.png', height = 7, width = 7, units = "in", res = 300)
plot(mca.res, choix = "quanti.sup")
dev.off()

# Plotting variables and individuals biplot
gg <- fviz_mca_biplot(mca.res, repel = TRUE) + theme_bw()
ggsave("./_results/mca_biplot_map.png", plot = gg, width = 22, height = 10, units = 'in'); rm(gg)


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

# ----------------------------------------------------------------------------------- #
# Position analysis
# ----------------------------------------------------------------------------------- #

# Heatmap Posicion vs Categoria

gg <- df %>%
  select(CATEGORIA, POSICION) %>%
  table() %>% as.tibble() %>% ggplot(aes(x = CATEGORIA, y = POSICION, fill = 100 * n/nrow(df))) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis(name = "Porcentaje (%)") +
  coord_equal() +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 12))
ggsave("./_results/heatmap_posicion_categoria.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

# Heatmap Posicion vs Momento de lesion

gg <- df %>%
  select(MOMENTO_LESION, POSICION) %>%
  table() %>% as.tibble() %>% ggplot(aes(x = MOMENTO_LESION, y = POSICION, fill = 100 * n/nrow(df))) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis(name = "Porcentaje (%)") +
  coord_equal() +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 12))
ggsave("./_results/heatmap_posicion_momento_lesion.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

# Heatmap Posicion vs Categoria/Momento de lesion per last variable

gg <- df %>%
  select(CATEGORIA, POSICION, MOMENTO_LESION) %>%
  table() %>% as.tibble() %>% ggplot(aes(x = CATEGORIA, y = POSICION, fill = 100 * n/nrow(df))) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis(name = "Porcentaje (%)") +
  coord_equal() +
  theme_bw() +
  facet_wrap(~MOMENTO_LESION) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 12))
ggsave("./_results/heatmap_posicion_categoria_momento_lesion.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

# Heatmap Posicion vs Tipo de lesion

gg <- df %>%
  select(TIPO_LESION, POSICION) %>%
  table() %>% as.tibble() %>% ggplot(aes(x = TIPO_LESION, y = POSICION, fill = 100 * n/nrow(df))) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis(name = "Porcentaje (%)") +
  coord_equal() +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 12))
ggsave("./_results/heatmap_posicion_tipo_lesion.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

# Heatmap Posicion vs Area lesionada

gg <- df %>%
  select(AREA_CORPORAL, POSICION) %>%
  table() %>% as.tibble() %>% ggplot(aes(x = AREA_CORPORAL, y = POSICION, fill = 100 * n/nrow(df))) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis(name = "Porcentaje (%)") +
  coord_equal() +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 12))
ggsave("./_results/heatmap_posicion_area_lesionada.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

# Heatmap Posicion vs edad

gg <- df %>% ggplot(aes(x = POSICION, y = EDAD, fill = POSICION)) +
  geom_boxplot() +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 12))
ggsave("./_results/boxplot_posicion_edad.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

# Heatmap Posicion vs tejido lesionado

gg <- df %>%
  select(TEJIDO_LESIONADO, POSICION) %>%
  table() %>% as.tibble() %>% ggplot(aes(x = TEJIDO_LESIONADO, y = POSICION, fill = 100 * n/nrow(df))) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis(name = "Porcentaje (%)") +
  coord_equal() +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 12))
ggsave("./_results/heatmap_posicion_tejido_lesionado.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

# Heatmap Posicion vs dias de ausencia/mecanismo

gg <- df %>% ggplot(aes(x = POSICION, y = DIAS_INCAPACIDAD, fill = POSICION)) +
  geom_boxplot() +
  facet_wrap(~MECANISMO) +
  ylab("DIAS DE AUSENCIA") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 12))
ggsave("./_results/boxplot_posicion_dias_ausencia_mecanismo.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)
