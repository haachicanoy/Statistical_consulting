# Practicas innovadoras - Proyecto Univalle Psicologia: cargar y explorar datos
# H. Achicanoy
# 2018

# R options
options(warn = -1, scipen = 999)

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
suppressMessages(library(tabplot))
suppressMessages(library(ltm))

# ------------------------------------------------------- #
# Own functions
# ------------------------------------------------------- #
# Frequency plots
freq_plots <- function(df, grouped = T, text_size = 6, titl_size = 7){
  
  # Frequency table
  fqTable <- df %>%
    tidyr::gather(measure, value) %>%
    dplyr::count(measure, value)
  fqTable <- fqTable[complete.cases(fqTable),]; rownames(fqTable) <- 1:nrow(fqTable); colnames(fqTable)[1:2] <- c("Category", "Label")
  fqTable <- fqTable %>% dplyr::mutate(prcn = n/nrow(df))
  
  if(grouped){
    
    fqTable %>% ggplot(aes(x = Category, y = prcn*100, fill = Label)) +
      geom_bar(stat = "identity") +
      xlab("") + ylab("Porcentaje (%)") +
      theme_bw() +
      theme(strip.text = element_text(size = text_size, face = "bold")) +
      theme(axis.title.x = element_text(size = titl_size, face = 'bold'),
            axis.title.y = element_text(size = titl_size, face = 'bold'),
            axis.text.x = element_text(size = text_size, angle = 90, hjust = 0.95, vjust = 0.2),
            axis.text.y = element_text(size = text_size)) +
      scale_fill_brewer(palette = "Paired") # +
    # guides(fill = FALSE)
    
  } else {
    
    fqTable %>% ggplot(aes(x = Label, y = prcn*100, fill = Label)) +
      geom_bar(stat = "identity") +
      xlab("") + ylab("Porcentaje (%)") +
      coord_flip() +
      facet_wrap(~ Category, scales = "free_y") +
      theme_bw() +
      theme(strip.text = element_text(size = text_size, face = "bold")) +
      theme(axis.title.x = element_text(size = titl_size, face = 'bold'),
            axis.title.y = element_text(size = titl_size, face = 'bold'),
            axis.text = element_text(size = text_size)) +
      scale_fill_brewer(palette = "Paired") +
      guides(fill = FALSE)
    
  }
  
}

# Chi-square test of independence for excluding non-asociated variables
independence_analysis <- function(df){
  
  options(warn = -1)
  p.chisq = matrix(0, nrow = ncol(df), ncol = ncol(df), byrow = T)
  for(i in 1:ncol(df)){
    for(j in 1:ncol(df)){
      p.chisq[i,j] = round(chisq.test(df[,i], df[,j])$p.value, 3)
      # print(paste0("Variable ", i, " and variable ", j))
    }
  }; rm(i); rm(j)
  
  diag(p.chisq) = NA
  colnames(p.chisq) = colnames(df)
  rownames(p.chisq) = colnames(df)
  
  # color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
  # png('./_results/chi_test.png', height = 7, width = 7, units = "in", res = 300)
  heatmap.2(p.chisq,
            main="Independence test",
            key.title="Chi-square test",
            key.xlab="p-value",
            Rowv=NULL,
            Colv=NULL,
            col=viridis::viridis(50, direction = -1),
            linecol=NULL,
            tracecol=NULL,
            density.info="density",
            denscol="blue",
            margins=c(11,11)) %>% return()
  # dev.off()
  
  sgnf_assc <- (sum(p.chisq < 0.05, na.rm = T)/2)/((dim(p.chisq)[1] * dim(p.chisq)[2])/2)
  whch <- which(p.chisq < 0.05, arr.ind = TRUE)
  sgnf_vars <- c(rownames(p.chisq)[whch[,1]], rownames(p.chisq)[whch[,2]]) %>% unique
  
  return(list(sgnf_assc, sgnf_vars))
  
}

# Calculate Cronbach alpha for excluding non-asociated variables
calc_cronbach <- function(df = df, cor = 0.2){
  
  df <- df
  df[] <- as.numeric(factor(as.matrix(df)))
  cronbach <- psych::alpha(df, warnings = F)
  selected_vars <- rownames(cronbach$item.stats[cronbach$item.stats$std.r > cor,])
  
  return(selected_vars)
  
}

# Calculate Cronbach alpha
calc_cronbach2 <- function(df = df){
  
  df <- df
  df[] <- as.numeric(factor(as.matrix(df)))
  cronbach <- psych::alpha(df, warnings = F)
  
  return(cronbach)
  
}

# Calculate multivariate index from MCA results
calc_index <- function(res.mca){
  
  w <- res.mca$eig[,1]                                         # Eigen values extraction
  q <- w/sum(w)                                                # Explained variance percentage
  i <- q[1]*res.mca$var$coord[,1] + q[2]*res.mca$var$coord[,2] # Variables weighting
  l <- i/sum(i)                                                # Standardization
  Ind0 <- scale(res.mca$call$Xtot)%*%l                         # Individuals weighting
  Ind1 <- (exp(Ind0))/(1+(exp(Ind0)))*100                      # Final index
  
  return(Ind1)
  
}

# Ploting MCA resuLts
plot_mca <- function(res.mca = res.mca){
  # Eigen values
  plot1 <- res.mca %>% factoextra::fviz_screeplot(addlabels = TRUE,
                                                  repel = T,
                                                  ggtheme = theme_bw(),
                                                  ylim = c(0, 45))
  # Biplot
  plot2 <- res.mca %>% factoextra::fviz_mca_biplot(repel = TRUE,
                                                   ggtheme = theme_bw(),
                                                   # habillage = in_data$Clasificacion,
                                                   geom = "point")
  # Correlation vars and dimensions
  plot3 <- res.mca %>% factoextra::fviz_mca_var(choice = "mca.cor",
                                                ggtheme = theme_bw(),
                                                repel = TRUE)
  # Vars plot with cos2
  plot4 <- res.mca %>% factoextra::fviz_mca_var(col.var = "cos2",
                                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                                repel = TRUE,
                                                ggtheme = theme_bw())
  # Inds plot with cos2
  plot5 <- res.mca %>% factoextra::fviz_mca_ind(col.ind = "cos2", 
                                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                                repel = TRUE,
                                                ggtheme = theme_bw())
  return(list(plot1, plot2, plot3, plot4, plot5))
}

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
in_data$Q_33 <- in_data$Q_35 <- in_data$Q_37 <- in_data$Q_39 <- in_data$Q_41 <- in_data$Q_43 <- NULL
in_data$Q_45 <- in_data$Q_59_television <- in_data$Q_59_telefonia <- in_data$Q_59_teleconferencia <- NULL
in_data$Q_64 <- in_data$Q_69_otros <- in_data$Q_71 <- in_data$Q_73 <- in_data$Q_75 <- in_data$Q_82 <- in_data$Q_85 <- NULL
in_data$Q_86 <- in_data$Q_87 <- in_data$Q_88 <- NULL

in_data %>% glimpse

# Adding enterprise classification
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
  
}; rm(i, ordinal_cont)
in_data$Clasificacion <- in_data$Clasificacion %>% as.character() %>% as.factor()
in_data$Q_89 <- in_data$Q_89 %>% as.character() %>% as.factor()
in_data$Clasificacion2 <- in_data$Clasificacion
levels(in_data$Clasificacion2) <- c("Grandes empresas",
                                    "Empresas innovadoras",
                                    "Grandes empresas",
                                    "Empresas innovadoras",
                                    "Empresas innovadoras",
                                    "Empresas innovadoras",
                                    "Ninguno")

# ------------------------------------------------------- #
# Descriptive analysis
# ------------------------------------------------------- #
# funModeling::df_status(in_data)
# funModeling::plot_num(in_data)
# funModeling::profiling_num(in_data)
# funModeling::freq(in_data, plot = F)
# funModeling::correlation_table(in_data, "Q_16")
# funModeling::var_rank_info(in_data, "Q_4")
# funModeling::cross_plot(data=in_data, input=c("Q_6", "Q_4"), target="Q_9")

# Economic sector
freq_plots(df = in_data %>% dplyr::select(Q_4), grouped = F, text_size = 12, titl_size = 13)
# Employment
freq_plots(df = in_data %>% dplyr::select(Q_5:Q_7), grouped = T, text_size = 12, titl_size = 13)
# Enterprise characterization
freq_plots(df = in_data %>% dplyr::select(Clasificacion), grouped = F, text_size = 12, titl_size = 13)
# Knowledge section
freq_plots(df = in_data %>% dplyr::select(Q_19:Q_31_no_permanece), grouped = T, text_size = 12, titl_size = 13)
freq_plots(df = in_data %>%
             dplyr::filter(Clasificacion == "EMPRESAS ALTAMENTE INNOVADORAS COLCIENCIAS") %>%
             dplyr::select(Q_19:Q_31_no_permanece), grouped = T, text_size = 12, titl_size = 13)
# Organization section
freq_plots(df = in_data %>% dplyr::select(Q_32:Q_60_implementar_innovaciones, Q_62_flexible:Q_62_rutinaria), grouped = T, text_size = 12, titl_size = 13)
freq_plots(df = in_data %>% dplyr::select(Q_61), grouped = F, text_size = 12, titl_size = 13)
# Management section
freq_plots(df = in_data %>% dplyr::select(Q_63:Q_69_redes_sociales), grouped = T, text_size = 12, titl_size = 13)
# Technology section
freq_plots(df = in_data %>% dplyr::select(Q_70:Q_84), grouped = T, text_size = 12, titl_size = 13)

# ------------------------------------------------------- #
# Correlation analysis
# ------------------------------------------------------- #
df_tmpr <- in_data[,sapply(in_data, is.factor)]
df_tmpr$Q_1 <- df_tmpr$Q_89 <- NULL

independence_analysis(df = df_tmpr); rm(df_tmpr)

calc_cronbach2(df = in_data %>% dplyr::select(Q_10:Q_18_relaciones))   # People
calc_cronbach2(df = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_10:Q_18_relaciones))])   # People
calc_cronbach2(df = in_data %>% dplyr::select(Q_19:Q_31_no_permanece))   # Knowledge
calc_cronbach2(df = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_19:Q_31_no_permanece))])   # Knowledge
calc_cronbach2(df = in_data %>% dplyr::select(Q_32:Q_62_rutinaria))      # Organization
calc_cronbach2(df = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_32:Q_62_rutinaria))])   # Organization
calc_cronbach2(df = in_data %>% dplyr::select(Q_63:Q_69_redes_sociales)) # Management
calc_cronbach2(df = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_63:Q_69_redes_sociales))])   # Management
calc_cronbach2(df = in_data %>% dplyr::select(Q_70:Q_84))                # Technology
calc_cronbach2(df = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_70:Q_84))])   # Technology

# ------------------------------------------------------- #
# Multivariate analysis: people
# ------------------------------------------------------- #

mca_people <- FactoMineR::MCA(X = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_10:Q_18_relaciones))], graph = F)
in_data$Indice_personas <- calc_index(mca_people) %>% as.numeric
in_data$Indice_personas <- (1-in_data$Indice_personas/100)*100

write.csv(mca_knowledge$var$eta2, "mca_correlaciones_conocimientos.csv")

# ------------------------------------------------------- #
# Multivariate analysis: knowledge
# ------------------------------------------------------- #
## Without exclusion criteria
# mca_knowledge <- FactoMineR::MCA(X = in_data %>% dplyr::select(Q_19:Q_31_no_permanece), graph = F)
## With Chi-square exclusion criteria
# sgnf_assc <- independence_analysis(df = in_data %>% dplyr::select(Q_19:Q_31_no_permanece))
# mca_knowledge <- FactoMineR::MCA(X = in_data[,sgnf_assc[[2]]], graph = F)
## With Cronbach alpha criteria
mca_knowledge <- FactoMineR::MCA(X = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_19:Q_31_no_permanece))], graph = F)
in_data$Indice_conocimientos <- calc_index(mca_knowledge) %>% as.numeric
in_data$Indice_conocimientos <- (1-in_data$Indice_conocimientos/100)*100

write.csv(mca_knowledge$var$eta2, "mca_correlaciones_conocimientos.csv")

# plot_mca(res.mca = mca_knowledge)
# mca_knowledge %>% fviz_mca_ind(col.ind = "cos2",
#                                ggtheme = theme_bw(),
#                                habillage = in_data$Clasificacion2,
#                                repel = TRUE) # select.ind = list(cos2 = 0.4)

# ------------------------------------------------------- #
# Multivariate analysis: organization
# ------------------------------------------------------- #
## Without exclusion criteria
# mca_organization <- FactoMineR::MCA(X = in_data %>% dplyr::select(Q_32:Q_62_rutinaria), graph = F)
## With Chi-square exclusion criteria
# sgnf_assc <- independence_analysis(df = in_data %>% dplyr::select(Q_32:Q_62_rutinaria))
# mca_organization <- FactoMineR::MCA(X = in_data[,sgnf_assc[[2]]], graph = F) # Q_44
## With Cronbach alpha criteria
mca_organization <- FactoMineR::MCA(X = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_32:Q_62_rutinaria))], graph = F) # Q_44
in_data$Indice_organizacion <- calc_index(mca_organization) %>% as.numeric
in_data$Indice_organizacion <- (1-in_data$Indice_organizacion/100)*100

write.csv(mca_organization$var$eta2, "mca_correlaciones_organizacion.csv")

# plot_mca(res.mca = mca_organization)

# ------------------------------------------------------- #
# Multivariate analysis: management
# ------------------------------------------------------- #
## Without exclusion criteria
# mca_management <- FactoMineR::MCA(X = in_data %>% dplyr::select(Q_63:Q_69_redes_sociales), graph = T)
## With Chi-square exclusion criteria
# sgnf_assc <- independence_analysis(df = in_data %>% dplyr::select(Q_63:Q_69_redes_sociales))
# mca_management <- FactoMineR::MCA(X = in_data[,sgnf_assc[[2]]], graph = F)
## With Cronbach alpha criteria
mca_management <- FactoMineR::MCA(X = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_63:Q_69_redes_sociales))], graph = F)
in_data$Indice_gestion <- calc_index(mca_management) %>% as.numeric
in_data$Indice_gestion <- (1-in_data$Indice_gestion/100)*100

write.csv(mca_management$var$eta2, "mca_correlaciones_gestion.csv")

# plot_mca(res.mca = mca_management)

# ------------------------------------------------------- #
# Multivariate analysis: technology
# ------------------------------------------------------- #
## Without exclusion criteria
# mca_technology <- FactoMineR::MCA(X = in_data %>% dplyr::select(Q_70:Q_84), graph = T) # Q_76_educacion
## With Chi-square exclusion criteria
# sgnf_assc <- independence_analysis(df = in_data %>% dplyr::select(Q_70:Q_84))
# mca_technology <- FactoMineR::MCA(X = in_data[,sgnf_assc[[2]]], graph = F) # Q_76_educacion
## With Cronbach alpha criteria
mca_technology <- FactoMineR::MCA(X = in_data[,calc_cronbach(df = in_data %>% dplyr::select(Q_70:Q_84))], graph = F) # Q_76_educacion
in_data$Indice_tecnologia <- calc_index(mca_technology) %>% as.numeric
in_data$Indice_tecnologia <- (1-in_data$Indice_tecnologia/100)*100

write.csv(mca_technology$var$eta2, "mca_correlaciones_tecnologia.csv")

in_data$Indice_general <- rowMeans(in_data %>% dplyr::select(Indice_conocimientos:Indice_tecnologia), na.rm = T)

rank(in_data$Indice_general)

# plot_mca(res.mca = mca_technology)

pairs(in_data %>% dplyr::select(Indice_conocimientos:Indice_tecnologia), pch = 20)

in_data %>% ggplot(aes(x = Clasificacion2, y = Indice_tecnologia)) + geom_boxplot()

indices <- in_data %>% dplyr::select(Q_89, Indice_conocimientos:Indice_tecnologia)
indices <- indices %>% tidyr::gather(key = Label, value = Indice_conocimientos:Indice_tecnologia, -Q_89)
names(indices)[3] <- "Value"
indices <- indices %>% tidyr::spread(key = Q_89, value = Value)
chartJSRadar(scores = indices)

test <- mca_knowledge$ind$coord %>% as.data.frame()
test$Clasificacion2 <- in_data$Clasificacion2
test$Empresa <- in_data$Q_89

best_companies <- test %>% dplyr::filter(`Dim 1` < 0 & `Dim 2` < 0)
table(best_companies$Clasificacion2)

test$average <- (test$`Dim 1` + test$`Dim 2`)/2

test %>% ggplot(aes(x = Clasificacion2, y = average)) + geom_boxplot()
