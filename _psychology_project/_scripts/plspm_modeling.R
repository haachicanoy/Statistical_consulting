# Practicas innovadoras - Proyecto Univalle Psicologia: modelamiento de rutas
# H. Achicanoy
# 2019

# R options
options(warn = -1, scipen = 999)

# Load packages
suppressMessages(library(plspm))

## Just to know the structure
# in_data %>% dplyr::select(Q_10:Q_18_relaciones)     # People
# in_data %>% dplyr::select(Q_19:Q_31_no_permanece)   # Knowledge
# in_data %>% dplyr::select(Q_32:Q_62_rutinaria)      # Organization
# in_data %>% dplyr::select(Q_63)                     # Management
# in_data %>% dplyr::select(Q_65:Q_69_redes_sociales) # Knowledge management
# in_data %>% dplyr::select(Q_70:Q_80)                # Technology
# in_data %>% dplyr::select(Q_81:Q_84)                # Innovation

## Transforming factor to numeric for all variables of interest
indx           <- sapply(in_data, is.factor)
in_data2       <- in_data
in_data2[indx] <- lapply(in_data2[indx], function(x) as.numeric(x))

## Defining the relationships in the model
Personas             <- rep(0, 7)
Conocimiento         <- rep(0, 7)
Organizacion         <- rep(0, 7)
Gestion              <- rep(0, 7)
Tecnologia           <- rep(0, 7)
Gestion_conocimiento <- c(1, 1, 1, 1, 1, 0, 0)
Innovacion           <- c(0, 0, 0, 0, 0, 1, 0)
inv_path             <- rbind(Personas, Conocimiento, Organizacion, Gestion, Tecnologia, Gestion_conocimiento, Innovacion)
colnames(inv_path)   <- rownames(inv_path)

## Looking the structural model
plspm::innerplot(inv_path)

## Defining blocks
inv_blocks  <- list(8:17,    # Personas
                    18:47,   # Conocimiento
                    48:85,   # Organizacion
                    86,      # Gestion
                    105:114, # Tecnologia
                    87:104,  # Gestion del conocimiento
                    115:117) # Innovacion

## Defining the type of scaling parameter for each variable
inv_scaling <- list(c(rep("NOM", 2), rep("ORD", 4), rep("NOM", 4)), # Personas
                    c(rep("ORD", 9), rep("NOM", 21)), # Conocimiento
                    c(rep("NOM", 7), rep("ORD", 13), rep("NOM", 18)), # Organizacion
                    c("NOM"), # Gestion
                    c(rep("NOM", 6), rep("ORD", 4)), # Tecnologia
                    c(rep("ORD", 3), rep("NOM", 15)), # Gestion del conocimiento
                    c("NOM", rep("ORD", 2)) # Innovacion
                    )

## Running the models
# Formative mode
inv_pls1 <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                         modes = rep("B", 7), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

# Reflective mode
inv_pls2 <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                         modes = c(rep("A", 7)), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

# newA mode
inv_pls3 <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                         modes = c(rep("newA", 7)), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

# PLScore mode
inv_pls4 <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                         modes = c(rep("PLScore", 7)), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

# PLScow mode
inv_pls5 <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                         modes = c(rep("PLScow", 7)), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

## Plotting results from model
plot_res <- function(plsmod = inv_pls2){
  
  df <- plsmod$scores %>% base::as.data.frame()
  df$Class <- in_data$Clasificacion
  
  gg <- df %>% ggplot2::ggplot(aes(x = Gestion_conocimiento, y = Innovacion, colour = Class)) +
    geom_point()
  
  return(print(gg))
}
plot_res(inv_pls2)
cor(inv_pls2$scores, method = "pearson") %>% corrplot.mixed(., lower = "number", upper = "square", tl.cex )

## Jackknifing the model. Removing one company at the time
models <- lapply(1:nrow(in_data2), function(i){
  tryCatch(expr={
    fit <- plspm::plspm(in_data2[-i,], inv_path, inv_blocks, scaling = inv_scaling,
                 modes = c(rep("A", 7)), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)
  },
  error=function(e){
    cat(paste0("Modeling process failed for company: ", i,"\n"))
    return("Done\n")
  })
  if(exists('fit')){
    return(fit)
  } else {
    cat("Model was not fitted\n")
  }
})
models <- models %>% purrr::compact()

models %>% purrr::map(function(x) x$path_coefs[7,6]) %>% unlist %>% sd

inv_pls
inv_hclus = hclust(dist(inv_pls2$scores), method = "ward.D")

plot(inv_hclus, xlab = "", sub = "", cex = 0.8)
abline(h = 15, col = "#bc014655", lwd = 4)

clusters = cutree(inv_hclus, k = 3)
table(clusters)

inv_scores = as.data.frame(inv_pls2$scores)
inv_scores$Cluster = as.factor(clusters)

head(inv_scores, n = 5)

library(plyr)
centroids = ddply(inv_scores,
                  .(Cluster),
                  summarise,
                  AvgPersonas = mean(Personas),
                  AvgConocimiento = mean(Conocimiento),
                  AvgOrganizacion = mean(Organizacion),
                  AvgGestion = mean(Gestion),
                  AvgTecnologia = mean(Tecnologia),
                  AvgGestionCono = mean(Gestion_conocimiento),
                  AvgInnovacion = mean(Innovacion))

inv_rebus = rebus.pls(inv_pls2)
