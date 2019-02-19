in_data %>% dplyr::select(Q_10:Q_18_relaciones)     # People
in_data %>% dplyr::select(Q_19:Q_31_no_permanece)   # Knowledge
in_data %>% dplyr::select(Q_32:Q_62_rutinaria)      # Organization
in_data %>% dplyr::select(Q_63)                     # Management
in_data %>% dplyr::select(Q_65:Q_69_redes_sociales) # Knowledge management
in_data %>% dplyr::select(Q_70:Q_80)                # Technology
in_data %>% dplyr::select(Q_81:Q_84)                # Innovation

indx <- sapply(in_data, is.factor)
in_data2 <- in_data
in_data2[indx] <- lapply(in_data2[indx], function(x) as.numeric(x))

Personas     <- rep(0, 7)
Conocimiento <- rep(0, 7)
Organizacion <- rep(0, 7)
Gestion      <- rep(0, 7)
Tecnologia   <- rep(0, 7)
Gestion_conocimiento <- c(1, 1, 1, 1, 1, 0, 0)
Innovacion           <- c(0, 0, 0, 0, 0, 1, 0)
inv_path     <- rbind(Personas, Conocimiento, Organizacion, Gestion, Tecnologia, Gestion_conocimiento, Innovacion)
# add optional column names
colnames(inv_path) <- rownames(inv_path)
plspm::innerplot(inv_path)

inv_modes  <- c(rep("B", 7))

inv_blocks  <- list(8:17,    # Personas
                    18:47,   # Conocimiento
                    48:85,   # Organizacion
                    86,      # Gestion
                    105:114, # Tecnologia
                    87:104,  # Gestion del conocimiento
                    115:117) # Innovacion

inv_scaling <- list(c(rep("NOM", 2), rep("ORD", 4), rep("NOM", 4)), # Personas
                    c(rep("ORD", 9), rep("NOM", 21)), # Conocimiento
                    c(rep("NOM", 7), rep("ORD", 13), rep("NOM", 18)), # Organizacion
                    c("NOM"), # Gestion
                    c(rep("NOM", 6), rep("ORD", 4)), # Tecnologia
                    c(rep("ORD", 3), rep("NOM", 15)), # Gestion del conocimiento
                    c("NOM", rep("ORD", 2)) # Innovacion
                    )

# PLS-PM
inv_pls <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                 modes = inv_modes, scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

inv_pls2 <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                 modes = c(rep("A", 7)), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

inv_pls3 <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                         modes = c(rep("newA", 7)), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

inv_pls4 <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                         modes = c(rep("PLScore", 7)), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

inv_pls5 <- plspm::plspm(in_data2, inv_path, inv_blocks, scaling = inv_scaling,
                         modes = c(rep("PLScow", 7)), scheme = "path", plscomp = rep(1, 7), tol = 0.0000001)

# Jackknife
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





coefs <- NA
for(i in 1:length(models)){
  coefs[i] <- models[[i]]$path_coefs[3,2]
}

plot(inv_pls)

data(russett)

# path matrix (inner model realtionships)
AGRIN = c(0, 0, 0)
INDEV = c(0, 0, 0)
POLINS = c(1, 1, 0)
rus_path = rbind(AGRIN, INDEV, POLINS)
# add optional column names
colnames(rus_path) = rownames(rus_path)

# list indicating what variables are associated with what latent variables
rus_blocks = list(1:3, 4:5, 6:11)

# all latent variables are measured in a reflective way
rus_modes = rep("A", 3)

# run plspm analysis
rus_pls = plspm(russett, rus_path, rus_blocks, modes = rus_modes)

models <- lapply(1:nrow(russett), function(i){
  fit <- plspm(russett[-i,], rus_path, rus_blocks, modes = rus_modes)
})

coefs <- NA
for(i in 1:length(models)){
  coefs[i] <- models[[i]]$path_coefs[3,2]
}
