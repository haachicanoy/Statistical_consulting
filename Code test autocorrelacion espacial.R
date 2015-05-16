
#### Calculo del Indice de Moran para probar autocorrelaci?n espacial ####

#### Exportaci?n de Datos

mdata = read.csv("iniData.csv", header=T)

#install.packages("ape")
library(ape)

attach(mdata) ; class(mdata)

head(mdata)

names(mdata)

# COORDENADAS

coor.tramo = read.table("C:/Users/Fay/Dropbox/Consultoria Claudia/Coordenadas/trans_coordinates.csv", header=T, sep=",")

# Matriz de Distancias 

tramo.dists <- as.matrix(dist(cbind(coor.tramo$Long, coor.tramo$Lat))) 

## Matriz de pesos de distancia inversa

tramo.dists.inv <- 1/tramo.dists
diag(tramo.dists.inv) <- 0

tramo.dists.inv[1:5, 1:5]
tramo.dists.inv

#### TEST DE MORAN POR DECADA Y POR VARIABLE (por ahora con las variables que aparentemente estan completas) ####

#### DECADA 1 ####

dec1 = subset(mdata, periodo=="1")

# Hipotesis nula: No existe correlaci?n espacial 

#Moran.I(dec1$pendiente, tramo.dists.inv)$p.value   # datos missings

#Moran.I(dec1$ancho_banca, tramo.dists.inv)$p.value  # datos missings

p1.3 = Moran.I(dec1$sinuosidad, tramo.dists.inv, na.rm = T)$p.value   #por lo menos en esta variable no existe correlaci?n espacial

p1.4 = Moran.I(dec1$amp_meandros, tramo.dists.inv)$p.value

p1.5 = Moran.I(dec1$long_meandros, tramo.dists.inv)$p.value  # existe

p1.6 = Moran.I(dec1$t_movilidad, tramo.dists.inv)$p.value

p1.7 = Moran.I(dec1$t_movilidad_max, tramo.dists.inv)$p.value

#Moran.I(dec1$caudal_medio, tramo.dists.inv)$p.value  # datos missings

#Moran.I(dec1$caudal_maximo, tramo.dists.inv)$p.value

#Moran.I(dec1$caudal_50, tramo.dists.inv)$p.value

#Moran.I(dec1$caudal_banca, tramo.dists.inv)$p.value

#Moran.I(dec1$velocidad, tramo.dists.inv)$p.value

#Moran.I(dec1$ancho_superficial, tramo.dists.inv)$p.value

p1.14 = Moran.I(dec1$diametro_promedio, tramo.dists.inv)$p.value

p1.15 = Moran.I(dec1$dev_granulometrica, tramo.dists.inv)$p.value

p1.16 = Moran.I(dec1$coef_uniformidad, tramo.dists.inv)$p.value

#Moran.I(dec1$carga_media, tramo.dists.inv)$p.value # datos missings

#### DECADA 2 ####

dec2 = subset(mdata, periodo=="2")

# Hipotesis nula: No existe correlaci?n espacial 


p2.1 = Moran.I(dec2$pendiente, tramo.dists.inv)$p.value   

p2.2 = Moran.I(dec2$ancho_banca, tramo.dists.inv)$p.value  

p2.3 = Moran.I(dec2$sinuosidad, tramo.dists.inv)$p.value   

p2.4 = Moran.I(dec2$amp_meandros, tramo.dists.inv)$p.value

p2.5 = Moran.I(dec2$long_meandros, tramo.dists.inv)$p.value 

p2.6 = Moran.I(dec2$t_movilidad, tramo.dists.inv)$p.value

p2.7 = Moran.I(dec2$t_movilidad_max, tramo.dists.inv)$p.value

p2.8 = Moran.I(dec2$caudal_medio, tramo.dists.inv)$p.value  

p2.9 = Moran.I(dec2$caudal_maximo, tramo.dists.inv)$p.value

p2.10 = Moran.I(dec2$caudal_50, tramo.dists.inv)$p.value

p2.11 = Moran.I(dec2$caudal_banca, tramo.dists.inv)$p.value

p2.12 = Moran.I(dec2$velocidad, tramo.dists.inv)$p.value

p2.13 = Moran.I(dec2$ancho_superficial, tramo.dists.inv)$p.value

p2.14 = Moran.I(dec2$diametro_promedio, tramo.dists.inv)$p.value

p2.15 = Moran.I(dec2$dev_granulometrica, tramo.dists.inv)$p.value

p2.16 = Moran.I(dec2$coef_uniformidad, tramo.dists.inv)$p.value

p2.17 = Moran.I(dec2$carga_media, tramo.dists.inv)$p.value 

#### DECADA 3 ####

dec3 = subset(mdata, periodo=="3")

# Hipotesis nula: No existe correlaci?n espacial 

p3.1 = Moran.I(dec3$pendiente, tramo.dists.inv)$p.value   

p3.2 = Moran.I(dec3$ancho_banca, tramo.dists.inv)$p.value  

p3.3 = Moran.I(dec3$sinuosidad, tramo.dists.inv)$p.value   

p3.4 = Moran.I(dec3$amp_meandros, tramo.dists.inv)$p.value

p3.5 = Moran.I(dec3$long_meandros, tramo.dists.inv)$p.value 

p3.6 = Moran.I(dec3$t_movilidad, tramo.dists.inv)$p.value

p3.7 = Moran.I(dec3$t_movilidad_max, tramo.dists.inv)$p.value

p3.8 = Moran.I(dec3$caudal_medio, tramo.dists.inv)$p.value  

p3.9 = Moran.I(dec3$caudal_maximo, tramo.dists.inv)$p.value

p3.10 = Moran.I(dec3$caudal_50, tramo.dists.inv)$p.value

p3.11 = Moran.I(dec3$caudal_banca, tramo.dists.inv)$p.value

p3.12 = Moran.I(dec3$velocidad, tramo.dists.inv)$p.value

p3.13 = Moran.I(dec3$ancho_superficial, tramo.dists.inv)$p.value

p3.14 = Moran.I(dec3$diametro_promedio, tramo.dists.inv)$p.value

p3.15 = Moran.I(dec3$dev_granulometrica, tramo.dists.inv)$p.value

p3.16 = Moran.I(dec3$coef_uniformidad, tramo.dists.inv)$p.value

p3.17 = Moran.I(dec3$carga_media, tramo.dists.inv)$p.value 

#### DECADA 4 ####

dec4 = subset(mdata, periodo=="4")

# Hipotesis nula: No existe correlaci?n espacial 

p4.1 = Moran.I(dec4$pendiente, tramo.dists.inv)$p.value   

p4.2 = Moran.I(dec4$ancho_banca, tramo.dists.inv)$p.value  

p4.3 = Moran.I(dec4$sinuosidad, tramo.dists.inv)$p.value   

p4.4 = Moran.I(dec4$amp_meandros, tramo.dists.inv)$p.value

p4.5 = Moran.I(dec4$long_meandros, tramo.dists.inv)$p.value 

p4.6 = Moran.I(dec4$t_movilidad, tramo.dists.inv)$p.value

p4.7 = Moran.I(dec4$t_movilidad_max, tramo.dists.inv)$p.value

p4.8 = Moran.I(dec4$caudal_medio, tramo.dists.inv)$p.value  

p4.9 = Moran.I(dec4$caudal_maximo, tramo.dists.inv)$p.value

p4.10 = Moran.I(dec4$caudal_50, tramo.dists.inv)$p.value

p4.11 = Moran.I(dec4$caudal_banca, tramo.dists.inv)$p.value

p4.12 = Moran.I(dec4$velocidad, tramo.dists.inv)$p.value

p4.13 = Moran.I(dec4$ancho_superficial, tramo.dists.inv)$p.value

p4.14 = Moran.I(dec4$diametro_promedio, tramo.dists.inv)$p.value

p4.15 = Moran.I(dec4$dev_granulometrica, tramo.dists.inv)$p.value

p4.16 = Moran.I(dec4$coef_uniformidad, tramo.dists.inv)$p.value

p4.17 = Moran.I(dec4$carga_media, tramo.dists.inv)$p.value 

#### DECADA 5 ####

dec5 = subset(mdata, periodo=="5")

# Hipotesis nula: No existe correlaci?n espacial 

p5.1 = Moran.I(dec5$pendiente, tramo.dists.inv)$p.value   

p5.2 = Moran.I(dec5$ancho_banca, tramo.dists.inv)$p.value  

p5.3 = Moran.I(dec5$sinuosidad, tramo.dists.inv)$p.value   

p5.4 = Moran.I(dec5$amp_meandros, tramo.dists.inv)$p.value

p5.5 = Moran.I(dec5$long_meandros, tramo.dists.inv)$p.value 

p5.6 = Moran.I(dec5$t_movilidad, tramo.dists.inv)$p.value

p5.7 = Moran.I(dec5$t_movilidad_max, tramo.dists.inv)$p.value

p5.8 = Moran.I(dec5$caudal_medio, tramo.dists.inv)$p.value  

p5.9 = Moran.I(dec5$caudal_maximo, tramo.dists.inv)$p.value

p5.10 = Moran.I(dec5$caudal_50, tramo.dists.inv)$p.value

p5.11 = Moran.I(dec5$caudal_banca, tramo.dists.inv)$p.value

p5.12 = Moran.I(dec5$velocidad, tramo.dists.inv)$p.value

p5.13 = Moran.I(dec5$ancho_superficial, tramo.dists.inv)$p.value

p5.14 = Moran.I(dec5$diametro_promedio, tramo.dists.inv)$p.value

p5.15 = Moran.I(dec5$dev_granulometrica, tramo.dists.inv)$p.value

p5.16 = Moran.I(dec5$coef_uniformidad, tramo.dists.inv)$p.value

p5.17 = Moran.I(dec5$carga_media, tramo.dists.inv)$p.value 

#### DECADA 6 ####

dec6 = subset(mdata, periodo=="6")

# Hipotesis nula: No existe correlaci?n espacial 

p6.1 = Moran.I(dec6$pendiente, tramo.dists.inv)$p.value   

p6.2 = Moran.I(dec6$ancho_banca, tramo.dists.inv)$p.value  

p6.3 = Moran.I(dec6$sinuosidad, tramo.dists.inv)$p.value   

p6.4 = Moran.I(dec6$amp_meandros, tramo.dists.inv)$p.value

p6.5 = Moran.I(dec6$long_meandros, tramo.dists.inv)$p.value 

p6.6 = Moran.I(dec6$t_movilidad, tramo.dists.inv)$p.value

p6.7 = Moran.I(dec6$t_movilidad_max, tramo.dists.inv)$p.value

p6.8 = Moran.I(dec6$caudal_medio, tramo.dists.inv)$p.value  

p6.9 = Moran.I(dec6$caudal_maximo, tramo.dists.inv)$p.value

p6.10 = Moran.I(dec6$caudal_50, tramo.dists.inv)$p.value

p6.11 = Moran.I(dec6$caudal_banca, tramo.dists.inv)$p.value

p6.12 = Moran.I(dec6$velocidad, tramo.dists.inv)$p.value

p6.13 = Moran.I(dec6$ancho_superficial, tramo.dists.inv)$p.value

p6.14 = Moran.I(dec6$diametro_promedio, tramo.dists.inv)$p.value

p6.15 = Moran.I(dec6$dev_granulometrica, tramo.dists.inv)$p.value

p6.16 = Moran.I(dec6$coef_uniformidad, tramo.dists.inv)$p.value

p6.17 = Moran.I(dec6$carga_media, tramo.dists.inv)$p.value 

pvs = c(NA, NA, p1.3, p1.4, p1.5, p1.6, p1.7, NA, NA, NA, NA, NA, NA, p1.14, p1.15, p1.16, NA,
        p2.1, p2.2, p2.3, p2.4, p2.5, p2.6, p2.7, p2.8, p2.9, p2.10, p2.11, p2.12, p2.13, p2.14, p2.15, p2.16, p2.17,
        p3.1, p3.2, p3.3, p3.4, p3.5, p3.6, p3.7, p3.8, p3.9, p3.10, p3.11, p3.12, p3.13, p3.14, p3.15, p3.16, p3.17,
        p4.1, p4.2, p4.3, p4.4, p4.5, p4.6, p4.7, p4.8, p4.9, p4.10, p4.11, p4.12, p4.13, p4.14, p4.15, p4.16, p4.17,
        p5.1, p5.2, p5.3, p5.4, p5.5, p5.6, p5.7, p5.8, p5.9, p5.10, p5.11, p5.12, p5.13, p5.14, p5.15, p5.16, p5.17,
        p6.1, p6.2, p6.3, p6.4, p6.5, p6.6, p6.7, p6.8, p6.9, p6.10, p6.11, p6.12, p6.13, p6.14, p6.15, p6.16, p6.17)

pvs = round(pvs,4)

## Matriz de p - valores del test de Moran. tama?o 6 periodos por 17 variables 

mp = matrix(pvs, nrow = 6, ncol = 17, byrow = TRUE, dimnames = list(c("D1", "D2", "D3","D4", "D5", "D6"),
                                                                c("Pendiente", "ancho_banca", "sinuosidad", "amp_meandros", "long_meandros", "t_movilidad", "t_movilidad_max", "caudal_medio", "caudal_maximo", "caudal_50", "caudal_banca", "velocidad", "ancho_superficial", "diametro_promedio", "dev_granulometrica", "coef_uniformidad", "carga_media")))
                                                          

tmp =t(mp)

View(tmp)

# Hi world!

gffhfh




