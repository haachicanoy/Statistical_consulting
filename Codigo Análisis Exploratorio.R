
------------------------------------------------------------
############## ANÁLISIS EXPLORATORIO DE DATOS ##############
------------------------------------------------------------

#### Exportación de Datos

mdata = read.csv("C:/Users/Fay/Dropbox/Consultoria Claudia/iniData.csv", header=T, sep=",")

#mdata = read.csv("C:/Users/Harold/Documents/iniData.csv", header=T, sep=",")

attach(mdata) ; class(mdata)

head(mdata)

names(mdata)

library('ggplot2')

#### Grafico general sin considerar los tramos para la variable pendiente en el tiempo

plot(periodo, pendiente) ## pendiente por periodo

plot(pendiente ~ periodo)

plot(pendiente ~ tramo)  ## pendiente por tramo 

------------------------------------------------------------------------------------
#### Graficos para analizar tendencias en el tiempo en cada variable por tramo ####
------------------------------------------------------------------------------------       
  
library(zoo)
library(reshape)
library(ggplot2)
library(fitdistrplus)
library(gridExtra)
library(plyr)

#### Graficas Variables Morfologicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 7 graficos por variable morfologica. 

m1 = qplot(periodo, pendiente, data = mdata, colour = as.factor(tramo), geom = "line")
m1 = m1 + scale_x_continuous(breaks=1:6)
m1 = m1 + labs(colour='Tramo')
m1 = m1 + xlab('Decada') + ylab('Pendiente del cauce')

m2 = qplot(periodo, ancho_banca, data = mdata, colour = as.factor(tramo), geom = "line")
m2 = m2 + scale_x_continuous(breaks=1:6)
m2 = m2 + labs(colour='Tramo')
m2 = m2 + xlab('Decada') + ylab('Ancho a banca llena del cauce')

m3 = qplot(periodo, sinuosidad, data = mdata, colour = as.factor(tramo), geom = "line")
m3 = m3 + scale_x_continuous(breaks=1:6)
m3 = m3 + labs(colour='Tramo')
m3 = m3 + xlab('Decada') + ylab('Índice de sinuosidad')

m4 = qplot(periodo, amp_meandros, data = mdata, colour = as.factor(tramo), geom = "line")
m4 = m4 + scale_x_continuous(breaks=1:6)
m4 = m4 + labs(colour='Tramo')
m4 = m4 + xlab('Decada') + ylab('Amplitud de meandros')

m5 = qplot(periodo, long_meandros, data = mdata, colour = as.factor(tramo), geom = "line")
m5 = m5 + scale_x_continuous(breaks=1:6)
m5 = m5 + labs(colour='Tramo')
m5 = m5 + xlab('Decada') + ylab('Longitud de onda de meandros')

m6 = qplot(periodo, t_movilidad, data = mdata, colour = as.factor(tramo), geom = "line")
m6 = m6 + scale_x_continuous(breaks=1:6)
m6 = m6 + labs(colour='Tramo')
m6 = m6 + xlab('Decada') + ylab('Tasa de movilidad')

m7 = qplot(periodo, t_movilidad_max, data = mdata, colour = as.factor(tramo), geom = "line")
m7 = m7 + scale_x_continuous(breaks=1:6)
m7 = m7 + labs(colour='Tramo')
m7 = m7 + xlab('Decada') + ylab('Tasa de movilidad máxima')

#multiplot(v1, v2)

grid.arrange(m1, m2, m3, m4)
grid.arrange(m1, m5, m6, m7)


#### Graficas Variables Hidrológicas e Hidráulicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 6 graficos por variable HH.

h1 = qplot(periodo, caudal_medio, data = mdata, colour = as.factor(tramo), geom = "line")
h1 = h1 + scale_x_continuous(breaks=1:6)
h1 = h1 + labs(colour='Tramo')
h1 = h1 + xlab('Decada') + ylab('Caudal medio multianual') 

h2 = qplot(periodo, caudal_maximo, data = mdata, colour = as.factor(tramo), geom = "line")
h2 = h2 + scale_x_continuous(breaks=1:6)
h2 = h2 + labs(colour='Tramo')
h2 = h2 + xlab('Decada') + ylab('Caudal máximo histórico')

h3 = qplot(periodo, caudal_50, data = mdata, colour = as.factor(tramo), geom = "line")
h3 = h3 + scale_x_continuous(breaks=1:6)
h3 = h3 + labs(colour='Tramo')
h3 = h3 + xlab('Decada') + ylab('Caudal 50 años período retorno')

h4 = qplot(periodo, caudal_banca, data = mdata, colour = as.factor(tramo), geom = "line")
h4 = h4 + scale_x_continuous(breaks=1:6)
h4 = h4 + labs(colour='Tramo')
h4 = h4 + xlab('Decada') + ylab('Caudal de banca llena')

h5 = qplot(periodo, velocidad, data = mdata, colour = as.factor(tramo), geom = "line")
h5 = h5 + scale_x_continuous(breaks=1:6)
h5 = h5 + labs(colour='Tramo')
h5 = h5 + xlab('Decada') + ylab('Velocidad para el caudal de banca llena')

h6 = qplot(periodo, ancho_superficial, data = mdata, colour = as.factor(tramo), geom = "line")
h6 = h6 + scale_x_continuous(breaks=1:6)
h6 = h6 + labs(colour='Tramo')
h6 = h6 + xlab('Decada') + ylab('Ancho superficial del flujo para el caudal de banca llena')

grid.arrange(h1, h2, h3, h4, h5, h6)

#### Graficas Variables Sedimentológicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 4 graficos por variable Sedimentologica.

s1 = qplot(periodo, diametro_promedio, data = mdata, colour = as.factor(tramo), geom = "line")
s1 = s1 + scale_x_continuous(breaks=1:6)
s1 = s1 + labs(colour='Tramo')
s1 = s1 + xlab('Decada') + ylab('Diámetro promedio del material del lecho')

s2 = qplot(periodo, dev_granulometrica, data = mdata, colour = as.factor(tramo), geom = "line")
s2 = s2 + scale_x_continuous(breaks=1:6)
s2 = s2 + labs(colour='Tramo')
s2 = s2 + xlab('Decada') + ylab('Desviación típica granulométrica')

s3 = qplot(periodo, coef_uniformidad, data = mdata, colour = as.factor(tramo), geom = "line")
s3 = s3 + scale_x_continuous(breaks=1:6)
s3 = s3 + labs(colour='Tramo')
s3 = s3 + xlab('Decada') + ylab('Coeficiente de uniformidad')

s4 = qplot(periodo, carga_media, data = mdata, colour = as.factor(tramo), geom = "line")
s4 = s4 + scale_x_continuous(breaks=1:6)
s4 = s4 + labs(colour='Tramo')
s4 = s4 + xlab('Decada') + ylab('Carga media anual del material del lecho')

grid.arrange(s1, s2, s3, s4)

--------------------------------------------------------------------------------------
#### Graficos para analizar tendencias en el espacio en cada variable por periodo ####
--------------------------------------------------------------------------------------  

#### Graficas Variables Morfologicas. Detro de cada grafico 6 series por periodo y en total 7 graficos por variable morfologica. 
  
mr1 = qplot(tramo, pendiente, data = mdata, colour = as.factor(periodo), geom = "line")
mr1 = mr1 + scale_x_continuous(breaks=1:6)
mr1 = mr1 + labs(colour='Periodo')
mr1 = mr1 + xlab('Tramo del cauce') + ylab('Pendiente del cauce')

mr2 = qplot(tramo, ancho_banca, data = mdata, colour = as.factor(periodo), geom = "line")
mr2 = mr2 + scale_x_continuous(breaks=1:6)
mr2 = mr2 + labs(colour='Periodo')
mr2 = mr2 + xlab('Tramo del cauce') + ylab('Ancho a banca llena del cauce')

mr3 = qplot(tramo, sinuosidad, data = mdata, colour = as.factor(periodo), geom = "line")
mr3 = mr3 + scale_x_continuous(breaks=1:6)
mr3 = mr3 + labs(colour='Periodo')
mr3 = mr3 + xlab('Tramo del cauce') + ylab('Índice de sinuosidad')

mr4 = qplot(tramo, amp_meandros, data = mdata, colour = as.factor(periodo), geom = "line")
mr4 = mr4 + scale_x_continuous(breaks=1:6)
mr4 = mr4 + labs(colour='Periodo')
mr4 = mr4 + xlab('Tramo del cauce') + ylab('Amplitud de meandros')

mr5 = qplot(tramo, long_meandros, data = mdata, colour = as.factor(periodo), geom = "line")
mr5 = mr5 + scale_x_continuous(breaks=1:6)
mr5 = mr5 + labs(colour='Periodo')
mr5 = mr5 + xlab('Tramo del cauce') + ylab('Longitud de onda de meandros')

mr6 = qplot(tramo, t_movilidad, data = mdata, colour = as.factor(periodo), geom = "line")
mr6 = mr6 + scale_x_continuous(breaks=1:6)
mr6 = mr6 + labs(colour='Periodo')
mr6 = mr6 + xlab('Tramo del cauce') + ylab('Tasa de movilidad')

mr7 = qplot(tramo, t_movilidad_max, data = mdata, colour = as.factor(periodo), geom = "line")
mr7 = mr7 + scale_x_continuous(breaks=1:6)
mr7 = mr7 + labs(colour='Periodo')
mr7 = mr7 + xlab('Tramo del cauce') + ylab('Tasa de movilidad máxima')

#multiplot(v1, v2)

grid.arrange(mr1, mr2, mr3, mr4)
grid.arrange(mr1, mr5, mr6, mr7)

#### Graficas Variables Hidrológicas e Hidráulicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 6 graficos por variable HH.

hr1 = qplot(tramo, caudal_medio, data = mdata, colour = as.factor(periodo), geom = "line")
hr1 = hr1 + scale_x_continuous(breaks=1:6)
hr1 = hr1 + labs(colour='Periodo')
hr1 = hr1 + xlab('Tramo del cauce') + ylab('Caudal medio multianual') 

hr2 = qplot(tramo, caudal_maximo, data = mdata, colour = as.factor(periodo), geom = "line")
hr2 = hr2 + scale_x_continuous(breaks=1:6)
hr2 = hr2 + labs(colour='Periodo')
hr2 = hr2 + xlab('Tramo del cauce') + ylab('Caudal máximo histórico')

hr3 = qplot(tramo, caudal_50, data = mdata, colour = as.factor(periodo), geom = "line")
hr3 = hr3 + scale_x_continuous(breaks=1:6)
hr3 = hr3 + labs(colour='Periodo')
hr3 = hr3 + xlab('Tramo del cauce') + ylab('Caudal 50 años período retorno')

hr4 = qplot(tramo, caudal_banca, data = mdata, colour = as.factor(periodo), geom = "line")
hr4 = hr4 + scale_x_continuous(breaks=1:6)
hr4 = hr4 + labs(colour='Periodo')
hr4 = hr4 + xlab('Tramo del cauce') + ylab('Caudal de banca llena')

hr5 = qplot(tramo, velocidad, data = mdata, colour = as.factor(periodo), geom = "line")
hr5 = hr5 + scale_x_continuous(breaks=1:6)
hr5 = hr5 + labs(colour='Periodo')
hr5 = hr5 + xlab('Tramo del cauce') + ylab('Velocidad para el caudal de banca llena')

hr6 = qplot(tramo, ancho_superficial, data = mdata, colour = as.factor(periodo), geom = "line")
hr6 = hr6 + scale_x_continuous(breaks=1:6)
hr6 = hr6 + labs(colour='Periodo')
hr6 = hr6 + xlab('Tramo del cauce') + ylab('Ancho superficial del flujo para el caudal de banca llena')

grid.arrange(hr1, hr2, hr3, hr4, hr5, hr6)

#### Graficas Variables Sedimentológicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 4 graficos por variable Sedimentologica.

sr1 = qplot(tramo, diametro_promedio, data = mdata, colour = as.factor(periodo), geom = "line")
sr1 = sr1 + scale_x_continuous(breaks=1:6)
sr1 = sr1 + labs(colour='Periodo')
sr1 = sr1 + xlab('Tramo del cauce') + ylab('Diámetro promedio del material del lecho')

sr2 = qplot(tramo, dev_granulometrica, data = mdata, colour = as.factor(periodo), geom = "line")
sr2 = sr2 + scale_x_continuous(breaks=1:6)
sr2 = sr2 + labs(colour='Periodo')
sr2 = sr2 + xlab('Tramo del cauce') + ylab('Desviación típica granulométrica')

sr3 = qplot(tramo, coef_uniformidad, data = mdata, colour = as.factor(periodo), geom = "line")
sr3 = sr3 + scale_x_continuous(breaks=1:6)
sr3 = sr3 + labs(colour='Periodo')
sr3 = sr3 + xlab('Tramo del cauce') + ylab('Coeficiente de uniformidad')

sr4 = qplot(tramo, carga_media, data = mdata, colour = as.factor(periodo), geom = "line")
sr4 = sr4 + scale_x_continuous(breaks=1:6)
sr4 = sr4 + labs(colour='Periodo')
sr4 = sr4 + xlab('Tramo del cauce') + ylab('Carga media anual del material del lecho')

grid.arrange(sr1, sr2, sr3, sr4)




