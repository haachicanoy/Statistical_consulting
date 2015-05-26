# Exploratory analysis
# M. López & H. Achicanoy
# 2015

mdata <- read.csv("iniData.csv")
attach(mdata) ; class(mdata)

View(mdata)

library('ggplot2')

# ------------------------------------------------------------------------------------ #
# Grafico general sin considerar los tramos para la variable pendiente en el tiempo
# ------------------------------------------------------------------------------------ #

plot(periodo, pendiente) ## pendiente por periodo
plot(pendiente ~ periodo)
plot(pendiente ~ tramo)  ## pendiente por tramo

# ------------------------------------------------------------------------------------ #
# Graficos para analizar tendencias en el tiempo en cada variable por tramo
# ------------------------------------------------------------------------------------ #

library(zoo)
library(reshape)
library(ggplot2)
library(fitdistrplus)
library(gridExtra)
library(plyr)
library(grid)

# Graficas Variables Morfologicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 7 graficos por variable morfologica. 

m1 = qplot(periodo, pendiente, data = mdata, colour = as.factor(tramo))
m1 = m1 + geom_point(size=4)
m1 = m1 + geom_line(linetype = "dashed", size=1)
m1 = m1 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                      labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
m1 = m1 + labs(colour='Tramo')
m1 = m1 + xlab('Decada') + ylab(expression('S'[0]))
m1 = m1 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
m1 = m1 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
m1 = m1 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
m1 = m1 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
m1 = m1 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
m1 = m1 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


m2 = qplot(periodo, ancho_banca, data = mdata, colour = as.factor(tramo))
m2 = m2 + geom_point(size=4)
m2 = m2 + geom_line(linetype = "dashed", size=1)
m2 = m2 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
m2 = m2 + labs(colour='Tramo')
m2 = m2 + xlab('Decada') + ylab('B')
m2 = m2 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
m2 = m2 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
m2 = m2 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
m2 = m2 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
m2 = m2 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
m2 = m2 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


m3 = qplot(periodo, sinuosidad, data = mdata, colour = as.factor(tramo))
m3 = m3 + geom_point(size=4)
m3 = m3 + geom_line(linetype = "dashed", size=1)
m3 = m3 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
m3 = m3 + labs(colour='Tramo')
m3 = m3 + xlab('Decada') + ylab('IS')
m3 = m3 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
m3 = m3 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
m3 = m3 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
m3 = m3 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
m3 = m3 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
m3 = m3 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


m4 = qplot(periodo, amp_meandros, data = mdata, colour = as.factor(tramo))
m4 = m4 + geom_point(size=4)
m4 = m4 + geom_line(linetype = "dashed", size=1)
m4 = m4 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
m4 = m4 + labs(colour='Tramo')
m4 = m4 + xlab('Decada') + ylab(expression('M'[b]))
m4 = m4 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
m4 = m4 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
m4 = m4 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
m4 = m4 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
m4 = m4 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
m4 = m4 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


m5 = qplot(periodo, long_meandros, data = mdata, colour = as.factor(tramo))
m5 = m5 + geom_point(size=4)
m5 = m5 + geom_line(linetype = "dashed", size=1)
m5 = m5 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
m5 = m5 + labs(colour='Tramo')
m5 = m5 + xlab('Decada') + ylab(expression(lambda))
m5 = m5 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
m5 = m5 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
m5 = m5 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
m5 = m5 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
m5 = m5 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
m5 = m5 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


m6 = qplot(periodo, t_movilidad, data = mdata, colour = as.factor(tramo))
m6 = m6 + geom_point(size=4)
m6 = m6 + geom_line(linetype = "dashed", size=1)
m6 = m6 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
m6 = m6 + labs(colour='Tramo')
m6 = m6 + xlab('Decada') + ylab(expression('T'[m]))
m6 = m6 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
m6 = m6 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
m6 = m6 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
m6 = m6 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
m6 = m6 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
m6 = m6 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


m7 = qplot(periodo, t_movilidad_max, data = mdata, colour = as.factor(tramo))
m7 = m7 + geom_point(size=4)
m7 = m7 + geom_line(linetype = "dashed", size=1)
m7 = m7 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
m7 = m7 + labs(colour='Tramo')
m7 = m7 + xlab('Decada') + ylab(expression('T'[mmax]))
m7 = m7 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
m7 = m7 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
m7 = m7 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
m7 = m7 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
m7 = m7 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
m7 = m7 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))

grid.arrange(m1, m2, m3, m4)
grid.arrange(m5, m6, m7)

# Graficas Variables Hidrologicas e Hidraulicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 6 graficos por variable HH.

h1 = qplot(periodo, caudal_medio, data = mdata, colour = as.factor(tramo))
h1 = h1 + geom_point(size=4)
h1 = h1 + geom_line(linetype = "dashed", size=1)
h1 = h1 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
h1 = h1 + labs(colour='Tramo')
h1 = h1 + xlab('Decada') + ylab(expression('Q'[m]))
h1 = h1 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
h1 = h1 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
h1 = h1 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
h1 = h1 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
h1 = h1 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
h1 = h1 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


h2 = qplot(periodo, caudal_maximo, data = mdata, colour = as.factor(tramo))
h2 = h2 + geom_point(size=4)
h2 = h2 + geom_line(linetype = "dashed", size=1)
h2 = h2 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
h2 = h2 + labs(colour='Tramo')
h2 = h2 + xlab('Decada') + ylab(expression('Q'[max]))
h2 = h2 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
h2 = h2 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
h2 = h2 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
h2 = h2 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
h2 = h2 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
h2 = h2 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


h3 = qplot(periodo, caudal_50, data = mdata, colour = as.factor(tramo))
h3 = h3 + geom_point(size=4)
h3 = h3 + geom_line(linetype = "dashed", size=1)
h3 = h3 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
h3 = h3 + labs(colour='Tramo')
h3 = h3 + xlab('Decada') + ylab(expression('Q'[50]))
h3 = h3 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
h3 = h3 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
h3 = h3 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
h3 = h3 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
h3 = h3 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
h3 = h3 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


h4 = qplot(periodo, caudal_banca, data = mdata, colour = as.factor(tramo))
h4 = h4 + geom_point(size=4)
h4 = h4 + geom_line(linetype = "dashed", size=1)
h4 = h4 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
h4 = h4 + labs(colour='Tramo')
h4 = h4 + xlab('Decada') + ylab(expression('Q'[bl]))
h4 = h4 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
h4 = h4 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
h4 = h4 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
h4 = h4 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
h4 = h4 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
h4 = h4 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


h5 = qplot(periodo, velocidad, data = mdata, colour = as.factor(tramo))
h5 = h5 + geom_point(size=4)
h5 = h5 + geom_line(linetype = "dashed", size=1)
h5 = h5 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
h5 = h5 + labs(colour='Tramo')
h5 = h5 + xlab('Decada') + ylab(expression('V'))
h5 = h5 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
h5 = h5 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
h5 = h5 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
h5 = h5 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
h5 = h5 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
h5 = h5 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


h6 = qplot(periodo, ancho_superficial, data = mdata, colour = as.factor(tramo))
h6 = h6 + geom_point(size=4)
h6 = h6 + geom_line(linetype = "dashed", size=1)
h6 = h6 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
h6 = h6 + labs(colour='Tramo')
h6 = h6 + xlab('Decada') + ylab(expression('B'[s]))
h6 = h6 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
h6 = h6 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
h6 = h6 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
h6 = h6 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
h6 = h6 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
h6 = h6 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))

grid.arrange(h1, h2, h3, h4, h5, h6)

# Graficas Variables Sedimentologicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 4 graficos por variable Sedimentologica.

s1 = qplot(periodo, diametro_promedio, data = mdata, colour = as.factor(tramo))
s1 = s1 + geom_point(size=4)
s1 = s1 + geom_line(linetype = "dashed", size=1)
s1 = s1 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
s1 = s1 + labs(colour='Tramo')
s1 = s1 + xlab('Decada') + ylab(expression('d'[50]))
s1 = s1 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
s1 = s1 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
s1 = s1 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
s1 = s1 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
s1 = s1 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
s1 = s1 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


s2 = qplot(periodo, dev_granulometrica, data = mdata, colour = as.factor(tramo))
s2 = s2 + geom_point(size=4)
s2 = s2 + geom_line(linetype = "dashed", size=1)
s2 = s2 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
s2 = s2 + labs(colour='Tramo')
s2 = s2 + xlab('Decada') + ylab(expression(sigma[50]))
s2 = s2 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
s2 = s2 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
s2 = s2 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
s2 = s2 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
s2 = s2 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
s2 = s2 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


s3 = qplot(periodo, coef_uniformidad, data = mdata, colour = as.factor(tramo))
s3 = s3 + geom_point(size=4)
s3 = s3 + geom_line(linetype = "dashed", size=1)
s3 = s3 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
s3 = s3 + labs(colour='Tramo')
s3 = s3 + xlab('Decada') + ylab(expression('C'[u]))
s3 = s3 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
s3 = s3 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
s3 = s3 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
s3 = s3 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
s3 = s3 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
s3 = s3 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


s4 = qplot(periodo, carga_media, data = mdata, colour = as.factor(tramo))
s4 = s4 + geom_point(size=4)
s4 = s4 + geom_line(linetype = "dashed", size=1)
s4 = s4 + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
s4 = s4 + labs(colour='Tramo')
s4 = s4 + xlab('Decada') + ylab(expression('S'[b]))
s4 = s4 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
s4 = s4 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
s4 = s4 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
s4 = s4 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
s4 = s4 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
s4 = s4 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))

grid.arrange(s1, s2, s3, s4)

# ------------------------------------------------------------------------------------ #
# Graficos para analizar tendencias en el espacio en cada variable por periodo
# ------------------------------------------------------------------------------------ #
  
# Graficas Variables Morfologicas. Detro de cada grafico 6 series por periodo y en total 7 graficos por variable morfologica. 

mr1 = qplot(tramo, pendiente, data = mdata, colour = as.factor(periodo))
mr1 = mr1 + geom_point(size=4)
mr1 = mr1 + geom_line(linetype = "dashed", size=1)
mr1 = mr1 + scale_x_continuous(breaks=1:6)
mr1 = mr1 + labs(colour='Periodo')
mr1 = mr1 + scale_colour_discrete(name="Periodo",
                         breaks=c("1", "2", "3", "4", "5", "6"),
                         labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
mr1 = mr1 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
mr1 = mr1 + xlab('Tramo del cauce') + ylab(expression('S'[0]))
mr1 = mr1 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
mr1 = mr1 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
mr1 = mr1 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
mr1 = mr1 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
mr1 = mr1 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
mr1 = mr1 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


mr2 = qplot(tramo, ancho_banca, data = mdata, colour = as.factor(periodo))
mr2 = mr2 + geom_point(size=4)
mr2 = mr2 + geom_line(linetype = "dashed", size=1)
mr2 = mr2 + scale_x_continuous(breaks=1:6)
mr2 = mr2 + labs(colour='Periodo')
mr2 = mr2 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
mr2 = mr2 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
mr2 = mr2 + xlab('Tramo del cauce') + ylab(expression('B'))
mr2 = mr2 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
mr2 = mr2 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
mr2 = mr2 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
mr2 = mr2 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
mr2 = mr2 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
mr2 = mr2 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


mr3 = qplot(tramo, sinuosidad, data = mdata, colour = as.factor(periodo))
mr3 = mr3 + geom_point(size=4)
mr3 = mr3 + geom_line(linetype = "dashed", size=1)
mr3 = mr3 + scale_x_continuous(breaks=1:6)
mr3 = mr3 + labs(colour='Periodo')
mr3 = mr3 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
mr3 = mr3 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
mr3 = mr3 + xlab('Tramo del cauce') + ylab(expression('IS'))
mr3 = mr3 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
mr3 = mr3 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
mr3 = mr3 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
mr3 = mr3 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
mr3 = mr3 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
mr3 = mr3 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


mr4 = qplot(tramo, amp_meandros, data = mdata, colour = as.factor(periodo))
mr4 = mr4 + geom_point(size=4)
mr4 = mr4 + geom_line(linetype = "dashed", size=1)
mr4 = mr4 + scale_x_continuous(breaks=1:6)
mr4 = mr4 + labs(colour='Periodo')
mr4 = mr4 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
mr4 = mr4 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
mr4 = mr4 + xlab('Tramo del cauce') + ylab(expression('M'[b]))
mr4 = mr4 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
mr4 = mr4 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
mr4 = mr4 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
mr4 = mr4 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
mr4 = mr4 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
mr4 = mr4 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


mr5 = qplot(tramo, long_meandros, data = mdata, colour = as.factor(periodo))
mr5 = mr5 + geom_point(size=4)
mr5 = mr5 + geom_line(linetype = "dashed", size=1)
mr5 = mr5 + scale_x_continuous(breaks=1:6)
mr5 = mr5 + labs(colour='Periodo')
mr5 = mr5 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
mr5 = mr5 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
mr5 = mr5 + xlab('Tramo del cauce') + ylab(expression(lambda))
mr5 = mr5 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
mr5 = mr5 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
mr5 = mr5 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
mr5 = mr5 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
mr5 = mr5 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
mr5 = mr5 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


mr6 = qplot(tramo, t_movilidad, data = mdata, colour = as.factor(periodo))
mr6 = mr6 + geom_point(size=4)
mr6 = mr6 + geom_line(linetype = "dashed", size=1)
mr6 = mr6 + scale_x_continuous(breaks=1:6)
mr6 = mr6 + labs(colour='Periodo')
mr6 = mr6 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
mr6 = mr6 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
mr6 = mr6 + xlab('Tramo del cauce') + ylab(expression('T'[m]))
mr6 = mr6 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
mr6 = mr6 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
mr6 = mr6 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
mr6 = mr6 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
mr6 = mr6 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
mr6 = mr6 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


mr7 = qplot(tramo, t_movilidad_max, data = mdata, colour = as.factor(periodo))
mr7 = mr7 + geom_point(size=4)
mr7 = mr7 + geom_line(linetype = "dashed", size=1)
mr7 = mr7 + scale_x_continuous(breaks=1:6)
mr7 = mr7 + labs(colour='Periodo')
mr7 = mr7 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
mr7 = mr7 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
mr7 = mr7 + xlab('Tramo del cauce') + ylab(expression('T'[mmax]))
mr7 = mr7 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
mr7 = mr7 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
mr7 = mr7 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
mr7 = mr7 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
mr7 = mr7 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
mr7 = mr7 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))

grid.arrange(mr1, mr2, mr3, mr4)
grid.arrange(mr5, mr6, mr7)

# Graficas Variables Hidrol?gicas e Hidr?ulicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 6 graficos por variable HH.

hr1 = qplot(tramo, caudal_medio, data = mdata, colour = as.factor(periodo))
hr1 = hr1 + geom_point(size=4)
hr1 = hr1 + geom_line(linetype = "dashed", size=1)
hr1 = hr1 + scale_x_continuous(breaks=1:6)
hr1 = hr1 + labs(colour='Periodo')
hr1 = hr1 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
hr1 = hr1 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
hr1 = hr1 + xlab('Tramo del cauce') + ylab(expression('Q'[m]))
hr1 = hr1 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
hr1 = hr1 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
hr1 = hr1 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
hr1 = hr1 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
hr1 = hr1 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
hr1 = hr1 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


hr2 = qplot(tramo, caudal_maximo, data = mdata, colour = as.factor(periodo))
hr2 = hr2 + geom_point(size=4)
hr2 = hr2 + geom_line(linetype = "dashed", size=1)
hr2 = hr2 + scale_x_continuous(breaks=1:6)
hr2 = hr2 + labs(colour='Periodo')
hr2 = hr2 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
hr2 = hr2 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
hr2 = hr2 + xlab('Tramo del cauce') + ylab(expression('Q'[max]))
hr2 = hr2 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
hr2 = hr2 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
hr2 = hr2 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
hr2 = hr2 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
hr2 = hr2 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
hr2 = hr2 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


hr3 = qplot(tramo, caudal_50, data = mdata, colour = as.factor(periodo))
hr3 = hr3 + geom_point(size=4)
hr3 = hr3 + geom_line(linetype = "dashed", size=1)
hr3 = hr3 + scale_x_continuous(breaks=1:6)
hr3 = hr3 + labs(colour='Periodo')
hr3 = hr3 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
hr3 = hr3 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
hr3 = hr3 + xlab('Tramo del cauce') + ylab(expression('Q'[50]))
hr3 = hr3 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
hr3 = hr3 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
hr3 = hr3 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
hr3 = hr3 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
hr3 = hr3 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
hr3 = hr3 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


hr4 = qplot(tramo, caudal_banca, data = mdata, colour = as.factor(periodo))
hr4 = hr4 + geom_point(size=4)
hr4 = hr4 + geom_line(linetype = "dashed", size=1)
hr4 = hr4 + scale_x_continuous(breaks=1:6)
hr4 = hr4 + labs(colour='Periodo')
hr4 = hr4 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
hr4 = hr4 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
hr4 = hr4 + xlab('Tramo del cauce') + ylab(expression('Q'[bl]))
hr4 = hr4 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
hr4 = hr4 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
hr4 = hr4 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
hr4 = hr4 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
hr4 = hr4 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
hr4 = hr4 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


hr5 = qplot(tramo, velocidad, data = mdata, colour = as.factor(periodo))
hr5 = hr5 + geom_point(size=4)
hr5 = hr5 + geom_line(linetype = "dashed", size=1)
hr5 = hr5 + scale_x_continuous(breaks=1:6)
hr5 = hr5 + labs(colour='Periodo')
hr5 = hr5 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
hr5 = hr5 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
hr5 = hr5 + xlab('Tramo del cauce') + ylab(expression('V'))
hr5 = hr5 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
hr5 = hr5 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
hr5 = hr5 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
hr5 = hr5 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
hr5 = hr5 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
hr5 = hr5 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


hr6 = qplot(tramo, ancho_superficial, data = mdata, colour = as.factor(periodo))
hr6 = hr6 + geom_point(size=4)
hr6 = hr6 + geom_line(linetype = "dashed", size=1)
hr6 = hr6 + scale_x_continuous(breaks=1:6)
hr6 = hr6 + labs(colour='Periodo')
hr6 = hr6 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
hr6 = hr6 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
hr6 = hr6 + xlab('Tramo del cauce') + ylab(expression('B'[s]))
hr6 = hr6 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
hr6 = hr6 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
hr6 = hr6 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
hr6 = hr6 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
hr6 = hr6 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
hr6 = hr6 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))

grid.arrange(hr1, hr2, hr3, hr4, hr5, hr6)

# Graficas Variables Sedimentol?gicas (Serie temporal por tramo). Detro de cada grafico 6 series por tramo y en total 4 graficos por variable Sedimentologica.

sr1 = qplot(tramo, diametro_promedio, data = mdata, colour = as.factor(periodo))
sr1 = sr1 + geom_point(size=4)
sr1 = sr1 + geom_line(linetype = "dashed", size=1)
sr1 = sr1 + scale_x_continuous(breaks=1:6)
sr1 = sr1 + labs(colour='Periodo')
sr1 = sr1 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
sr1 = sr1 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
sr1 = sr1 + xlab('Tramo del cauce') + ylab(expression('d'[50]))
sr1 = sr1 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
sr1 = sr1 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
sr1 = sr1 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
sr1 = sr1 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
sr1 = sr1 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
sr1 = sr1 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


sr2 = qplot(tramo, dev_granulometrica, data = mdata, colour = as.factor(periodo))
sr2 = sr2 + geom_point(size=4)
sr2 = sr2 + geom_line(linetype = "dashed", size=1)
sr2 = sr2 + scale_x_continuous(breaks=1:6)
sr2 = sr2 + labs(colour='Periodo')
sr2 = sr2 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
sr2 = sr2 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
sr2 = sr2 + xlab('Tramo del cauce') + ylab(expression(sigma[50]))
sr2 = sr2 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
sr2 = sr2 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
sr2 = sr2 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
sr2 = sr2 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
sr2 = sr2 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
sr2 = sr2 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


sr3 = qplot(tramo, coef_uniformidad, data = mdata, colour = as.factor(periodo))
sr3 = sr3 + geom_point(size=4)
sr3 = sr3 + geom_line(linetype = "dashed", size=1)
sr3 = sr3 + scale_x_continuous(breaks=1:6)
sr3 = sr3 + labs(colour='Periodo')
sr3 = sr3 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
sr3 = sr3 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
sr3 = sr3 + xlab('Tramo del cauce') + ylab(expression('C'[u]))
sr3 = sr3 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
sr3 = sr3 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
sr3 = sr3 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
sr3 = sr3 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
sr3 = sr3 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
sr3 = sr3 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))


sr4 = qplot(tramo, carga_media, data = mdata, colour = as.factor(periodo))
sr4 = sr4 + geom_point(size=4)
sr4 = sr4 + geom_line(linetype = "dashed", size=1)
sr4 = sr4 + scale_x_continuous(breaks=1:6)
sr4 = sr4 + labs(colour='Periodo')
sr4 = sr4 + scale_colour_discrete(name="Periodo",
                                  breaks=c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1955-1965", "1965-1975", "1975-1985", "1985-1995", "1995-2005", "2005-2015"))
sr4 = sr4 + guides(colour = guide_legend(keywidth = 2, keyheight = 1))
sr4 = sr4 + xlab('Tramo del cauce') + ylab(expression('S'[b]))
sr4 = sr4 + theme(axis.title.y = element_text(size=24, colour = rgb(0,0,0)))
sr4 = sr4 + theme(axis.title.x = element_text(size=24, colour=rgb(0,0,0)))
sr4 = sr4 + theme(axis.text.x = element_text(size=16, colour=rgb(0,0,0)))
sr4 = sr4 + theme(axis.text.y = element_text(size=16, colour = rgb(0,0,0)))
sr4 = sr4 + theme(legend.title = element_text(colour=rgb(0,0,0), size=15 ))
sr4 = sr4 + theme(legend.text = element_text(colour=rgb(0,0,0), size = 13))

grid.arrange(sr1, sr2, sr3, sr4)

by(mdata, mdata$tramo, summary)
by(mdata, mdata$peiodo, summary)

tapply(mdata$pendiente, mdata$tramo,mean)
tapply(mdata$ancho_banca, mdata$tramo,mean)
tapply(mdata$sinuosidad, mdata$tramo,mean)

tramo1 = subset(mdata, tramo=="1")
