#setwd("C:/Users/JosePortatil/Dropbox/8 semestre/Estadistica Descriptiva/TrabajoEnGrupo/Bases_de_Datos/Juan/07Analisis_Resultados_ICFES_2012")
setwd("C:/Users/EQUIPO JOSE/Dropbox/8 semestre/Estadistica Descriptiva/TrabajoEnGrupo/Bases_de_Datos/Juan/07Analisis_Resultados_ICFES_2012")
################################################################################
# Ejemplos de gráficas exploratorias comunes.
# Base de datos de los resultados del ICFES del año 2012.
# Fecha: 24 de Septiembre de 2013.
# Autor: Kenneth Roy Cabrera Torres.
################################################################################

# Lectura de la base de datos
icfes2012<-read.csv2("icfes2012.csv",enc="latin1")
str(icfes2012)

# Exportación de la gráfica a formato ".eps" o postscript.
# También se pueden utilizar las funciones pdf(), jpeg(), png(), para
# exportar las gráficas en otros formatos.
jpeg("01histograma1.jpeg")
with(icfes2012,hist(PROMEDIOMATEMATICA,
                    xlab="Promedio de puntaje en Matemáticas",
                    ylab="Frecuencia",
                    main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11"))
dev.off()

postscript("02stripchart1.eps")
with(icfes2012,stripchart(PROMEDIOMATEMATICA,
              main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11"))
dev.off()


postscript("03dotchart1.eps")
with(icfes2012,dotchart(PROMEDIOMATEMATICA,
                       main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11"))
dev.off()

postscript("04boxplot1.eps")
with(icfes2012,boxplot(PROMEDIOMATEMATICA,
                       horizontal=TRUE,
                    main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11"))
points(mean(icfes2012$PROMEDIOMATEMATICA),1,col="red",pch=19,cex=1.5)
points(mean(icfes2012$PROMEDIOMATEMATICA)+sd(icfes2012$PROMEDIOMATEMATICA),1,col="red",pch="|",cex=1.5)
points(mean(icfes2012$PROMEDIOMATEMATICA)-sd(icfes2012$PROMEDIOMATEMATICA),1,col="red",pch="|",cex=1.5)
points(mean(icfes2012$PROMEDIOMATEMATICA)+2*sd(icfes2012$PROMEDIOMATEMATICA),1,col="red",pch="|",cex=1)
points(mean(icfes2012$PROMEDIOMATEMATICA)-2*sd(icfes2012$PROMEDIOMATEMATICA),1,col="red",pch="|",cex=1)
dev.off()

# Selección de una subbase para tener las instituciones de MEDELLIN, únicamente.
icfes2012_medellin<-droplevels(subset(icfes2012,NOMBREMUNICIPIO=="MEDELLIN"))

# Histograma de la distribución de los puntajes promdio de las instituciones
# para Medellín.
jpeg("01histograma1Med.jpeg")
with(icfes2012_medellin,hist(PROMEDIOMATEMATICA,
                    xlab="Promedio de puntaje en Matemáticas",
                    ylab="Frecuencia",freq=FALSE,
                    xlim=c(20,80),ylim=c(0,0.12),
                    main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11 \n para Medellín"))
with(icfes2012_medellin,lines(density(PROMEDIOMATEMATICA),col="blue",lty=2,lwd=2))
dev.off()


postscript("05stripchart1.eps")
with(icfes2012_medellin,stripchart(PROMEDIOMATEMATICA,
          main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11 \n para Medellín"))
dev.off()

postscript("05stripchart2.eps")
with(icfes2012_medellin,stripchart(PROMEDIOMATEMATICA,method="stack",
          main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11 \n para Medellín"))
dev.off()

# Diagrama de caja y bigotes para todas las instituciones de Medellín.
jpeg("05boxplot2.jpeg")
with(icfes2012_medellin,boxplot(PROMEDIOMATEMATICA,
                       horizontal=TRUE,
                       main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11 en Medellín"))
points(mean(icfes2012_medellin$PROMEDIOMATEMATICA),1,col="red",pch=19,cex=1.5)
points(mean(icfes2012_medellin$PROMEDIOMATEMATICA)+sd(icfes2012_medellin$PROMEDIOMATEMATICA),1,col="red",pch="|",cex=1.5)
points(mean(icfes2012_medellin$PROMEDIOMATEMATICA)-sd(icfes2012_medellin$PROMEDIOMATEMATICA),1,col="red",pch="|",cex=1.5)
points(mean(icfes2012_medellin$PROMEDIOMATEMATICA)+2*sd(icfes2012_medellin$PROMEDIOMATEMATICA),1,col="red",pch="|",cex=1)
points(mean(icfes2012_medellin$PROMEDIOMATEMATICA)-2*sd(icfes2012_medellin$PROMEDIOMATEMATICA),1,col="red",pch="|",cex=1)
dev.off()

# Comparación de los diagramas de caja y bigotes por NATURALEZA
postscript("06boxplot3.eps")
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,3,0,0))
bx<-with(icfes2012_medellin,boxplot(PROMEDIOMATEMATICA~NATURALEZA,
                                horizontal=TRUE,las=1,
                                main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11 en Medellín \n comparación Oficial-No Oficial"))
par(op)
dev.off()

# Comparación de los diagramas de caja y bigotes por NATURALEZA
# Adicionando la media y las desviaciones estándar
# Cálculo de los promedios para cada institución
medias<-with(icfes2012_medellin,tapply(PROMEDIOMATEMATICA,NATURALEZA,mean))
ds<-with(icfes2012_medellin,tapply(PROMEDIOMATEMATICA,NATURALEZA,sd))

jpeg("06boxplot4.jpeg")
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,3,0,0))
bx<-with(icfes2012_medellin,boxplot(PROMEDIOMATEMATICA~NATURALEZA,
                                    horizontal=TRUE,las=1,
                                    main="Distribución del puntaje promedio de las instituciones \n de la prueba saber 11 en Medellín \n comparación Oficial-No Oficial"))
points(medias,c(1,2),pch=19,col="red")
points(medias+ds,c(1,2),pch="|",cex=1.5,col="red")
points(medias-ds,c(1,2),pch="|",cex=1.5,col="red")
points(medias+2*ds,c(1,2),pch="|",cex=1,col="red")
points(medias-2*ds,c(1,2),pch="|",cex=1,col="red")
par(op)
dev.off()

