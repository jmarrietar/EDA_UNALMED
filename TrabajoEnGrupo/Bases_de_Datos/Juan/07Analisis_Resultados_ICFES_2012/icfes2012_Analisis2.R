################################################################################
# Ejemplos de gráficas exploratorias comunes.
# Base de datos de los resultados del ICFES del año 2012.
# Resolver algunas preguntas realizadas por grupos
# de estudiantes.
# Fecha: 26 de Septiembre de 2013.
# Autor: Kenneth Roy Cabrera Torres.
################################################################################

# Lectura de la base de datos
icfes2012<-read.csv2("icfes2012.csv",enc="latin1")
str(icfes2012)

# Cálculo de promedio de cada área por departamento por grupo
promDepto<-aggregate(subset(icfes2012,select=PROMEDIOMATEMATICA:PROMEDIOSOCIALES),
          list(DEPARTAMENTO=icfes2012$DEPARTAMENTO),mean,na.rm=T)
promDepto

# Mejores departamentos por PROMEDIOMATEMATICA
promDepto[order(promDepto$PROMEDIOMATEMATICA,decreasing=T),]


# Gráfica de resultado por deptartamento
require(lattice)
# Diagrama de puntos
conf1<-simpleTheme(pch=19,col="blue")
xyplot(DEPARTAMENTO~PROMEDIOMATEMATICA,data=icfes2012,
       par.settings=conf1)
# Diagrama de caja y bigotes
bwplot(DEPARTAMENTO~PROMEDIOMATEMATICA,data=icfes2012,
       par.settings=conf1)

# Cálculo de la mediana por departamento
medianasDepto<-with(icfes2012,tapply(PROMEDIOMATEMATICA,
                                     DEPARTAMENTO,median))
medianasDepto
# Ordenar de mayor a menor por medianas
medianasDepto<-sort(medianasDepto,dec=T)
medianasDepto

# Reordenar el orden de las etiquetas en la base de datos
# de tal manera que la gráfica apareza ordenada
icfes2012$DEPARTAMENTO<-factor(icfes2012$DEPARTAMENTO,
                               levels=names(medianasDepto))
conf1<-simpleTheme(pch=19,col="blue")
bwplot(DEPARTAMENTO~PROMEDIOMATEMATICA,data=icfes2012,
       par.settings=conf1)

# Ordenado por instituciones y caracterizar las instituciones
# con más alto puntaje.
promCodInst<-aggregate(subset(icfes2012,select=PROMEDIOMATEMATICA:PROMEDIOSOCIALES),
                     list(CODINST=icfes2012$CODINST,
                          NOMBREINSTITUCION=icfes2012$NOMBREINSTITUCION,
                          CODIGOMUNICIPIO=icfes2012$CODIGOMUNICIPIO,
                          NOMBREMUNICIPIO=icfes2012$NOMBREMUNICIPIO,
                          DEPARTAMENTO=icfes2012$DEPARTAMENTO,
                          CALENDARIO=icfes2012$CALENDARIO,
                          NATURALEZA=icfes2012$NATURALEZA),mean,na.rm=T)
head(promCodInst)
# Ordenado por puntaje en PROMEDIOMATEMATICA
promCodInst<-promCodInst[order(promCodInst$PROMEDIOMATEMATICA,decreasing=T),]
head(promCodInst)

# Distribución de los promedios por colegio o centro educativo para
# PORMEDIOMATEMÁTICA
with(promCodInst,hist(PROMEDIOMATEMATICA,freq=F))
with(promCodInst,lines(density(PROMEDIOMATEMATICA),col="red",lwd=2))
# Gráfica de caja y bigotes
bxp<-with(promCodInst,boxplot(PROMEDIOMATEMATICA,horizontal=T))
bxp

# Seleccion de las instituciones con valore atípicos superiores
# Límite superior del bigote
limitebigotesup<-bxp$stats[5,]
limitebigotesup
# Selección 
promCodInstSup<-droplevels(subset(promCodInst,PROMEDIOMATEMATICA>=limitebigotesup))
# Caracterización de los valores superiores
sort(summary(promCodInstSup$DEPARTAMENTO),dec=T)
sort(summary(promCodInstSup$NOMBREMUNICIPIO),dec=T)


# Afecta la jornada el resultado del ICFES
bwplot(JORNADA~PROMEDIOMATEMATICA,data=icfes2012)
conf1<-simpleTheme(lwd=3)
densityplot(~PROMEDIOMATEMATICA,group=JORNADA,data=icfes2012,
            auto.key=list(columns=3),
            par.settings=conf1)

# Desempeño en la diferentes áreas
areas<-subset(icfes2012,select=PROMEDIOMATEMATICA:PROMEDIOSOCIALES)
icfes2012_a<-stack(areas)
names(icfes2012_a)<-c("PROMEDIO","AREA")
icfes2012_a<-data.frame(subset(icfes2012,select=CODINST:JORNADA)[rep(1:nrow(icfes2012),ncol(areas)),],
                        icfes2012_a)
require(lattice)
xyplot(AREA~PROMEDIO,data=icfes2012_a)
bwplot(AREA~PROMEDIO,data=icfes2012_a)
# Ordenación
medianas_area<-with(icfes2012_a,tapply(PROMEDIO,AREA,median,na.rm=T))
medianas_area<-sort(medianas_area,dec=TRUE)
medianas_area
icfes2012_a$AREA<-factor(icfes2012_a$AREA,levels=names(medianas_area))
bwplot(AREA~PROMEDIO,data=icfes2012_a)


