################################################################################
# Educación formal desde preescolar hasta educación media
# en Colombia.
# Número de alumnos matriculado por nivel educativo y sector
# Información definitiva - Año 2011
# DANE.
# Análisis con población en edad de estudiar para
# determinar cubrimiento.
# Fecha: 10 de Septiembre de 2013
################################################################################

EdFor<-read.csv2("EFormal_alumnos_sector_2011_Adecuada.csv",enc="latin1",row.names=1)
str(EdFor)
head(EdFor)
summary(EdFor)

edadEscolar<-read.csv2("edadEscolar.csv",enc="latin1",
                       colClasses=c("factor","factor","numeric"))
str(edadEscolar)

# Agregar la información a la base de datos
EdFor<-data.frame(edadEscolar,EdFor)
head(EdFor)
# Cálculo de los totales para edades escolares
EdFor$Total.MatricEE<-with(EdFor,PRIMARIA.Total+SECUNDARIA.Y.MEDIA.Total+ACELERACION.DEL.APRENDIZAJE.Total)
# Cálculo del cubrimiento de la edad escolar
EdFor$Cobertura<-with(EdFor,Total.MatricEE/Total.2011*100)

cobertura<-EdFor$Cobertura
names(cobertura)<-rownames(EdFor)
cobertura<-sort(cobertura)

op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(cobertura,horiz=T,names=names(cobertura),
        las=1,cex.names=0.8,xlab="Porcentaje",
        xlim=c(0,80),
        main="Distribución de la cobertura en edad escolar\n por departamento")
par(op)


