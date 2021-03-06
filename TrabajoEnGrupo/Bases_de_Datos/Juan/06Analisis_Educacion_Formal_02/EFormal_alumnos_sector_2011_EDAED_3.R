################################################################################
# Educaci�n formal desde preescolar hasta educaci�n media
# en Colombia.
# N�mero de alumnos matriculado por nivel educativo y sector
# Informaci�n definitiva - A�o 2011
# DANE.
# An�lisis con poblaci�n en edad de estudiar para
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

# Agregar la informaci�n a la base de datos
EdFor<-data.frame(edadEscolar,EdFor)
head(EdFor)
# C�lculo de los totales para edades escolares
EdFor$Total.MatricEE<-with(EdFor,PRIMARIA.Total+SECUNDARIA.Y.MEDIA.Total+ACELERACION.DEL.APRENDIZAJE.Total)
# C�lculo del cubrimiento de la edad escolar
EdFor$Cobertura<-with(EdFor,Total.MatricEE/Total.2011*100)

cobertura<-EdFor$Cobertura
names(cobertura)<-rownames(EdFor)
cobertura<-sort(cobertura)

op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(cobertura,horiz=T,names=names(cobertura),
        las=1,cex.names=0.8,xlab="Porcentaje",
        xlim=c(0,80),
        main="Distribuci�n de la cobertura en edad escolar\n por departamento")
par(op)


