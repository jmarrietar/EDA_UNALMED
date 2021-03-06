################################################################################
# Educaci�n formal desde preescolar hasta educaci�n media
# en Colombia.
# N�mero de alumnos matriculado por nivel educativo y sector
# Informaci�n definitiva - A�o 2011
# DANE.
# Lectura y adecuaci�n de la base de datos.
# Fecha: 10 de Septiembre de 2013
################################################################################
# Lectura de la base de datos
EdFor<-read.csv2("EFormal_alumnos_sector_2011.csv",skip=12,enc="latin1")
# Ver la estructura de la base de datos
str(EdFor)
# Suprimir columnas vac�as.
EdFor<-subset(EdFor,select=-c(X.1,X.2,X.3,X.4,X.5))
# Seleccionar s�lo los reg�stros desde el 1 al 33
EdFor<-EdFor[1:33,]
# Ver la estructura de la base de datos
str(EdFor)

#------------------------------------------------------------------------------#
# Lectura de un s�lo rengl�n de la base de datos
titulos<-read.csv2("EFormal_alumnos_sector_2011.csv",skip=10,nrows=1,enc="latin1")
# Quitar los elementos Na del primer rengl�n de la base de datos titulos.
tipos<-titulos[1,][!is.na(titulos[1,])]
# Quita el primer elemento del vector tipos
tipos<-tipos[-1]
# Concatena y repite los nombres de acuerdo a las columnas necesarias
nombres<-c(rep(tipos[1:4],each=4),rep(tipos[5],2),rep(tipos[6],each=4))
# Nombres secundarios
nombres2<-c(rep(names(EdFor)[2:5],4),names(EdFor)[2:3],names(EdFor)[2:5])
# Asigna los nuevos nombres a la base de datos
names(EdFor)<-make.names(c("Depto",paste(nombres,nombres2,sep=".")))
# Ver la estructura de la base de datos
str(EdFor)

# Funci�n para quitar puntos y convertirlos a n�meros
conv_num<-function(x) as.numeric(gsub("[.]","",x))
  
# Ejemplo del uso de la funci�n anteriormente construida.
with(EdFor,conv_num(PREESCOLAR.Total))
# Aplicaci�n a toda la base de datos la funci�n para convertir a 
# formato num�rico
temp1<-data.frame(sapply(subset(EdFor,select=PREESCOLAR.Total:TOTAL.Subsidiada),conv_num))
# Selecci�n de los nombres de los departamentos
Deptos<-EdFor$Depto
# Asignaci�n de la nueva base de datos
EdFor<-temp1
# Asignaci�n a los nombres de las filas de la nueva base de datos
row.names(EdFor)<-Deptos

# Estructura final de la base de datos
str(EdFor)
# Cabecera de la base de datos
head(EdFor)
# Registros finales de la base de datos
tail(EdFor)

# Exportar la base de datos para ser posteriormente analizada
write.csv2(EdFor,file="EFormal_alumnos_sector_2011_Adecuada.csv",
           fileEncoding="latin1")