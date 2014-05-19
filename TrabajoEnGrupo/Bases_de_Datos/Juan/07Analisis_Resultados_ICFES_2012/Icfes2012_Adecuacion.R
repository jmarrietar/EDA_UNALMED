################################################################################
# Adecuación de la
# base de datos de los resultados del ICFES del año 2012
# Fecha: 24 de Septiembre de 2013
# Autor: Kenneth Roy Cabrera Torres
################################################################################

icfes2012<-read.csv2( "Tabla resul agregados saber 11 2012 instituciones educativas.csv",
                      enc="latin1",skip=3)

# Quitar los puntos de miles en la variable EVALUADOS y convertirlo a numérico
icfes2012$EVALUADOS<-as.numeric(gsub("[.]","",as.character(icfes2012$EVALUADOS)))

# Estructura de la base de datos
str(icfes2012)

# Exportación de la base de datos
write.csv2(icfes2012,file="icfes2012.csv",fileEncoding="latin1",row.names=F)