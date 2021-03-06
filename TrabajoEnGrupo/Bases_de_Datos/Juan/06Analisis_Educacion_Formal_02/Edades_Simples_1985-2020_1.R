################################################################################
# Estimaci�n de poblaci�n para colombia
# Adecuaci�n de la base de datos
# De las edades entres 1 y 26 a�os por departamento
################################################################################

Edades<-read.csv2("Edades_Simples_1985-2020_1.csv",enc="latin1",skip=10)
str(Edades)
a�os<-read.csv2("Edades_Simples_1985-2020_1.csv",enc="latin1",skip=8,nrows=1)
a�os<-a�os[1,][!is.na(a�os[1,])]
a�os
names(Edades)<-c(names(Edades)[1:2],paste(names(Edades)[3:5],
                                          rep(a�os,each=3),sep="."))
# Seleccionar s�lo los nombres de los departamentos
deptos<-droplevels(Edades$Edad[seq(1,nrow(Edades),28)])
# Substituir el par�ntesis y el n�mero por un nulo
deptos<-gsub("[\\(0-9\\)]","",as.character(deptos))
# Quitar los espacios en blanco al final de la sarta
deptos<-factor(gsub("\\s+$","",as.character(deptos)))
# Quitar el nombre vac�o
deptos<-deptos[deptos!=""]
deptos

# Seleccionar s�lo los c�digos
codigos<-droplevels(Edades$C�digo[seq(1,nrow(Edades),28)])
# Seleccionar s�lo los c�digos de los departamentos
codigos<-droplevels(codigos[1:length(deptos)])
# Constuir una base de datos con los nombres de los departamentos
# y sus c�digos repetidos 27 veces que son las edades de 0 a 26.
NomCodDepto<-data.frame(codigos,deptos)[rep(1:length(codigos),each=27),]
# Eliminar de la base de datos los registros que no tienen informaci�n relevante
Edades<-Edades[!is.na(Edades$Total.1985) & Edades$Total.1985!="",]
# Unir las dos bases de datos que tienen los c�digos y la informaci�n
Edades<-data.frame(NomCodDepto,subset(Edades,select=Edad:Mujeres.2020))
# Estructura de la base de datos total
str(Edades)    

# Funci�n para quitar puntos y convertirlos a n�meros
conv_num<-function(x) as.numeric(gsub("[.]","",x))
# Aplicaci�n a toda la base de datos la funci�n para convertir a # formato num�rico
temp1<-data.frame(sapply(subset(Edades,select=Total.1985:Mujeres.2020),conv_num))

# Unir de nuevo las bases de datos corregidas
Edades<-data.frame(subset(Edades,select=codigos:Edad),temp1)
# Convertir la Edad a num�rico.
Edades$Edad<-as.numeric(as.character(Edades$Edad))
str(Edades)

# Selecci�n del total de personas por edad para el a�os 2011
# Excluyendo el total nacional
poblEdad2012<-droplevels(subset(Edades,select=c(codigos,deptos,Edad,Total.2011),
                     deptos!="NACIONAL"))

str(poblEdad2012)

# Seleccionar las edades mayores o iguales a 5 a�os y
# menores iguales a 20 a�os, considerada la edad escolar.
temp2<-subset(poblEdad2012,Edad>=5 & Edad<=20,select=codigos:Total.2011)
# Suma de la estimaci�n de la poblaci�n comprendidas en esas edades
edadEscolar<-aggregate(subset(temp2,select=Total.2011),
          by=with(temp2,list(codigos=codigos,deptos=deptos)),sum)

edadEscolar

# Exportar la base de datos de la poblaci�n total por
# departamento para unirla con la de matriculados
write.csv2(edadEscolar,file="edadEscolar.csv",fileEncoding="latin1",row.names=FALSE)


