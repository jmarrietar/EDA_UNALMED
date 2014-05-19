################################################################################
# Estad�stica Descriptiva y An�lisis Exploratorio de Datos
# Depuraci�n e integraci�n de las bases de datos sobre
# tama�o de archivos en disco.
# Por: Kenneth Roy Cabrera Torres
# Fecha: 29 de agosto de 2013
# Modificaci�n: 3 de septiembre de 2013
################################################################################

# Nombres unificada de las variables.
nombresVar<-c("Integrante", "TipoEquipo", "SO", "TipoSO", "FormatoDD",
              "ProcesaTexto", "VersionProcesa", "Formato",
              "NumLetrasa", "Tama�oArchivo")


# Lectura de las bases de datos para depurar e integrar

g1<-read.csv2( "grupo01.csv" ,enc="latin1")
g2<-read.csv2( "grupo02.csv" ,enc="latin1")
g3<-read.csv2( "grupo03.csv" ,enc="latin1")

#------------------------------------------------------------------------------#
# Depuraci�n de cada base de datos para su posterior integraci�n
# Grupo 1
#------------------------------------------------------------------------------#
str(g1)
# Selecci�n de la subbase que contendr� los datos relevantes
g1d<-subset(g1,select=INTEGRANTE:TAMA�OARCHIVODISCO)
# Asignaci�n a los nombre de las variables de acuerdo a la convenci�n planteada.
names(g1d)<-nombresVar
# Estructura de la base de datos
str(g1d)
# Resumen de la base de datos
summary(g1d)
# Mostrar los registros que tienen valores faltantes
subset(g1d,is.na(Integrante))
# Eliminar de la base de datos los renglones con valores faltantes debido
# a fantasmas en la base de datos original
g1d<-droplevels(subset(g1d,!is.na(Integrante)))
str(g1d)
summary(g1d)
# A�adir una columna que identificar� el grupo.
g1d$Grupo<-1
summary(g1d)

#------------------------------------------------------------------------------#
# Depuraci�n de cada base de datos para su posterior integraci�n
# Grupo 2
#------------------------------------------------------------------------------#
# Ver estructura
str(g2)
# Seleccionar variables relevantes
g2d<-subset(g2,select=Integrante:Tama�oArchivo..KB.)
# Asignar nombres unificados
names(g2d)<-nombresVar
# Nueva estructura
str(g2d)
# Resumen de la base de datos
summary(g2d)
# Asignaci�n de grupo
g2d$Grupo<-2
# Resumen del resultado final
summary(g2d)

#------------------------------------------------------------------------------#
# Depuraci�n de cada base de datos para su posterior integraci�n
# Grupo 3
#------------------------------------------------------------------------------#
# Ver estructura
str(g3)
# Seleccionar variables relevantes
g3d<-subset(g3,select=c(integrante:SO,TipoSO:Tama�oArchivo))
# Asignar nombres unificados
names(g3d)<-nombresVar
# Nueva estructura
str(g3d)
# Resumen de la base de datos
summary(g3d)
# Asignaci�n de grupo
g3d$Grupo<-3
# Resumen del resultado final
summary(g3d)

################################################################################
# Unificaci�n de las bases de datos
################################################################################
# Para unificar las bases de datos deben tener el mismo n�mero de columnas
# con el mismo nombre y el mismo tipo.
baseT<-rbind(g1d,g2d,g3d)
str# Estructura de la base unificada
str(baseT)
# Resumen de la base de datos unificada
summary(baseT)

# Recodificaci�n de variables y depuraci�n
#------------------------------------------------------------------------------#
# Variable Integrante
#------------------------------------------------------------------------------#
# Se puede convertir en "factor" debido a que es m�s una variable categ�rica
# que cuantitativa
str(baseT)
baseT$Integrante<-factor(baseT$Integrante)
# Resumen de la variable convertida a factor
summary(baseT$Integrante)

#------------------------------------------------------------------------------#
# Variable TipoEquipo
#------------------------------------------------------------------------------#
str(baseT)
levels(baseT$TipoEquipo)
require(car)
levels(recode(baseT$TipoEquipo,"'escritorio'='Escritorio';
                         c('portatil','Portatil')='Port�til'"))
baseT$TipoEquipo<-recode(baseT$TipoEquipo,"'escritorio'='Escritorio';
                         c('portatil','Portatil')='Port�til'")
summary(baseT)

#------------------------------------------------------------------------------#
# Variable SO
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Niveles 
levels(baseT$SO)
require(car)
baseT$SO<-recode(baseT$SO,
          "c('windows 7','windows 7 home basic','windows 7 professional',
             'windows 7 ultimate','Windows 7 Home Basic',
             'Windows 7 Professional','Windows 7 Ultimate')='Win 7';
           c('windows xp','Windows XP Professional')='Win XP';
            'windows 8'='Win 8'")
levels(baseT$SO)

#------------------------------------------------------------------------------#
# Variable TipoSO
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Niveles 
levels(baseT$TipoSO)

#------------------------------------------------------------------------------#
# Variable FormatoDD
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Niveles 
levels(baseT$FormatoDD)
require(car)
baseT$FormatoDD<-recode(baseT$FormatoDD,"'ntfs'='NTFS'")
levels(baseT$FormatoDD)

#------------------------------------------------------------------------------#
# Variable ProcesaTexto
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Niveles 
levels(baseT$ProcesaTexto)
require(car)
baseT$ProcesaTexto<-recode(baseT$ProcesaTexto,
                           "c('word','Microsoft Office Word')='Microsoft Word'")
levels(baseT$ProcesaTexto)

#------------------------------------------------------------------------------#
# Variable VersionProcesa
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Tabla de frecuencia de una variable tipo entero (int)
table(baseT$VersionProcesa)

#------------------------------------------------------------------------------#
# Variable Formato
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Niveles de la variable categ�rica Formato
levels(baseT$Formato)
require(car)
baseT$Formato<-recode(baseT$Formato,
                           "c('doc','doc 97-2003')='.doc';
                      'docx'='.docx';
                      'HTM'='.html';
                      'odt'='.odt';'RTF'='.rtf'")
levels(baseT$Formato)

#------------------------------------------------------------------------------#
# Variable NumLetrasa
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Tabla de frecuencia de una variable tipo entero (int)
table(baseT$NumLetrasa)

#------------------------------------------------------------------------------#
# Variable NumLetrasa
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Tabla de frecuencia de una variable tipo entero (int)
table(baseT$NumLetrasa)


#------------------------------------------------------------------------------#
# Variable  Tama�oArchivo
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Resumen de una variable cuantitativa "cont�nua"
summary(baseT$Tama�oArchivo)

#------------------------------------------------------------------------------#
# Variable  Grupo
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Tabla de frecuencia de una variable tipo entero (int)
table(baseT$Grupo)


# Exporta la base de datos final
write.csv2(baseT,file="BaseTotalTamArch.csv",fileEncoding="latin1",row.names=F)

