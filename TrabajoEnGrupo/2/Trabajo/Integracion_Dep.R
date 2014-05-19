################################################################################
# Depuración e integración de las bases de datos sobre
# tamaño de archivos en disco.
################################################################################

# Nombres unificada de las variables.
nombresVar<-c("Integrante", "TipoEquipo", "SO", "TipoSO", "FormatoDD",
              "ProcesaTexto", "VersionProcesa", "Formato",
              "NumLetrasa", "TamañoArchivo")

# Lectura de las bases de datos para depurar e integrar
g1<-read.csv2( "grupo01.csv" ,enc="latin1")
g2<-read.csv2( "grupo02.csv" ,enc="latin1")
g3<-read.csv2( "grupo03.csv" ,enc="latin1")
g4<-read.csv2( "grupo04.csv" ,enc="latin1")
g5<-read.csv2( "grupo05.csv" ,enc="latin1")
g6<-read.csv2( "grupo06.csv" ,enc="latin1")
g7<-read.csv2( "grupo07.csv" ,enc="latin1")
g8<-read.csv2( "grupo08.csv" ,enc="latin1")
g9<-read.csv( "grupo09.csv" ,enc="latin1")
g10<-read.csv2( "grupo10.csv" ,enc="latin1") # Modificado Externamente 
g11<-read.csv2( "grupo11.csv" ,enc="latin1")
g12<-read.csv2( "grupo12.csv" ,enc="latin1")
g13<-read.csv2( "grupo13.csv" ,enc="latin1")
g14<-read.csv2( "grupo14.csv" ,enc="latin1")
g15<-read.csv2( "grupo15.csv" ,enc="latin1") # Modificado Externamente 
g16<-read.csv2( "grupo16.csv" ,enc="latin1") # modificado externamente error de digitacion
g17<-read.csv2( "grupo17.csv" ,enc="latin1")
g18<-read.csv( "grupo18.csv" ,enc="latin1")
g20<-read.csv2( "grupo20.csv" ,enc="latin1") # Modificado externamente

#------------------------------------------------------------------------------#
# Depuración de cada base de datos para su posterior integración
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Grupo 1
#------------------------------------------------------------------------------#
str(g1)
# Selección de la subbase que contendrá los datos relevantes
g1d<-subset(g1,select=INTEGRANTE:TAMAÑOARCHIVODISCO)
# Asignación a los nombre de las variables de acuerdo a la convención planteada.
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
# Añadir una columna que identificará el grupo.
g1d$Grupo<-1
summary(g1d)

#------------------------------------------------------------------------------#
# Grupo 2
#------------------------------------------------------------------------------#
# Ver estructura
str(g2)
# Seleccionar variables relevantes
g2d<-subset(g2,select=Integrante:TamañoArchivo..KB.)
# Asignar nombres unificados
names(g2d)<-nombresVar
# Nueva estructura
str(g2d)
# Resumen de la base de datos
summary(g2d)
# Mostrar los registros que tienen valores faltantes
subset(g2d,is.na(Integrante))

# Asignación de grupo
g2d$Grupo<-2
# Resumen del resultado final
summary(g2d)

#------------------------------------------------------------------------------#
# Grupo 3
#------------------------------------------------------------------------------#
# Ver estructura
str(g3)
# Seleccionar variables relevantes
g3d<-subset(g3,select=c(integrante:SO,TipoSO:TamañoArchivo))
# Asignar nombres unificados
names(g3d)<-nombresVar
# Nueva estructura
str(g3d)
# Resumen de la base de datos
summary(g3d)
# Mostrar los registros que tienen valores faltantes
subset(g3d,is.na(Integrante))
# Asignación de grupo
g3d$Grupo<-3
# Resumen del resultado final
summary(g3d)

#------------------------------------------------------------------------------#
# Grupo 4
#------------------------------------------------------------------------------#
str(g4)

# Selección de la subbase que contendrá los datos relevantes
g4d<-subset(g4,select=Integrante:TamañoArchivo)

# Asignar nombres unificados
names(g4d)<-nombresVar
# Nueva estructura
str(g4d)
# Mostrar los registros que tienen valores faltantes
subset(g4d,is.na(Integrante))
# Asignación de grupo
g4d$Grupo<-4
summary(g4d)

#------------------------------------------------------------------------------#
# Grupo 5
#------------------------------------------------------------------------------#
str(g5)
# Selección de la subbase que contendrá los datos relevantes
g5d<-subset(g5,select=Integrante:Tama.oArchivo)

names(g5d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g5d,is.na(Integrante))
# Asignación de grupo
g5d$Grupo<-5
summary(g5d)

#------------------------------------------------------------------------------#
# Grupo 6
#------------------------------------------------------------------------------#
str(g6)
# Selección de la subbase que contendrá los datos relevantes
g6d<-subset(g6,select=Integrante:TamañoArchivo)

names(g6d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g6d,is.na(Integrante))
# Asignación de grupo
g6d$Grupo<-6
summary(g6d)


#------------------------------------------------------------------------------#
# Grupo 7
#------------------------------------------------------------------------------#
str(g7)
# Selección de la subbase que contendrá los datos relevantes
g7d<-subset(g7,select=Integrante:TamanoArchivo)

names(g7d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g7d,is.na(Integrante))
# Asignación de grupo
g7d$Grupo<-7
str(g7d)
summary(g7d)

#------------------------------------------------------------------------------#
# Grupo 8
#------------------------------------------------------------------------------#

str(g8)
# Selección de la subbase que contendrá los datos relevantes
g8d<-subset(g8,select=Integrante:TamañoArchivo)

names(g8d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g8d,is.na(Integrante))
# Asignación de grupo
g8d$Grupo<-8
str(g8d)
summary(g8d)

#------------------------------------------------------------------------------#
# Grupo 9
#------------------------------------------------------------------------------#
str(g9)
# Selección de la subbase que contendrá los datos relevantes
g9d<-subset(g9,select=Integrante:TamañoDisco.Kb.)

names(g9d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g9d,is.na(Integrante))
# Asignación de grupo
g9d$Grupo<-9
str(g9d)
summary(g9d)

#------------------------------------------------------------------------------#
# Grupo 10
#------------------------------------------------------------------------------#

str(g10)
# Selección de la subbase que contendrá los datos relevantes
g10d<-subset(g10,select=Numero.de.integrante :Numero.de.Kb.ocupados.por.el.archivo.en.disco)

names(g10d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g10d,is.na(Integrante))
g10d<-droplevels(subset(g10d,!is.na(Integrante)))
# Asignación de grupo
g10d$Grupo<-10
str(g10d)
summary(g10d)

#------------------------------------------------------------------------------#
# Grupo 11
#------------------------------------------------------------------------------#
str(g11)
# Selección de la subbase que contendrá los datos relevantes
g11d<-subset(g11,select=Integrante:TamañoArchivo..KB.)

names(g11d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g11d,is.na(Integrante))
# Asignación de grupo
g11d$Grupo<-11
str(g11d)
summary(g11d)


#------------------------------------------------------------------------------#
# Grupo 12
#------------------------------------------------------------------------------#
str(g12)
# Selección de la subbase que contendrá los datos relevantes
g12d<-subset(g12,select=Integrante:TamañoArchivo)

names(g12d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g12d,is.na(Integrante))
g12d<-droplevels(subset(g12d,!is.na(Integrante)))
# Asignación de grupo
g12d$Grupo<-12
str(g12d)
summary(g12d)

#------------------------------------------------------------------------------#
# Grupo 13
#------------------------------------------------------------------------------#
str(g13)
# Selección de la subbase que contendrá los datos relevantes
g13d<-subset(g13,select=Integrante:TamañoArchivoDisco)

names(g13d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g13d,is.na(Integrante))
# Asignación de grupo
g13d$Grupo<-13
str(g13d)
summary(g13d)

#------------------------------------------------------------------------------#
# Grupo 14
#------------------------------------------------------------------------------#
str(g14)
# Selección de la subbase que contendrá los datos relevantes
g14d<-subset(g14,select=INTEGRANTE:TAMAÑO.ARCHIVO)

names(g14d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g14d,is.na(Integrante))
# Asignación de grupo
g14d$Grupo<-14
str(g14d)
summary(g14d)

#------------------------------------------------------------------------------#
# Grupo 15
#------------------------------------------------------------------------------#

str(g15)
# Selección de la subbase que contendrá los datos relevantes
g15d<-subset(g15,select=Integrantes:Tama.oAchivo)
names(g15d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g15d,is.na(Integrante))
# Asignación de grupo
g15d$Grupo<-15
str(g15d)
summary(g15d)

#------------------------------------------------------------------------------#
# Grupo 16
#------------------------------------------------------------------------------#
str(g16)
# Selección de la subbase que contendrá los datos relevantes
g16d<-subset(g16,select=Integrante:TamañoArchivo.KB)

names(g16d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g16d,is.na(Integrante))
# Asignación de grupo
g16d$Grupo<-16
str(g16d)
summary(g16d) 

#------------------------------------------------------------------------------#
# Grupo 17
#------------------------------------------------------------------------------#
str(g17)
# Selección de la subbase que contendrá los datos relevantes
g17d<-subset(g17,select=Integrante:TamañoArchivoKB)
names(g17d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g17d,is.na(Integrante))
# Asignación de grupo
g17d$Grupo<-17
str(g17d)
summary(g17d) 

#------------------------------------------------------------------------------#
# Grupo 18
#------------------------------------------------------------------------------#
str(g18)
# Selección de la subbase que contendrá los datos relevantes
g18d<-subset(g18,select=Integrante:TamañoArchivo)
names(g18d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g18d,is.na(Integrante))
# Asignación de grupo
g18d$Grupo<-18
str(g18d)
summary(g18d) 

#------------------------------------------------------------------------------#
# Grupo 19  No hay
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Grupo 20
#------------------------------------------------------------------------------#
str(g20)
# Selección de la subbase que contendrá los datos relevantes
g20d<-subset(g20,select=Integrante:TamañoArchivo)
names(g20d)<-nombresVar
# Mostrar los registros que tienen valores faltantes
subset(g20d,is.na(Integrante))
g20d<-droplevels(subset(g20d,!is.na(Integrante)))
# Asignación de grupo
g20d$Grupo<-20
str(g20d)
summary(g20d) 

################################################################################
# Unificación de las bases de datos
################################################################################
# Para unificar las bases de datos deben tener el mismo número de columnas
# con el mismo nombre y el mismo tipo.
# Para unificar las bases de datos deben tener el mismo número de columnas
# con el mismo nombre y el mismo tipo.

#Colocar mismo tipo columnas que tenga problemas de incopatibilidad.
g1d$VersionProcesa <- factor(g1d$VersionProcesa)
g2d$VersionProcesa <- factor(g2d$VersionProcesa)
g3d$VersionProcesa <- factor(g3d$VersionProcesa)
g4d$VersionProcesa<- factor(g4d$VersionProcesa)
g5d$VersionProcesa <- factor(g5d$VersionProcesa)
g6d$VersionProcesa <- factor(g6d$VersionProcesa)

g10d$VersionProcesa <- factor(g10d$VersionProcesa)
g7d$TipoSO<-factor(g7d$TipoSO)

#g7d$VersionProcesa<-g7d$VersionProcesa<-as.integer(as.character(g7d$VersionProcesa))

baseT<-rbind(g1d,g2d,g3d,g4d,g5d,g6d,g7d,g8d,g9d,g10d,g11d,g12d,g13d,g14d,g15d,g16d,g17d,g18d,g20d)

#
#g7d$TipoSO<-as.integer(as.character(g7d$TipoSO))


str(baseT)
# Resumen de la base de datos unificada
summary(baseT)

#Esto es para quitarle los que decine KB y remplzarle comas por puntos
baseT$TamañoArchivo<-as.numeric(gsub("([0-9]+).*$", "\\1", baseT$TamañoArchivo))
baseT$TamañoArchivo <- factor(baseT$TamañoArchivo)
baseT$TamañoArchivo <- chartr(",", ".", baseT$TamañoArchivo)

# convierte puntos a espacios y luego quita espacios.
baseT$NumLetrasa<- chartr(".", " ", baseT$NumLetrasa)
baseT$NumLetrasa<- gsub("[ ]+", "", baseT$NumLetrasa)

baseT$TamañoArchivo<-as.integer(as.character(baseT$TamañoArchivo))

#------------------------------------------------------------------------------#
# Variable TipoEquipo
#------------------------------------------------------------------------------#
levels(baseT$TipoEquipo)
require(car)

baseT$TipoEquipo<-recode(baseT$TipoEquipo," c('escritorio','Escritorio ')='Escritorio';
                         c('portatil','Portatil','portátil')='Portátil'")
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
                 'Windows 7 Professional','Windows 7 Ultimate','wimdows 7','Windows 7 ultimate','Windows 7 Home basic',
                  'Window7','Windows 7','Windows7')='Win 7';
                 c('windows xp','Windows XP Professional','Windows XP')='Win XP';
                 c('windows 8','Windows 8','Windows 8 Pro','Windows 8.1')='Win 8';
                  c('Linux Ubuntu 10.04.4 LTS','Ubuntu 13.04')='Ubuntu';
                  c('Windows Vista Home ','Windows Vista')='Win Vista'")
levels(baseT$SO)

#------------------------------------------------------------------------------#
# Variable TipoSO
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Niveles 
levels(baseT$TipoSO)
baseT$TipoSO<-recode(baseT$TipoSO,
                     "c('64bits','65 bits','66 bits','67 bits','68 bits','69 bits','64','64 Bits')='64 bits';
                     c('32','32 Bits','32bits')='32 bits'") 

#------------------------------------------------------------------------------#
# Variable FormatoDD
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Niveles 
levels(baseT$FormatoDD)
require(car)
baseT$FormatoDD<-recode(baseT$FormatoDD,"
                        'ext4'='Ext4';
                        c('ntfs','NFTS')='NTFS'")
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
                           "c('word','Microsoft Office Word','Micorsoft word ','Microsoft Office Word',
                           
                           'Word','word','microsoft word ','Microsof Word')='Microsoft Word';
                           
                           'LibreOffice Writer'='LibreOffice';
                           c('PDF','Adobe Reader ')='Adobe Reader'")
levels(baseT$ProcesaTexto)

#------------------------------------------------------------------------------#
# Variable VersionProcesa FALTA CORREGIR ESTA!!
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Tabla de frecuencia de una variable tipo entero (int)
levels(baseT$VersionProcesa)
table(baseT$VersionProcesa)

#------------------------------------------------------------------------------#
# Variable Formato
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Niveles de la variable categórica Formato
levels(baseT$Formato)
require(car)
baseT$Formato<-recode(baseT$Formato,
                      "c('doc','doc 97-2003','Doc')='.doc';
                      c('docx','Docx')='.docx';
                      c('HTM','html','HTML')='.html';
                      'abw'='.abw';
                      c('PDF','pdf')='.pdf';
                      'odt'='.odt';c('RTF','(.RTF)','.rft ','rtf')='.rtf';
                      'Documento xml'='.xml'")
levels(baseT$Formato)
#------------------------------------------------------------------------------#
# Variable NumLetrasa
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Tabla de frecuencia de una variable tipo entero (int)
table(baseT$NumLetrasa)

#------------------------------------------------------------------------------#
# Variable  TamañoArchivo
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Resumen de una variable cuantitativa "contínua"
summary(baseT$TamañoArchivo)

#------------------------------------------------------------------------------#
# Variable  Grupo
#------------------------------------------------------------------------------#
# Estructura
str(baseT)
# Tabla de frecuencia de una variable tipo entero (int)
table(baseT$Grupo)

# Exporta la base de datos final
write.csv2(baseT,file="BaseTotalTamArch.csv",fileEncoding="latin1",row.names=F)


