################################################################################
# Estad�stica Descriptiva y An�lisis Exploratorio de Datos
# Ejemplo de exploraci�n de los resultados de la base de datos de los
# tama�os de archivos en disco.
# Por: Kenneth Roy Cabrera Torres
# Fecha: 3 de septiembre de 2013
################################################################################

# Lectura de la base de datos
baseT<-read.csv2("BaseTotalTamArch.csv",enc="latin1")
# Estructura de la base de datos
str(baseT)
# Resumen global de la base de datos.
summary(baseT)

#------------------------------------------------------------------------------#
# Gr�fica global
# Relaci�n entre n�mero de letras y tama�o del archivo en disco
# de manera gener-----------------------------------------------------------------------------#
require(lattice) # Este paquete no necesita ser instalado, viene con el R.
# Gr�fica b�sica (sin pulir)
xyplot(Tama�oArchivo~NumLetrasa,data=baseT)

# Con opciones adicionales
conf1<-simpleTheme(pch=19,col="blue")
xyplot(Tama�oArchivo~NumLetrasa/1000,data=baseT,
       par.settings=conf1,ylab="Tama�o del archivo (Kb)",
       xlab="N�mero de letras \'a\' (miles)",
       main="Comportamiento del tama�o del archivo en disco \n en relaci�n al n�mero de letras \'a\'")

#------------------------------------------------------------------------------#
# Gr�fica exploratoria en relaci�n al formato
# Relaci�n entre n�mero de letras y tama�o del archivo en disco
# de teniendo en cuenta el formato
#------------------------------------------------------------------------------#

conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(Tama�oArchivo~NumLetrasa/1000,data=baseT,
       group=Formato,
       auto.key=list(columns=5),
       par.settings=conf1,ylab="Tama�o del archivo (Kb)",
       xlab="N�mero de letras \'a\' (miles)",
       main="Tama�o del archivo vs n�mero de letras por formato")


#------------------------------------------------------------------------------#
# Gr�fica exploratoria en relaci�n al formato
# Relaci�n entre n�mero de letras y tama�o del archivo en disco
# de teniendo en cuenta el formato
# Seleccionando s�lo hasta 100 mil letras "a".
#------------------------------------------------------------------------------#
temp<-droplevels(subset(baseT,NumLetrasa<=100000))

conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(Tama�oArchivo~NumLetrasa/1000,data=temp,
       group=Formato,
       auto.key=list(columns=5),
       par.settings=conf1,ylab="Tama�o del archivo (Kb)",
       xlab="N�mero de letras \'a\' (miles)",
       main="Tama�o del archivo vs n�mero de letras por formato")

# La misma gr�fica pero en l�neas promedio por formato
conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(Tama�oArchivo~NumLetrasa/1000,data=temp,
       group=Formato,
       auto.key=list(columns=5),
       par.settings=conf1,ylab="Tama�o del archivo (Kb)",
       xlab="N�mero de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tama�o del archivo vs n�mero de letras por formato")

# Examinemos m�s de cerca el formato ".docx"
# �A qu� se pudo haber debido la diferencia?
# Seleccionamos s�lo los del formato ".docx"
#formato.docx<-droplevels(subset(baseT,Formato==".docx"))

formato.docx<-droplevels(subset(temp,Formato==".docx"))
summary(formato.docx)
# Parece que la diferencia la est� estableciendo el sistema opeativo,
# aunque la versi�n tambi�n podr�a estar interviniendo
# Gr�fica exploratoria por sistema operativo (SO)
conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(Tama�oArchivo~NumLetrasa/1000,data=formato.docx,
       group=SO,
       auto.key=list(columns=2),
       par.settings=conf1,ylab="Tama�o del archivo (Kb)",
       xlab="N�mero de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tama�o del archivo vs n�mero de letras por formato .docx")

# Gr�fica exploratoria por sistema operativo (Versi�n)
conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(Tama�oArchivo~NumLetrasa/1000,data=formato.docx,
       group=VersionProcesa,
       auto.key=list(columns=2),
       par.settings=conf1,ylab="Tama�o del archivo (Kb)",
       xlab="N�mero de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tama�o del archivo vs n�mero de letras por formato .docx")
# Al tener la tabla de contingencia, se nota que las variables
# VersionProcesa y SO est�n sobrepuestas, porque no existen todas
# las combinaciones posibles.
with(formato.docx,table(SO,VersionProcesa))

# Creamos una nueva variable para distinguir cada individuo que tom�
# los datos para determinar si la diferencia se debe al individuo.
formato.docx$Grupo_Integ<-factor(with(formato.docx,
                                      paste(Grupo,Integrante,sep="-")))
conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(Tama�oArchivo~NumLetrasa/1000,data=formato.docx,
       group=Grupo_Integ,
       auto.key=list(columns=2),
       par.settings=conf1,ylab="Tama�o del archivo (Kb)",
       xlab="N�mero de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tama�o del archivo vs n�mero de letras por formato .docx")

# Resumen de la base de datos, discriminado por Grupo_Integ
by(formato.docx,formato.docx$Grupo_Integ,summary)
