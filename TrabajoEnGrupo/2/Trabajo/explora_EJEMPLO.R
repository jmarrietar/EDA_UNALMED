################################################################################
# Estadística Descriptiva y Análisis Exploratorio de Datos
# Ejemplo de exploración de los resultados de la base de datos de los
# tamaños de archivos en disco.
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
# Gráfica global
# Relación entre número de letras y tamaño del archivo en disco
# de manera gener-----------------------------------------------------------------------------#
require(lattice) # Este paquete no necesita ser instalado, viene con el R.
# Gráfica básica (sin pulir)
xyplot(TamañoArchivo~NumLetrasa,data=baseT)

# Con opciones adicionales
conf1<-simpleTheme(pch=19,col="blue")
xyplot(TamañoArchivo~NumLetrasa/1000,data=baseT,
       par.settings=conf1,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       main="Comportamiento del tamaño del archivo en disco \n en relación al número de letras \'a\'")

#------------------------------------------------------------------------------#
# Gráfica exploratoria en relación al formato
# Relación entre número de letras y tamaño del archivo en disco
# de teniendo en cuenta el formato
#------------------------------------------------------------------------------#

conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(TamañoArchivo~NumLetrasa/1000,data=baseT,
       group=Formato,
       auto.key=list(columns=5),
       par.settings=conf1,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       main="Tamaño del archivo vs número de letras por formato")


#------------------------------------------------------------------------------#
# Gráfica exploratoria en relación al formato
# Relación entre número de letras y tamaño del archivo en disco
# de teniendo en cuenta el formato
# Seleccionando sólo hasta 100 mil letras "a".
#------------------------------------------------------------------------------#
temp<-droplevels(subset(baseT,NumLetrasa<=100000))

conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(TamañoArchivo~NumLetrasa/1000,data=temp,
       group=Formato,
       auto.key=list(columns=5),
       par.settings=conf1,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       main="Tamaño del archivo vs número de letras por formato")

# La misma gráfica pero en líneas promedio por formato
conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(TamañoArchivo~NumLetrasa/1000,data=temp,
       group=Formato,
       auto.key=list(columns=5),
       par.settings=conf1,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tamaño del archivo vs número de letras por formato")

# Examinemos más de cerca el formato ".docx"
# ¿A qué se pudo haber debido la diferencia?
# Seleccionamos sólo los del formato ".docx"
#formato.docx<-droplevels(subset(baseT,Formato==".docx"))

formato.docx<-droplevels(subset(temp,Formato==".docx"))
summary(formato.docx)
# Parece que la diferencia la está estableciendo el sistema opeativo,
# aunque la versión también podría estar interviniendo
# Gráfica exploratoria por sistema operativo (SO)
conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(TamañoArchivo~NumLetrasa/1000,data=formato.docx,
       group=SO,
       auto.key=list(columns=2),
       par.settings=conf1,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tamaño del archivo vs número de letras por formato .docx")

# Gráfica exploratoria por sistema operativo (Versión)
conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(TamañoArchivo~NumLetrasa/1000,data=formato.docx,
       group=VersionProcesa,
       auto.key=list(columns=2),
       par.settings=conf1,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tamaño del archivo vs número de letras por formato .docx")
# Al tener la tabla de contingencia, se nota que las variables
# VersionProcesa y SO están sobrepuestas, porque no existen todas
# las combinaciones posibles.
with(formato.docx,table(SO,VersionProcesa))

# Creamos una nueva variable para distinguir cada individuo que tomó
# los datos para determinar si la diferencia se debe al individuo.
formato.docx$Grupo_Integ<-factor(with(formato.docx,
                                      paste(Grupo,Integrante,sep="-")))
conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
xyplot(TamañoArchivo~NumLetrasa/1000,data=formato.docx,
       group=Grupo_Integ,
       auto.key=list(columns=2),
       par.settings=conf1,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tamaño del archivo vs número de letras por formato .docx")

# Resumen de la base de datos, discriminado por Grupo_Integ
by(formato.docx,formato.docx$Grupo_Integ,summary)
