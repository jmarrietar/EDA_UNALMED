# Lectura de la base de datos
baseT<-read.csv2("BaseTotalTamArch.csv",enc="latin1")
# Estructura de la base de datos
str(baseT)
# Resumen global de la base de datos.
summary(baseT)
require(lattice)
#--------------------------------------------------------------------
# Seleccionando sólo hasta 100 mil letras "a".
#------------------------------------------------------------------------------#
temp<-droplevels(subset(baseT,NumLetrasa<=100000))
# Seleccionando sólo hasta 100 mil letras "a", para tener una muestra pequeña 
temp10<-temp<-droplevels(subset(baseT,NumLetrasa<=10000)) # este hasta 10 mil 

conf1<-simpleTheme(pch=19,col=c("blue","green","red","magenta","brown"))
conf2<-simpleTheme(pch=19,col=c("blue","yellow","red","darkviolet","brown","black"))

#Se procede a hacer grafica exploratoria solamente agrupado Solamente por SO obviando demas vbles. 
xyplot(TamañoArchivo~NumLetrasa/1000,data=temp,
       group=SO,
       auto.key=list(columns=2), # nose q esesto 
       par.settings=conf2,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
      # type=c("p","a"),
       main="Tamaño del archivo vs número de letras" ) 

#Tomemos solamente los de WINDOWS 7 y veamos porque en algunos caracteres el archivo es grande y para otro
# para el mismo sistema operativo el archivo es pequeño . 
SO.Win7<-droplevels(subset(temp,SO=="Win 7"))
SO.Win8<-droplevels(subset(temp,SO=="Win 8"))

#Grafica exploratoria de Sistema operativo Windows 7 , agrupados por procesador. 

xyplot(TamañoArchivo~NumLetrasa/1000,data=SO.Win7,
       group=ProcesaTexto,
       auto.key=list(columns=2), 
       par.settings=conf2,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tamaño del archivo vs número de letras Win7" ) 




Procesa.Word<-droplevels(subset(SO.Win7,ProcesaTexto=="Microsoft Word"))
Procesa.Word10<-droplevels(subset(SO.Win7,ProcesaTexto=="Microsoft Word"))
Procesa.Word2<-droplevels(subset(SO.Win8,ProcesaTexto=="Microsoft Word"))

# Gráfica exploratoria por sistema operativo Window para proceador word hasta 100 mil caracteres

xyplot(TamañoArchivo~NumLetrasa/1000,data=Procesa.Word,
       group=VersionProcesa,
       auto.key=list(columns=2),
       par.settings=conf1,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       # type=c("p","a"),
       main="Tamaño del archivo vs número de letras win7 por procesador WORD")




#Grafica exploratoria de Sistema operativo Windows 8 , agrupados por procesador. 

xyplot(TamañoArchivo~NumLetrasa/1000,data=SO.Win8,
       group=ProcesaTexto,
       auto.key=list(columns=2), 
       par.settings=conf2,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
       type=c("p","a"),
       main="Tamaño del archivo vs número de letras Win8" ) 


# Gráfica exploratoria por sistema operativo Window para proceador word hasta 100 mil caracteres

xyplot(TamañoArchivo~NumLetrasa/1000,data=Procesa.Word,
       group=VersionProcesa,
       auto.key=list(columns=2),
       par.settings=conf1,ylab="Tamaño del archivo (Kb)",
       xlab="Número de letras \'a\' (miles)",
    type=c("p","a"),
       main="Tamaño del archivo vs número de letras win7 por procesador WORD")


