################################################################################
# Estadística Descriptiva y Análisis Exploratorio de Datos
# Anotación sobre gráfica de tamaño de archivo
# Por: Kenneth Roy Cabrera Torres
# Fecha: 29 de Agosto de 2013
# Modificación: 
################################################################################

baseT<-read.csv2("BaseTotalTamArch.csv",enc="latin1") 
str(baseT)

# Selección de los datos del grupo 2
g2<-droplevels(subset(baseT,Grupo==2))
str(g2)

# Gráfica inicial
with(g2,plot(TamañoArchivo~NumLetrasa))

# Grafica definitiva
with(g2,plot(NumLetrasa/1000,TamañoArchivo,type="n",
             main="Tamaño del archivo vs Número de letras a",
             xlab="Número de letras \"a\" (en miles)",
             ylab="Tamaño del archivo (Kb)",
             ylim=c(0,130)))
with(subset(g2,Formato==".doc"),lines(NumLetrasa/1000,TamañoArchivo,col="red"))
with(subset(g2,Formato==".docx"),lines(NumLetrasa/1000,TamañoArchivo,col="blue"))
legend("topleft",legend=c(".doc",".docx"),col=c("red","blue"),lty=1)


