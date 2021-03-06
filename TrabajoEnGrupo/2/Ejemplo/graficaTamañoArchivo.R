################################################################################
# Estad�stica Descriptiva y An�lisis Exploratorio de Datos
# Anotaci�n sobre gr�fica de tama�o de archivo
# Por: Kenneth Roy Cabrera Torres
# Fecha: 29 de Agosto de 2013
# Modificaci�n: 
################################################################################

baseT<-read.csv2("BaseTotalTamArch.csv",enc="latin1") 
str(baseT)

# Selecci�n de los datos del grupo 2
g2<-droplevels(subset(baseT,Grupo==2))
str(g2)

# Gr�fica inicial
with(g2,plot(Tama�oArchivo~NumLetrasa))

# Grafica definitiva
with(g2,plot(NumLetrasa/1000,Tama�oArchivo,type="n",
             main="Tama�o del archivo vs N�mero de letras a",
             xlab="N�mero de letras \"a\" (en miles)",
             ylab="Tama�o del archivo (Kb)",
             ylim=c(0,130)))
with(subset(g2,Formato==".doc"),lines(NumLetrasa/1000,Tama�oArchivo,col="red"))
with(subset(g2,Formato==".docx"),lines(NumLetrasa/1000,Tama�oArchivo,col="blue"))
legend("topleft",legend=c(".doc",".docx"),col=c("red","blue"),lty=1)


