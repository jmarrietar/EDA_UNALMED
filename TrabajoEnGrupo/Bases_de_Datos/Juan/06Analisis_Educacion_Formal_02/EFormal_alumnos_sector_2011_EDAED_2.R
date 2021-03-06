################################################################################
# Educaci�n formal desde preescolar hasta educaci�n media
# en Colombia.
# N�mero de alumnos matriculado por nivel educativo y sector
# Informaci�n definitiva - A�o 2011
# DANE.
# An�lisis preeliminar de la base de datos.
# Segunda parte
# Fecha: 10 de Septiembre de 2013
################################################################################
EdFor<-read.csv2("EFormal_alumnos_sector_2011_Adecuada.csv",enc="latin1",row.names=1)
str(EdFor)
summary(EdFor)

# Ordenar la base de datos por la variable TOTAL.Total
EdFor<-EdFor[order(EdFor$TOTAL.Total),]
# Grupo de colores primarios
colores1<-c("blue","red","green")
# Grupo de colores de la tabla de colores
#colores1<-c("palevioletred1","paleturquoise2","palegreen3")
# Grupo de colores con Color Brewer
#colores1<-c("#7FC97F","#BEAED4","#FCC086")

op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(t(as.matrix(subset(EdFor,select=TOTAL.Oficial:TOTAL.Subsidiada)))/1000,
        horiz=T,las=1,legend.text=c("Oficial","No oficial","Subsidiada"),
        args.legend=list(x="bottomright"),
        cex.names=0.8,col=colores1,
        xlab="N�mero de matriculados (miles)",
        main="Distribuci�n por tipo de matr�cula\n por departamento")
par(op)

# Por distribuci�n de porcentajes
porcentaje1<-t(as.matrix(subset(EdFor,select=TOTAL.Oficial:TOTAL.Subsidiada)/EdFor$TOTAL.Total))*100

op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(porcentaje1,horiz=T,las=1,xlim=c(0,100),cex.names=0.8,
        legend.text=c("Oficial","No oficial","Subsidiada"),col=colores1,
        args.legend=list(x="bottomleft"),
        xlab="Porcentaje",
        main="Distribuci�n porcentual del tipo de matr�cula\n por departamento")
par(op)



