################################################################################
# Educaci�n formal desde preescolar hasta educaci�n media
# en Colombia.
# N�mero de alumnos matriculado por nivel educativo y sector
# Informaci�n definitiva - A�o 2011
# DANE.
# An�lisis preeliminar de la base de datos.
# Fecha: 10 de Septiembre de 2013
################################################################################
EdFor<-read.csv2("EFormal_alumnos_sector_2011_Adecuada.csv",enc="latin1",row.names=1)
str(EdFor)
summary(EdFor)

# Gr�fica de barras incial del n�mero de estudiantes totales
# En todos los niveles.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(EdFor$TOTAL.Total,horiz=T,names=row.names(EdFor),
        las=1,cex.names=0.8)
par(op)

# Selecci�n de la variable TOTAL.Total
total<- EdFor$TOTAL.Total
# Nombre de los elementos del vector
names(total)<-row.names(EdFor)
# Vector con los nombres de los departamentos
total
# Ordenaci�n del vector de menor a mayor.
total<-sort(total)

# Gr�fica de barra del n�mero de estudiantes por departamento
# Mejorada, con t�tulo.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(total/1000,horiz=T,names=names(total),
        xlim=c(0,1600),
        las=1,cex.names=0.8,xlab="N�mero de estudiantes (miles)",
        main="Distribuci�n por departamento del n�mero de estudiantes totales\n en educaci�n formal")
par(op)

# C�lculo de la raz�n entre estudiantes de Secundaria y Primaria.
TasaPrim_Bach<-with(EdFor,SECUNDARIA.Y.MEDIA.Total/PRIMARIA.Total)
# Asignaci�n de los nombres a los elementos del vector.
names(TasaPrim_Bach)<-row.names(EdFor)
# Ordenaci�n del vector
TasaPrim_Bach<-sort(TasaPrim_Bach)

# Gr�fica de la raz�n entre estudiantes de Secundaria y Primaria
# Por departamento.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(TasaPrim_Bach,horiz=T,names=names(total),
        las=1,cex.names=0.8,xlab="Raz�n bachillerato/primaria",
        xlim=c(0,1.5),
        main="Distribuci�n de la raz�n primaria/Bachillerato \n por departamento")
abline(v=1,col="red",lwd=2)
par(op)

# Gr�fica de la raz�n entre estudiantes matriculados en No Oficial y Oficial
RazonOfiNoOfi<-with(EdFor,TOTAL.Oficial/TOTAL.No.Oficial)
PorcentajeNoOfiTotal<-with(EdFor,TOTAL.No.Oficial/TOTAL.Total)*100

names(RazonOfiNoOfi)<-row.names(EdFor)
names(PorcentajeNoOfiTotal)<-row.names(EdFor)

RazonOfiNoOfi<-sort(RazonOfiNoOfi)
PorcentajeNoOfiTotal<-sort(PorcentajeNoOfiTotal)

RazonOfiNoOfi
PorcentajeNoOfiTotal


# Gr�fica de la raz�n entre estudiantes de Sector No oficial y Oficial
# Por departamento.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(RazonOfiNoOfi,horiz=T,names=names(RazonOfiNoOfi),
        las=1,cex.names=0.8,xlab="Raz�n Oficial/No Oficial",
        #xlim=c(0,1.5),
        main="Distribuci�n de la raz�n Oficial/No Oficial\n por departamento")
par(op)

# Gr�fica del procentaje del Sector No oficial con respecto al total
# Por departamento.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(PorcentajeNoOfiTotal,horiz=T,names=names(PorcentajeNoOfiTotal),
        las=1,cex.names=0.8,xlab="Porcentaje",
        xlim=c(0,40),
        main="Distribuci�n del porcentaje de los No oficiales con respecto al total\n por departamento")
par(op)

barplot(t(as.matrix(subset(EdFor,select=TOTAL.Oficial:TOTAL.Subsidiada))),
        horiz=T,las=1)


