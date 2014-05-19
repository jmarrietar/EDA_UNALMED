################################################################################
# Educación formal desde preescolar hasta educación media
# en Colombia.
# Número de alumnos matriculado por nivel educativo y sector
# Información definitiva - Año 2011
# DANE.
# Análisis preeliminar de la base de datos.
# Fecha: 10 de Septiembre de 2013
################################################################################
EdFor<-read.csv2("EFormal_alumnos_sector_2011_Adecuada.csv",enc="latin1",row.names=1)
str(EdFor)
summary(EdFor)

# Gráfica de barras incial del número de estudiantes totales
# En todos los niveles.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(EdFor$TOTAL.Total,horiz=T,names=row.names(EdFor),
        las=1,cex.names=0.8)
par(op)

# Selección de la variable TOTAL.Total
total<- EdFor$TOTAL.Total
# Nombre de los elementos del vector
names(total)<-row.names(EdFor)
# Vector con los nombres de los departamentos
total
# Ordenación del vector de menor a mayor.
total<-sort(total)

# Gráfica de barra del número de estudiantes por departamento
# Mejorada, con título.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(total/1000,horiz=T,names=names(total),
        xlim=c(0,1600),
        las=1,cex.names=0.8,xlab="Número de estudiantes (miles)",
        main="Distribución por departamento del número de estudiantes totales\n en educación formal")
par(op)

# Cálculo de la razón entre estudiantes de Secundaria y Primaria.
TasaPrim_Bach<-with(EdFor,SECUNDARIA.Y.MEDIA.Total/PRIMARIA.Total)
# Asignación de los nombres a los elementos del vector.
names(TasaPrim_Bach)<-row.names(EdFor)
# Ordenación del vector
TasaPrim_Bach<-sort(TasaPrim_Bach)

# Gráfica de la razón entre estudiantes de Secundaria y Primaria
# Por departamento.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(TasaPrim_Bach,horiz=T,names=names(total),
        las=1,cex.names=0.8,xlab="Razón bachillerato/primaria",
        xlim=c(0,1.5),
        main="Distribución de la razón primaria/Bachillerato \n por departamento")
abline(v=1,col="red",lwd=2)
par(op)

# Gráfica de la razón entre estudiantes matriculados en No Oficial y Oficial
RazonOfiNoOfi<-with(EdFor,TOTAL.Oficial/TOTAL.No.Oficial)
PorcentajeNoOfiTotal<-with(EdFor,TOTAL.No.Oficial/TOTAL.Total)*100

names(RazonOfiNoOfi)<-row.names(EdFor)
names(PorcentajeNoOfiTotal)<-row.names(EdFor)

RazonOfiNoOfi<-sort(RazonOfiNoOfi)
PorcentajeNoOfiTotal<-sort(PorcentajeNoOfiTotal)

RazonOfiNoOfi
PorcentajeNoOfiTotal


# Gráfica de la razón entre estudiantes de Sector No oficial y Oficial
# Por departamento.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(RazonOfiNoOfi,horiz=T,names=names(RazonOfiNoOfi),
        las=1,cex.names=0.8,xlab="Razón Oficial/No Oficial",
        #xlim=c(0,1.5),
        main="Distribución de la razón Oficial/No Oficial\n por departamento")
par(op)

# Gráfica del procentaje del Sector No oficial con respecto al total
# Por departamento.
op <- par(no.readonly = TRUE)
par(mar=op$mar+c(0,5,0,0))
barplot(PorcentajeNoOfiTotal,horiz=T,names=names(PorcentajeNoOfiTotal),
        las=1,cex.names=0.8,xlab="Porcentaje",
        xlim=c(0,40),
        main="Distribución del porcentaje de los No oficiales con respecto al total\n por departamento")
par(op)

barplot(t(as.matrix(subset(EdFor,select=TOTAL.Oficial:TOTAL.Subsidiada))),
        horiz=T,las=1)


