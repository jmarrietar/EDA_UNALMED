setwd("C:/Users/EQUIPO JOSE/Dropbox/8 semestre/Estadistica Descriptiva/Trabajo_Final_EDAED/Bases_de_Datos")

# Lectura de la base de datos
Departamentos<-read.csv2("Departamentos.csv",enc="latin1")
str(Departamentos)
newdata <- Departamentos[order(Departamentos$Promedio),]

#name=c(as.character(Departamentos$DEPARTAMENTO))
op<-par(no.readonly = TRUE)
par(mar=op$mar+c(0,1.5,0,0))


barplot(newdata $Promedio,names = newdata $DEPARTAMENTO,horiz=TRUE,las=1, main="Promedio Total departamentos ", 
        xlab="Promedio ",cex.names=0.7,xlim=c(0,60))


