
cost_polit<-read.csv2("Encuesta_depurada.csv",enc="latin1")


#------------------------------------------------------------------------------#
# Analisis PROBLEMÁTICA 
#------------------------------------------------------------------------------#

# Construye una tabla de frecuencia ordenada de mayora a menor
# de la variable PROBLEMÁTICA
tablaProblm<-with(cost_polit,sort(table(PROBLEMÁTICA),decreasing=T))
tablaProblm

# Construye una tabla en donde se obtienen los porcentajes.
tablaProblmp<-tablaProblm/sum(tablaProblm)*100
tablaProblmp

# Graficación del diagrama de barras para la problemática
op<-par(no.readonly = TRUE)
par(mar=op$mar+c(0,6,0,0))
b1<-barplot(tablaProblmp,horiz=TRUE,las=1,xlim=c(0,30),
            main="Problemática principal en el país",
            xlab="Procentaje")
text(tablaProblmp+2.5,b1,paste(format(tablaProblmp,digits=2),"%"))
par(op)

# Cruce la variable PROBLEMÁTICA con ESTRATO.
tablaProbXEstrato<-with(cost_polit,table(PROBLEMÁTICA,ESTRATO))

tablaProbXEstrato

# Porcentajes por filas
tablaProbXEstrato/apply(tablaProbXEstrato,1,sum)*100
round(tablaProbXEstrato/apply(tablaProbXEstrato,1,sum)*100,1)

# Porcentaje por columnas
t(t(tablaProbXEstrato)/apply(tablaProbXEstrato,2,sum))*100
round(t(t(tablaProbXEstrato)/apply(tablaProbXEstrato,2,sum))*100,1)

#
#write.csv2(tablaProblm,file="prueba.csv",fileEncoding="latin1")
#write.csv2(tablaProblmp,file="porcentajep.csv",fileEncoding="latin1")

#------------------------------------------------------------------------------#
# Analisis Genero
#------------------------------------------------------------------------------#
# Construye una tabla de frecuencia ordenada de mayora a menor
# de la variable GENERO
tablaGENE<-with(cost_polit,sort(table(GÉNERO),decreasing=T))
tablaGENE

# Construye una tabla en donde se obtienen los porcentajes.
tablaGENEP<-tablaGENE/sum(tablaGENE)*100
tablaGENEP

# Graficación del diagrama de barras para GENERO
op2<-par(no.readonly = TRUE)
par(mar=op2$mar+c(0,6,0,0))
b2<-barplot(tablaGENEP,horiz=TRUE,las=1,xlim=c(0,100),
            main="Género de los encuestados",
            xlab="Procentaje")
text(tablaGENEP+2.5,b2,paste(format(tablaGENEP,digits=3),"%"))
par(op2)




#write.csv2(tablaProblm,file="prueba.csv",fileEncoding="latin1")
#write.csv2(tablaProblmp,file="porcentajep.csv",fileEncoding="latin1")

#------------------------------------------------------------------------------#
# Analisis Edad   
#------------------------------------------------------------------------------#

# Histograma de EDAD
h1<-hist(cost_polit$EDAD)
h1
# Excesivo número de intervalos de clase
#h2<-hist(cost_polit$EDAD,nclass=100)
# Escaso número de intervalos de clase
h3<-hist(cost_polit$EDAD,nclass=4)

# Estimación de la densidad mediante un kernel.
h4<-hist(cost_polit$EDAD,freq=F)
lines(density(cost_polit$EDAD),lty=2,col="red",lwd=3)

# Mejora en la presentación del histograma
# Y su estimación de función de densidad.
h5<-hist(cost_polit$EDAD,freq=F,main="Histograma de EDAD",
         ylab="Densidad",xlab="Edad",xlim=c(14,35))
lines(density(cost_polit$EDAD),lty=3,col="red",lwd=3)

# Cálculos con los elementos del histograma
# Elementos del histograma
h5
# Límites de los intervalos de clase
h5$breaks

# Longitud de los intervalos de clase
diff(h5$breaks)

# Altura de cada barra en términos de densidad.
h5$density

# Estimación de la densidad de cadad barra
diff(h5$breaks)*h5$density

# Verificación de la densidad total
sum(diff(h5$breaks)*h5$density)


tablaEDAD<-with(cost_polit,sort(table(EDAD),decreasing=T))
tablaEDAD

write.csv2(tablaEDAD,file="edadf.csv",fileEncoding="latin1")



#------------------------------------------------------------------------------#
# Analisis PROGRAMA
#------------------------------------------------------------------------------#


# Construye una tabla de frecuencia ordenada de mayora a menor
# de la variable PROGRAMA
tablaPROGRAMA<-with(cost_polit,sort(table(PROGRAMA),decreasing=T))
tablaPROGRAMA

# Construye una tabla en donde se obtienen los porcentajes.
tablaPROGRAMAP<-tablaPROGRAMA/sum(tablaPROGRAMA)*100
tablaPROGRAMAP

# Graficación del diagrama de barras para PROGRAMA
op3<-par(no.readonly = TRUE)
par(mar=op3$mar+c(0,6,0,0))

b3<-barplot(tablaPROGRAMAP,horiz=TRUE,las=1,xlim=c(0,70),
            main="Programa al que pertenecen los encuestados",
            xlab="Porcentaje")
text(tablaPROGRAMAP+2.5,b3,paste(format(tablaPROGRAMAP,digits=3),"%"))
par(op3)



#------------------------------------------------------------------------------#
# Analisis Semestre
#------------------------------------------------------------------------------#

tablaSEMESTRE<-with(cost_polit,sort(table(SEMESTRE),decreasing=T))
tablaSEMESTRE

# Histograma de Semestre
h6<-hist(cost_polit$SEMESTRE)
h6
# Excesivo número de intervalos de clase
#h2<-hist(cost_polit$Semestre,nclass=100)
# Escaso número de intervalos de clase
h7<-hist(cost_polit$SEMESTRE,nclass=10)

# Estimación de la densidad mediante un kernel.
h8<-hist(cost_polit$SEMESTRE,freq=F)
lines(density(cost_polit$SEMESTRE),lty=2,col="red",lwd=3)

# Mejora en la presentación del histograma
# Y su estimación de función de densidad.
h9<-hist(cost_polit$SEMESTRE,freq=F,main="Histograma de SEMESTRE",
         ylab="Densidad",xlab="SEMESTRE",xlim=c(0,10))
lines(density(cost_polit$SEMESTRE),lty=3,col="red",lwd=3)




#write.csv2(tablaSEMESTRE,file="semestre.csv",fileEncoding="latin1")

#------------------------------------------------------------------------------#
# Analisis ESTRATO
#------------------------------------------------------------------------------#


# Histograma de ESTRATO
h10<-hist(cost_polit$ESTRATO)
h10
# Excesivo número de intervalos de clase
#h2<-hist(cost_polit$EDAD,nclass=100)
# Escaso número de intervalos de clase
h11<-hist(cost_polit$ESTRATO,nclass=10)

# Estimación de la densidad mediante un kernel.
h12<-hist(cost_polit$ESTRATO,freq=F)
lines(density(cost_polit$ESTRATO),lty=2,col="red",lwd=3)

# Mejora en la presentación del histograma
# Y su estimación de función de densidad.
h13<-hist(cost_polit$ESTRATO,freq=F,main="Histograma de ESTRATO",
         ylab="Densidad",xlab="ESTRATO",xlim=c(0,6))
lines(density(cost_polit$ESTRATO),lty=3,col="red",lwd=3)


tablaESTRATO<-with(cost_polit,sort(table(ESTRATO),decreasing=T))
tablaEDAD

write.csv2(tablaESTRATO,file="estratof.csv",fileEncoding="latin1")

#------------------------------------------------------------------------------#
# Analisis RED.SOCIAL
#------------------------------------------------------------------------------#

tablaRED.SOCIAL<-with(cost_polit,sort(table(RED.SOCIAL),decreasing=T))
tablaRED.SOCIAL

# Construye una tabla en donde se obtienen los porcentajes.
tablaRED.SOCIALP<-tablaRED.SOCIAL/sum(tablaRED.SOCIAL)*100
tablaRED.SOCIALP

# Graficación del diagrama de barras para RED.SOCIAL
op5<-par(no.readonly = TRUE)
par(mar=op2$mar+c(0,6,0,0))
b5<-barplot(tablaRED.SOCIALP,horiz=TRUE,las=1,xlim=c(0,100),
            main="¿Tiene RED Social?",
            xlab="Porcentaje")
text(tablaRED.SOCIALP+2.5,b5,paste(format(tablaRED.SOCIALP,digits=3),"%"))
par(op5)

write.csv2(tablaRED.SOCIAL,file="red.csv",fileEncoding="latin1")
write.csv2(tablaRED.SOCIALP,file="redp.csv",fileEncoding="latin1")


#------------------------------------------------------------------------------#
# Analisis PARTIDO
#------------------------------------------------------------------------------#



tablaPARTIDO<-with(cost_polit,sort(table(PARTIDO),decreasing=T))
tablaPARTIDO

# Construye una tabla en donde se obtienen los porcentajes.
tablaPARTIDOP<-tablaPARTIDO/sum(tablaPARTIDO)*100
tablaPARTIDOP

# Graficación del diagrama de barras para RED.SOCIAL
op6<-par(no.readonly = TRUE)
par(mar=op6$mar+c(0,6,0,0))
b6<-barplot(tablaPARTIDOP,horiz=TRUE,las=1,xlim=c(0,50),
            main="Partido politico",
            xlab="Porcentaje")
text(tablaPARTIDOP+3.5,b6,paste(format(tablaPARTIDOP,digits=3),"%"))
par(op6)

write.csv2(tablaPARTIDO,file="partido.csv",fileEncoding="latin1")
write.csv2(tablaPARTIDOP,file="partidop.csv",fileEncoding="latin1")

#------------------------------------------------------------------------------#
# Analisis POLÍICO.ID
#------------------------------------------------------------------------------#


tablaPOLITICO.ID<-with(cost_polit,sort(table(POLÍTICO.ID),decreasing=T))
tablaPOLITICO.ID

# Construye una tabla en donde se obtienen los porcentajes.
tablaPOLITICO.IDP<-tablaPOLITICO.ID/sum(tablaPOLITICO.ID)*100
tablaPOLITICO.IDP

# Graficación del diagrama de barras para POLÍICO.ID
op7<-par(no.readonly = TRUE)
par(mar=op7$mar+c(0,6,0,0))
b7<-barplot(tablaPOLITICO.IDP,horiz=TRUE,las=1,xlim=c(0,50),
            main="Politico identificaco",
            xlab="Porcentaje")
text(tablaPOLITICO.IDP+2.5,b7,paste(format(tablaPOLITICO.IDP,digits=3),"%"))
par(op7)

write.csv2(tablaPOLITICO.ID,file="politico.csv",fileEncoding="latin1")
write.csv2(tablaPOLITICO.IDP,file="politicop.csv",fileEncoding="latin1")


#------------------------------------------------------------------------------#
# Analisis VOTAR
#------------------------------------------------------------------------------#


tablaVOTAR<-with(cost_polit,sort(table(VOTAR),decreasing=T))
tablaVOTAR

tablaVOTARP<-tablaVOTAR/sum(tablaVOTAR)*100
tablaVOTARP


op8<-par(no.readonly = TRUE)
par(mar=op8$mar+c(0,6,0,0))
b8<-barplot(tablaVOTARP,horiz=TRUE,las=1,xlim=c(0,100),
            main="¿Tiene pensado votar en las próximas elecciones?.",
            xlab="Porcentaje")
text(tablaVOTARP+1.5,b8,paste(format(tablaVOTARP,digits=2),"%"))
par(op8)

#write.csv2(tablaVOTAR,file="votar.csv",fileEncoding="latin1")
#write.csv2(tablaVOTARP,file="votarp.csv",fileEncoding="latin1")

#------------------------------------------------------------------------------#
# Analisis VP.VOTAR
#------------------------------------------------------------------------------#

tablaVP.VOTAR<-with(cost_polit,sort(table(VP.VOTAR),decreasing=T))
tablaVP.VOTAR

tablaVP.VOTARP<-tablaVP.VOTAR/sum(tablaVP.VOTAR)*100
tablaVP.VOTARP


op9<-par(no.readonly = TRUE)
par(mar=op9$mar+c(0,6,0,0))
b9<-barplot(tablaVP.VOTAR,horiz=TRUE,las=1,xlim=c(0,70),
            main="¿Vale la pena  votar en las próximas elecciones?.",
            xlab="Porcentaje")
text(tablaVP.VOTARP,b9,paste(format(tablaVP.VOTARP,digits=2),"%"))
par(op9)

write.csv2(tablaVP.VOTAR,file="vpvotar.csv",fileEncoding="latin1")
write.csv2(tablaVP.VOTARP,file="vpvotarp.csv",fileEncoding="latin1")



#------------------------------------------------------------------------------#
# Analisis POSTURA.EUN
#------------------------------------------------------------------------------#



tablaPOSTURA.EUN<-with(cost_polit,sort(table(POSTURA.EUN),decreasing=T))
tablaPOSTURA.EUN

tablaPOSTURA.EUNP<-tablaPOSTURA.EUN/sum(tablaPOSTURA.EUN)*100
tablaPOSTURA.EUNP

op10<-par(no.readonly = TRUE)
par(mar=op10$mar+c(0,6,0,0))
b10<-barplot(tablaVP.VOTAR,horiz=TRUE,las=1,xlim=c(0,100),
            main="¿Es necesario tener una postura política clara?.",
            xlab="Porcentaje")
text(tablaPOSTURA.EUNP+1.5,b10,paste(format(tablaPOSTURA.EUNP,digits=2),"%"))
par(op10)

write.csv2(tablaPOSTURA.EUN,file="posturaeun.csv",fileEncoding="latin1")
write.csv2(tablaPOSTURA.EUNP,file="posturaeunp.csv",fileEncoding="latin1")


#------------------------------------------------------------------------------#
# Analisis PERT.PARTIDO
#------------------------------------------------------------------------------#



tablaPERT.PARTIDO<-with(cost_polit,sort(table(PERT.PARTIDO),decreasing=T))
tablaPERT.PARTIDO

tablaPERT.PARTIDOP<-tablaPERT.PARTIDO/sum(tablaPERT.PARTIDO)*100
tablaPERT.PARTIDOP

op11<-par(no.readonly = TRUE)
par(mar=op11$mar+c(0,6,0,0))
b11<-barplot(tablaPERT.PARTIDOP,horiz=TRUE,las=1,xlim=c(0,100),
             main="¿El  estudiante  debe pertenecer a un partido político?.",
             xlab="Porcentaje")
text(tablaPERT.PARTIDOP+6.5,b10,paste(format(tablaPERT.PARTIDOP,digits=2),"%"))
par(op11)

write.csv2(tablaPERT.PARTIDO,file="PERT.PARTIDO.csv",fileEncoding="latin1")
write.csv2(tablaPERT.PARTIDOP,file="PERT.PARTIDOP.csv",fileEncoding="latin1")

#------------------------------------------------------------------------------#
# Analisis VOTO.P
#------------------------------------------------------------------------------#

tablaVOTO.P<-with(cost_polit,sort(table(VOTO.P),decreasing=T))
tablaVOTO.P

tablaVOTO.PP<-tablaVOTO.P/sum(tablaVOTO.P)*100
tablaVOTO.PP

op12<-par(no.readonly = TRUE)
par(mar=op12$mar+c(0,6,0,0))
b12<-barplot(tablaVOTO.PP,horiz=TRUE,las=1,xlim=c(0,100),
             main="¿Votó su padre en las anteriores elecci´ones presidenciales?",
             xlab="Porcentaje")
text(tablaVOTO.PP+4.5,b12,paste(format(tablaVOTO.PP,digits=2),"%"))
par(op12)

write.csv2(tablaVOTO.P,file="VOTO.csv",fileEncoding="latin1")
write.csv2(tablaVOTO.PP,file="VOTO.PP.csv",fileEncoding="latin1")

#------------------------------------------------------------------------------#
# Analisis CANDIDATO.P
#------------------------------------------------------------------------------#


tablaCANDIDATO.P<-with(cost_polit,sort(table(CANDIDATO.P),decreasing=T))
tablaCANDIDATO.P

tablaCANDIDATO.PP<-tablaCANDIDATO.P/sum(tablaCANDIDATO.P)*100
tablaCANDIDATO.PP

op13<-par(no.readonly = TRUE)
par(mar=op13$mar+c(0,6,0,0))
b13<-barplot(tablaCANDIDATO.PP,horiz=TRUE,las=1,xlim=c(0,100),
             main="¿Por cuál candidato votó su padre si él lo hizo?",
             xlab="Porcentaje")
text(tablaCANDIDATO.PP+4.5,b13,paste(format(tablaCANDIDATO.PP,digits=2),"%"))
par(op13)

write.csv2(tablaCANDIDATO.P,file="candidato.p.csv",fileEncoding="latin1")
write.csv2(tablaCANDIDATO.PP,file="candidato.PP.csv",fileEncoding="latin1")



#------------------------------------------------------------------------------#
# Analisis VOTO.M
#------------------------------------------------------------------------------#

tablaVOTO.M<-with(cost_polit,sort(table(VOTO.M),decreasing=T))
tablaVOTO.M

tablaVOTO.MP<-tablaVOTO.M/sum(tablaVOTO.M)*100
tablaVOTO.MP

op13<-par(no.readonly = TRUE)
par(mar=op13$mar+c(0,6,0,0))
b13-barplot(tablaVOTO.MP,horiz=TRUE,las=1,xlim=c(0,100),
             main="¿Votó su madre en las anteriores elecci´ones presidenciales?",
             xlab="Porcentaje")
text(tablaVOTO.MP+4.5,b12,paste(format(tablaVOTO.MP,digits=2),"%"))
par(op13)

write.csv2(tablaVOTO.M,file="VOTOM.csv",fileEncoding="latin1")
write.csv2(tablaVOTO.MP,file="VOTO.MP.csv",fileEncoding="latin1")


#------------------------------------------------------------------------------#
# Analisis CANDIDATO.P
#------------------------------------------------------------------------------#


tablaCANDIDATO.M<-with(cost_polit,sort(table(CANDIDATO.M),decreasing=T))
tablaCANDIDATO.M

tablaCANDIDATO.MP<-tablaCANDIDATO.M/sum(tablaCANDIDATO.M)*100
tablaCANDIDATO.MP

op14<-par(no.readonly = TRUE)
par(mar=op13$mar+c(0,6,0,0))
b14<-barplot(tablaCANDIDATO.MP,horiz=TRUE,las=1,xlim=c(0,50),
             main="¿Por cuál candidato votó su madre si él lo hizo?",
             xlab="Porcentaje")
text(tablaCANDIDATO.MP+4.5,b14,paste(format(tablaCANDIDATO.MP,digits=2),"%"))
par(op14)

write.csv2(tablaCANDIDATO.M,file="candidato.p.csv",fileEncoding="latin1")
write.csv2(tablaCANDIDATO.MP,file="candidato.PP.csv",fileEncoding="latin1")





