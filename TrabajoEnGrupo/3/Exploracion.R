BD<-read.csv2("BD_completa.csv",enc="latin1")


# Construye una tabla de frecuencia ordenada de mayora a menor
# de la variable Nivel Academico
tablaNivelA<-with(BD,sort(table(Nivel.Academ),decreasing=T))
tablaNivelA

# Construye una tabla en donde se obtienen los porcentajes.
tablaNivelAp<-tablaNivelA/sum(tablaNivelA)*100
tablaNivelAp

# Graficación del diagrama de barras para Nivel academico

op<-par(no.readonly = TRUE)
par(mar=op$mar+c(0,6,0,0))
b1<-barplot(tablaNivelAp,horiz=TRUE,las=1,xlim=c(0,100),
            main="Nivel academico",
            xlab="Procentaje")
text(tablaNivelAp+2.5,b1,paste(format(tablaNivelAp,digits=2),"%"))
par(op)


# Construye una tabla de frecuencia ordenada de mayora a menor
# de la variable Forma audio
tablaFormaA<-with(BD,sort(table(Forma.audio),decreasing=T))
tablaFormaA

# COnstruye diagrama de pastel para formato de audio 
slices <-tablaFormaA
lbls <- c("Audifonos""Parlantes")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Forma de audios")

# COnstruye diagrama de pastel para ¿Le Gusta estudiar con musica? 
Gusta.estud.musica<-with(BD,sort(table(Gusta.estud.musica),decreasing=T))
Gusta.estud.musica

slices <-Gusta.estud.musica
lbls <- c("Si","No")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="¿Le Gusta estudiar con musica? ",radius=1)


# Construye una tabla de frecuencia ordenada de mayora a menor
# del Genero Musical 
GeneroMusical <-with(BD,sort(table(Genero.musical),decreasing=T))
GeneroMusical

# Construye una tabla en donde se obtienen los porcentajes.
GeneroMusicalp<-GeneroMusical/sum(GeneroMusical)*100
GeneroMusicalp

# Graficación del diagrama de barras para GeneroMusicalp

op<-par(no.readonly = TRUE)
par(mar=op$mar+c(0,3,0,0))
b1<-barplot(GeneroMusicalp,horiz=TRUE,las=1,xlim=c(0,100),
            main="Genero Musical ",
            xlab="Procentaje")
text(GeneroMusicalp+0.5,b1,paste(format(GeneroMusicalp,digits=2),"%"))
par(op)


#------------------------------------------------------------------------------#
# Analisis Genero
#------------------------------------------------------------------------------#
# Construye una tabla de frecuencia ordenada de mayora a menor
# de la variable GENERO
tablaGENE<-with(BD,sort(table(Genero),decreasing=T))
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

#------------------------------------------------------------------------------#

# Histograma de EDAD
h1<-hist(BD$Edad)
h1

# Estimación de la densidad mediante un kernel.
h4<-hist(BD$Edad,freq=F)
h4
lines(density(BD$Edad),lty=2,col="red",lwd=3)

#------------------------------------------------------------------------------#

# Resumenes de tiempo con musica y sin Musica
summary(BD$Tiempo.con.musica) # importante tener en cuenta
summary (BD$Tiempo.sin.musica) # importante tener en cuenta

# Histograma de  Tiempo con musica
h2<-hist(BD$Tiempo.con.musica,main="Tiempo con musica",col="gray",xlab="Tiempo en segundos")
lines(density(BD$Tiempo.con.musica))
# Histograma de Tiempo sin musica 
h2<-hist(breaks=7,BD$Tiempo.sin.musica,main="Tiempo sin musica",col="gray",xlab="Tiempo en segundos")


# Estimación de la densidad mediante un kernel con musica
h4<-hist(BD$Tiempo.con.musica,freq=F,xlab="Tiempo en segundos",main="Tiempo con musica")
lines(density(BD$Tiempo.con.musica),lty=2,col="red",lwd=3)

# Estimación de la densidad mediante un kernel sin musica
h4<-hist(BD$Tiempo.sin.musica,freq=F,xlab="Tiempo en segundos",main="Tiempo sin musica")
lines(density(BD$Tiempo.con.musica),lty=2,col="red",lwd=3)

# Boxplot Para tiempo con musica
boxplot(BD$Tiempo.con.musica,data=mtcars, main="Tiempo con musica", 
         ylab="Segundos")
# Boxplot Para tiempo sin musica
boxplot(BD$Tiempo.sin.musica,data=mtcars, main="Tiempo sin musica", 
        ylab="Segundos")
