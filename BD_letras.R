BD<-read.csv2("Tama�oArchivosBD.csv",enc="latin1")

Integrante1<-subset(BD,Integrante==1)
Integrante2<-subset(BD,Integrante==2)
Integrante3<-subset(BD,Integrante==3)
Integrante4<-subset(BD,Integrante==4)


with(Integrante1,plot(Numero.de.letras..a...Miles.,Tama�o.de.archivo.en.disco..KB.,type="l",col="red"))

lines(Integrante2$Numero.de.letras..a...Miles.,Integrante2$Tama�o.de.archivo.en.disco..KB.,col="green")
lines(Integrante3$Numero.de.letras..a...Miles.,Integrante3$Tama�o.de.archivo.en.disco..KB.,col="blue")
lines(Integrante4$Numero.de.letras..a...Miles.,Integrante4$Tama�o.de.archivo.en.disco..KB.,col="black")