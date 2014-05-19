setwdsetwd("C:/Users/JosePortatil/Dropbox/8 semestre/Estadistica Descriptiva/Trabajo_Final_EDAED/Bases_de_Datos")

BD<-read.table("icfes2012 POR DEPTO.csv",header=  TRUE, sep='',  dec=',')
BD2<-read.table('icfes2012.csv', header=  TRUE, sep='',	dec=',')

BD3 <- read.csv2("Municipios y Centros Poblados.csv",enc="latin1") 
  
  
  
str(BD2)
str(BD3)
colnames(BD) <- c("Código.Municipio", "Puntaje")

prueba<-unique(BD3[,c("Código.Municipio","Nombre.Municipio")])
prueba$row.names <- NULL
total<- merge(prueba,BD,by="Código.Municipio")
BD.Municipios<-total[c("Código.Municipio","Nombre.Municipio","Puntaje")]

boxplot (BD.Municipios$Puntaje,ylab="Puntaje ", main="Puntaje Municipios")

write.csv2(BD.Municipios,file="Municipios.csv",fileEncoding="latin1")



