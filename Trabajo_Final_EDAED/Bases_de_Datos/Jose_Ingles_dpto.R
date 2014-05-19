setwd("C:/Users/JosePortatil/Dropbox/8 semestre/Estadistica Descriptiva/Trabajo_Final_EDAED/Bases_de_Datos")
# Lectura de la base de datos
icfes2012<-read.csv2("icfes2012.csv",enc="latin1")
str(icfes2012)

# Cálculo de promedio de cada área por departamento por grupo
promDepto<-aggregate(subset(icfes2012,select=PROMEDIOMATEMATICA:PROMEDIOSOCIALES),
                     list(DEPARTAMENTO=icfes2012$DEPARTAMENTO),mean,na.rm=T)
promDepto


# Mejores departamentos por PROMEDIOINGLES
promDepto[order(promDepto$PROMEDIOINGLES,decreasing=T),]

# Gráfica de resultado por deptartamento
require(lattice)
conf1<-simpleTheme(pch=19,col="blue")
# Diagrama de caja y bigotes
bwplot(DEPARTAMENTO~PROMEDIOINGLES,data=icfes2012,
       par.settings=conf1, main="Resultados en Ingles ICFES")


# Cálculo de la mediana por departamento
medianasDepto<-with(icfes2012,tapply(PROMEDIOINGLES,
                                     DEPARTAMENTO,median))
medianasDepto
# Ordenar de mayor a menor por medianas
medianasDepto<-sort(medianasDepto,dec=T)
medianasDepto

# Reordenar el orden de las etiquetas en la base de datos
# de tal manera que la gráfica apareza ordenada
icfes2012$DEPARTAMENTO<-factor(icfes2012$DEPARTAMENTO,
                               levels=names(medianasDepto))
conf1<-simpleTheme(pch=19,col="blue")
bwplot(DEPARTAMENTO~PROMEDIOINGLES,data=icfes2012,
       par.settings=conf1)

myfunction <- function(x){

  return(x)
}


promDepto$Promedio<-sapply(( 3*promDepto$PROMEDIOMATEMATICA+3*promDepto$PROMEDIOLENGUAJE+2*promDepto$PROMEDIOSOCIALES+ promDepto$PROMEDIOBIOLOGIA+promDepto$PROMEDIOFISICA +promDepto$PROMEDIOQUIMICA+promDepto$PROMEDIOFILOSOFIA+promDepto$PROMEDIOINGLES)/13,myfunction); 



write.csv2(promDepto,file="Departamentos.csv",fileEncoding="latin1")



