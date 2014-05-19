################################################################################
# Estadística Descriptiva y Análisis Exploratorio de Datos
# Base de datos sobre encuesta de costumbres políticas
# Por: Kenneth Roy Cabrera Torres
# Fecha: 14 de Agosto de 2013
# Modificación: 20 de Agosto de 2013
################################################################################
# Evaluación inicial de la base de datos
################################################################################
# Lectura de la base de datos
cost_polit<-read.csv2("ENCUESTA.csv",enc="latin1")
# Estructura de la base de datos
str(cost_polit)
# Primeros seis regístros de la base de datos
head(cost_polit)
# Resumen de la base de datos
summary(cost_polit)

################################################################################
# Depuración de la base de datos
################################################################################
#------------------------------------------------------------------------------#
# Exploración para EDAD
#------------------------------------------------------------------------------#
# Histograma de EDAD
h1<-hist(cost_polit$EDAD)
h1
# Excesivo número de intervalos de clase
h2<-hist(cost_polit$EDAD,nclass=100)
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

#------------------------------------------------------------------------------#
# Exploración y depuración para PROGRAMA
#------------------------------------------------------------------------------#
# Toda la variables
cost_polit$PROGRAMA
# Los niveles
levels(cost_polit$PROGRAMA)
# Otra forma de mostrar los niveles
with(cost_polit,levels(PROGRAMA))

# Unificación de los niveles
# Uso de bibliotecas de funciones adicionales
require(car)
with(cost_polit,recode(PROGRAMA,"'I sistemas'='I Sistemas';
                       'I forestal'='I Forestal';
                       'zootecnia'='Zootecnia';
                       'I administrativa'='I Administrativa';
                       'I industrial'='I Industrial'"))

cost_polit$PROGRAMA<-with(cost_polit,recode(PROGRAMA,"'I sistemas'='I Sistemas';
                       'I forestal'='I Forestal';
                       'zootecnia'='Zootecnia';
                       'I administrativa'='I Administrativa';
                       'I industrial'='I Industrial'"))

# Verificación de los nuevos niveles de la variable PROGRAMA
with(cost_polit,levels(PROGRAMA))

# Tabla de frecuencias de PROGRAMA
with(cost_polit,table(PROGRAMA))

# Diagrama de barras para PROGRAMA
barplot(with(cost_polit,table(PROGRAMA)))

# Diagrama de barras para PROGRAMA, ordenado
barplot(with(cost_polit,sort(table(PROGRAMA))))

# Diagrama de barras para PROGRAMA, ordenado de mayor a menor
barplot(with(cost_polit,sort(table(PROGRAMA),decreasing=T)))

# Cálculo de porcentajes
tablaPROGRAMA<-with(cost_polit,sort(table(PROGRAMA),decreasing=T))
tablaPROGRAMA
tablaPROGRAMA/sum(tablaPROGRAMA)*100
tablaPROGRAMA<-tablaPROGRAMA/sum(tablaPROGRAMA)*100
tablaPROGRAMA

# Diagrama de barras para PROGRAMA, ordenado de mayor a menor
# con visualización de porcentajes
b1<-barplot(with(cost_polit,tablaPROGRAMA),ylim=c(0,60),ylab="Porcentaje",
            xlab="Programa",
            main="Diagrama de barras de \n distribución de porcentajes en programas")
text(b1,tablaPROGRAMA+1,paste(format(tablaPROGRAMA,digits=2),"%"))

# Unificación de criterios de la PROBLEMÁTICA
with(cost_polit,levels(PROBLEMÁTICA))
require(car)
levels(with(cost_polit,recode(PROBLEMÁTICA,
     "c('corrupción','mal manejo dinerodel gob','corrupción','corrupción política')='Corrupción'")))
