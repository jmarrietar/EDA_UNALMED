
# Lectura de la base de datos
cost_polit<-read.csv2("ENCUESTA.csv",enc="latin1")

# Uso de bibliotecas de funciones adicionales
require(car)

#------------------------------------------------------------------------------#
# Depuración para PROGRAMA
#------------------------------------------------------------------------------#

with(cost_polit,levels(PROGRAMA))

cost_polit$PROGRAMA<-with(cost_polit,recode(PROGRAMA,"'I sistemas'='I Sistemas';
                       'I forestal'='I Forestal';
                       'zootecnia'='Zootecnia';
                       'I administrativa'='I Administrativa';
                       'I industrial'='I Industrial'"))

# Verificación de los nuevos niveles de la variable PROGRAMA
with(cost_polit,levels(PROGRAMA))

#------------------------------------------------------------------------------#
# Depuración para PROBLEMÁTICA
#------------------------------------------------------------------------------#

with(cost_polit,levels(PROBLEMÁTICA))

# Unificación de criterios de la PROBLEMÁTICA
cost_polit$PROBLEMÁTICA<-with(cost_polit,recode(PROBLEMÁTICA,
                                                "c('corrupción','mal manejo dinerodel gob','corrupción','corrupción política','política')='Corrupción';
      c('Agricultura','las lluvias','TLC')='Problema agrario';
      c('alta tasa accidentes','conductores ebrios')='Seguridad vial';
      c('bacrim','impunidad derechos','inseguridad','la justicia','seguridad','violencia','narcotráfico')='Seguridad ciudadana';
      c('charlas con guerrilla','conflicto armado','guerra Intena','la guerra','Proceso de paz')='Conflicto armado';
      c('desigualdad','desplazamiento','La economía','pobreza','Pobreza','hidroeléctricas','paro Naciona')='Economía y equidad';
      c('educación','educación Calidad')='Educación';
      c('salud')='Salud'"))

# Convierte una factor a sarta (string)
cost_polit$PROBLEMÁTICA<-as.character(cost_polit$PROBLEMÁTICA)

# Reemplaza los vacíos por "No responde"
cost_polit$PROBLEMÁTICA[cost_polit$PROBLEMÁTICA==""]<-"No responde"

#------------------------------------------------------------------------------#
# Depuración para PARTIDO
#------------------------------------------------------------------------------#

# Unificación de criterios para PARTIDO
cost_polit$PARTIDO<-with(cost_polit,recode(PARTIDO,"'uribista'='u';
                        'no respondo'='No responde';
                       c('na','n/a','ninguno')='Ninguno';"))

# Convierte una factor a sarta (string)
cost_polit$PARTIDO<-as.character(cost_polit$PARTIDO)

# Reemplaza los vacíos por "No responde"
cost_polit$PARTIDO[cost_polit$PARTIDO==""]<-"No responde"


# De nuevo convierte la variable a tipo factor ## ¿Que es variable factor?
cost_polit$PARTIDO<-factor(cost_polit$PARTIDO)


#------------------------------------------------------------------------------#
# Depuración para POLITICO.ID
#------------------------------------------------------------------------------#
# Unificación de criterios para POLÍTICO.ID
cost_polit$POLÍTICO.ID<-with(cost_polit,recode(POLÍTICO.ID,"
                       
                        'no respondo'='No responde';
                       c('na','n/a','ninguno','ns/nr')='Ninguno';"))


# Convierte una factor a sarta (string)
cost_polit$POLÍTICO.ID<-as.character(cost_polit$POLÍTICO.ID)

# Reemplaza los vacíos por "No responde"
cost_polit$POLÍTICO.ID[cost_polit$POLÍTICO.ID==""]<-"No responde"


# De nuevo convierte la variable a tipo factor 
cost_polit$POLÍTICO.ID<-factor(cost_polit$POLÍTICO.ID)


#------------------------------------------------------------------------------#
# Depuración para VOTAR
#------------------------------------------------------------------------------#
# Unificación de criterios para VOTAR 
cost_polit$VOTAR<-with(cost_polit,recode(VOTAR,"
                             'si'='Si';            
                        c('no','no sabe')='No'; 
                         c('no sabe','no puedo')='No responde';"))


# Convierte una factor a sarta (string)
cost_polit$VOTAR<-as.character(cost_polit$VOTAR)

# Reemplaza los vacíos por "No responde"
cost_polit$VOTAR[cost_polit$VOTAR==""]<-"No responde"


# De nuevo convierte la variable a tipo factor 
cost_polit$VOTAR<-factor(cost_polit$VOTAR)

#------------------------------------------------------------------------------#
# Depuración para VP.VOTAR
#------------------------------------------------------------------------------#
cost_polit$VP.VOTAR<-with(cost_polit,recode(VP.VOTAR, "
                            c('si','cvb si')='Si'; 
                            c('no','n')='No'; 
                              c('no sabe','Ns/nr','depende')='No responde';"))

# Convierte una factor a sarta (string)
cost_polit$VP.VOTAR<-as.character(cost_polit$VP.VOTAR)

# Reemplaza los vacíos por "No responde"
cost_polit$VP.VOTAR[cost_polit$VP.VOTAR==""]<-"No responde"


# De nuevo convierte la variable a tipo factor 
cost_polit$VP.VOTAR<-factor(cost_polit$VP.VOTAR)


#------------------------------------------------------------------------------#
# Depuración para POSTURA.EUN 
#------------------------------------------------------------------------------#
cost_polit$POSTURA.EUN<-with (cost_polit,recode(POSTURA.EUN,"
                                                c('Si','si')='Si';
                                                  'no'='No';
                                                c('n/a','no necesariamente','posiblemente')='No responde';" ) )

# Convierte una factor a sarta (string)
cost_polit$POSTURA.EUN<-as.character(cost_polit$POSTURA.EUN)

# Reemplaza los vacíos por "No responde"
cost_polit$POSTURA.EUN[cost_polit$POSTURA.EUN==""]<-"No responde"


# De nuevo convierte la variable a tipo factor 
cost_polit$POSTURA.EUN<-factor(cost_polit$POSTURA.EUN)



#------------------------------------------------------------------------------#
# Depuración para PERT.PARTIDO
#------------------------------------------------------------------------------#
cost_polit$PERT.PARTIDO<-with (cost_polit,recode(PERT.PARTIDO,"
                                                c('s','si')='Si';
                                                c('No sabe','no necesariamente','depende')='No responde';
                                                c('n o','no')='No' ;" ) )

# Convierte una factor a sarta (string)
cost_polit$PERT.PARTIDO<-as.character(cost_polit$PERT.PARTIDO)

# Reemplaza los vacíos por "No responde"
cost_polit$PERT.PARTIDO[cost_polit$PERT.PARTIDO==""]<-"No responde"


# De nuevo convierte la variable a tipo factor 
cost_polit$PERT.PARTIDO<-factor(cost_polit$PERT.PARTIDO)


#------------------------------------------------------------------------------#
# Depuración para VOTO.P
#------------------------------------------------------------------------------#

cost_polit$VOTO.P<-with (cost_polit,recode(VOTO.P,"
                                               'si'='Si';
                                                'no sabe'='No sabe'; 
                                                'na'='No responde';
                                                c('fallecido','no')='No' ;" ) )



# Convierte una factor a sarta (string)
cost_polit$VOTO.P<-as.character(cost_polit$VOTO.P)

# Reemplaza los vacíos por "No responde"
cost_polit$VOTO.P[cost_polit$VOTO.P==""]<-"No responde"


# De nuevo convierte la variable a tipo factor 
cost_polit$VOTO.P<-factor(cost_polit$VOTO.P)

#------------------------------------------------------------------------------#
# Depuración para CANDIDATO.P 
#------------------------------------------------------------------------------#

cost_polit$CANDIDATO.P <-with (cost_polit,recode(CANDIDATO.P ,"
                                               'si'='Si';
'en blanco'='En blanco';
'santos'= 'Santos';
'mockus'='Mockus';
'fajardo'='Fajardo';
'uribe'='Uribe'; 
'no sabe'='No sabe';
                                                c('n/a','na')='No aplica';
                                                'no respondo'='No responde' ;" ) )

cost_polit$CANDIDATO.P<-as.character(cost_polit$CANDIDATO.P)


cost_polit$CANDIDATO.P[cost_polit$CANDIDATO.P==""]<-"No responde"


cost_polit$CANDIDATO.P<-factor(cost_polit$CANDIDATO.P)


#------------------------------------------------------------------------------#
# Depuración para VOTO.M
#------------------------------------------------------------------------------#

cost_polit$VOTO.M<-with (cost_polit,recode(VOTO.M,"
                                               'si'='Si';
                                                'No sabe'='No responde';
                                           'no sabe'='No sabe';
                                           'no'='No' ;" ) )

cost_polit$VOTO.M<-as.character(cost_polit$VOTO.M)

cost_polit$VOTO.M[cost_polit$VOTO.M==""]<-"No responde"

cost_polit$VOTO.M<-factor(cost_polit$VOTO.M)

#------------------------------------------------------------------------------#
# Depuración para  CANDIDATO.M
#------------------------------------------------------------------------------#

cost_polit$CANDIDATO.M <-with (cost_polit,recode(CANDIDATO.M ,"
                                               'si'='Si';
'en blanco'='En blanco';
'santos'= 'Santos';
'mockus'='Mockus';
'fajardo'='Fajardo';
'uribe'='Uribe'; 
'no sabe'='No sabe';
                                              'n/a'='No aplica';
                                                'no respondo'='No responde' ;" ) )

cost_polit$CANDIDATO.M <-as.character(cost_polit$CANDIDATO.M )


cost_polit$CANDIDATO.M [cost_polit$CANDIDATO.M ==""]<-"No responde"


cost_polit$CANDIDATO.M <-factor(cost_polit$CANDIDATO.M )





# Exportar una base de datos
write.csv2(cost_polit,file="Encuesta_depurada.csv",fileEncoding="latin1")




