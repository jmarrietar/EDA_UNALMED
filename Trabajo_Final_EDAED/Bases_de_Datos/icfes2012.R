set = read.table('icfes2012.csv', header=	TRUE, sep='',	dec=',')

set2 = matrix(nrow=nrow(set), ncol=3)
for(i in 1:nrow(set)){
  set2[i,1] = set$CODIGOMUNICIPIO[i]
  set2[i,2] = (3*set$PROMEDIOMATEMATICA[i]+3*set$PROMEDIOLENGUAJE[i]+2*set$PROMEDIOSOCIALES[i]+set$PROMEDIOBIOLOGIA[i]+set$PROMEDIOFISICA[i]+set$PROMEDIOQUIMICA[i]+set$PROMEDIOINGLES[i]+set$PROMEDIOFILOSOFIA[i])/13
  set2[i,3] = set$EVALUADOS[i]
}

set3 = matrix(nrow=1104, ncol=2)
for(i in 1:nrow(set3)){
  dep = set2[1,1]
  set3[i,1] = dep
  count = 0
  acum = 0
  j=1
  done = FALSE
  while(done == FALSE && nrow(set2)>0 && j<=nrow(set2)){
    if(set2[j,1] == dep){
      count = count+set2[j,3]
      acum = acum+set2[j,2]*set2[j,3]
      if(nrow(set2)==2) done = TRUE
      set2 = set2[-j,]
    }
    else
      j=j+1
  }
  if(done){      
    if(set2[1] == dep){
      count = count+set2[3]
      acum = acum+set2[2]*set2[3]
      set3[i,2] = acum/count
    }
    else{
      set3[i,2] = acum/count
      dep = set2[1]
      set3[i+1,1] = dep
      set3[i+1,2] = set2[2]
    }
    break
  }
  set3[i,2] = acum/count
}

write.table(set3, file='icfes2012 POR DEPTO.csv', append = FALSE, row.names = FALSE, sep='\t',	dec=',')