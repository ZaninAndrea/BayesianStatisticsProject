setwd("C:/Users/panze/Desktop/BAYESIAN STATISTICS/PROGETTO/Dataset")

#Costruisco la Proximity Matrix matrixpos[i,j] dove matrixpos[i,j]=1 se
#- i è una casa di un certo neighborhood e j è un'altra casa di quello stesso neighborhood  
#- i è una casa di un certo neighborhood e j è una qualsiasi casa di un neighborhood confinante con il neighborhood a cui appartiene i  

train <- read.csv("traintransf.csv", header=TRUE, sep=";")
n <- dim(train)[1]

matrixpos <- matrix(rep(0,n*n),nrow=n,ncol=n)


#matrixpos[i,j]=1 se i è una casa di un certo neighborhood e j è una qualsiasi casa di un neighborhood confinante con il neighborhood a cui appartiene i  

#Blmngtn-Gilbert
for(i in 1:24) {
  for(j in 613:737){
    matrixpos[i,j] <- 1
  }
}

#Blueste-Crawfor
for(i in 25:33) {
  for(j in 383:463){
    matrixpos[i,j] <- 1
  }
}

#Blueste-Timber
for(i in 25:33) {
  for(j in 2234:2288){
    matrixpos[i,j] <- 1
  }
}

#BrDale-NAmes
for(i in 34:59) {
  for(j in 940:1293){
    matrixpos[i,j] <- 1
  }
}

#BrDale-NPkVill
for(i in 34:59) {
  for(j in 1354:1372){
    matrixpos[i,j] <- 1
  }
}

#BrDale-NWAmes
for(i in 34:59) {
  for(j in 1506:1608){
    matrixpos[i,j] <- 1
  }
}

#BrkSide-IDOTRR
for(i in 60:143) {
  for(j in 738:814){
    matrixpos[i,j] <- 1
  }
}

#BrkSide-NAmes
for(i in 60:143) {
  for(j in 940:1293){
    matrixpos[i,j] <- 1
  }
}

#BrkSide-OldTown
for(i in 60:143) {
  for(j in 1609:1804){
    matrixpos[i,j] <- 1
  }
}

#ClearCr-CollgCr
for(i in 144:178) {
  for(j in 179:382){
    matrixpos[i,j] <- 1
  }
}

#ClearCr-Edwards
for(i in 144:178) {
  for(j in 464:612){
    matrixpos[i,j] <- 1
  }
}

#ClearCr-Sawyer
for(i in 144:178) {
  for(j in 1805:1924){
    matrixpos[i,j] <- 1
  }
}

#ClearCr-SawyerW
for(i in 144:178) {
  for(j in 1925:2020){
    matrixpos[i,j] <- 1
  }
}

#CollgCr-ClearCr
for(i in 179:382) {
  for(j in 144:178){
    matrixpos[i,j] <- 1
  }
}

#CollgCr-Edwards
for(i in 179:382) {
  for(j in 464:612){
    matrixpos[i,j] <- 1
  }
}

#CollgCr-SawyerW
for(i in 179:382) {
  for(j in 1925:2020){
    matrixpos[i,j] <- 1
  }
}

#Crawfor-Blueste
for(i in 383:463) {
  for(j in 25:33){
    matrixpos[i,j] <- 1
  }
}

#Crawfor-SWISU
for(i in 383:463) {
  for(j in 2197:2233){
    matrixpos[i,j] <- 1
  }
}

#Edwards-ClearCr
for(i in 464:612) {
  for(j in 144:178){
    matrixpos[i,j] <- 1
  }
}

#Edwards-CollgCr
for(i in 464:612) {
  for(j in 179:382){
    matrixpos[i,j] <- 1
  }
}

#Edwards-SWISU
for(i in 464:612) {
  for(j in 2197:2233){
    matrixpos[i,j] <- 1
  }
}

#Gilbert-Blmngtn
for(i in 613:737) {
  for(j in 1:24){
    matrixpos[i,j] <- 1
  }
}

#Gilbert-NWAmes
for(i in 613:737) {
  for(j in 1506:1608){
    matrixpos[i,j] <- 1
  }
}

#Gilbert-Somerst
for(i in 613:737) {
  for(j in 2021:2157){
    matrixpos[i,j] <- 1
  }
}

#Gilbert-StoneBr
for(i in 613:737) {
  for(j in 2158:2196){
    matrixpos[i,j] <- 1
  }
}

#IDOTRR-BrkSide
for(i in 738:814) {
  for(j in 60:143){
    matrixpos[i,j] <- 1
  }
}

#IDOTRR-OldTown
for(i in 738:814) {
  for(j in 1609:1804){
    matrixpos[i,j] <- 1
  }
}

#MeadowV-Mitchel
for(i in 815:844) {
  for(j in 845:939){
    matrixpos[i,j] <- 1
  }
}

#Mitchel-MeadowV
for(i in 845:939) {
  for(j in 815:844){
    matrixpos[i,j] <- 1
  }
}

#NAmes-BrDale
for(i in 940:1293) {
  for(j in 34:59){
    matrixpos[i,j] <- 1
  }
}

#NAmes-BrkSide
for(i in 940:1293) {
  for(j in 60:143){
    matrixpos[i,j] <- 1
  }
}

#NAmes-NPkVill
for(i in 940:1293) {
  for(j in 1354:1372){
    matrixpos[i,j] <- 1
  }
}

#NAmes-NWAmes
for(i in 940:1293) {
  for(j in 1506:1608){
    matrixpos[i,j] <- 1
  }
}

#NAmes-OldTown
for(i in 940:1293) {
  for(j in 1609:1804){
    matrixpos[i,j] <- 1
  }
}

#NoRidge-NridgHt
for(i in 1294:1353) {
  for(j in 1373:1505){
    matrixpos[i,j] <- 1
  }
}

#NoRidge-Somerst
for(i in 1294:1353) {
  for(j in 2021:2157){
    matrixpos[i,j] <- 1
  }
}

#NoRidge-Veenker
for(i in 1294:1353) {
  for(j in 2289:2305){
    matrixpos[i,j] <- 1
  }
}

#NPkVill-BrDale
for(i in 1354:1372) {
  for(j in 34:59){
    matrixpos[i,j] <- 1
  }
}

#NPkVill-NAmes
for(i in 1354:1372) {
  for(j in 940:1293){
    matrixpos[i,j] <- 1
  }
}

#NridgHt-NoRidge
for(i in 1373:1505) {
  for(j in 1294:1353){
    matrixpos[i,j] <- 1
  }
}

#NWAmes-BrDale
for(i in 1506:1608) {
  for(j in 34:59){
    matrixpos[i,j] <- 1
  }
}

#NWAmes-Gilbert
for(i in 1506:1608) {
  for(j in 613:737){
    matrixpos[i,j] <- 1
  }
}

#NWAmes-NAmes
for(i in 1506:1608) {
  for(j in 940:1293){
    matrixpos[i,j] <- 1
  }
}

#NWAmes-Somerst
for(i in 1506:1608) {
  for(j in 2021:2157){
    matrixpos[i,j] <- 1
  }
}

#OldTown-BrkSide
for(i in 1609:1804) {
  for(j in 60:143){
    matrixpos[i,j] <- 1
  }
}

#OldTown-IDOTRR
for(i in 1609:1804) {
  for(j in 738:814){
    matrixpos[i,j] <- 1
  }
}

#OldTown-NAmes
for(i in 1609:1804) {
  for(j in 940:1293){
    matrixpos[i,j] <- 1
  }
}

#Sawyer-ClearCr
for(i in 1805:1924) {
  for(j in 144:178){
    matrixpos[i,j] <- 1
  }
}

#Sawyer-SawyerW
for(i in 1805:1924) {
  for(j in 1925:2020){
    matrixpos[i,j] <- 1
  }
}

#SawyerW-ClearCr
for(i in 1925:2020) {
  for(j in 144:178){
    matrixpos[i,j] <- 1
  }
}

#SawyerW-CollgCr
for(i in 1925:2020) {
  for(j in 179:382){
    matrixpos[i,j] <- 1
  }
}

#SawyerW-Sawyer
for(i in 1925:2020) {
  for(j in 1805:1924){
    matrixpos[i,j] <- 1
  }
}

#Somerst-Gilbert
for(i in 2021:2157) {
  for(j in 613:737){
    matrixpos[i,j] <- 1
  }
}

#Somerst-NoRidge
for(i in 2021:2157) {
  for(j in 1294:1353){
    matrixpos[i,j] <- 1
  }
}

#Somerst-NWAmes
for(i in 2021:2157) {
  for(j in 1506:1608){
    matrixpos[i,j] <- 1
  }
}

#StoneBr-Gilbert
for(i in 2158:2196) {
  for(j in 613:737){
    matrixpos[i,j] <- 1
  }
}

#SWISU-Crawfor
for(i in 2197:2233) {
  for(j in 383:463){
    matrixpos[i,j] <- 1
  }
}

#SWISU-Edwards
for(i in 2197:2233) {
  for(j in 464:612){
    matrixpos[i,j] <- 1
  }
}

#Timber-Blueste
for(i in 2234:2288) {
  for(j in 25:33){
    matrixpos[i,j] <- 1
  }
}

#Veenker-NoRidge
for(i in 2289:2305) {
  for(j in 1294:1353){
    matrixpos[i,j] <- 1
  }
}


#matrixpos[i,j]=1 se i è una casa di un certo neighborhood e j è un'altra casa di quello stesso neighborhood

#Blmngtn-Blmngtn
for(i in 1:24) {
  for(j in 1:24){
    matrixpos[i,j] <- 1
  }
}

#Blueste-Blueste
for(i in 25:33) {
  for(j in 25:33){
    matrixpos[i,j] <- 1
  }
}

#BrDale-BrDale
for(i in 34:59) {
  for(j in 34:59){
    matrixpos[i,j] <- 1
  }
}

#BrkSide-BrkSide
for(i in 60:143) {
  for(j in 60:143){
    matrixpos[i,j] <- 1
  }
}

#ClearCr-ClearCr
for(i in 144:178) {
  for(j in 144:178){
    matrixpos[i,j] <- 1
  }
}

#CollgCr-CollgCr
for(i in 179:382) {
  for(j in 179:382){
    matrixpos[i,j] <- 1
  }
}

#Crawfor-Crawfor
for(i in 383:463) {
  for(j in 383:463){
    matrixpos[i,j] <- 1
  }
}

#Edwards-Edwards
for(i in 464:612) {
  for(j in 464:612){
    matrixpos[i,j] <- 1
  }
}

#Gilbert-Gilbert
for(i in 613:737) {
  for(j in 613:737){
    matrixpos[i,j] <- 1
  }
}

#IDOTRR-IDOTRR
for(i in 738:814) {
  for(j in 738:814){
    matrixpos[i,j] <- 1
  }
}

#MeadowV-MeadowV
for(i in 815:844) {
  for(j in 815:844){
    matrixpos[i,j] <- 1
  }
}

#Mitchel-Mitchel
for(i in 845:939) {
  for(j in 845:939){
    matrixpos[i,j] <- 1
  }
}

#NAmes-NAmes
for(i in 940:1293) {
  for(j in 940:1293){
    matrixpos[i,j] <- 1
  }
}

#NoRidge-NoRidge
for(i in 1294:1353) {
  for(j in 1294:1353){
    matrixpos[i,j] <- 1
  }
}

#NPkVill-NPkVill
for(i in 1354:1372) {
  for(j in 1354:1372){
    matrixpos[i,j] <- 1
  }
}

#NridgHt-NridgHt
for(i in 1373:1505) {
  for(j in 1373:1505){
    matrixpos[i,j] <- 1
  }
}

#NWAmes-NWAmes
for(i in 1506:1608) {
  for(j in 1506:1608){
    matrixpos[i,j] <- 1
  }
}

#OldTown-OldTown
for(i in 1609:1804) {
  for(j in 1609:1804){
    matrixpos[i,j] <- 1
  }
}

#Sawyer-Sawyer
for(i in 1805:1924) {
  for(j in 1805:1924){
    matrixpos[i,j] <- 1
  }
}

#SawyerW-SawyerW
for(i in 1925:2020) {
  for(j in 1925:2020){
    matrixpos[i,j] <- 1
  }
}

#Somerst-Somerst
for(i in 2021:2157) {
  for(j in 2021:2157){
    matrixpos[i,j] <- 1
  }
}

#StoneBr-StoneBr
for(i in 2158:2196) {
  for(j in 2158:2196){
    matrixpos[i,j] <- 1
  }
}

#SWISU-SWISU
for(i in 2197:2233) {
  for(j in 2197:2233){
    matrixpos[i,j] <- 1
  }
}

#Timber-Timber
for(i in 2234:2288) {
  for(j in 2234:2288){
    matrixpos[i,j] <- 1
  }
}

#Veenker-Veenker
for(i in 2289:2305) {
  for(j in 2289:2305){
    matrixpos[i,j] <- 1
  }
}

#Metto a 0 la diagonale principale 
for(i in 1:2305) {
  for(j in 1:2305){
    if (i==j) {
      matrixpos[i,j] <- 0
    }
  }
}


#Verifico simmetria (se p=1 la matrice è simmetrica)
p=1
for (i in 1:2305) {
  for (j in 1:2305) {
    if(matrixpos[i,j]!=matrixpos[j,i]){
      p=0
    }
  }
}
p


write.table(matrixpos, "matrixposout.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ",",             
            na = "",
            quote = TRUE)

