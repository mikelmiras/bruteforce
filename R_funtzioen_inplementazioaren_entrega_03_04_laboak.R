
# Ikerketa Operatiboa 
# R funtzioen inplementazioa (3. eta 4. laborategien entrega)
######################################
## Egileak: Mikel Miras eta Urtats Berrocal
## Data: 2023/03/06
##########################################

#######################################################
# Oharrak: 
# Ez aldatu izena entregatzen duzuen fitxategiari, ezta inplementatu beharreko funtzioei. 
# 
# Dokumentu honetan ez jarri probarako deirik, funtzioen kodea bakarrik. 
# Zuzentzean, exekutatuko ditut zuen funtzioak nire deiekin, irteera egokia ematen dutela egiaztatzeko.

# Funtzioaren kodea azaldu, deskribapen sinple baten bidez.

###########################################
# 3. laborategiko funtzioak
##########################################
# 1. ariketa.  basic.solution funtzioa
# Kodearen azalpena:
# Funtzio honek ekuazio-sistemaren soluzioa bueltatzen du. Determinantea 0 bada sistemak soluziorik ez duela esan nahi du eta -1 osatutako bektorea bueltatzen du.
basic.solution <- function(A, b, column.ind.vector){
  
  A1 <- A[,column.ind.vector]
  x.B <- rep(0,ncol(A))
  if (det(A1)!=0){
    x.B[column.ind.vector]<-solve(A1,b)
  }else{
    x.B<- rep(-1,ncol(A))
  }
  return(x.B)
}

##########################################################################
# 2. ariketa.  'all.basic_solutions' funtzioak.
# Inplementazioan erabil aurreko ariketan definitutako `basic.solution` funtzioa, 
# A matrizeko zutabeen konbinazio bakoitzarekin dei eginez.

# **2.1.**  `for` begizten bidezko inplementazioa. 
# Kodearen azalpena:
# Funtzio honek for egitura erabiliz sistemaren soluzio guztiak kalkulatu eta bueltatzen ditu.

all.basic_solutions_for <- function(A, b){
  
  D <- combn(ncol(A),length(b))
  ema <- list()
  for (i in 1:ncol(D)){
    ema[[i]] <- basic.solution(A,b,D[,i])
  }
  return(ema)
}

######
# 2.2. Inplementazioa `apply` funtzioa erabiliz. 
# Kodearen azalpena:
# Funtzio honek `apply` funtzioa erabiliz sistemaren soluzio guztiak kalkulatu eta bueltatzen ditu.

all.basic_solutions_apply <- function(A,b){
  D <- combn(ncol(A), length(b))
  ema <- apply(D, 2, function(x) basic.solution(A, b, x))
  return(ema)
}


########################################################
## 3. ariketa.  `basic.feasible.solutions` funtzioak
# Egokitu 2. ariketako funtzioak, bideragarriak diren oinarriko soluzioak bakarrik itzultzeko.  

## 3.1.  `for` begizten bidezko inplementazioaren egokitzapena. 
# Kodearen azalpena:
# Funtzio honek sistemaren soluzio bideragarriak kalkulatzen ditu for bat bidez.

basic.feasible.solutions_for <- function(A, b){
  lista <- all.basic_solutions_for(A,b)
  emaitza <- list()
  for(i in 1:length(lista)){
    if(all(lista[[i]]>=0)){
      emaitza[[length(emaitza)+1]] <- lista[[i]] 
    }
  }
  return(emaitza)
}

#########################################################
# 3.2. `apply` funtzioaren bidezko inplementazioaren egokitzapena. 
# Kodearen azalpena:
# Funtzio honek sistemaren soluzio bideragarriak kalkulatzen ditu `apply` funtzioa erabiliz kalkulatzen ditu.
basic.feasible.solutions_apply <- function(A,b) {
  konbinazioak = combn(ncol(A), length(b))
  emaitza = apply(konbinazioak, 2, FUN = basic.solution, A=A, b=b)
  zutabe = apply(emaitza, MARGIN=2, FUN = function(x){all(x>=0)})
  return(emaitza[,zutabe])
}

#########################################################
###########################################
# 4. laborategiko funtzioak: `solveProblem` funtzioak
##########################################

## 1. ariketa  `for` begizten bidezko inplementazioa. 
# Funtzioak dei egingo dio 3. laborategi-saioan inplementatutako  
# `basic.feasible.solutions_for` funtzioari. 
# Kodearen azalpena:
# funtzio honek matrize bat eta bektore bat emanda hauek ostatatko sistemaren soluzio maximo posible(ak) bueltatzen ditu
solveProblem_for <- function(A, b, c){ 
  emaitzak = basic.feasible.solutions_for(A, b); # soluzio posibleak
  if (length(emaitzak) == 0) { # soluziorik ez badaude errorea bota 
    message("Problema honek ez du soluziorik")
  }else{  
    z = rep(1, length(emaitzak)) # bektore bat prestatuak datuak gordetzeko
    emaitza_optimoak = c(1, length(emaitzak))
    for (i in 1:length(emaitzak)){ # soluzioak lortu
      z[i] = sum(c * emaitzak[[i]])
    }
    emaitza_optimoa = max(z) # soluzio optimoa(k) aukeratu
    emaitza_optimoa_pos = which(max(z)==z) # soluzioen posizio edo posizioak lortu
    if (length(which(z==emaitza_optimoa)) > 1){ # soluzio bat baino gehiago kasu
      emaitza = matrix(unlist(emaitzak[which(z==emaitza_optimoa)]), nrow=length(which(z==emaitza_optimoa)), byrow=TRUE)
    }else{ # hainbat soluzio kasua
      emaitza = emaitzak[[emaitza_optimoa_pos]]
    }
    return (list(emaitza, emaitza_optimoa)) # emaitzak bueltatu
  }
}

#########################################
## 2. ariketa  `apply` funtzioen bidezko inplementazioa. 
## Funtzioak dei egingo dio 3. laborategi-saioan inplementatutako  
## `basic.feasible.solutions_apply` funtzioari. 
# Kodearen azalpena:
# Funtzioak dei egingo dio 3. laborategi-saioan inplementatutako  
# `basic.feasible.solutions_apply` funtzioari. Soluzioen helburu-funtzioaren 
# $z$ balioa kalkulatzeko, erabil ezazu beste `apply` funtzio bat. 

solveProblem_apply <- function(A, b, c){ 
  emaitzak = basic.feasible.solutions_apply(A, b) # soluzioak lortu
  if (length(emaitzak) == 0) { # soluziorik ez badadu errorea bota
    message("Problema honek ez du soluziorik")
  } else { # soluzioak badaude
    z = apply(emaitzak, MARGIN=2, FUN = function(x,c) sum(c * x), c=c)
    emaitza = list()
    emaitza_optimoa = max(z)
    emaitza_optimoa_pos = which(max(z)==z)
    emaitza_optimoa_posible = length(emaitza_optimoa_pos)
    # if the solution has more than 1 solution  make a matrix
    if(emaitza_optimoa_posible > 1){
      message("Soluzio optimo anizkoitzak daude")
      emaitza[[1]] = emaitzak[,emaitza_optimoa_pos]
      emaitza[[2]] = emaitza_optimoa
      return(emaitza)
    }else { # return a list with the solutions
      message("Soluzio optimo bakarra")
      emaitza[[1]] = emaitzak[,emaitza_optimoa_pos] 
      emaitza[[2]] = emaitza_optimoa    
    }
      return(emaitza)
  }
}


## Entregaren amaiera
##################################################################################


