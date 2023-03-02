# Ikerketa Operatiboa 
# 4. Laboratorio-saioa: Eredu Linealen Ebazpena

######################################
# R-ren funtzio interesgarri batzuk
# which funtzioa eta max funtzioa (ikus 1. laborategi-saioa)
# mezuak pantailan idazteko message funtzioa erabil dezakezu


## `solveProblem` funtzioak
# Probak egiteko eredua:

A <- matrix(c(2, 1, 1, 8, 1, 1, 2, 1), nrow=2, byrow=TRUE)
b <- c(6, 4)
c <- c(3, 4, 5, 6)

# Soluzio optimoa:  
# x1*=0.0000000, x2*=3.7142857, x3*=0.0000000, x4*=0.2857143
# z*=16.57143

##################################################################
## 1. ariketa  `for` begizten bidezko inplementazioa. 
# Funtzioak dei egingo dio aurreko laboratorio-saioan inplementatutako  `basic.feasible.solutions_for` funtzioari. 
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
all.basic_solutions_for <- function(A, b){
  
  D <- combn(ncol(A),length(b))
  ema <- list()
  for (i in 1:ncol(D)){
    ema[[i]] <- basic.solution(A,b,D[,i])
  }
  return(ema)
}
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






solveProblem_for <- function(A, b, c){ 
  emaitzak <- basic.feasible.solutions_for(A, b);
  if (length(emaitzak) == 0) {
    message("Problema honek ez du soluziorik")
  }else{
    z = rep(1, length(emaitzak))
    emaitza_optimoak = c(1, length(emaitzak))
    for (i in 1:length(emaitzak)){
      z[i] = sum(c * basic.feasible.solutions_for(A, b)[[i]])
    }
    emaitza_optimoa = max(z)
    emaitza_optimoa_pos = which(max(z)==z)
    if (length(which(z==emaitza_optimoa)) > 1){
      emaitza = matrix(c(emaitzak[which(z==emaitza_optimoa)]), nrow=length(which(z==emaitza_optimoa)), byrow=TRUE)
    }else{
      emaitza = emaitzak[[emaitza_optimoa_pos]]
    }
    return (list(emaitza, emaitza_optimoa))
  }
}

solveProblem_for(A,b,c)

# Problemak oinarriko soluzio bideragarri optimo bakarra dauka.
# $soluzioak
# $soluzioak[[1]]
# [1] 0.0000000 3.7142857 0.0000000 0.2857143

# $balio.optimoa
# [1] 16.57143


#######################################################
## 2. ariketa  `apply` funtzioen bidezko inplementazioa. 
#
## Funtzioak dei egingo dio 3. laborategi-saioan inplementatutako  
## `basic.feasible.solutions_apply` funtzioari. Soluzioen helburu-funtzioaren 
## $z$ balioa kalkulatzeko, erabil ezazu beste `apply` funtzio bat. 

solveProblem_apply <- function(A, b, c){ 

  emaitza <- apply(A,)
  
  
  
  }

solveProblem_apply(A,b,c)
# Problemak oinarriko soluzio bideragarri optimo bakarra dauka.
# $soluzioak
# [1] 0.0000000 3.7142857 0.0000000 0.2857143
# 
# $balio.optimoa
# [1] 16.57143

#############################################################################
# Eredu linealak
################################################################################
# Erabil itzazu definitutako funtzioak honako eredu lineal hauek ebazteko, 
# eta egiazta ezazu soluzioa.

# 1. eredua.
A <- matrix(c(2, 1, 2, -1, 0, 0, 4, 2, 2, 0, -1, 0, 6, 1, 4, 0, 0, -1), nrow=3, byrow=TRUE)
b <- c(8, 10, 12)
c <- c(-2, -4, -3, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
# Problemak oinarriko soluzio bideragarri optimo bakarra dauka.
# (x^*_1, x^*_2, x^*_3, x^*_4, x^*_5, x^*_6)=(4, 0, 0, 0, 6, 12) 
# z^*=-8
##################################################3
## 2. eredua. 
A <- matrix(c(2, 2, 2, 2, -1, 0, 0, 4, 4, 1, 4, 0, 1, 0, 2, 8, 2, 1, 0, 0, -1), nrow=3, byrow=TRUE)
b <- c(22, 20, 15)
c <- c(-2, -1, -3, -2, 0, 0, 0)
solveProblem_for(A,b,c)  # z-ren zeinua aldatuta ematen da...
solveProblem_apply(A,b,c) # z-ren zeinua aldatuta ematen da...
# Problemak oinarriko soluzio bideragarri optimo bakarra dauka.
# (x^*_1, x^*_2, x^*_3, x^*_4, x^*_5, x^*_6)=(0, 3, 8, 0, 0, 0, 25)
# z^*=27 
################################################
## 3. eredua. 
A <- matrix(c(1, 2, 1, 0, 0, 1, 1, 0, -1, 0, 1, -1, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(5, 2, 4)
c <- c(1, 2, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
# Problemak 2 oinarriko soluzio bideragarri optimo ditu.
# (x^*_1, x^*_2, x^*_3, x^*_4, x^*_5)=(4.3,\ 0.33,\ 0,\ 2.66,\ 0), z^*= 5
# (x^*_1, x^*_2, x^*_3, x^*_4, x^*_5)=(0.0,\  2.5,\  0.0,\  0.5,\  6.5), z^*= 5
########################################################
## 4. eredua.
A <- matrix(c(1, -2, 1, 0, 0, 1, 1, 0, -1, 0, 2, 3, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(4, 6, 2)
c <- c(3, 4, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
# Problema bideraezina da: ez du soluziorik.

#############################################################################
#                    Simplex ariketen orriko eredu linealak
#########################################################################3
# Ebatz itzazu Simplex ariketen orriko ereduak `solveProblem_for` eta 
# `solveProblem_apply` funtzioak erabiliz eta egiaztatu soluzioak.

# 6.1 Problemak oinarriko soluzio bideragarri optimo bakarra dauka.
# x^*= (3.6 \ 0.8\  0.0\  0.0), z^*= 2.8
A <- matrix(c(1, -2, 1, 0, 4, -3, 0, 1), nrow=2, byrow=TRUE)
b <- c(2, 12)
c <- c(1, -1, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
###################################
##6.2 Problemak 3 oinarriko soluzio bideragarri optimo ditu mutur-puntu berberari dagozkionak 
# ($x^*_1=6, x^*_2=0$).  
# x^*= (6\  0\  0\ 18\  0)
# x^*= (6\  0\  0\ 18\  0)
# x^*= (6\  0\  0\ 18\  0), z^*= 6
A <- matrix(c(1, 6, -1, 0, 0, -2, 3, 0, 1, 0, 1, 2, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(6, 6, 6)
c <- c(1, 1, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
#####################################
## 6.3 Problemak oinarriko soluzio bideragarri optimo bakarra dauka.
# x^*= (3.33\  0.33\ 10.00\  0.00\  0.00), z^*= 12
A <- matrix(c(-2, 2, 1, 0, 0, 2, -2, 0, 1, 0, 1, -4, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(4, 6, 2)
c <- c(4, -4, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c)  
######################################
## 6.4 Problemak 2 oinarriko soluzio bideragarri optimo ditu.
# x^*= (4.3\ 0.33\ 0\ 2.66\ 0), z^*= 5
# x^*= (0.0\  2.5\  0.0\  0.5\  6.5), z^*= 5
A <- matrix(c(1, 2, 1, 0, 0, 1, 1, 0, -1, 0, 1, -1, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(5, 2, 4)
c <- c(1, 2, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
########################################
## 6.5 Problemak 2 oinarriko soluzio bideragarri optimo ditu.
# x^*= (1\ 2\ 3\ 0\ 0), z^*= 6
# x^*= (2.5\ 0.5\ 0.0\ 0.0\ 1.5), z^*= 6
A <- matrix(c(1, -1, 1, 0, 0, 2, 2, 0, 1, 0, 1, 2, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(2, 6, 5)
c <- c(2, 2, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
#########################################################
## 6.6 Problema bornegabea da. Ez egin. Mutur-puntuen kalkuluarekin ezin da detektatu...
##############################################
## 6.7 Problema bornegabea da. Ez egin. Mutur-puntuen kalkuluarekin ezin da detektatu...
###############################################
## 6.8 Problema bideraezina da; ez du soluziorik.
A <- matrix(c(1, -2, 1, 0, 0, 1, 1, 0, -1, 0, 2, 3, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(4, 6, 2)
c <- c(3, 4, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
#################################################
## 7.1 Problemak oinarriko soluzio bideragarri optimo bakarra dauka.
# x^*= (0\  8\  0\ 12\  0), z^*= 16
A <- matrix(c(1, -1, 1, 1, 0, 2, 1, 4, 0, 1), nrow=2, byrow=TRUE)
b <- c(4, 8)
c <- c(3, 2, 1, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
##################################################
## 7.2 Problemak oinarriko soluzio bideragarri optimo bakarra dauka.
# x^*= (1\ 7\ 0\ 9\ 0), z^*= 12. Zeinu aldaketak daude.
# x^*= (-1\ 7\ 0\ 9\ 0), z^*= -12. Hau da jatorrizko problemarako soluzio optimoa.
A <- matrix(c(-1, 1, -1, 0, 0, -1, 2, -1, -1, 0, 1, 1, 2, 0, 1), nrow=3, byrow=TRUE)
b <- c(6, 4, 8)
c <- c(5, 1, 2, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
#####################################################
## 7.3 Problemak oinarriko soluzio bideragarri optimo bakarra dauka.
# x^*= (5\ 6\ 0\ 0\ 0\ 6), z^*= 13. Balio optimoaren zeinua aldatu, z^*= -13.
A <- matrix(c(3, -1, 2, 1, 0, 0, -2, 4, 1, 0, 1, 0, -4, 4, 8, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(9, 14, 10)
c <- c(-1, 3, -2, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
####################################################
## 7.4 Problemak 2 oinarriko soluzio bideragarri optimo ditu.
# x^*= (0\ 2\ 0\ 2\ 0\ 0), x^*= (0\ 3\ 0\ 0\ 2\ 0) 
# z^*= -24. Balio optimoaren zeinua aldatu, z^*= 24
A <- matrix(c(2, 4, 2, 1, -1, 0, -4, 4, -1, 2, 0, -1), nrow=2, byrow=TRUE)
b <- c(10, 12)
c <- c(-10, -8, -6, -4, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
##############################################################
## 7.5 Problemak 2 oinarriko soluzio bideragarri optimo ditu.
# x^*= (4.5\ 0.0\ 0.0\ 0.5\ 0.0\ 0.0\ 0.0)
# x^*= (0\  0\  0\ 23\  9\  0\  0)
# z^*= 73$. Balio optimoaren zeinua aldatu, z^*= -73
A <- matrix(c(3, 1, 3, -3, 9, 1, 0, 2, 8, 4, 2, -4, 0, 1), nrow=2, byrow=TRUE)
b <- c(12, 10)
c <- c(16, -2, -1, 2, 3, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
##########################################################
## 7.6 Problema bideraezina da; ez du soluziorik.
A <- matrix(c(2, 1, 1, 2, 1, 0, 0, 8, 4, -2, -1, 0, -1, 0, 4, 7, 2, 1, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(2, 10, 4)
c <- c(9, 5, 4, 1, 0, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
#################################################################
## 7.7 Problema bornegabea da. Ez egin. Ezin da detektatu...
####################################################################
## 7.8 Problema bideraezina da; ez du soluziorik.
A <- matrix(c(1, 2, 2, 1, 1, 1, 0, 2, 1, 3, 2, 2, 0, -1), nrow=2, byrow=TRUE)
b <- c(2, 12)
c <- c(-3, -1, 2, 2, -1, 0, 0)
solveProblem_for(A,b,c) 
solveProblem_apply(A,b,c) 
##################################################################
##    Laborategi-saioaren amaiera
######################################################################