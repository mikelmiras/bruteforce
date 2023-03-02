
# 3. Laborategi-saioa: Oinarriko Soluzio Bideragarrien kalkulua 

######################################
# R-ren funtzio interesgarri batzuk

# det(B)
rm(list=ls())
A <- matrix(c(7, 9, 5, 8, 6, 6, 8, 7, 12, 9), nrow=2, byrow=TRUE)
A
det(A[,c(2,5)])!=0

# solve(B,b)
b <- c(54, 72)
solve(A[,c(2,5)], b)

# combn(x,m)
x <- c(1:5)
x
combn(x, 2)
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    1    1    1    1    2    2    2    3    3     4
# [2,]    2    3    4    5    3    4    5    4    5     5
combn(x, 2)[,7]
A[,combn(x, 2)[,7]]
solve(A[,combn(x, 2)[,7]], b)

######################################

# 1. ariketa.  basic.solution funtzioa
basic.solution <- function(A, b, column.ind.vector){
 if (det(A[,column.ind.vector]) != 0) {
   result = rep(0, ncol(A))
   result[column.ind.vector] = solve(A[,column.ind.vector], b)
   return (result)
 }else{
   return (rep(-1, length(A)))
 }
}

# Probak egiteko eredu bat:
A <- matrix(c(2, 1, 1, 8, 1, 1, 2, 1), nrow=2, byrow=TRUE)
b <- c(6, 4)
c <- c(3, 4, 5, 6)

# Probarako deiak:
basic.solution(A, b, c(1,2))
# [1] 2 2 0 0
basic.solution(A, b, c(1,3))
# [1] 2.6666667 0.0000000 0.6666667 0.0000000
basic.solution(A, b, c(1,4))
# [1]  4.3333333  0.0000000  0.0000000 -0.3333333
basic.solution(A, b, c(2,3))
# [1]  0  8 -2  0
basic.solution(A, b, c(2,4))
# [1] 0.0000000 3.7142857 0.0000000 0.2857143
basic.solution(A, b, c(3,4))
# [1] 0.0000000 0.0000000 1.7333333 0.5333333

#######################################################
# Mendekotasun lineala ondo detektatzen dela egiaztatzeko, honako ereduarekin 
# egin dezakezu proba ("Aljebra lineala" gaiko ariketen orriko 2.3 ariketako 
# bektoreekin osatutako eredu lineal bat da):
A1 <- matrix(c(1,-2,1,1,0,0,-2,4,-2,0,1,0,3,-2,-1,0,0,1), nrow=3, byrow=TRUE)
b1 <- c(10,14,12)
c1 <- c(1,1,1,0,0,0)

basic.solution(A1, b1, c(1,2,3))
# [1] -1 -1 -1 -1 -1 -1     # bektoreak mendekoak dira. Ez da oinarria!
basic.solution(A1, b1, c(1,2,4))
# [1]  9.50  8.25  0.00 17.00  0.00  0.00    ## Oinarriko soluzio bat
basic.solution(A1, b1, c(4,5,6))
# [1]  0  0  0 10 14 12      # oinarria kanonikoa da. Soluzioa berehalakoa: 10, 14, 12


##########################################################################
# 2. ariketa.  'all.basic_solutions' funtzioak.
# Inplementazioan erabil ezazu aurreko ariketan definitutako `basic.solution` 
# funtzioa, A matrizeko zutabeen konbinazio bakoitzarekin dei eginez.

# **2.1.**  `for` begizten bidezko inplementazioa. 
# Emaitza zerrenda batean itzul dezakezu, adibidez. Probak egiteko 
# erabil ezazu 1. ariketako eredu bera.
all.basic_solutions_for <- function(A, b){
  
  D <- combn(ncol(A),length(b))
  ema <- list()
  for (i in 1:ncol(D)){
    ema[[i]] <- basic.solution(A,b,D[,i])
  }
  return(ema)
}
# This function returns a list.
all.basic_solutions_for(A,b)
## [[1]]
## [1] 2 2 0 0
##
## [[2]]
## [1] 2.6666667 0.0000000 0.6666667 0.0000000
##
## [[3]]
## [1] 4.3333333  0.0000000  0.0000000 -0.3333333
##
## [[4]]
## [1] 0 8 -2 0
##
## [[5]]
## [1] 0.0000000 3.7142857 0.0000000 0.2857143
##
## [[6]]
## [1] 0.0000000 0.0000000 1.7333333 0.5333333

###########################################################################
# 2.2. Inplementazioa `apply` funtzioa erabiliz. 
# Emaitza matrize batean itzul dezakezu, adibidez. 
# Probak egiteko erabil ezazu 1. ariketako eredu bera.

all.basic_solutions_apply <- function(A,b){
    D <- combn(ncol(A), length(b))
    ema <- apply(D, 2, function(x) basic.solution(A, b, x))
    return(ema)
  }
# This function returns a matrix. Basic solutions are shown in columns
all.basic_solutions_apply(A,b) 
##    [,1]      [,2]       [,3] [,4]      [,5]      [,6]
##[1,]    2 2.6666667  4.3333333    0 0.0000000 0.0000000
##[2,]    2 0.0000000  0.0000000    8 3.7142857 0.0000000
##[3,]    0 0.6666667  0.0000000   -2 0.0000000 1.7333333
##[4,]    0 0.0000000 -0.3333333    0 0.2857143 0.5333333

########################################################

## 3. ariketa.  `basic.feasible.solutions` funtzioak

# Egoki itzazu 2. ariketako funtzioak, bideragarriak diren oinarriko soluzioak bakarrik itzultzeko.  

## 3.1.  `for` begizten bidezko inplementazioaren egokitzapena. 2. ariketako `for` begizten bidezko 
# inplementazioa kopiatu eta egokitu dezakezu, edo bestela inplementazio horri dei egiten dion 
# beste funtzio bat idatzi. Ez erabili `apply` funtzioen bidezko inplementazioa. Emaitza zerrenda 
# batean itzul dezakezu, adibidez. Probak egiteko erabil ezazu 1. ariketako eredu bera.
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
# This function returns a list.
basic.feasible.solutions_for(A,b)
## [[1]]
## [1] 2 2 0 0
## 
## [[2]]
## [1] 2.6666667 0.0000000 0.6666667 0.0000000
## 
## [[3]]
## [1] 0.0000000 3.7142857 0.0000000 0.2857143
## 
## [[4]]
## [1] 0.0000000 0.0000000 1.7333333 0.5333333
#
# Interpretation: There are four basic feasible solutions. 
# x1=2, x2=2, x3=0, x4=0
# x1=2.6666667, x2=0.0000000, x3=0.6666667, x4=0.0000000
# x1=0.0000000, x2=3.7142857, x3=0.0000000, x4=0.2857143
# x1=0.0000000, x2=0.0000000, x3=1.7333333, x4=0.5333333

#########################################################
# 3.2. `apply` funtzioaren bidezko inplementazioaren egokitzapena. 2. ariketako 
# `apply` funtzioen bidezko inplementazioa kopiatu eta egokitu dezakezu, edo bestela 
# inplementazio horri dei egiten dion beste funtzio bat idatzi. Ez erabili `for` funtzioen 
# bidezko inplementazioa. Emaitza matrize batean itzul dezakezu, adibidez. 
# Probak egiteko erabil ezazu 1. ariketako eredu bera.

basic.feasible.solutions_apply <- function(A, b) {
  lista <- all.basic_solutions_for(A, b)
  emaitza <- lapply(lista, function(x) if (all(x >= 0)) x)
 
}
# This function returns a matrix. Basic solutions are shown in columns
basic.feasible.solutions_apply(A,b)  
##      [,1]      [,2]      [,3]      [,4]
## [1,]    2 2.6666667 0.0000000 0.0000000
## [2,]    2 0.0000000 3.7142857 0.0000000
## [3,]    0 0.6666667 0.0000000 1.7333333
## [4,]    0 0.0000000 0.2857143 0.5333333

# Interpretation: There are four basic feasible solutions. 
# x1=2, x2=2, x3=0, x4=0
# x1=2.6666667, x2=0.0000000, x3=0.6666667, x4=0.0000000
# x1=0.0000000, x2=3.7142857, x3=0.0000000, x4=0.2857143
# x1=0.0000000, x2=0.0000000, x3=1.7333333, x4=0.5333333

##################################################################
# Probetarako eredu linealak

# Definitutako funtzioen zuzentasuna eredu gehiagorekin probatu nahi baduzu, hona hemen batzuk. 
## "Aljebra lineala" gaiko ariketen orriko 6. ariketako ekuazio-sistema.

A <- matrix(c(2, 3, -1, 0, 0, 1, 1, 0, 1, 0, -1, 2, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(1, 3, 5)
# 1. Oinarriko soluzioak banaka kalkulatzea, `basic.solution` funtzioa erabiliz.
basic.solution(A, b, c(1,2,3))
# [1] 0.3333333 2.6666667 7.6666667 0.0000000 0.0000000
basic.solution(A, b, c(1,2,4))
# [1] -1.857143  1.571429  0.000000  3.285714  0.000000
basic.solution(A, b, c(1,2,5))
# [1]  8 -5  0  0 23
basic.solution(A, b, c(1,3,4))
# [1]  -5   0 -11   8   0
basic.solution(A, b, c(1,3,5))
# [1] 3 0 5 0 8
basic.solution(A, b, c(1,4,5))
# [1] 0.5 0.0 0.0 2.5 5.5
basic.solution(A, b, c(2,3,4))
#  0.0 2.5 6.5 0.5 0.0
basic.solution(A, b, c(2,3,5))
# [1]  0  3  8  0 -1
basic.solution(A, b, c(2,4,5))
# [1] 0.0000000 0.3333333 0.0000000 2.6666667 4.3333333
basic.solution(A, b, c(3,4,5))
# [1]  0  0 -1  3  5

## 2. Oinarriko soluzio guztiak kalkulatzea `all.basic_solutions` funtzioak erabiliz. 

## 2.1. `for` begiztekin `all.basic_solutions_for` funtzioa erabiliz:
all.basic_solutions_for(A,b)
# [[1]]
# [1] 0.3333333 2.6666667 7.6666667 0.0000000 0.0000000
# 
# [[2]]
# [1] -1.857143  1.571429  0.000000  3.285714  0.000000
# 
# [[3]]
# [1]  8 -5  0  0 23
# 
# [[4]]
# [1]  -5   0 -11   8   0
# 
# [[5]]
# [1] 3 0 5 0 8
# 
# [[6]]
# [1] 0.5 0.0 0.0 2.5 5.5
# 
# [[7]]
# [1] 0.0 2.5 6.5 0.5 0.0
# 
# [[8]]
# [1]  0  3  8  0 -1
# 
# [[9]]
# [1] 0.0000000 0.3333333 0.0000000 2.6666667 4.3333333
# 
# [[10]]
# [1]  0  0 -1  3  5

## 2.2. `apply` funtzioekin  `all.basic_solutions_apply` funtzioa erabiliz:
all.basic_solutions_apply(A,b) 
#           [,1]      [,2] [,3] [,4] [,5] [,6] [,7] [,8]      [,9] [,10]
# [1,] 0.3333333 -1.857143    8   -5    3  0.5  0.0    0 0.0000000     0
# [2,] 2.6666667  1.571429   -5    0    0  0.0  2.5    3 0.3333333     0
# [3,] 7.6666667  0.000000    0  -11    5  0.0  6.5    8 0.0000000    -1
# [4,] 0.0000000  3.285714    0    8    0  2.5  0.5    0 2.6666667     3
# [5,] 0.0000000  0.000000   23    0    8  5.5  0.0   -1 4.3333333     5

## 3. Bideragarriak diren oinarriko soluzioak bakarrik itzultzea `basic.feasible.solutions` 
## funtzioak erabiliz.  

## 3.1. `for` begiztekin `basic.feasible.solutions_for` funtzioa erabiliz:
basic.feasible.solutions_for(A,b) 
# [[1]]
# [1] 0.3333333 2.6666667 7.6666667 0.0000000 0.0000000
# 
# [[2]]
# [1] 3 0 5 0 8
# 
# [[3]]
# [1] 0.5 0.0 0.0 2.5 5.5
# 
# [[4]]
# [1] 0.0 2.5 6.5 0.5 0.0
# 
# [[5]]
# [1] 0.0000000 0.3333333 0.0000000 2.6666667 4.3333333

## 3.2. `apply` funtzioen bidez `basic.feasible.solutions_apply` funtzioa erabiliz:

basic.feasible.solutions_apply(A,b) 
#           [,1] [,2] [,3] [,4]      [,5]
# [1,] 0.3333333    3  0.5  0.0 0.0000000
# [2,] 2.6666667    0  0.0  2.5 0.3333333
# [3,] 7.6666667    5  0.0  6.5 0.0000000
# [4,] 0.0000000    0  2.5  0.5 2.6666667
# [5,] 0.0000000    8  5.5  0.0 4.3333333

## "Simplex Metodoa" gaiko ariketen orriko 2. ariketako ekuazio-sistema.

A <- matrix(c(-1, 1, 1, 0, 0, 2, 5, 0, 1, 0, 2, -1, 0, 0, 1), nrow=3, byrow=TRUE)
b <- c(4, 20, 2)

## 1. Oinarriko soluzioak banaka kalkulatzea, `basic.solution` funtzioa erabiliz.

basic.solution(A, b, c(1,2,3))
# [1] 2.5 3.0 3.5 0.0 0.0
basic.solution(A, b, c(1,2,4))
# [1] 6  10   0 -42   0
basic.solution(A, b, c(1,2,5))
# [1]  -1.776357e-15  4.000000e+00  0.000000e+00  0.000000e+00  6.000000e+0
# Berez 0 4 0 0 6 0 da...
basic.solution(A, b, c(1,3,4))
# [1]   1  0  5 18  0
basic.solution(A, b, c(1,3,5))
# [1]  10   0  14   0 -18
basic.solution(A, b, c(1,4,5))
# [1] -4  0  0 28 10
basic.solution(A, b, c(2,3,4))
# [1]  0 -2  6 30  0
basic.solution(A, b, c(2,3,5))
# [1] 0 4 0 0 6       --->     # Soluzio endekatua
basic.solution(A, b, c(2,4,5))
# [1]  0 4 0 0 6       --->     # Soluzio endekatua
basic.solution(A, b, c(3,4,5))
# [1]   0  0  4 20  2

## 2. Oinarriko soluzio guztiak kalkulatzea `all.basic_solutions` funtzioak erabiliz. 

## 2.1. `for` begiztekin `all.basic_solutions_for` funtzioa erabiliz:
all.basic_solutions_for(A,b)
# [[1]]
# [1] 2.5 3.0 3.5 0.0 0.0
# 
# [[2]]
# [1]   6  10   0 -42   0
# 
# [[3]]
# [1] -1.776357e-15  4.000000e+00  0.000000e+00  0.000000e+00  6.000000e+00
# 
# [[4]]
# [1]  1  0  5 18  0
# 
# [[5]]
# [1]  10   0  14   0 -18
# 
# [[6]]
# [1] -4  0  0 28 10
# 
# [[7]]
# [1]  0 -2  6 30  0
# 
# [[8]]
# [1] 0 4 0 0 6
# 
# [[9]]
# [1] 0 4 0 0 6
# 
# [[10]]
# [1]  0  0  4 20  2

## 2.2. `apply` funtzioekin  `all.basic_solutions_apply` funtzioa erabiliz:
all.basic_solutions_apply(A,b) 
#      [,1] [,2]          [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]  2.5    6 -1.776357e-15    1   10   -4    0    0    0     0
# [2,]  3.0   10  4.000000e+00    0    0    0   -2    4    4     0
# [3,]  3.5    0  0.000000e+00    5   14    0    6    0    0     4
# [4,]  0.0  -42  0.000000e+00   18    0   28   30    0    0    20
# [5,]  0.0    0  6.000000e+00    0  -18   10    0    6    6     2

## 3. Bideragarriak diren oinarriko soluzioak bakarrik itzultzea `basic.feasible.solutions` 
## funtzioak erabiliz.  

## 3.1. `for` begiztekin `basic.feasible.solutions_for` funtzioa erabiliz:
basic.feasible.solutions_for(A,b) 
# [[1]]
# [1] 2.5 3.0 3.5 0.0 0.0
# 
# [[2]]
# [1]  1  0  5 18  0
# 
# [[3]]
# [1] 0 4 0 0 6
# 
# [[4]]
# [1] 0 4 0 0 6
# 
# [[5]]
# [1]  0  0  4 20  2

## 3.2. `apply` funtzioen bidez `basic.feasible.solutions_apply` funtzioa erabiliz:
basic.feasible.solutions_apply(A,b) 
#      [,1] [,2] [,3] [,4] [,5]
# [1,]  2.5    1    0    0    0
# [2,]  3.0    0    4    4    0
# [3,]  3.5    5    0    0    4
# [4,]  0.0   18    0    0   20
# [5,]  0.0    0    6    6    2




  
  
  
  

  
  



  
  
  
  
  
  


