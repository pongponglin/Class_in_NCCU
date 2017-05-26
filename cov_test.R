
library(dplyr)

##### data ######
data("iris")
table(iris$Species)

filter(iris, Species == "setosa" )[,-5] %>% cov
filter(iris, Species == "versicolor"  )[,-5] %>% cov
filter(iris, Species == "virginica" )[,-5] %>% cov

## seed data
seed <- read.table("Seeds.txt",h=T)
seed[,8] <- as.factor(seed[,8])
table(seed$Y)
uniNorm(seed[,-8], type="Lillie", desc=T)$`Lilliefors (Kolmogorov-Smirnov)'s Normality Test`

## 自製相關係數為0.2 的二元常態分配
set.seed(100)
x=matrix(rnorm(200), ncol = 100)
A <- matrix(c(1,0.5,0.5,1), ncol = 2)
a=t(chol(A))
x1 <- a %*% x %>% t() %>% as.data.frame()
x1$y <- sample(2,100, replace = T) %>% as.factor()
table(x1$y)

filter(x1, y==1)[-3] %>% cov()
filter(x1, y==2)[-3] %>% cov()

## 不同var
n1 <- sample(c(30:50), 1)
x=matrix(rnorm(n1*2), ncol = n1)
A <- matrix(c(1,0.7,0.7,2), ncol = 2)
a=t(chol(A))
x3 <- a %*% x %>% t() %>% as.data.frame()
x3$y <- as.factor(1)
n2 <- sample(c(30:50), 1)
x=matrix(rnorm(n2*2), ncol = n2)
A <- matrix(c(1,0.2,0.2,1), ncol = 2)
a=t(chol(A))
x4 <- a %*% x %>% t() %>% as.data.frame()
x4$y <- as.factor(2)
dx <- rbind(x3,x4)
table(dx$y)
cov(x3[,-3])
cov(x4[,-3])


#### 自製三維多元常態 ######

x=matrix(rnorm(300), ncol = 100)
A <- matrix(c(1,0.5,0.2,0.5,1,0.8,0.2, 0.8,1), ncol = 3)
a=t(chol(A))
x2 <- a %*% x  %>% t() %>% as.data.frame()
x2$y <- sample(3,100, replace = T) %>% as.factor()
table(x2$y)

filter(x2, y==1)[-4] %>% cov()
filter(x2, y==2)[-4] %>% cov()
filter(x2, y==3)[-4] %>% cov()

## 不同var
n1 <- sample(c(20:50), 1)
x=matrix(rnorm(3* n1), ncol = n1)
A <- matrix(c(2,0.5,0.2,0.5,1,0.8,0.2, 0.8,1), ncol = 3)
a=t(chol(A))
x1 <- a %*% x  %>% t() %>% as.data.frame()
x1$y <- as.factor(1)

n2 <- sample(c(30:50), 1)
x=matrix(rnorm(3 * n2), ncol = n2)
A <- matrix(c(1,0.1,0.2,0.1,1,0.3,0.2, 0.3,1), ncol = 3)
a=t(chol(A))
x2 <- a %*% x  %>% t() %>% as.data.frame()
x2$y <- as.factor(2)

n3 <- sample(c(20:50), 1)
A <- matrix(c(2,0.4,0.2,0.4,1,0.8,0.2, 0.8,2), ncol = 3)
x=matrix(rnorm(3 * n3), ncol = n3)
a=t(chol(A))
x3 <- a %*% x  %>% t() %>% as.data.frame()
x3$y <- as.factor(3)

d3 <- rbind(x1,x2) %>% rbind(x3)
table(d3$y)


##### Testing method ######
library(biotools)
boxM(iris[,-5],iris$Species)
# package ‘tcltk’ could not be loaded -> tcltk2


#### Box's M-test  ########
# 建議每個種類下的個數要大於20較好
# Box's M-test for testing homogeneity of covariance matrices
#
# Written by Andy Liaw (2004) converted from Matlab
# Andy's note indicates that he has left the original Matlab comments intact
#
# Slight clean-up and fix with corrected documentation provided by Ranjan Maitra (2012)
#

BoxMTest <- function(X, cl, alpha=0.05) { 
  ## Multivariate Statistical Testing for the Homogeneity of Covariance 
  ## Matrices by the Box's M. 
  ## 
  ## Syntax: function [MBox] = BoxMTest(X,alpha) 
  ## 
  ## Inputs: 
  ## X - data matrix (Size of matrix must be n-by-p;  # RM changed
  ## variables=column 1:p). 
  ## alpha - significance level (default = 0.05). 
  ## Output: 
  ## MBox - the Box's M statistic. 
  ## Chi-sqr. or F - the approximation statistic test. 
  ## df's - degrees' of freedom of the approximation statistic test. 
  ## P - observed significance level. 
  ## 
  ## If the groups sample-size is at least 20 (sufficiently large), 
  ## Box's M test takes a Chi-square approximation; otherwise it takes 
  ## an F approximation. 
  ## 
  ## Example: For a two groups (g = 2) with three independent variables 
  ## (p = 3), we are interested in testing the homogeneity of covariances 
  ## matrices with a significance level = 0.05. The two groups have the 
  ## same sample-size n1 = n2 = 5. 
  ## Group 
  ## --------------------------------------- 
  ## 1 2 
  ## --------------------------------------- 
  ## x1 x2 x3 x1 x2 x3 
  ## --------------------------------------- 
  ## 23 45 15 277 230 63 
  ## 40 85 18 153 80 29 
  ## 215 307 60 306 440 105 
  ## 110 110 50 252 350 175 
  ## 65 105 24 143 205 42 
  ## --------------------------------------- 
  ##
  ##
  ## Not true for R
  ##
  ##  
  ## Total data matrix must be: 
  ## X=[1 23 45 15;1 40 85 18;1 215 307 60;1 110 110 50;1 65 105 24; 
  ## 2 277 230 63;2 153 80 29;2 306 440 105;2 252 350 175;2 143 205 42]; 
  ##
  ##  
  ## Calling on Matlab the function: 
  ## MBoxtest(X,0.05) 
  ## 
  ## Answer is: 
  ## 
  ## ------------------------------------------------------------ 
  ## MBox F df1 df2 P 
  ## ------------------------------------------------------------ 
  ## 27.1622 2.6293 6 463 0.0162 
  ## ------------------------------------------------------------ 
  ## Covariance matrices are significantly different. 
  ## 
  
  ## Created by A. Trujillo-Ortiz and R. Hernandez-Walls 
  ## Facultad de Ciencias Marinas 
  ## Universidad Autonoma de Baja California 
  ## Apdo. Postal 453 
  ## Ensenada, Baja California 
  ## Mexico. 
  ## atrujo_at_uabc.mx 
  ## And the special collaboration of the post-graduate students of the 2002:2 
  ## Multivariate Statistics Course: Karel Castro-Morales, 
  ## Alejandro Espinoza-Tenorio, Andrea Guia-Ramirez, Raquel Muniz-Salazar, 
  ## Jose Luis Sanchez-Osorio and Roberto Carmona-Pina. 
  ## November 2002. 
  ## 
  ## To cite this file, this would be an appropriate format: 
  ## Trujillo-Ortiz, A., R. Hernandez-Walls, K. Castro-Morales, 
  ## A. Espinoza-Tenorio, A. Guia-Ramirez and R. Carmona-Pina. (2002). 
  ## MBoxtest: Multivariate Statistical Testing for the Homogeneity of 
  ## Covariance Matrices by the Box's M. A MATLAB file. [WWW document]. 
  ## URL http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=2733&objectType=FILE 
  ## 
  ## References: 
  ## 
  ## Stevens, J. (1992), Applied Multivariate Statistics for Social Sciences. 
  ## 2nd. ed., New-Jersey:Lawrance Erlbaum Associates Publishers. pp. 260-269. 
  
  if (alpha <= 0 || alpha >= 1) 
    stop('significance level must be between 0 and 1') 
  g = nlevels(cl) ## Number of groups. 
  n = table(cl) ## Vector of groups-size. 
  N = nrow(X) 
  p = ncol(X) 
  bandera = 2 
  if (any(n >= 20))
    bandera = 1 
  ## Partition of the group covariance matrices. 
  
  covList <- tapply(as.matrix(X), rep(cl, ncol(X)), function(x, nc) cov(matrix(x, nc = nc)),
                    ncol(X))
  deno = sum(n) - g 
  suma = array(0, dim=dim(covList[[1]])) 
  for (k in 1:g) 
    suma = suma + (n[k] - 1) * covList[[k]] 
  Sp = suma / deno ## Pooled covariance matrix. 
  Falta=0 
  for (k in 1:g) 
    Falta = Falta + ((n[k] - 1) * log(det(covList[[k]]))) 
  
  MB = (sum(n) - g) * log(det(Sp)) - Falta ## Box's M statistic. 
  suma1 = sum(1 / (n[1:g] - 1)) 
  suma2 = sum(1 / ((n[1:g] - 1)^2)) 
  C = (((2 * p^2) + (3 * p) - 1) / (6 * (p + 1) * (g - 1))) * 
    (suma1 - (1 / deno)) ## Computing of correction factor. 
  if (bandera == 1)
  { 
    X2 = MB * (1 - C) ## Chi-square approximation. 
    v = as.integer((p * (p + 1) * (g - 1)) / 2) ## Degrees of freedom. 
    ## Significance value associated to the observed Chi-square statistic. 
    P = pchisq(X2, v, lower=FALSE)  #RM: corrected to be the upper tail 
    cat('------------------------------------------------\n'); 
    cat(' MBox Chi-sqr. df P\n') 
    cat('------------------------------------------------\n') 
    cat(sprintf("%10.4f%11.4f%12.i%13.4f\n", MB, X2, v, P)) 
    cat('------------------------------------------------\n') 
    if (P >= alpha) { 
      cat('Covariance matrices are not significantly different.\n') 
    } else { 
      cat('Covariance matrices are significantly different.\n') 
    } 
    return(list(MBox=MB, ChiSq=X2, df=v, pValue=P)) 
  }
  else
  { 
    ## To obtain the F approximation we first define Co, which combined to 
    ## the before C value are used to estimate the denominator degrees of 
    ## freedom (v2); resulting two possible cases. 
    Co = (((p-1) * (p+2)) / (6 * (g-1))) * (suma2 - (1 / (deno^2))) 
    if (Co - (C^2) >= 0) { 
      v1 = as.integer((p * (p + 1) * (g - 1)) / 2) ## Numerator DF. 
      v21 = as.integer(trunc((v1 + 2) / (Co - (C^2)))) ## Denominator DF. 
      F1 = MB * ((1 - C - (v1 / v21)) / v1) ## F approximation. 
      ## Significance value associated to the observed F statistic. 
      P1 = pf(F1, v1, v21, lower=FALSE) 
      cat('\n------------------------------------------------------------\n') 
      cat(' MBox F df1 df2 P\n') 
      cat('------------------------------------------------------------\n') 
      cat(sprintf("%10.4f%11.4f%11.i%14.i%13.4f\n", MB, F1, v1, v21, P1)) 
      cat('------------------------------------------------------------\n') 
      if (P1 >= alpha) { 
        cat('Covariance matrices are not significantly different.\n') 
      } else { 
        cat('Covariance matrices are significantly different.\n') 
      } 
      return(list(MBox=MB, F=F1, df1=v1, df2=v21, pValue=P1)) 
    } else { 
      v1 = as.integer((p * (p + 1) * (g - 1)) / 2) ## Numerator df. 
      v22 = as.integer(trunc((v1 + 2) / ((C^2) - Co))) ## Denominator df. 
      b = v22 / (1 - C - (2 / v22)) 
      F2 = (v22 * MB) / (v1 * (b - MB)) ## F approximation. 
      ## Significance value associated to the observed F statistic. 
      P2 = pf(F2, v1, v22, lower=FALSE) 
      
      cat('\n------------------------------------------------------------\n') 
      cat(' MBox F df1 df2 P\n') 
      cat('------------------------------------------------------------\n') 
      cat(sprintf('%10.4f%11.4f%11.i%14.i%13.4f\n', MB, F2, v1, v22, P2)) 
      cat('------------------------------------------------------------\n') 
      
      if (P2 >= alpha) { 
        cat('Covariance matrices are not significantly different.\n') 
      } else { 
        cat('Covariance matrices are significantly different.\n') 
      } 
      return(list(MBox=MB, F=F2, df1=v1, df2=v22, pValue=P2)) 
    } 
  }
}


#### Test ########

## Multivariate Test
mardiaTest(d3[,-4],qqplot=F) 
hzTest(d3[,-4],qqplot=F) 
roystonTest(d3[,-4],qqplot=F) 

mardiaTest(seed[,c(3,6)],qqplot=F) 
hzTest(seed[,c(3,6)],qqplot=F)
roystonTest(seed[,c(3,6)],qqplot=F) 


BoxMTest(iris[,-5], iris$Species)
BoxMTest(x1[,-3], x1$y)
BoxMTest(x2[,-4], x2$y)
BoxMTest(dx[,-3], dx$y)
BoxMTest(d3[,-4], d3$y)
BoxMTest(seed[,c(3,6)], seed$Y)





