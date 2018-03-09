
##### Factor Analysis #######
sub <- read.table("spearman.txt")
# 只有correlation matrix，因為標準化之後correlation = covariance
sub

#### one factor #####
spearman.mle<-factanal(covmat=as.matrix(sub),factors=1,n.obs = 33)
# n=33 要給他觀測值的數量，才能做likelihood ratio test
spearman.mle
# 說只要一個factor就夠，但通常不會只用一個來解釋
1-spearman.mle$uniq #可被解釋的部分
# 最後兩個解釋得較不好

#### two factor #####
spearman.mle2<-factanal(covmat=as.matrix(sub),factors=2,n.obs=33)
spearman.mle2
1-spearman.mle2$uniq
# 最後一個解釋得較不好，有重複解釋的部分


# where 3 is the maximum number of factors for fitting the model
m=6
k=3
df = m*(m+1)/2-(m*k+m-k*(k-1)/2)
# m is the number of variables and k the number of factors.

##### promax ##### 用non-orthogonal rotation
spearman.mle3<-factanal(covmat=as.matrix(sub),factors=2,rotation="promax",n.obs=33)
spearman.mle3
1-spearman.mle3$uniq

help("factanal")


# In case the input x is a data matrix, you can request factor scores as follows:

x <- read.table("citycrime.txt")
factor.analysis <- factanal(x,factors=2,scores="Bartlett")
plot(factor.analysis$scores[,1],factor.analysis$scores[,2], type = "n")
text(factor.analysis$scores[,1],factor.analysis$scores[,2],row.names(x), cex = 0.5) 
factor.analysis









