
library(dplyr)
library(cluster)
library(MVN) 
library(som)
library(kohonen)
library(MASS)
library(ca)
library(stats)
library(RCurl)

######## Q1-1 Hierarchical tree #############
wine <- read.table("wine.txt", header = T) 
n <- length(wine$class.id)
summary(wine)
x <- daisy(wine[,-1], stand = TRUE)
agn <- agnes(x, metric = "euclidean", stand = FALSE, method = "single")
plot(agn, which.plots=2,main="Dendrogram of agen(method=single)")
agn <- agnes(x, metric = "euclidean", stand = FALSE, method = "complete")
plot(agn, which.plots=2,main="Dendrogram of agen(method=complete)")
agn <- agnes(x, metric = "euclidean", stand = FALSE, method = "average")
plot(agn, which.plots=2,main="Dendrogram of agen(method=average)")
agn <- agnes(x, metric = "euclidean", stand = FALSE, method = "ward")
plot(agn, which.plots=2,main="Dendrogram of agen(method=ward)")

agn$ac
agn$order
sum(wine$class.id[agn$order] != wine$class.id)/n

plot(agn, which.plots=1)
wine[,1][agn$order] 

# use 
which.max(agn$height)
agn$order[57]


######## Q1-2 I.	K-medoids #############
medio <- pam(x,3,diss = T)
clusplot(medio)
plot(medio, which.plots = 2)

# 跟pca比較看看
pca.wine<-princomp(scale(wine[,-1],scale=TRUE,center=TRUE),cor=FALSE) 
pcs.wine<-predict(pca.wine)
plot(pcs.wine[,1:2], type="n", main = "k-medios with PCA")
text(pcs.wine,as.character(medio$clustering),col=medio$clustering,cex=1)
# real data in pca
plot(pcs.wine[,1:2],type="n",xlab='1st PC',ylab='2nd PC', main = "real labels with PCA")
text(pcs.wine[,1:2],as.character(wine$class.id),col=as.numeric(wine$class.id),cex=1)

# 
sum(medio$clustering != as.numeric(wine$class.id))/n

######## Q1-3 SOM #############
n.wine<-normalize(wine[,-1])
wine.som<-som(n.wine,grid = somgrid(12,12, "hexagonal"))
plot(wine.som,type="mapping",labels=wine[,1])
#很多格子有好幾個觀測值
plot(wine.som, type="dist.neighbours", main = "SOM neighbour distances")

som.hc <- cutree(hclust(dist(wine.som$codes[[1]])), 3)#用樹的切法
add.cluster.boundaries(wine.som,som.hc)#樹切法的boundery
# 但這樣就不適用som的切法，因為他是想要用看的用顏色去分
cutree(hclust(dist(wine.som$codes[[1]])), 3) 
cutree(hclust(dist(wine.som$codes[[1]])), 3) %>% matrix(ncol=12) %>% View()

sum(wine$class.id[wine.som$unit.classif] != wine$class.id)/n
wine$class.id[c(1:55,123:168,56:122)]

wine.som2<-som(n.wine, grid = somgrid(12, 12, "hexagonal"), rlen=168)
plot(wine.som2, type="dist.neighbours", main = "SOM neighbour distances")
wine$class.id[wine.som2$unit.classif]

######## Q3 MDS #############
## classical MDS
variris <- apply(wine[,-1],2,var)
wine.adjusted <- sweep(wine[,-1],2,sqrt(variris),"/")
wine.scal <- cmdscale(dist(wine.adjusted),k=5,eig=T)
eqscplot(wine.scal$points,type="n", main=" Classical Torgerson-Gower MDS")
text(wine.scal$point,c(rep("A",55),rep("B",67),rep("C",46) ),col=as.numeric(wine$class.id),cex=.8)
# proportion of variance explained by the first 2 dimensions
wine.scal$GOF

## 看變數跟變數之間的相似度 用cor 
variable.scal <- cmdscale(1/cor(wine[,-1]),k=2,eig=T)
eqscplot(variable.scal$points,type="n")
text(variable.scal$point,row.names(cor(wine[,-1])),cex=.8)

## isotonic regression
wine.iso <- isoMDS(dist(wine.adjusted), k=3)# 預設為二維度

eqscplot(wine.iso$points,type="n", main="Nonmetric MDS based on isotonic regression")
text(wine.iso$points,c(rep("A",55),rep("B",67),rep("C",46) ),col=as.numeric(wine$class.id),cex=.8)
100-wine.iso$stress

scree.plot(dist(wine.adjusted), k=8)

# Now we check out the Shepard diagram for a 2D solution:
wine.sh<-Shepard(dist(wine.adjusted), wine.iso$points, p=3)
plot(wine.sh, pch=".")
lines(wine.sh$x, wine.sh$yf, type = "S")
# x軸為投影過後的 y軸為原來的


######## Q4 CA #############
father <- read.table("father_sons.txt", header = T)
father.ca <- ca(father, nd=2)
father.ca
plot(father.ca)

######## Q5 MCA #############
ques <- read.table("question.txt", header = T)
ques.mca <- mjca(ques, nd=2, lambda="Burt") 
ques.mca
plot(ques.mca)
plot(ques.mca, what = c("all", "all"), col=c("blue","red"), main="MCA")

######## Q6 FA #############
sale <- read.table("sales.txt", header = T)
v <- apply(sale,2,var)
sale.scale <- sweep(sale,2,sqrt(v),"/")
sale.cov <- cov(sale.scale)
sale.mle<-factanal(covmat=as.matrix(sale.cov),factors=2,n.obs = 50)
sale.mle
1-sale.mle$uniquenesses
# 用non-orthogonal rotation
sale.pro<-factanal(covmat=as.matrix(sale.cov),factors=2,rotation="promax",n.obs=33)
sale.pro
1-sale.pro$uniquenesses

# where 3 is the maximum number of factors for fitting the model
m=7
k=3
df = m*(m+1)/2-(m*k+m-k*(k-1)/2)
# m is the number of variables and k the number of factors.

# 直接丟data
factor.analysis <- factanal(sale,factors=2,scores="Bartlett")
1-factor.analysis$uniquenesses

plot(factor.analysis$scores[,1],factor.analysis$scores[,2], 
     type = "n", xlab='factor 1',ylab='factor 2', main = "FA")
text(factor.analysis$scores[,1],factor.analysis$scores[,2],row.names(sale), cex = 0.8) 

# compair with PCA
sale.pca<-princomp(scale(sale,scale=TRUE,center=TRUE),cor=TRUE) 
sale.pcs <- predict(sale.pca) 
plot(sale.pcs[,1:2],type="n",xlab='1st PC',ylab='2nd PC', main = "PCA") 
text(sale.pcs[,1:2],row.names(sale), cex = 0.8)

par(mfrow=c(1,2))












#####################
library(kohonen)
data(wines)
set.seed(7)

#create SOM grid
sommap <- som(scale(wines), grid = somgrid(2, 2, "hexagonal"))

## use hierarchical clustering to cluster the codebook vectors
groups<-3
som.hc <- cutree(hclust(dist(sommap$codes)), groups)

#plot
plot(sommap, type="codes", bgcol=rainbow(groups)[som.hc])

#cluster boundaries
add.cluster.boundaries(sommap, som.hc)

