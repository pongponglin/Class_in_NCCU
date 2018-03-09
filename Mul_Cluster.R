### Cluster
library(dplyr)

#### * Heirarchical cluster ###########################################

olive <- read.table("olive.txt", header = T) 
newolive <- olive[,3:10] 
# 把region ,area兩個變數拿掉，因為是類別型變數，算距離無意義
dim(newolive)#

### agnes ###################################################
library(cluster)
x <- daisy(newolive) #算不相似度
agn <- agnes(x, metric = "euclidean", stand = FALSE, method = "single")
# stand=F 不標準化，保留原來data的scale

# 但是傾向標準化，不確定哪個變數重要的時候盡量先使用標準化
x <- daisy(newolive, stand = TRUE) # 標準化後
agn <- agnes(x, metric = "euclidean", method = "single")

plot(agn, ask=T)
plot(agn, which.plots=2)
plot(agn, which.plots=1)
# 圖可以看出有outlier的情形

agn$ac
# 0.734639
# 拿掉離群值會讓ac變小

olive[,1][agn$order]#I would say “yes”, except for 3 region “1” in the last line.
olive[,2][agn$order]#I would say “no” here.

## 拿掉outlier看看
x <- daisy(newolive[-522,], stand = TRUE)
agn2 <- agnes(x, metric = "euclidean", method = "single")
plot(agn2, which.plots=2)

## complete
cagn <- agnes(x, metric = "euclidean", stand = FALSE, method = "complete")
plot(cagn, which.plots=2)
cagn$ac

## average
aagn <- agnes(x, metric = "euclidean", stand = FALSE, method = "average")
plot(aagn, which.plots=2)
aagn$ac

## ward 做得最好～
wagn <- agnes(x, metric = "euclidean", stand = FALSE, method = "ward")
plot(wagn, ask = T)
wagn$ac

olive[,1][wagn$order] #前面123都混雜一起
olive[,2][wagn$order] #反應的部分的


### diana ##########################################
x <- daisy(newolive, stand = TRUE) 
di <- diana(x, metric = "enclidean")
print(di)#這是啥？
plot(di, which.plots=2)#tree
plot(di, which.plots=1)
di$dc
olive[,1][di$order] 
olive[,2][di$order] 

#### mona ############################# try self

#### * Partitioning cluster ##################################

#### k-means ####################################
km<-kmeans(newolive,3,20)
pca.newolive<-princomp(scale(newolive,scale=TRUE,center=TRUE),cor=FALSE) 
pcs.newolive<-predict(pca.newolive)
plot(pcs.newolive[,1:2], type="n")
text(pcs.newolive,as.character(km$cluster),col=km$cluster,cex=0.6)
#放在pca的維度上看看pca是不是可以分得好

#真正的region
plot(pcs.newolive[,1:2],type="n",xlab='1st PC',ylab='2nd PC')
text(pcs.newolive[,1:2],as.character(olive$Region),col=olive$Region,cex=0.6)

#### k-medios (pam) ########################################

pa<-pam(daisy(newolive,stand=T),3,diss=T) 
plot(pa,ask=T)
pa$clustering
plot(pcs.newolive[,1:2], type="n")
text(pcs.newolive,as.character(pa$clustering),col=pa$clustering,cex=0.6)

# Q: Change the number of clusters, what is the number that achieves the highest SC?
pa2<-pam(daisy(newolive,stand=T),4,diss=T) 
plot(pa2,ask=T)

# pam()的加速版：clara()

#### SOM #################
library(som)
library(kohonen)
n.newolive<-normalize(newolive)#標準化

olive.som<-som(n.newolive,grid = somgrid(20, 20, "hexagonal"))#放400格
plot(olive.som,type="mapping",labels=olive[,1])
#很多格子有好幾個觀測值
plot(olive.som, type="dist.neighbours", main = "SOM neighbour distances")
som.hc <- cutree(hclust(dist(olive.som$codes[[1]])), 5)#用樹的切法
add.cluster.boundaries(olive.som,som.hc)#樹切法的boundery
# 但這樣就不適用som的切法，因為他是想要用看的用顏色去分

cutree(hclust(dist(olive.som$codes[[1]])), 5) %>% matrix(ncol=20) %>% View()






