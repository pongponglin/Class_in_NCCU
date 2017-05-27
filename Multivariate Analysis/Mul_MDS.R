
data(iris)
variris <- apply(iris[,-5],2,var)
iris.adjusted <- sweep(iris[,-5],2,sqrt(variris),"/")
#### obj. obj.之間 ######
iris.scal <- cmdscale(dist(iris.adjusted),k=2,eig=T)

# Let's plot the results now:
library(MASS)
eqscplot(iris.scal$points,type="n")
text(iris.scal$point,row.names(iris),cex=.8)

iris.scal$GOF
# [1] 0.9581321 0.9581321
# 兩個方法算出來的eigenvalue

#### 看變數跟變數之間 用cor ####
variable.scal <- cmdscale(1/cor(iris[,-5]),k=2,eig=T)
eqscplot(variable.scal$points,type="n")
text(variable.scal$point,row.names(cor(iris[,-5])),cex=.8)
# 看變數之間的相似度

## isotonic regression
# 要拿掉102筆 因為跟另外143筆相似度太高
iris.iso <- isoMDS(dist(iris.adjusted[-102,]))# 預設為二維度

eqscplot(iris.iso$points,type="n")
text(iris.iso$points,label=row.names(iris[-102,]),cex=.8)

iris.iso$stress
# [1] 4.182693  (in percent)
# 低於5% 算good

scree.plot = function(d, k) {
  stresses=isoMDS(d, k=k)$stress
  for(i in rev(seq(k-1)))  
    stresses=append(stresses,isoMDS(d, k=i)$stress)
  plot(seq(k),rev(stresses), type="b", xaxp=c(1,k, k-1), ylab="Stress", xlab="Number of dimensions")
}

# Check out the scree plot up to 6 dimensions:
scree.plot(dist(iris.adjusted[-102,]), k=6)
# 三維度做得比較好

## 看投影前投影後
# Now we check out the Shepard diagram for a 2D solution:
iris.sh<-Shepard(dist(iris.adjusted[-102,]), iris.iso$points, p=2)
plot(iris.sh, pch=".")
lines(iris.sh$x, iris.sh$yf, type = "S")
# x軸為投影過後的 y軸為原來的
# 二維度的就還不錯

#### nonlinear mapping (sammon) ######
iris.sammon <- sammon(dist(iris.adjusted[-102,]),k=2)
eqscplot(iris.sammon$points,type="n")
text(iris.sammon$points,label=row.names(iris[-102,]),cex=.8)

iris.sammon$stress
# [1] 0.006206077 更準

scree.plot.s = function(d, k) {
  stresses=sammon(d, k=k)$stress
  for(i in rev(seq(k-1)))  
    stresses=append(stresses,sammon(d, k=i)$stress)
  plot(seq(k),rev(stresses), type="b", xaxp=c(1,k, k-1), ylab="Stress", xlab="Number of dimensions")
}

# Check out the scree plot up to 6 dimensions:
scree.plot.s(dist(iris.adjusted[-102,]), k=6)
# The scree plot shows a clear elbow at dimension = 2, 
# which suggests that a 2D solution should be adequate.


iris.sh<-Shepard(dist(iris.adjusted[-102,]), iris.sammon$points, p=2)
plot(iris.sh, pch=".")
lines(iris.sh$x, iris.sh$yf, type = "S")
  



