
##### classification tree #######

fish <- read.table("fish.txt", h=T)
summary(fish)
cor(fish[,2:6])
table(fish$Species)
L21=L2-L1
L32=L3-L2
L31=L3-L1

# Classification tree
library(rpart)
# 要先設minisplit:至少有幾個觀測值才能繼續切
# minibucket : note下最少要有幾個 > 要低於最小class的個數
# xval:cross validation 先設為0(就是不做)
fish.control <- rpart.control(minisplit=10, minbucket=3, xval = 0)
fish.treeorig <- rpart(Species~Weight+L1+L2+L3+Height+Width,data=fish,method="class",control=fish.control)
plot(fish.treeorig)
text(fish.treeorig)
# Also check out the complexity parameter (CP)
printcp(fish.treeorig)
summary(fish.treeorig)

# cost-complexitymeasure
fish.prunetree <- prune.rpart(fish.treeorig, cp=0.02)
plot(fish.prunetree)
text(fish.prunetree)

L21 = fish$L2-fish$L1
L32 = fish$L3-fish$L2
L31 = fish$L3-fish$L1
newfish <- cbind(fish, L21, L32, L31)
newfish.treenew <- rpart(Species~.,
                         data=newfish,method="class",
                         parms=list(split="information"),control=fish.control)
printcp(newfish.treenew)
plot(newfish.treenew)
text(newfish.treenew)

# xval:cross validation
fish.control1 <- rpart.control(minbucket=3,minsplit=10,xval=148)
newfish.treenewcv <-rpart(Species~.,data=newfish,method="class",
                          parms=list(split="information"),control=fish.control1)
printcp(newfish.treenewcv) #找到認為最好的樹

# 預測分類
newfish.test <- read.table("fish_test.txt", h=T)
newfish.test$L21 = newfish.test$L2-newfish.test$L1
newfish.test$L32 = newfish.test$L3-newfish.test$L2
newfish.test$L31 = newfish.test$L3-newfish.test$L1
newfish.tpred <- predict(newfish.treenewcv, newfish.test)
newfish.tpred

############## Linear Discriminant Analysis(LDA) #####################
library(MASS)
newfish.lda <- lda(Species~., data=newfish)
# warning L2 L3高度共線
cor(newfish[,2:10])
newfish.lda <- lda(Species~Weight+L1+Height+Width+L21+L32, data = newfish)
newfish.lda
# 結果為六個維度 後面兩個很小可能也可以不用擺進去 結果差不多
# Weight 的係數非常小

newfish.ldapred <- predict(newfish.lda, newfish[,-1])
table(newfish$Species, newfish.ldapred$class) #confusion matrix

# cross validation = TRUE
newfish.ldacv <- lda(Species~Weight+L1+Height+Width+L21+L32, data = newfish, CV=T)
table(newfish$Species, newfish.ldacv$class) #confusion matrix
eqscplot(newfish.ldapred$x, type="n",xlab="1st LD",ylab="2nd LD")
fish.species<-c(rep("B",33),rep("W",5),rep("R",18),rep("Pa",10),rep("S",12),
                rep("Pi",16),rep("Pe",54))
fish.colors<-c(rep(1,33),rep(2,5),rep(3,18),rep(4,10),rep(5,12),rep(6,16),rep(7,54))
text(newfish.ldapred$x[,1:2],fish.species,col=fish.colors)

# 用結果丟test進去預測
newfish.ldatest<-predict(newfish.lda,newfish.test)
newfish.ldatest$class
# the results agree with those obtained from the classification tree.


########## Quadratic Discriminant Analysis (QDA) #####################
# check normal
newfish.qda <-qda(Species~.,data=newfish)# 有些group 太小
# 通常是把小群拿掉 或是跟別人合併 但是這邊不知道要跟誰併
newfish.q <-read.table("newfish.txt",h=T)
newfish.qda <-qda(Species~.,data=newfish.q) # 有共線性 inverse不存在
newfish.qda<-qda(Species~Weight+L1+Height+Width+L21+L32,data=newfish.q)
newfish.qdapred<-predict(newfish.qda,newfish.q)
table(newfish.q$Species,newfish.qdapred$class) #結果全對

# 用結果丟test進去預測 但就沒有白魚這群
predict(newfish.qda,newfish.test)$class 

# cross validation = TRUE
newfish.qda<-qda(Species~Weight+L1+Height+Width+L21+L32,data=newfish.q,CV=T)
table(newfish.q$Species,newfish.qda$class) #error rate 變小


########## Nearest Neighbor (NN) #####################
library(class)
# k=3 21%
newfish.knn <-knn(newfish[,2:10],newfish[,2:10],newfish[,"Species"],k=3,prob=T)
table(newfish$Species,newfish.knn) 
# k=2 17%
newfish.knn <-knn(newfish[,2:10],newfish[,2:10],newfish[,"Species"],k=2,prob=T)
table(newfish$Species,newfish.knn) 
#k=1 全對
newfish.knn <-knn(newfish[,2:10],newfish[,2:10],newfish[,"Species"],k=1,prob=T)
table(newfish$Species,newfish.knn) 

# cross validation = TRUE / prob=T 平手的時候random
newfish1<-newfish[,c(1,2,3,6,8,9)]
newfish.knncv<-knn.cv(newfish1[,2:6],newfish1[,"Species"],k=1,prob=T)
table(newfish1$Species,newfish.knncv) #k=1結果很慘 35.8%
newfish.knncv<-knn.cv(newfish1[,2:6],newfish1[,"Species"],k=2,prob=T)
table(newfish1$Species,newfish.knncv) #k=2結果很慘 42.6%

# 用結果丟test進去預測
newfish1.test<-newfish.test[,c(1,2,5,7,8)]
newfish.knntest<-knn(newfish1[,2:6],newfish1.test,newfish1[,"Species"],k=1,prob=T)
newfish.knntest


########## Logistic Discrimination #####################
library(nnet)
newfish.logd <- multinom(Species~Weight+L1+Height+Width+L21+L32,data=newfish,maxit=250)
newfish.logd
table(newfish$Species,predict(newfish.logd,newfish)) #結果全對 跟LDA結果接近

# cross validation
install.packages("glmnet")
library(glmnet)
x <- as.matrix(newfish[,-1])
y <- newfish$Species
cvfit <- cv.glmnet(x, y, family="multinomial", type.measure="class", nfolds=148)
predict.value <- predict(cvfit, x, s = "lambda.min", type = "class")
table(predict.value,newfish$Species) #結果全對

# 用結果丟test進去預測
predict(newfish.logd,newfish.test)


# 要比較結果誰比較好 要比
