
library(dplyr)
library(RCurl)
library(lme4)
library(MVN)
library(CCP)
library(rpart)
library(MASS)
library(class)
library(GGally)
library(ggplot2)
library(adabag)
library(randomForest)

######## Q1 PCA ##############################
# The data are from the US Census Bureau and describe the changes 
# in the population of 51 states between 2000 and 2001. 
# Note that all variables have been transformed to 
# “rates/numbers per 1000 inhabitants”.
# 
# X1 = difference of “Net domestic migration rate”, 
#   i.e. the change of “1000 x ((domestic in-migration)-(domestic out-migration))/(total population)” 
# X2 = difference of “Federal/Civilian move from abroad”, 
#   i.e. the change of “1000 x (# of civilian move from abroad)/(total population)” 
# X3 = difference of “Net international migration rate”, 
#   i.e. the change of “1000 x ((international in-migration)-(international out-migration))/(total population)”
#   每一千個居民(時間中段的人數)中移民入境的人數和移民出境人數的差值。正值表示遷入的人多於遷出的人
# X4 = birth rate for the period, i.e. 1000 x (# of births)/(# of women)
# X5 = death rate for the period, i.e. 1000 x (# of deaths)/(total population)
# X6 = # of population less than 65 years old (per 1000 inhabitants)
# X7 = # of population over 65 years old (per 1000 inhabitants)
#         
US <- read.table("US_census.txt", h=T)
pairs(US)
ggpairs(US)
US <- US[,-7]
newus <- US[,-c(6,7)]
# outlier
us.out <- mvOutlier(US, qqplot = TRUE, alpha=1, method = "adj.quan")
newus <- us.out$newData

pca.us <-princomp(scale(US,scale=TRUE,center=TRUE),cor=TRUE)
summary(pca.us) 
loadings(pca.us) 
pcs.us<-predict(pca.us) 

pca.newus <-princomp(scale(newus,scale=TRUE,center=TRUE),cor=TRUE)
summary(pca.newus) 
loadings(pca.newus) 

# plot
biplot(pca.us,scale=1)
biplot(pca.newus,scale=1)

plot(pcs.us[,1:2],type="n",xlab='1st PC',ylab='2nd PC') 
text(pcs.us[,1:2],row.names(US))

sign.pc<-function(x,R=1000,m=length(x), cor=T,...){
  # run PCA
  pc.out<-princomp(x,cor=cor,...)
  # the proportion of variance of each PC
  pve=(pc.out$sdev^2/m)[1:m]
  # a matrix with R rows and m columns that contains
  # the proportion of variance explained by each pc
  # for each randomization replicate.
  pve.perm<-matrix(NA,ncol=m,nrow=R)
  for(i in 1:R){
    # permutation each column
    x.perm<-apply(x,2,sample) #sample指令
    # run PCA 
    pc.perm.out<-princomp(x.perm,cor=cor,...)
    # the proportion of variance of each PC.perm
    pve.perm[i,]=(pc.perm.out$sdev^2/m)[1:m]
  }
  # calcalute the p-values
  pval<-apply(t(pve.perm)>pve,1,sum)/R
  return(list(pve=pve,pval=pval))
}

sign.pc(US)
#只有第一個顯著

######## Q2 CCA ##############################
# Observations: 41 US cities
# 
# Variables: 
#   
# PT: Particulates content of air ( in 10^(-6) grams per cubic meter )
# CO: Carbon monoxide content of air (in ppm)
# SO2: Sulfur dioxide content of air (in ppb)  
# PSI: Air pollution index, PSI=0 means "good", PSI=1 means "moderate", PSI=2 means "Unhealthy".
# Temp: Average annual temperature in degrees Fahrenheit 
# Man: Number of manufacturing enterprises employing 20 or more workers 
# Pop: Population size in thousands from the 1970 census 
# Rain: Average annual precipitation in inches 

air <- read.table("Air_Pollution.txt", h=T) %>% dplyr::select(-4)
# Multivariate normal test
mardiaTest(air,qqplot=F) 
hzTest(air,qqplot=F) 
roystonTest(air,qqplot=F) 
# Result  : Data are not multivariate normal. 

# result<-boxcox(y[,3]~1,seq(-2,2,1/100))
# lambda<-result$x[which.max(result$y)]
# y3new<-(y[,3]^lambda-1)/lambda
# hist(y3new)


#(1) 針對SO2轉換
# 先取log，在取lambda = 0.64的Boxcox轉換，得到新的轉換資料
x = air[,3] %>% log() #先取log
gm = exp(mean(log(air[,3])))  #幾何平均
lambda = 0.64
{(x^lambda-1)/lambda* gm^(1-lambda)} %>% hist()
y = {(x^lambda-1)/lambda* gm^(1-lambda)}
ks.test((y-mean(y))/sd(y), "pnorm")

air$SO2 = y
# 作轉換  

air$Man <- log(air$Man)
air$Pop <- log(air$Pop)

# test each normal
uniNorm(air, type="Lillie", desc=T)

# check outlier
result <- mvOutlier(air, qqplot = TRUE, alpha=0.85, method = "adj.quan")
result
#
newair <- result$newData
mardiaTest(newair,qqplot=F)
hzTest(newair,qqplot=F) 
roystonTest(newair,qqplot=F) 

# check normality test on each variable 
qqnorm(air$SO2)
qqnorm(log(air$Man))
qqnorm(log(air$Pop)) 
par(mfrow=c(1,3))
hist(air$SO2)
hist(air$Man)
hist(air$Pop)


# data split
x <- dplyr::select(newair, 4:7)
y <- dplyr::select(newair, 1:3)
cxy<-cancor(scale(x,scale=T,center=T),scale(y,scale=T,center=T)) #標準化
cxy
cxy$cor %>% p.asym(34,3,4,tstat="Wilks")
# plot
xx<-scale(x,scale=T,center=T) 
yy<-scale(y,scale=T,center=T) 
scorex<-xx%*%cxy$xcoef[,1]  # 矩陣的乘法 線性組合
scorey<-yy%*%cxy$ycoef[,1] 
plot(scorex,scorey,type="n", xlab = "U1", ylab = "V1") 
text(scorex,scorey,row.names(newair),cex=.6)


######## Q3  ##############################
# This data set describes seven measured geometric parameters of 
# 205 wheat kernels which belong to three different types of wheat: 
# Kama (Y=1), Rosa (Y=2) and Canadian (Y=3).
# 
# X1 = area (mm^2)
# X2 = perimeter (mm)
# X3 = compactness (4*pi*area/perimeter^2)
# X4 = length of kernel (mm)
# X5 = width of kernel (mm)
# X6 = asymmetry coefficient 非對稱係數
# X7 = length of kernel groove (mm)

seed <- read.table("Seeds.txt",h=T)
table(seed$Y)
ggplot() +
  geom_point(aes(x=X1, y=X6, color=factor(Y)), data = seed)
ggpairs(seed[,-8])


##### classification tree
scontrol <- rpart.control(minisplit=20, minbucket=7, xval = 0)
treeorig <- rpart(Y~.,data=seed,method="class", control = scontrol)
plot(treeorig)
text(treeorig)
printcp(treeorig) #X1 X6 X7
summary(treeorig)
# prune
prunetree <- prune.rpart(treeorig, cp=0.01)
plot(prunetree)
text(prunetree) #沒差

# cv
scontrolcv <- rpart.control(minisplit=20, minbucket=7, xval = 205)
treecv <- rpart(Y~.,data=seed,method="class", control = scontrolcv)
plot(treecv)
text(treecv)
printcp(treecv) #X1 X6 X7
summary(treecv)

# error rate
(136*0.14706)/205 *100

##### LDA
cor(seed)
seedlda <- lda(Y~., data=seed) # No warnings
seedlda2 <- lda(Y~X1+X6+X7, data=seed) 
seedlda
# predict
ldapred <- predict(seedlda, seed[,-8])
table(seed$Y, ldapred$class) #confusion matrix
# 6 error
6/205 *100

ldapred2 <- predict(seedlda2, seed[,-8])
table(seed$Y, ldapred2$class)
# 6 error

# cv
seedldacv <- lda(Y~., data = seed, CV=T)
table(seed$Y, seedldacv$class) #confusion matrix
# 6 error

# plot
eqscplot(ldapred$x, type="n",xlab="1st LD",ylab="2nd LD")
ys<-c(rep("1",69),rep("2",68),rep("3",68))
ycolors<-c(rep(1,69),rep(2,68),rep(3,68))
text(ldapred$x[,1:2],ys,col=ycolors)

##### QDA
seedqda <-qda(Y~.,data=seed)
qdapred<-predict(seedqda,seed)
table(seed$Y,qdapred$class)
# 8 error
8/205*100

# cv
qdacv<-qda(Y~.,data=seed,CV=T)
table(seed$Y,qdacv$class)
# 10 error

##### NN
knn3 <-knn(seed[,-8],seed[,-8],seed[,"Y"],k=3,prob=T)
table(seed$Y,knn3) 
7/205*100

knn2 <-knn(seed[,-8],seed[,-8],seed[,"Y"],k=2,prob=T)
table(seed$Y,knn2) 
14/205*100

knn1 <-knn(seed[,-8],seed[,-8],seed[,"Y"],k=1,prob=T)
table(seed$Y,knn1) 
13/205*100

# cv
knncv3<-knn.cv(seed[,-8],seed[,"Y"],k=3,prob=T)
table(seed$Y,knncv3) 
24/205*100

knncv2<-knn.cv(seed[,-8],seed[,"Y"],k=2,prob=T)
table(seed$Y,knncv2) 
20/205*100

knncv1<-knn.cv(seed[,-8],seed[,"Y"],k=1,prob=T)
table(seed$Y,knncv1) 
18/205*100

######## Q4 compare  ##############################
# tree
treecv <- rpart(Y~X1+X6+X7,data=seed,method="class", control = scontrolcv)
plot(treecv)
text(treecv)
printcp(treecv) #X1 X6 X7
# LDA
seedldacv <- lda(Y~X1+X6+X7, data = seed, CV=T)
table(seed$Y, seedldacv$class)
# QDA
qdacv<-qda(Y~X1+X6+X7,data=seed,CV=T)
table(seed$Y,qdacv$class)
# NN
seednn <- seed[,c(1,6,7,8)]
knncv1<-knn.cv(seednn[,-4],seed[,"Y"],k=1,prob=T)
table(seednn$Y,knncv1) 

### Predict
seed.test <- read.table("Seeds_test.txt")
colnames(seed.test) = colnames(seed)[-8]
# LDA
seedlda <- lda(Y~., data=seed) # No warnings
seedlda <- lda(Y~X1+X6+X7, data=seed) 
predict(seedlda,seed.test)$class
#tree
treecv <- rpart(Y~.,data=seed,method="class", control = scontrolcv)
treecv <- rpart(Y~X1+X6+X7,data=seed,method="class", control = scontrolcv)
predict(treecv, seed.test)
# QDA
seedqda <-qda(Y~.,data=seed)
qdacv<-qda(Y~X1+X6+X7,data=seed)
predict(seedqda,seed.test)$class 
# NN
knn(seed[,-8],seed.test,seed[,"Y"], k=1, prob = T)
knn(seed[,c(1,6,7)],seed.test[,c(1,6,7)],seed[,"Y"], k=1, prob = T)

######## Q5  ##############################

## random forest
seed.rf<-randomForest(Y~., importance=TRUE, proximity=TRUE, data=seed)
print(seed.rf)

# 把可以選的變數增加到4個
seed2.rf<-randomForest(Y~., importance=TRUE, mtry=3, proximity=TRUE, data=seed)
print(seed2.rf)

  # prediction error
seedcv<-rfcv(seed[,-8], seed[,8], cv.fold=205, step=0.5, scale = "log")
seedcv$n.var
seedcv$error.cv
#可以告訴你選幾個變數的時候最好
# 3最小

## boosting with boostrap re-sampling
seed$Y= factor(seed$Y)
control5<-rpart.control(minisplit=20,minbucket=7,cp=0.02)

adaboost <- boosting(Y~., data=seed ,boos=T, mfinal=10)
adaboost.pred <- predict.boosting(adaboost,seed)
adaboost.pred$confusion

adaboostcv<-boosting.cv(Y~., data=seed ,v=205,boos=T,mfinal=20, control = control5)
adaboostcv[-1]







