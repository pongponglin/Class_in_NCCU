
##### SVM in R  #######
library(e1071)

fish <-read.table("fish.txt",h=T)
s<-svm(fish[,2:7],fish[,1])
# Note that the default settings are: cost=1, kernel=RBF, gamma=1/(# of variables). 
summary(s)

pred<-predict(s,fish[,2:7])
table(pred,fish[,1])
# ==>The apparent error rate is 25/148.
# 結果不太好，去調參數

# We can change gamma to get a new (or better) classifier:
# 基本上把gamma放大，就會變準
s1<-svm(fish[,2:7],fish[,1],gamma=1)
pred1<-predict(s1,fish[,2:7])
table(pred1,fish[,1])
# ==>The new error rate is 16/148
s1<-svm(fish[,2:7],fish[,1],gamma=100)
pred1<-predict(s1,fish[,2:7])
table(pred1,fish[,1])
# 100 變成只有一個錯了：overfitting

# Theoretically, the increase of gamma will derive an apparent error rate 0.
# However, this might cause an over-fitting problem which affects the "true error rate".
# Now we use a 20-fold CV to derive a good combination of (cost,gamma) based on grid search:

### (1) Let's start with (cost=0.1,gamma=0.1): #####
c1<-svm(fish[,2:7],fish[,1],cost=0.1,gamma=0.1,cross=20)
summary(c1)

### (2) Change to (cost=0.5,gamma=0.1): #####
c2<-svm(fish[,2:7],fish[,1],cost=0.5,gamma=0.1,cross=20)
summary(c2)
# Total Accuracy: 82.43243

### (3) Change to (cost=100,gamma=0.2): #####
c3<-svm(fish[,2:7],fish[,1],cost=130,gamma=0.2,cross=20)
summary(c3)
# Total Accuracy: 90.54054

# Question: How to find the best combination of (cost, gamma) that minimizes the prediction error?
# To find the best combination of the tuning parameters (cost, gamma), one can perform a grid search 
# over a respecified parameter range. Suppose now we search the best (cost, gamma) over the region 
# of [100,1000]x[0.5,5], with 10 equally spaced points allocated for each dimension (thus 100 grid points to be compared).
# The result based on a 10-fold cross validation (default) can be produced by:

tobj <- tune.svm(Species ~ ., data=fish, cost= 100*(1:10), gamma=5*(1:10))  
tobj <- tune.svm(Species ~ ., data=fish, cost= 10*(1:20), gamma=0.5*(1:20))
tobj <- tune.svm(Species ~ ., data=fish, cost= 100*(1:10), gamma=0.1*(1:10))
tobj <- tune.svm(Species ~ ., data=fish, cost= 100*(1:10), gamma=0.05*(1:10))
summary(tobj)
# cv:10-fold，會告訴你最好的組合在哪裡
a <- tune.control(cross = 148)
tobj <- tune.svm(Species ~ ., data=fish, cost=150*(1:20), gamma=0.005*(1:20), tunecontrol=a) 
summary(tobj)
# 去改cv次數


# For the overall comparison, one can produce the contour plot 
# of prediction errors over the search range of (cost, gamma) by:

plot(tobj, xlab = "gamma", ylab="C")

# Based on the optimal choice (cost=300, gamma=0.5), we can fit a new SVM model for all training data:
  
c1<-svm(fish[,2:7],fish[,1],cost=300,gamma=0.5)
pred<-predict(c1,fish[,2:7])
table(pred,fish[,1])

# Now we use this classifier to predict the test data:
  
test<-read.table("fish_test.txt",h=T)
predict(c1,test)





