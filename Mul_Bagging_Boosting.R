
##### Bagging Boosting #######

### (1) Bagging #####
fish <- read.table("fish.txt", h=T)
L21<-fish$L2-fish$L1
L32<-fish$L3-fish$L2
L31<-fish$L3-fish$L1
newfish<-cbind(fish,L21,L32,L31)

library(adabag)
fish.control<-rpart.control(minisplit=10,minbucket=3,cp=0.01) # 設定跟前面一樣
# the same tree with nsplit=8 was used before 
newfish.bagging<-bagging(Species~., data=newfish,control=fish.control) 
# the default number of iteration is 100 
newfish.bagging.pred <- predict.bagging(newfish.bagging,newfish) # 預測的結果
newfish.bagging.pred$confusion

newfish.bagging.pred$error
# The resulting apparent error rate = 0.676%

## cross validaton 
# 只用（跑）20個去做投票
newfish.baggingcv<-bagging.cv(Species~., data=newfish,v=148,mfinal=20,control=fish.control)
newfish.baggingcv[-1]
# The estimated true error = 1.35%. 


### (2) Random Forest ####
library(randomForest)

# default generates 500 trees
# default # of selected variables = sqrt(# of variables)
fish.rf<-randomForest(Species~., importance=TRUE, nodesize=3, proximity=TRUE, data=newfish)
print(fish.rf)

# 把可以選的變數增加到6個
fish.rf<-randomForest(Species~., importance=TRUE, mtry=6, nodesize=3, proximity=TRUE, data=newfish)
print(fish.rf)

# prediction error
fish.cv<-rfcv(newfish[,-1], newfish[,1], cv.fold=148, step=0.7)
fish.cv #可以告訴你選幾個變數的時候最好


### (3) Boosting ####
# 先做一次 不boostrap 
newfish.adaboost <- boosting(Species~., data=newfish, boos=F, mfinal=20,control=fish.control)
newfish.adaboost.pred <- predict.boosting(newfish.adaboost,newfish)
newfish.adaboost.pred$confusion
# The resulting apparent error rate = 0%

# 加boos=T 結果都是零
newfish.adaboost <- boosting(Species~., data=newfish, boos=T, mfinal=20)
newfish.adaboost.pred <- predict.boosting(newfish.adaboost,newfish)
newfish.adaboost.pred$confusion

## cross validation
newfish.adaboostcv<-boosting.cv(Species~., data=newfish,v=148,boos=F,mfinal=20,control=fish.control)
newfish.adaboostcv[-1]

# 試著去更改變數v=148,boos=T,mfinal=10 看結果是否相同
newfish.adaboostcv<-boosting.cv(Species~., data=newfish,v=148,boos=T,mfinal=10,control=fish.control)
newfish.adaboostcv[-1]

# Zhu 提出的
newfish.adaboostcv<-boosting.cv(Species~.,data=newfish,coeflearn='Zhu',v=148,mfinal=20,control=fish.control)
newfish.adaboostcv[-1]



