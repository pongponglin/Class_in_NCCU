
crime <- read.table("citycrime.txt")
pairs(crime) 
cor(crime) #see the correlation
# check outlier 可以做做看
# 不太有可以被剔除的變數,也不太有共線性(check)

library(stats)
library(RCurl)

pca.crime <- princomp(scale(crime), cor=TRUE) 
pca.crime<-princomp(scale(crime,scale=TRUE,center=TRUE),cor=TRUE) 
#,scale=TRUE,center=TRUE 標準化 (same)
summary(pca.crime) #Look at the results 
# 2個維度的結果看起來差不多了
loadings(pca.crime) #Calculate the loadings
# Note that it defines loadings without multiplying by the sqrt of lambda
# comp.1都是負的,代表在左邊的都是總體犯罪率高的,相對右邊都是整體來說犯罪率低的
# comp.2(很小的他就直接拿掉),有三個正三個負

#Calculate the PCs. 
pcs.crime <- predict(pca.crime) 

#Check out the screeplot. 
eigen<-eigen(cor(crime)) 
plot(eigen$values,type="h")  # eigenvalue由大排到小

#Plot the first 2 PCs. 
plot(pcs.crime[,1:2],type="n",xlab='1st PC',ylab='2nd PC') 
text(pcs.crime[,1:2],row.names(crime))

#Plot also the biplot.  
biplot(pca.crime,scale=1) #解釋<聚在一起的代表犯罪的情況差不多
# 上面的跟右邊的座標是給紅色的線(comp.2的位置)
# 第一個維度是最重要的,但第二個可以發現一些有趣的事



####  Permutation Test ###########################################

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


sign.pc(crime,cor=T)
#The p-values show that merely the 1st PC is significant!





