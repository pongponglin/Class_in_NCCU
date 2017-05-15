
col<-read.table("new_col2000.txt",h=T)
# Academic聲望 Grade畢業率 Accept錄取率
# SAT_P25較低的25% SAT_P75較高的75% HS_P10總成績在前10%
# 把資料分成兩個變數的data
x<-cbind(col[,1],col[,2],col[,6]) 
y<-cbind(col[,3],col[,4],col[,5])
cxy<-cancor(scale(x,scale=T,center=T),scale(y,scale=T,center=T)) #標準化
cxy
# 結果：三組 第一組相關係數最高

library(dplyr)
library(lme4) 
library(MVN) 

# 檢定是不是 mutivariate normal
mardiaTest(col,qqplot=F) # p.value 很小，表示不是MVN
hzTest(col,qqplot=F) # p.value 很小
roystonTest(col,qqplot=F) # p.value 很小
# 都很小的時後怎麼辦

#### Method1
# check outlier ：adjusted robust Mahalanobis distance
# alpha 會影響他給你的outlier個數
result <- mvOutlier(col, qqplot = TRUE, alpha=0.8, method = "adj.quan")
result # 跟你說哪些是outlier 七個
# The adjusted chi-square plot is given next, 
# where by default the outliers are those observations above the 97.5 percentile.
newcol<-result$newData # newData 就是這個pkg給你的 丟掉outlier之後的
roystonTest(newcol,qqplot=F) # p.value=0.053 稍微通過
mardiaTest(newcol,qqplot=F) 

#### Method2
# ks.test 看單一變量哪些不符合常態
uniNorm(col, type="Lillie", desc=T) # 第2,5個不是 可以做box-cox 轉換

library(CCP)

newx<-cbind(newcol[,1], newcol[,2], newcol[,6]) 
newy<-cbind(newcol[,3], newcol[,4], newcol[,5])
newcxy<-cancor(scale(newx, scale=T, center=T),scale(newy, scale=T, center=T))
newcxy$cor %>% p.asym(35,3,3,tstat="Wilks")   #Here N=35, p=q=3 
# 結果顯示前兩組顯著
newcxy # check 前兩個的 correlation 正相關
# $xcoef U1：只看第一個  U2：跟前面有重複的話解釋起來就會不太好
# $ycoef V1：看第一個跟第二個  V2：資訊有重疊
# 解釋：學校聲望越高，對應P75的要高 P25的要小（有保護措施）


# Let us visualize the result in the first canonical variate space
# 把U1跟V1的關係畫出來
xx<-scale(newx,scale=T,center=T) 
yy<-scale(newy,scale=T,center=T) 
scorex<-xx%*%newcxy$xcoef[,1]  # 矩陣的乘法 線性組合
scorey<-yy%*%newcxy$ycoef[,1] 
plot(scorex,scorey,type="n", xlab = "U1", ylab = "V1") 
text(scorex,scorey,row.names(newcol),cex=.6)
# x軸左邊就是學校聲望好的 y下方就是P75高 P25低


