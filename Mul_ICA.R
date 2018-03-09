
##### Independent Component Analysis #######
library(fastICA)

#### fastICA #####
c <-read.table("citycrime.txt",h=T)
  # 跟前面PCA做比較，但其實比較不符合ICA做

a <- fastICA(c,7)
# The estimated mixing matrix: (read in column)
a$A
# The estimated (orthonormal) un-mixing matrix (after centering and whitening):
# 我們要看的，看直的(column)，每次的結果會不太一樣，因為近似的結果收斂在不同地方
# 每個都同等重要
a$W

# 想要找到的答案比較stable一點，可以去改fastICA裡面的參數
newa <- fastICA(c,7, tol = 1e-06,maxit = 1000)
newa$W

# Plotting the first 3 original mixed signals in X:
  
par(mfcol = c(3, 2))
plot(1:72, c[,1], type = "l", main = "Mixed Signals X1")
plot(1:72, c[,2], type = "l", main = "Mixed Signals X2")
plot(1:72, c[,3], type = "l", main = "Mixed Signals X3")

# Plotting the first 3 ICA Source Estimates in S:

plot(1:72, a$S[,1], type = "l", main = "ICA Source Estimate S1")
plot(1:72, a$S[,2], type = "l", main = "ICA Source Estimate S2")
plot(1:72, a$S[,3], type = "l", main = "ICA Source Estimate S3")

# 但是順序對應不一定會是一樣，所以還是全部畫出來看比較準，但是R不能一次畫那麼多個

# 全部畫出來
par(mfcol = c(7, 2), mar=c(4,4,0,0))
for (i in 1:7) {
  plot(1:72, c[,i], type = "l", main = paste("Mixed Signals X", i, sep=""))
}
for (i in 1:7) {
  plot(1:72, a$S[,i], type = "l", main = paste("ICA Source Estimate S",i,sep = ""))
}
# 這份資料比較沒有可以解釋的空間

