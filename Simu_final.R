
#######
# 標題 動機與目的 研究方法與假設 結論與備案

library(dplyr)
### Final report

num=1:13
type=c("space", "heart", "diamond", "club")
interaction(num, type)
poker <- expand.grid(num, type)
poker <- paste(poker$Var1, poker$Var2)
s <- sample(poker, 13)

str <- strsplit(s, " ")
num <- unlist(str)[seq(1,26, by=2)] %>% as.numeric() %>% sort()
cha <- unlist(str)[seq(2,26, by=2)]
unique(num)[-1]-unique(num)[-length(unique(num))]


######## casino war ################################################
draw <- function(x){
  x <- sample(x, length(x))
  a <- sample(x, 1)
  s <- sum(x==a)-1
  x <- c(x[x!=a], rep(a,s))
  return(x)
}

### 1. #####
n <- 150
A <- matrix(ncol = 3, nrow = 1000)
for (i in 1:1000) {
  x <- rep(c(2:14),6*4)
  k <- vector(length = n)
  for (j in 1:n) {
    # player
    x <- sample(x, length(x))
    a <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    # 莊
    x <- sample(x, length(x))
    b <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    
    if (a>b){ k[j]=1 } else if (a==b) {k[j]=0} else  k[j]=-1
  }
  A[i,1] <- sum(k==1)/150
  A[i,2] <- sum(k==-1)/150
  A[i,3] <- sum(k==0)/150
}
tmp <- apply(A, 2, mean)
apply(A, 2, var)

312/(2*(tmp[1]+tmp[2])+10*tmp[3])
time <- 15*(tmp[1]+tmp[2])+90*tmp[3]
time*100/60

#### 2. ######

# 模擬玩 50 局 1000 次，記錄每次剩下的錢為多少，
# 當模擬的次數越多

x <- rep(c(2:14),6*4)
money = 1000
n <- 100
k <- vector(length = n)
money_n <- vector(length = n)
simu_money <- vector(length = 1000)
e = NULL

for (j in 1:n) {
  # player
  x <- sample(x, length(x))
  a <- sample(x, 1)
  s <- sum(x==a)-1
  x <- c(x[x!=a], rep(a,s))
  # 莊
  x <- sample(x, length(x))
  b <- sample(x, 1)
  s <- sum(x==a)-1
  x <- c(x[x!=a], rep(a,s))
  
  if (a>b){ k[j]=1 } else if (a==b) {k[j]=0} else  k[j]=-1
  if (k[j] != 0 ) { money <- money+k[j]*10 }
  else  {
    for (i in 1:3) {
      x<- draw(x)
    }
    # player
    x <- sample(x, length(x))
    a <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    for (i in 1:3) {
      x<- draw(x)
    }
    # 莊
    x <- sample(x, length(x))
    b <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    if (a>b){ g=1 } else if (a==b) {g=4} else  g=-2
    money <- money+g*10
    e  <- c(e,g)
  }
  money_n[j] <- money
  left <- length(x)
}

k
table(k)
e
money
money_n
plot(money_n, type = "l")


par(mfrow=c(3,3), mar=c(2,2,2,2))
for (i in 1:3) {
  x <- rep(c(2:14),6*4)
  money = 1000
  n_50 <- game(x,50,money)
  plot(n_50, type = "l", main = "n=50")
}
for (i in 1:3) {
  x <- rep(c(2:14),6*4)
  money = 1000
  n_70 <- game(x,70,money)
  plot(n_70, type = "l", main = "n=70")
}
for (i in 1:3) {
  x <- rep(c(2:14),6*4)
  money = 1000
  n_100 <- game(x,100,money)
  plot(n_100, type = "l", main = "n=100")
}

# 3. ######

game <- function(x,n,money){
  money_n <- vector(length = n)
  for (j in 1:n) {
    # player
    x <- sample(x, length(x))
    a <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    # 莊
    x <- sample(x, length(x))
    b <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    
    if (a > b){ k=1 } else if (a==b) {k=0} else  k=-1
    if (k != 0 ) { money <- money + k*10 }
    else  {
      for (i in 1:3) {
        x<- draw(x)
      }
      # player
      x <- sample(x, length(x))
      a <- sample(x, 1)
      s <- sum(x==a)-1
      x <- c(x[x!=a], rep(a,s))
      for (i in 1:3) {
        x<- draw(x)
      }
      # 莊
      x <- sample(x, length(x))
      b <- sample(x, 1)
      s <- sum(x==a)-1
      x <- c(x[x!=a], rep(a,s))
      if (a > b){ g=1 } else if (a == b) {g = 4} else  g = -2
      money <- money + g*10
    }
    money_n[j] <- money
  }
  return(money = money_n)
}


n <- 70
simu_money <- matrix(ncol = n, nrow = 10000)
for (i in 1:10000) {
  x <- rep(c(2:14),7*4)
  money = 1000
  simu_money[i,] <- game(x,n,money) 
}

n <- 70
simu_money2 <- matrix(ncol = n, nrow = 10000)
for (i in 1:10000) {
  x <- rep(c(2:14),7*4)
  money = 1000
  for (j in 1:n) {
    # player
    x <- sample(x, length(x))
    a <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    # 莊
    x <- sample(x, length(x))
    b <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    
    if (a > b){ k=1 } else if (a==b) {k=-0.5} else  k=-1
    if (k != 0 ) { money <- money + k*10 }
    simu_money2[i,j] <- money
  } 
}

#### 30 50 70 #######
mean(simu_money[,35])
mean(simu_money[,50])
mean(simu_money[,70])
sum(simu_money[,1] > 1000)/10000
sum(simu_money[,50] < 1000)/10000
sum(simu_money[,70] < 1000)/10000

mean(simu_money2[,30])
mean(simu_money2[,50])
mean(simu_money2[,70])
sum(simu_money2[,30] > 1000)/10000
sum(simu_money2[,50] > 1000)/10000
sum(simu_money2[,70] > 1000)/10000


#### 最後的金額 #####
least <- simu_money[,70]
plot(density(simu_money[,70]), xlim=c(600,1450),ylim=c(0,0.007), main = "density plot n=70 ")
abline(v=mean(simu_money[,70]), col=2)
hist(simu_money[,70], xlim=c(600,1450), main = "density plot n=70 ")
mean(least)
sd(least)
min(least)
max(least)
quantile(least, c(0.05,0.95))
sum(least > 1000)/10000
sum(least < 1000)/10000

apply(simu_money, 2, mean) %>% rank()

##### 抓最高的數字 #######
sum(apply(simu_money, 1, max) >= 1000)
max_place <- apply(simu_money, 1, which.max)
table(max_place)
win <- simu_money[max_place != 1,]

apply(simu_money, 1, min)

m <- apply(simu_money, 1, max_t)
table(m)

x <- simu_money[1,]
plot(x, type = "l")

max_t <- function(x){
  a <-(x[-1] - x[-length(x)] > 0)*1 #後面的數字大於前面的數字設為1
  b <- which(a[-1] != a[-length(a)])+1 #找到0 1變號的位置
  b <- c(1,b,length(x))
  c <- b[-1] - b[-length(b)] #利用變號的位置相減 可以找到0或1重複的次數
  return(max(c))
}
table(c)
max(c) #0 1 重複的次數

max_t(x)

##### 策略一 #######
# 870 1120

sol1 <- function(x,n,money){
  while ((n < 50) * (900 < money ) == 1  ) {
  n=n+1
  # player
  x <- sample(x, length(x))
  a <- sample(x, 1)
  s <- sum(x==a)-1
  x <- c(x[x!=a], rep(a,s))
  # 莊
  x <- sample(x, length(x))
  b <- sample(x, 1)
  s <- sum(x==a)-1
  x <- c(x[x!=a], rep(a,s))
  
  if (a > b){ k=1 } else if (a==b) {k=0} else  k=-1
  if (k != 0 ) { money <- money + k*10 } else  {
      for (i in 1:3) {
        x<- draw(x)
      }
      # player
      x <- sample(x, length(x))
      a <- sample(x, 1)
      s <- sum(x==a)-1
      x <- c(x[x!=a], rep(a,s))
      for (i in 1:3) {
        x<- draw(x)
      }
      # 莊
      x <- sample(x, length(x))
      b <- sample(x, 1)
      s <- sum(x==a)-1
      x <- c(x[x!=a], rep(a,s))
      if (a > b){ g=1 } else if (a == b) {g = 4} else  g = -2
      money <- money + g*10 }
}
  return(c(n,money))
}

  
sol1_n <- matrix(nrow =  11000,ncol = 2)
for (i in 1:11000) {
  x <- rep(c(2:14),7*4)
  money = 1000
  n=0 
  sol1_n[i,] <- sol1(x,n,money)
}

sol1_nn <- sol1_n[ (900 <= sol1_n[,2] ),2]
mean(sol1_nn) 
sum(sol1_nn > 1000)/length(sol1_nn)
hist(sol1_nn, main = "Histogram ( 900 < money )")
abline(v=mean(sol1_nn) ,col=2)
hist(sol1_n[,1])
sum(sol1_n[(900 <= sol1_n[,2] ),1] != 50)/length(sol1_nn)


#### 策略二 ######

sol2 <- function(x,nn,money){
  n1 = 0
  n2 = 0
  while ( (nn < 50) * (n1 < 3)* (n2 > -4) == 1 ) {
    nn = nn+1
    omoney <- money
    # player
    x <- sample(x, length(x))
    a <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    # 莊
    x <- sample(x, length(x))
    b <- sample(x, 1)
    s <- sum(x==a)-1
    x <- c(x[x!=a], rep(a,s))
    
    if (a > b){ k=1 } else if (a==b) {k=0} else  k=-1
    if (k != 0 ) {  money <- money + k*10 
    } else  {
      for (i in 1:3) {
        x<- draw(x)
      }
      # player
      x <- sample(x, length(x))
      a <- sample(x, 1)
      s <- sum(x==a)-1
      x <- c(x[x!=a], rep(a,s))
      for (i in 1:3) {
        x<- draw(x)
      }
      # 莊
      x <- sample(x, length(x))
      b <- sample(x, 1)
      s <- sum(x==a)-1
      x <- c(x[x!=a], rep(a,s))
      if (a > b){ g=1 } else if (a == b) {g = 4} else  g = -2
      money <- money + g*10 }
    if (money > omoney){n1 = n1+1 } else {n1 = 0}
    if (money > omoney){n2 = 0 } else {n2 = n2-1}
  }
  return(c(nn,money))
}


nn = 0
x <- rep(c(2:14),7*4)
money = 1000
sol2_n <- matrix(nrow =  10000,ncol = 2)
for (i in 1:10000) {
  sol2_n[i,] <- sol2(x,nn,money)
}

apply(sol2_n, 2, mean)
par(mfrow=c(1,2))
hist(sol2_n[,2], main = "Histogram of money(3win or 4lose)")
hist(sol2_n[,1], main = "Histogram of games(3win or 4lose)")
sum(sol2_n[,2] > 1000)/10000
quantile(sol2_n[,2], c(0.05,0.95))
  