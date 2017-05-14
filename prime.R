##質數######

prime <- function(a,b){}
k=b-a+1  
A=matrix(nrow = 2, ncol = k)
A[1,]=5:17



## 以下為迴圈  
for (i in 2:17) {

n=ceiling(i/2)
j = 2
ans=1
while (j<=n) {
  
  if(i%%j == 0){
    ans=0}
  if(ans==0){break}
  j=j+1
  
}

ifelse(ans==1,print("yes"),print("no"))

}


## function
prime <- function(a,b){
for (i in a:b) {

n=ceiling(i/2)
j = 2
ans=1
while (j<=n) {
  
  if(i%%j == 0){
    ans=0}
  if(ans==0){break}
  j=j+1
}
if (ans == 1){print(i)}

}

}
prime(0,50)


