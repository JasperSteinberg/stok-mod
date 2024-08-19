#initial data
T = 59
lambda = 1.5
gamma = 10
Num = 1000
sampledX = rpois(1000, lambda = lambda*T)
sampledZ = vector("numeric", Num)
zcount = 0

#calculates the amount of generations that were above 8 million
for(i in sampledX){
  Z = 0
  Z = sum(rexp(i,gamma))
  sampledZ[zcount] = Z
  zcount = 1 + zcount
}
count = 0
for (i in 1:Num) {
  if (sampledZ[i] > 8) {
    count <- count + 1
  }
}
#prints
count/1000


  
  
  
  
  
  
  