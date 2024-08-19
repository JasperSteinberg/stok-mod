#for plotting the different cases
cols = c("red","purple","blue","green")
counter = 0
plot(c(0,300),c(0,600),type = "l", lwd=2, col = "white", ylab="People infected", xlab="Days")
for (vac in c(0,100,600,800)){
  counter = counter +1
  maxiS = c()
  #inherited code from 1e) and 1f)
  for (i in 1:1000){ 
    Y_0 = c(950-vac,50,0,vac)
    Y_N = Y_0
    I <- c()
    maxI = c("0,0")
    for (x in 1:300) {
      S_n = rbinom(1,Y_N[3],0.005)
      I_n = rbinom(1,Y_N[1],0.5*Y_N[2]/1000)
      R_n = rbinom(1,Y_N[2],0.1)
      Y_N = c(Y_N[1]-I_n+S_n,Y_N[2]-R_n+I_n,Y_N[3]-S_n+R_n)
      I = append(I,Y_N[2])
      if (Y_N[2] > maxI[1]){maxI = c(Y_N[2],x)} 
    }
    maxiS = append(maxiS,maxI)
  }
  eI = numeric(1000)
  eE = numeric(1000)
  for (n in 1:1000) {
    eI[maxiS[2*n]] = eI[maxiS[2*n]]+1
    eE[maxiS[2*n-1]] = eE[maxiS[2*n-1]]+1
  }
  for (jk in 1:1000){
    eI[jk] = eI[jk]/1000
    eE[jk] = eE[jk]/1000
  }
  expI = 0
  for (i in 1:1000){
    expI = expI + i*eI[i]}
  
  expE = 0
  for (i in 1:1000){
    expE = expE + i*eE[i]}
  
  print(expI)
  print(expE)
  #plots
  lines(I, col = cols[counter],lwd = 2)
}
legend(100,400,legend = c("0","100","600","800"), col = c("red","purple","blue","green"),lwd =c(2,2,2,2),cex = 0.8)