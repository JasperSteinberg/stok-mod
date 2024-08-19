#initial data
Y_0 = c(950,50,0)
Y_N = Y_0
S <- c()
I <- c()
R <- c()
for (x in 1:300) {
  #new amount of susceptible people
  S_n = rbinom(1,Y_N[3],0.005)
  #new amount of infected people
  I_n = rbinom(1,Y_N[1],0.5*Y_N[2]/1000)
  #etc
  R_n = rbinom(1,Y_N[2],0.1)
  #redistributes people between groups
  Y_N = c(Y_N[1]-I_n+S_n,Y_N[2]-R_n+I_n,Y_N[3]-S_n+R_n)
  #plotting purposes
  S =append(S,Y_N[1])
  I = append(I,Y_N[2])
  R =append(R,Y_N[3])
}

#plot
plot(c(1:300),S,type = "l", lwd=2, col ="blue", ylab="People", xlab="Days")
lines(I, col = "red",lwd = 2)
lines(R, col = "green",lwd = 2)
legend(40,700,legend = c("S","I","R"), col = c("blue","red","green"),lwd =c(2,2,2),cex = 0.8)