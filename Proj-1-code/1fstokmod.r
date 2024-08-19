#some inherited code from 1e
maxiS = c()
for (i in 1:1000){ 
 Y_0 = c(950,50,0)
  Y_N = Y_0
  maxI = c("0,0")
  for (x in 1:300) {
    S_n = rbinom(1,Y_N[3],0.005)
    I_n = rbinom(1,Y_N[1],0.5*Y_N[2]/1000)
    R_n = rbinom(1,Y_N[2],0.1)
    Y_N = c(Y_N[1]-I_n+S_n,Y_N[2]-R_n+I_n,Y_N[3]-S_n+R_n)
    #saves the peak and peak date
    if (Y_N[2] > maxI[1]){maxI = c(Y_N[2],x)} 
  }
  maxiS = append(maxiS,maxI)
}
#ad hoc solution for weird R formatting, creates list of inverse of occurrences 
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
#calculates expected values
expI = 0
for (i in 1:1000){
  expI = expI + i*eI[i]}
expE = 0
for (i in 1:1000){
  expE = expE + i*eE[i]}

print(expI)
print(expE)

#more ad hoc reformatting to find conf.ints.
dataI =c()
for (i in 1:100)
{dataI = append(dataI,maxiS[2*i])}
dataE =c()
for (i in 1:100)
{dataE = append(dataE,maxiS[2*i-1])}

#calculate, extract and print the confidence intervals
result <- t.test(dataI)
confidence_interval <- result$conf.int
confidence_interval

result <- t.test(dataE)
confidence_interval <- result$conf.int
confidence_interval
