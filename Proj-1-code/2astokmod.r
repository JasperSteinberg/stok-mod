T = 59 #Antall dager
lambda = 1.5 #parameteren vår
x = vector("numeric", 1000) #Lag en tom vektor
for(i in 1:1000){ #simuler poissonprossesen
  N = rpois(1, lambda = lambda*T)
  x[i] = N 
}

count = 0
for (i in 1:1000) {#Sjekker antall ganger vi har over 100 claims.
  if (x[i] > 100) {
    count <- count + 1
  }
}
count/1000 #finner den approksimerte sannsynligheten 
plot(NULL, NULL, xlim = c(0, T), ylim = c(0, 120), xlab = "Time", ylab = "Events", main = "Homogeneous Poisson process", lwd = 2) #plotter 10 realiseringer
for(j in 1:9){
N = rpois(1, lambda = lambda*T)
tVals = runif(N, min = 0, max = T)
tVals = c(0, sort(tVals), T)

xVals = c(0:N, N)
for(i in 1:(length(xVals)-1)){
  lines(tVals[i:(i+1)], rep(xVals[i],2), lwd = 2, col = j)
}
}
lines(c(0, T), c(0, 59*1.5), col = "red", lwd = 2)