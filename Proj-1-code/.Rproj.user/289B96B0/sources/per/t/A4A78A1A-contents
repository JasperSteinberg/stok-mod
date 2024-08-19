N = 20*365 #Antall dager i 20 år
P = matrix(c(0.99, 0, 0.005, 0.01, 0.9, 0, 0, 0.10, 0.995 ), nrow = 3) #Vår TPM
#print(P)

S_vec = vector("numeric", length = 30)
R_vec = vector("numeric", length = 30)
I_vec = vector("numeric", length = 30)
for(i in 1:30){ #Looper 30 ganger, som i oppgaveteksten

x = vector("numeric", length = N + 1) #Vår stokastiske prosess
x[1] = 0 #Vi starter som sårbar 

for (n in 1:N){ #Simulerer Markovkjeda
x[n + 1] = sample.int(3, 1, replace = TRUE, prob = P[x[n]+1,])-1
}
plot(0:N, x, type = "o", lwd = 2, cex.axis = 1.5, main = "Realization", #Plotter simuleringa
     xlab = "Time", ylab = "Value", cex.lab = 1.5, cex.main = 1.5)


S = 0 #Initialverdier
I = 0
R = 0
for (n in (10 * 365):(20 * 365)) { 
  if (x[n] == 0) {
    S = S + 1
  }
  if (x[n] == 1) {
    I = I + 1
  }
  if (x[n] == 2) {
    R = R + 1
  }
}

S_vec[i] = S #appender i-te entry den nyeste verdien av S,I eller R.
I_vec[i] = I
R_vec[i] = R
}

resultS <- t.test(S_vec) #Konstruerer konfidensintervall
print(resultS$conf.int)

resultI <- t.test(I_vec)
print(resultI$conf.int)

resultR <- t.test(R_vec)
print(resultR$conf.int)


#Sammenlikner de numeriske resultatene med de analytiske
S/(365*10)
10/31

I
I/(3650)
1/31

R
R/3650
20/31