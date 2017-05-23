# a) Eingabe: N Atome, Parameter p für Exponentialverteilung,
#             K Zeitintervalle, t = Länge der Zeitintervalle in Sekunden
zerfall <- function (N,p,K,t) {
  # Betrache exponentialverteilte Atome (mit Parameter p)
  z = rexp(N,p)
  v = c()
  # Für jedes Zeitintervall k
  for (k in (1:K)) {
    # Bestimme v[k] über die Anzahl der in k zerfallenen Atome
    v[k] = length(z[z>(k-1)*t & z<=k*t])
  }
  v
}

# b)
v = zerfall(2500000,3.8/7.5/2500000,2500,7.5)

hist(v,main="Zerfallene Atome pro Zeitintervall",
     ylab="Anzahl Zeitintervalle",xlab="Anzahl zerfallene Atome",
     breaks=0:(max(v)+1)-0.5,freq=F)
# Poissonverteilung - leider um 1 verrutscht, stand aber so in der Aufgabe...
points(dpois(0:max(v),3.8))

# c)
M = sum(v)
M
lambda = sum(v)/2500
lambda

# d)
c=c(57,203,383,525,532,408,273,139,45,27)
# Hier muss ich noch einmal durch lambda teilen, da sonst die Graphen nicht übereinander liegen
d=dpois(0:9,lambda)*M/lambda
plot(0:9,c,col="red",main="Vergleich unserer Verteilung mit
der realen Verteilung aus dem Jahre 1910",
    xlab="Anzahl zerfallene Atome",ylab="Anzahl Zeitintervalle")
points(0:9,d,col="blue")
legend("right", "1910", pch=1, title="Verteilung",col="red")
legend("topright", "Verteilung", pch=1, title="Opt. Poisson",col="blue")

