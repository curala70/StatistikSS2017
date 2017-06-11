beta <- function(n,m,x) {
  # Einfaches Anwenden der Formel...
  b = factorial(n-1) * factorial(m-1) /
      factorial(m+n-1)
  erg = (1/b) * x^(n-1) * (1-x)^(m-1)
  erg
}

rejection <- function(n,m,C) {
  # Vektor der Länge 100 reicht aus...
  x = runif(1:100)
  u = runif(1:100)
  # Berechne Vektor xk.
  xk = c()
  for (i in 1:length(x)) {
    xk[i] = beta(n,m,x[i]) / C
  }
  # Berechne das Minimum der k's...
  i=1
  while (i) {
    if (u[i]<=xk[i]) {
      K=i
      i=0
    }
    else i=i+1
  }
  # ... und gib es aus.
  K
}

# Beide Plots in einem Fenster...
par(mfrow=c(2,1))

# 1000 Simulationen für C=3
ks = replicate(1000,rejection(4,7,3))
km = sum(ks)/1000
hist(ks,freq=F,main=paste('Mittelwert von K für C=3: ',km))

# 1000 Simulationen für C=5
ks = replicate(1000,rejection(4,7,5))
km = sum(ks)/1000
hist(ks,freq=F,main=paste('Mittelwert von K für C=5: ',km))

