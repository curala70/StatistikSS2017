## Aufgabe 3

values <- c()

for (i in 1:10000) {
  num <- sample(0:255, size=1)
  bits <- as.integer(intToBits(num)[1:8])
  sum <- Reduce('+',bits)
  values[i] <- sum
}

hist(values,
     breaks=0:9-0.5,
     freq=FALSE,
     main="Binomialverteilung",
     xlab="Wert",
     ylab="relative Häufigkeit")


## Aufgabe 4

muenze = function(p) {
  Muenze = c("Kopf", "Zahl")
  count = 0
  if (p == 0) {
    return (-1)
  }
  probs = c(p, 1.0-p)
  while(sample(Muenze, 1, replace = FALSE, prob = probs)=="Zahl") {
    count = count+1
  }
  return (count)
}

# compute 10^4 times and store results
results = c()
for (i in 1:10^4) {
  results[i] = muenze(0.6)
}

# get amount of occurences
distr = c()
for (i in 0:10) {
  distr[i+1] = sum(results==i)
}

# plot geometric allocation
plot(0:10, distr, 
     xlab="n: Anzahl Wuerfe bis Kopf faellt",
     ylab="h(n): relative Haeufigkeit",
     main="Geometrische Verteilung")