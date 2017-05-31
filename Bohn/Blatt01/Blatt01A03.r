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