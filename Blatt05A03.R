# a)

lot <- function(lot) {
    alpha = runif(1, min=(-pi/2), max=(pi/2))
    x = lot * tan(alpha)
    return (x)
}

# b)
results = c()
for (i in 1:10^3) {
    results[i]=lot(0.5)
}
results = results[abs(results)<=10]
hist(results,
     breaks=200,
     main="Aufgabe 2",
     xlab="values of x",
     ylab="relative frequency",
     xlim=c(-10,10),
     freq=FALSE)

# d)
M = sum(dcauchy(-10:10))
lines(-10:10, (1/M)*dcauchy(-10:10))

