# a)
# change lot(l) to take number of simulations as well - takes too long otherwise
# Jan approved!
lot <- function(lot, n) {
    alpha = runif(n, min=(-pi/2), max=(pi/2))
    xs = lot * tan(alpha)
    return (xs)
}

# b)
# simulate 10^6 times with a lot of length 0.5
results=lot(0.5, 10^6)
# cut off all results with absolute value >10
results = results[abs(results)<=10]
# plot histogram
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

