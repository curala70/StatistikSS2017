
# a)
quantil_exp <- function(t,l) {
    return ((-1/l)*log(1-t)) # taken from example 1.32
}

# b)
empVF <- function(x,t) {
    y = x[x<=t]
    return (length(y)/length(x))
}

# c)
# create 1000 uniformly distributed ts
t = runif(1000, min=0, max=1)
# use ts to compute 1000 exponentially distributed qs
q = quantil_exp(t, (1/2))
f = c()
# amounnt of values to be computed
n = 100
# create vector of n vaöues between 0 and 10
x = seq(0,10,length.out = n)
# for each of those x-values, compute the corresponding f(x)-value
for (i in 0:n) {
    f[i] = empVF(q, x[i])
}
# plot computed distribution in red, theoretical distribution in blue
plot(x, f, col="red", type = "l", 
     main = "Exponentialfunktion", 
     xlab = "x-Werte",
     ylab = "y-Werte")
# add line of theoretical distribution to plot
lines(x, exp(-(1/2)*x), col="blue", type = "l")
# add legend to plot
legend("right",
       c("Theoretische Verteilungsfunktion", "Empirische Verteilungsfunktion"),
       lty=c(1,1), lwd=c(2.5,2.5), col=c("blue","red"))