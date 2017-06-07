
# a)
quantil_exp <- function(t,l) {
    return ((-1/l)*log(1-t)) # taken from Bsp 1.32
}

# b)
empVF <- function(x,t) {
    y = x[x<=t]
    return (length(y)/length(x))
}

# c) broken as shit
t = runif(1000, min=0, max=1)
q = quantil_exp(t, (1/2))
f = empVF(q, t)
x = c(0:10)
plot(x, f, col="red",exp(-(1/2)*x), col="blue")