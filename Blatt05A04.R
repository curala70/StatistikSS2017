# a)
box_muller <- function(n) {
    Us = runif(n, min=0, max=1)
    Vs = runif(n, min=0, max=1)
    Zs = list()
    for (i in 1:n) {
        X = sqrt((-2)*log(Us[i]))*cos(2*pi*Vs[i])
        Y = sqrt((-2)*log(Us[i]))*sin(2*pi*Vs[i])
        Zs[[i]] = c(X,Y)
    }
    return (Zs)
}

# b)
Xs = c()
Ys = c()
i=1
for (vec in box_muller(10^3)) {
    Xs[i] = vec[1]
    Ys[i] = vec[2]
    i = i+1
}

par(mfrow=c(2,2))
hist(Xs)
hist(Ys)
hist(Xs[Ys<0])
hist(Xs[Ys>=0])