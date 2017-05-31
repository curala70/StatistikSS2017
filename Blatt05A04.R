# a)
box_muller <- function(n) {
    # pick a U and a V, uniform, n-times each
    Us = runif(n, min=0, max=1)
    Vs = runif(n, min=0, max=1)
    Zs = list()
    # for each independent pair of U and V, compute X and Y 
    # and store as 'tupel' in Zs
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
# vor every vector returned by box_muller, store it's X value in Xs, Y in Ys
# warning: slow!
for (vec in box_muller(10^5)) {
    Xs[i] = vec[1]
    Ys[i] = vec[2]
    i = i+1
}
# compute histograms and show in a 2x2 pattern
par(mfrow=c(2,2))
hist(Xs)
hist(Ys)
hist(Xs[Ys<0])
hist(Xs[Ys>=0])