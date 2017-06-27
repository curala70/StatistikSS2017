
# a)
KSS <- function(x) {
    x = sort(x)
    n = length(x)
    d = c()
    for (i in 1:n-1) {
        Fx = (1/n)*sum(x<=x[i])
        d[i] = max(abs(Fx - pnorm(x[i])), abs(Fx - pnorm(x[i+1])))
    }
    d[n] = abs(1-pnorm(x[n]))
    return (max(d))
}

# b)

# create samples of size n, each 100 times
x10 = list()
for (i in 1:100) {
    x10[[i]] = rnorm(10)
}
x40 = c()
for (i in 1:100) {
    x40[[i]] = rnorm(40)
}
x160 = c()
for (i in 1:100) {
    x160[[i]] = rnorm(160)
}
x640 = c()
for (i in 1:100) {
    x640[[i]] = rnorm(640)
}
# compute KSS for each sample of size n=10 and store in d10
d10 = c()
for (i in 1:100) {
    d10[i] = KSS(x10[[i]])
}
# compute KSS for each sample of size n=40 and store in d40
d40 = c()
for (i in 1:100) {
    d40[i] = KSS(x40[[i]])
}
# compute KSS for each sample of size n=160 and store in d160
d160 = c()
for (i in 1:100) {
    d160[i] = KSS(x160[[i]])
}
# compute KSS for each sample of size n=640 and store in d640
d640 = c()
for (i in 1:100) {
    d640[i] = KSS(x640[[i]])
}
# create headline - hier d10 oder x10? Mittelwert wovon?
MW10=round(mean(d10)*sqrt(10),digits=2)
MW40=round(mean(d40)*sqrt(40),digits=2)
MW160=round(mean(d160)*sqrt(160),digits=2)
MW640=round(mean(d640)*sqrt(640),digits=2)
maintext=paste("n10:",MW10,";n40:",MW40,";n160:",MW160,";n640:",MW640)
# plot samples
plot(1:100, d10, col='black', type='p',main=maintext,xlab="Index",ylab="KSS",ylim=c(0,1))
points(1:100, d40, col='red')
points(1:100, d160, col='green')
points(1:100, d640, col='blue')
# create legend
legend("topright", c("n=10","n=40","n=160", "n=640"),
       lty=c(1,1), lwd=c(2,2), col=c("black","red","green","blue"))