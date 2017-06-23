# 3 a)
mk <- function(s0,m1,m2,p1,p2,n) {
  x = rbinom(n,m1,p1)
  y = rbinom(n,m2,p2)
  s = c()
  s[1] = s0
  for (i in 1:n) {
    s[i+1] = max(s[i] + x[i] - y[i],0)
  }
  s
}

# 3 b)
s1000 = replicate(1000,mk(25,5,6,0.6,0.6,1000))
s100 = replicate(1000,mk(25,5,6,0.6,0.6,100))
s10 = replicate(1000,mk(25,5,6,0.6,0.6,10))
s5 = replicate(1000,mk(25,5,6,0.6,0.6,5))
par(mfrow=c(2,2))
m1000 = mean(s1000)
m100 = mean(s100)
m10 = mean(s10)
m5 = mean(s5)
hist(s1000, main=paste("Mittelwert: ",m1000))
hist(s100, main=paste("Mittelwert: ",m100))
hist(s10, main=paste("Mittelwert: ",m10))
hist(s5, main=paste("Mittelwert: ",m5))

# 3 c)
mw <- function(x) {
    v <- c()
    for(k in 1:length(x)) {
        sum <- 0
        for(i in 1:k) {
            sum <- sum + x[i]/k
        }
        v[k] <- sum
    }
    v
}

# 3 d)
par(mfrow=c(1,1))
pfad = mk(25,5,6,0.6,0.6,1500)
v = mw(pfad)
plot(v, col='blue', type="s", lwd=2)
lines(pfad, col='red')
abline(h=m1000, col='green', lwd=2)
legend("topright", c("Anfragen/Sek","Anfragen","MW für S1000"),
       lty=c(1,1), lwd=c(2,2), col=c("red","blue","green"))
