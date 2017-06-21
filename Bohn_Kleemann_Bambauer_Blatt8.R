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
s1000 = replicate(1000,mk(25,5,6,0.6,0.6,1000))
s100 = replicate(1000,mk(25,5,6,0.6,0.6,100))
s10 = replicate(1000,mk(25,5,6,0.6,0.6,10))
s5 = replicate(1000,mk(25,5,6,0.6,0.6,5))
par(mfrow=c(2,2))
m1000 = sum(s1000) / 1000
m100 = sum(s100) / 1000
m10 = sum(s10) / 1000
m5 = sum(s5) / 1000
hist(s1000, main=paste("Mittelwert: ",m1000))
hist(s100, main=paste("Mittelwert: ",m100))
hist(s10, main=paste("Mittelwert: ",m10))
hist(s5, main=paste("Mittelwert: ",m5))


mw <- function(x) {
  v = c()
  sumx = 0
  for (i in 1:length(x)) {
    sumx = sumx + x[i]
    v[i] = sumx
  }
  v
}

par(mfrow=c(1,1))
pfad = mk(25,5,6,0.6,0.6,1500)
v = mw(pfad)
plot(v, col='blue', type="s")
lines(pfad, col='red')
abline(h=m1000, col='green')

