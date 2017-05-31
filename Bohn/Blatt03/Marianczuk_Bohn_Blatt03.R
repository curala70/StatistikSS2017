# a)
server <- function(u, k) {
    # create list with k entries of (1/k)
    probs = rep((1/k), each=k)
    # compute multinomial distribution 
    users = rmultinom(n=1, size=u, prob=probs)
    count = 0
    # count amount of overloaded servers
    for (user in users) {
        if (user > 300) {
            count = count+1
        }
    }
    return (count)
}

# b)
Servers = c()
# compute 10^4 times with 60000 users and 225 servers
for (i in 1:10^4) {
    Servers[i] = server(60000, 225)
}

# create histogram 
hist(Servers,
     breaks=min(Servers):(max(Servers)+1)-0.5,
     main="Multinomial distribution",
     xlab="Amount of overloaded servers",
     ylab="Frequency")

# c)
sums = c()
# for amount of servers ranging from 220 to 250...
for (k in 220:250) {
    count = 0
    # ... compute 10^3 times for 60000 users
    for (i in 1:10^3) {
        # count each occurence where at least 1 server is overloaded
        if (server(60000, k) > 0) {
            count = count+1
        }
    }
    # transfer count to probability
    sums[k-219] = count/1000
}
print(length(sums))

# plot coherence of probability of overloading servers and amount of users/servers
plot(x=220:250, y=sums, xlab="Amount of servers", ylab="Approximate probability")