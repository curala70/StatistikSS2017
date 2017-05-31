

Alphabet = c("a", "b")

experiment = function(alphabet, zielwort, N) {
  wort = c()
  for (i in 1:N) {
    wort[i] = sample(alphabet,1)
  }
  wort = paste(wort, collapse="")
  
  return(wort==zielwort)
}

N = 4
zielwort = "abba"
counter = 0
for (i in 1:10000) {
  if(experiment(Alphabet, zielwort, N)==TRUE) {
    counter = counter+1
  }
}
print (counter/10000)

