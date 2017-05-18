# a)
server <- function (u,k) {
  # Vektor mit Wahrscheinlichkeiten für k -> gleichverteilt mit Länge k
  w = rep(1/k,k)
  # Erzeuge zweidim. Array v[k,u] mit Verteilung von u usern auf k Server
  v = rmultinom(1,u,w)
  # Def. leeren Ergebnisvektor
  erg = character()
  # Schreibe Anzahl der User von Server i in erg[i]
  for (i in 1:k) {
    erg[i] <- v[i,1]
  }
  # Gib die Anzahl Server aus, auf denen mehr als 300 User sind
  length(erg[erg>300])
}

# b) Simuliere 10000 mal
NumOfSim=10000
#Anzahl Server
NumServer = 225
#Anzahl User
NumUser= 60000

S=replicate(NumOfSim,server(NumUser, NumServer))
# Erstelle Histogramm
h = hist(S,main='Verteilung der Anzahl überlasteter Server
     (von 225 bei 60000 Usern)',
     xlab='Überlastete Server',ylab='Häufigkeit',breaks=min(S):(max(S)+1)-0.5)


plot(h,freq=FALSE, main = "Relative Häufigkeit überlasteter Server\n (von 225 bei 60000 Usern)", 
     xlab='Überlastete Server',ylab='rel. Häufigkeit')




#c)
sim=character()


# x-Werte für Serveranzahl definieren
# Hier für Serveranzahl von 220 bis 250...
NumServer=c(220:250)
#Anzahl der User, hier 6000
NumUser = 60000
#Anzahl der Widerholungen
reps = 1000

for (k in NumServer) {
  # Simuliere reps mal mit NumUser Usern und verschiedenen Anzahlen an Servern.
  z = replicate(reps,server(NumUser,k))
  # Zähle, wie oft es überlastete Server gab, teile durch 1000
  # und speichere Ergebnis im Vektor sim
  sim[k-min(NumServer)+1] <- sum(length(z[z>0]))/reps
}

plot(NumServer,sim,main='Überlastete Server bei 60000 Usern
     in Abhängigkeit zur Gesamtanzahl Server',
     ylab='W\'keit für überl. Server',
     xlab='Gesamtanzahl Server')

