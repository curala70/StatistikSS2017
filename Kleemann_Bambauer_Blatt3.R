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
S=replicate(10000,server(60000,225))
# Erstelle Histogramm
hist(S,main='Verteilung der Anzahl überlasteter Server
     (von 225 bei 60000 Usern)',
     xlab='Überlastete Server',ylab='Häufigkeit',breaks=min(S):(max(S)+1)-0.5)

#c)
sim=character()
# Für Serveranzahl von 220 bis 250...
for (k in 220:250) {
  # Simuliere 20 mal mit 60000 Usern
  z = replicate(1000,server(60000,k))
  # Zähle, wie oft es überlastete Server gab, teile durch 1000
  # und speichere Ergebnis in Vektor sim
  sim[k-219] <- sum(length(z[z>0]))/1000
}
# x-Werte definieren
x=c(220:250)
plot(x,sim,main='Überlastete Server bei 60000 Usern
     in Abhängigkeit zur Gesamtanzahl Server',
     ylab='W\'keit für überl. Server',
     xlab='Gesamtanzahl Server')

