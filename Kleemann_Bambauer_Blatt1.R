# Aufgabe 3

byte = function() {
  # Konvertiere Zahl zwischen 0 und 255 in ein Byte
  as.integer(intToBits(sample(0:255,1))[1:8])
}

# Wiederhole 10^4 mal die Summe des Byte
exp=replicate(10000,sum(byte()))

# Erstelle Histogramm
hist(replicate(10000,sum(byte())),freq=T,main='Binomialverteilung',
     xlab='Wert',ylab='Relative Häufigkeit',breaks=0:9-0.5)

#Aufgabe 4

countbiskopf = function() {
  # p=0.6 bedeutet: Die Chance, Kopf zu werfen, beträgt 3/5
  m=c('Kopf','Zahl','Zahl','Kopf','Kopf')
  i='Zahl'
  count=0
  # Zähle die Würfe, bis Kopf kommt
  while (i=='Zahl') {
    i = sample(m,1)
    count=count+1
  }
  count-1
}

verteilung = function(wdh) {
  # Definiere Vektor der Länge 10
  a=c(0,0,0,0,0,0,0,0,0,0)
  # Für die Anzahl der Wiederholungen
  for (i in 1:wdh) {
    # Bestimme Wert
    k=countbiskopf()
    # Speichere den Wert im Vektor
    a[k] = a[k]+1
  }
  a
}

plot(verteilung(10000)/10000,main='Geometrische Verteilung',type='l'
     ,xlab='Anzahl Würfe bis Kopf',ylab='Häufigkeit')

