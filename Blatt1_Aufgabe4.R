countbiskopf = function() {
  # p=0.6 bedeutet: Die Chance, Kopf zu werfen, beträgt 3/5
  m=c('Kopf','Zahl','Zahl','Kopf','Kopf')
  i=0
  count=0
  # Zähle die Würfe, bis Kopf kommt
  while (i==0) {
    if (sample(m,1)=='Zahl') count=count+1
    if (sample(m,1)=='Kopf') i=1
  }
  count
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

plot(verteilung(10000),main='Geometrische Verteilung',type='l'
     ,xlab='Anzahl Würfe bis Kopf',ylab='Häufigkeit')


