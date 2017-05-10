# Definiere Wahrscheinlichkeitsvektor mit Elementen -1 und 1
v = c(-1,1)

# Definiere Wahrscheinlichkeitsfunktion wurf
wurf <- function (p) {
  # Generiere aus v mit W'keit 1-p eine -1 und mit W'keit p eine 1
  sample(v,1,prob=c(1-p,p))
}


# Definiere die versch. Strategien mit den Eingaben:
# k0 = Startkapital, k1 = Endkapital und p
strategie1 <- function (k0,k,p) {
  # Solange Endkapital nicht erreicht oder Kapital verbraucht
  while (0<k0 && k0<k) {
    # Ich setze 1
    # und bekomme mit Wahrscheinlichkeit p das gesetzte Kapital als Gewinn
    k0 = k0 + wurf(p) * 1
  }
  # gib 1 aus, falls k erreicht wurde, sonst 0
  if (k0 >= k) 1
  else 0
}

strategie2 <- function (k0,k,p) {
  while (0<k0 && k0<k) {
    # Ich setze alles (also k0)
    # und bekomme mit Wahrscheinlichkeit p das gesetzte Kapital als Gewinn
    k0 = k0 + wurf(p)*k0
  }
  # gib 1 aus, falls k erreicht wurde, sonst 0
  if (k0 >= k) 1
  else 0
}

strategie3 <- function (k0,k,p) {
  while (0<k0 && k0<k) {
    # Ich setze die aufgerundete Hälfte meines aktuellen Kapitals
    # und bekomme mit Wahrscheinlichkeit p das gesetzte Kapital als Gewinn
    k0 = k0 + wurf(p)*ceiling(k0/2)
  }
  # gib 1 aus, falls k erreicht wurde, sonst 0
  if (k0 >= k) 1
  else 0
}

strategie4 <- function (k0,k,p) {
  while (0<k0 && k0<k) {
    # Ich setze das aufgerundete Formelergebnis meines aktuellen Kapitals
    # und bekomme mit Wahrscheinlichkeit p das gesetzte Kapital als Gewinn
    k0 = k0 + wurf(p)*ceiling((k-k0)/k*k0)
  }
  # gib 1 aus, falls k erreicht wurde, sonst 0
  if (k0 >= k) 1
  else 0
}

# Definiere Variablen für die zu testenden Werte
k0=50
k1=100
k2=200
p1=18/37
p2=0.5
p3=0.75

# Hinweis: Die Berechnung für die Ausgabe dauert etwa 1 Minute.
# Mit cat funktionieren Zeilenumbrüche besser ;-)
# Führe Funktionen jeweils 1000 mal aus
# Summiere die Ergebnisse und teile durch 1000
# -> normierte Werte
cat('Startkapital: 50
Zielkapital: 100

Ergebnis:
p = 18 / 37
Strategie 1: ',sum(replicate(1000,strategie1(k0,k1,p1)))/1000,'
Strategie 2: ',sum(replicate(1000,strategie2(k0,k1,p1)))/1000,'
Strategie 3: ',sum(replicate(1000,strategie3(k0,k1,p1)))/1000,'
Strategie 4: ',sum(replicate(1000,strategie4(k0,k1,p1)))/1000,'

p = 1/ 2
Strategie 1: ',sum(replicate(1000,strategie1(k0,k1,p2)))/1000,'
Strategie 2: ',sum(replicate(1000,strategie2(k0,k1,p2)))/1000,'
Strategie 3: ',sum(replicate(1000,strategie3(k0,k1,p2)))/1000,'
Strategie 4: ',sum(replicate(1000,strategie4(k0,k1,p2)))/1000,'

p = 3 / 4
Strategie 1: ',sum(replicate(1000,strategie1(k0,k1,p3)))/1000,'
Strategie 2: ',sum(replicate(1000,strategie2(k0,k1,p3)))/1000,'
Strategie 3: ',sum(replicate(1000,strategie3(k0,k1,p3)))/1000,'
Strategie 4: ',sum(replicate(1000,strategie4(k0,k1,p3)))/1000,'

------------------------------

Startkapital: 50
Zielkapital: 200

Ergebnis:
p = 18 / 37
Strategie 1: ',sum(replicate(1000,strategie1(k0,k2,p1)))/1000,'
Strategie 2: ',sum(replicate(1000,strategie2(k0,k2,p1)))/1000,'
Strategie 3: ',sum(replicate(1000,strategie3(k0,k2,p1)))/1000,'
Strategie 4: ',sum(replicate(1000,strategie4(k0,k2,p1)))/1000,'

p = 1 / 2
Strategie 1: ',sum(replicate(1000,strategie1(k0,k2,p2)))/1000,'
Strategie 2: ',sum(replicate(1000,strategie2(k0,k2,p2)))/1000,'
Strategie 3: ',sum(replicate(1000,strategie3(k0,k2,p2)))/1000,'
Strategie 4: ',sum(replicate(1000,strategie4(k0,k2,p2)))/1000,'

p = 3 / 4
Strategie 1: ',sum(replicate(1000,strategie1(k0,k2,p3)))/1000,'
Strategie 2: ',sum(replicate(1000,strategie2(k0,k2,p3)))/1000,'
Strategie 3: ',sum(replicate(1000,strategie3(k0,k2,p3)))/1000,'
Strategie 4: ',sum(replicate(1000,strategie4(k0,k2,p3)))/1000,'

',sep="")

