#Alle notwendigen Packages####
install.packages("testthat")
install.packages("ggplot2")
install.packages("plotly")
install.packages("gridExtra")
install.packages("readr")
install.packages("dplyr")
library(testthat)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(readr)

# disclaimer: wenn man eine datei einliest die mit anführungszeichen ist, 
# funktioniert es leider nicht

# A3.1 --------------------------------------------------------------------

# (1 Punkt) An Weihnachten wird h¨aufig gewichtelt. Das Prinzip ist Ihnen bestimmt vertraut:
#   Alle bringen ein Geschenk mit, diese werden durchnummeriert, dann zieht jede Person eine Zahl
# und wird so ganz zuf¨allig beschenkt. Das macht besonders viel Spaß, wenn man das eigens
# mitgebrachte Geschenk nicht selbst wieder zugewiesen bekommt. Aber wie wahrscheinlich ist
# eigentlich ein solcher Ungl¨ucksfall? Bitte bestimmen Sie durch Simulation die Wahrscheinlichkeit,
# dass unter n = 10 Personen mindestens eine Person das eigene Geschenk zur¨uckerh¨alt.

# Berechnung: 1 Person unter 10 bekommt eigenes Wichtelgeschenk
# Rueckgabe: Prozentangabe Wahrscheinlichkeit (ungefaehr 63%)
#
# Initialisiere: 
anzahl_personen <- 10 # Anzahl der Personen - Integer
iterationen <- 1000 # Anzahl der Iterationen - Integer
zaehler <- 0 # Initialisiere Zähler für Person-zieht-eigenes-Geschenk-Fälle - Integer

for (i in 1:iterationen) { # For-Schleife für Iterationen - hier 1000
  # Zufällige Zuordnung von Geschenken zu Personen
  zuordnung <- sample(1:anzahl_personen, size = anzahl_personen, replace = FALSE)
  # beim ziehen wird nicht zurückgelegt - replace=FALSE
  
  # Überprüfen, ob mindestens eine Person ihr eigenes Geschenk gezogen hat
  if (any(zuordnung == 1:anzahl_personen)) {
    zaehler <- zaehler + 1
  }
}

p <- zaehler / iterationen # Wahrscheinlichkeit berechnen
prozent <- p * 100 # Wahrscheinlichkeit in Prozent umrechnen

# Ausgabe anzeigen
cat("Die Wahrscheinlichkeit, dass mindestens eine Person ihr eigenes Geschenk zieht, beträgt:", prozent, "%\n")

# 3.2 & 3.3---------------------------------------------------------------

# 3.2
# (2 Punkte) Wir wollen uns jetzt von dem Spezialfall n = 10 lösen. Bitte schreiben Sie Ihren R
# Code aus Aufgabe 3.1 in eine Funktion namens wichtel unglueck um. Dabei soll der Funktionsaufruf wichtel 
# unglueck(n, k, iterationen = 1000) die Wahrscheinlichkeit ausgeben,
# mit der mindestens k unter n Personen ihr eigenes Geschenk zurückerhalten. Mit dem Argument
# iterationen soll eine positive ganze Zahl ¨ubergeben werden, mit der die Anzahl der Iterationen
# für die Simulation einstellbar ist. Dieser Wert ist hier standardmäßig auf 1000 gesetzt.
#
# 3.3
# (2 Punkte) Damit Sie Ihren Code auch noch n¨achstes Jahr Weihnachten verstehen und einsetzen
# k¨onnen, ist es wichtig, dass er gut dokumentiert ist. Erg¨anzen Sie Kommentare in Ihrem Code,
# die insbesondere die Eingaben und die Ausgabe unmissverst¨andlich beschreiben.

# Funktion zur Berechnung des Wichtel Ungluecks von n Personen die mitmachen 
# und k Personen die ihr eigenes Geschenk zurueckbekommen
#
# Argumente:
# n (numeric): Eine Zahl zwischen 0 und 1000, Anzahl Teilnehmer
# k (numeric): Eine Zahl <= n, Anzahl Teilnehmer die ihr eigenes Geschenk ziehen
# iterationen (numeric): Eine Zahl zwischen 1 und 1000, Anzahl wie oft wir unser
# Experiment wiederholen
#
#
wichtel_unglueck <- function(n = NULL, k=NULL, iterationen=1000) {
  # Initialisiere Zähler für Geschenk-wird-selbst-gezogen-Fälle
  zaehler <- 0
  
  #Exceptions/Sonderfälle
  
  # n wird nicht übergeben
  if(is.null(n)) {
    stop("Das Argument für n wurde nicht übergeben, Wichtel_unglueck wird nun mit n=100 ausgeführt.")
  }
  
  # k wird nicht übergeben
  if(is.null(k)) {
    stop("Das Argument für n wurde nicht übergeben, Wichtel_unglueck wird nun mit k=10 ausgeführt.")
  }
  # falls n = k = 1 übergeben wird
  if(k==n& n==1) {
    return(1)
  }
  # falls k<0 übergeben wird
  if(k<0) {
    stop("Für k wurde weniger übergeben, bitte übergeben Sie mindestens k=1.")
  }
  # falls n = 0 übergeben wird
  if(n<=0) {
    stop("Für n wurde 0 oder weniger übergeben, bitte übergeben Sie mindestens n=1.")
  }
  # falls iterationen = 0 übergeben wird
  if(iterationen <= 0) {
    stop("Für das Argument iterationen wurde 0 oder weniger übergeben")
  }
  
  # falls k, n oder iterationen Buchstaben übergeben werden
  if(is.character(n)) {
    stop("Der Funktion wurde für das Argument n mindestens 1 Buchstabe übergeben, bitte geben Sie ausschließlich Zahlen ein.")
  }
  
  if(is.character(k)) {
    stop("Der Funktion wurde für das Argument k mindestens 1 Buchstabe übergeben, bitte geben Sie ausschließlich Zahlen ein.")
  }
  
  if(is.character(iterationen)) {
    stop("Der Funktion wurde für das Argument iterationen mindestens 1 Buchstabe übergeben, bitte geben Sie ausschließlich Zahlen ein.")
  }
  # falls k>n übergeben wird
  if(k>n) {
    stop("k darf nicht größer als n sein.")
  }
  
  # For-Schleife für die angegebene Anzahl von Iterationen
  for (i in 1:iterationen) {
    # Zufällige Zuordnung von Geschenken zu Personen
    zuordnung <- sample(1:n, size = n, replace = FALSE)
    
    # Überprüfen, ob genau k oder mehr als k Personen ihr eigenes Geschenk gezogen haben
    if (sum(zuordnung == 1:n) >= k) {
      zaehler <- zaehler + 1
    }
  }
  p <- zaehler / iterationen   # Wahrscheinlichkeit ausrechnen
  
  prozent <- p * 100 # Wahrscheinlichkeit in Prozent umrechnen
  
  # Ausgabe anzeigen
  cat("Die Wahrscheinlichkeit, dass mindestens", k, "Person(en) unter", n, "Person(en) ihr eigenes Geschenk ziehen, beträgt:", prozent, "%\n")
  
  return(p) # Rückgabe der Wahrscheinlichkeit
}

# Beispielaufrufe 

wichtel_unglueck(n=5, k=1)
wichtel_unglueck(n=200, k=1)
wichtel_unglueck(n=500, k=1)
wichtel_unglueck(n=100, k=2)

wichtel_unglueck(n=10, k = 1, iterationen = 1000)
wichtel_unglueck(n=1,k=1)
wichtel_unglueck(n="zahl", k=1)
wichtel_unglueck(n=123, k="t")
wichtel_unglueck(n=200, k=20)

# 3.4 ####
# A3.4 4 Testfälle
# Testfall für n=10, k=1, die Funktion wichtel_unglueck wird wie beabsichtigt genutzt und gibt eine W'keit von etwa 62% an
test_that("Die Funktion funktioniert so wie sie sollte.", {
  expect_no_error(wichtel_unglueck(n=10, k=1))
})
# Testfall für k=3, n=2, dies ist per Defintion verboten, wir suchen k unter n Personen die ihr eigenes Geschenk erhalten
test_that("k darf nicht größer als n sein, dies testen wir hier durch k=3, n=2", {
  expect_error(wichtel_unglueck(n=2, k=3), "k darf nicht größer als n sein.")
})
# Testfall für Buchstabeneingabe bei k
test_that("Die Funktion gibt einen Fehler wieder, da wir für k ein Zeichen angeben.", {
  expect_error(wichtel_unglueck(n=2, k="t", iterationen=150),
               "Der Funktion wurde für das Argument k mindestens 1 Buchstabe übergeben, bitte geben Sie ausschließlich Zahlen ein.")
})
# Achtung, 3.2 bzw. wichtel_unglueck wurde umgeschrieben, strings sind grundsätzlich größer als zahlen,deshalb testen wir erst auf strings und dann auf k>n

test_that("Die Funktion wichtel_unglueck bekommt keinen Wert für n übergeben", {
  expect_error(wichtel_unglueck(k=5), "Das Argument für n wurde nicht übergeben, Wichtel_unglueck wird nun mit n=100 ausgeführt.")
})


## Testfälle die wir zusätzlich geschrieben haben
# Testfall für n = k = 1 
test_that("Die Funktion berechne_wahrscheinlichkeit gibt 1 zurück, wenn k = 1 und n = 1", {
  # Überprüfe, ob das Ergebnis gleich 1 ist
  expect_equal(wichtel_unglueck(n=1, k=1, iterationen = 1000), 1)
})

# Testfall für Buchstabeneingabe bei n
test_that("Die Funktion gibt einen Fehler wieder, da wir für n ein Zeichen angeben.", {
  expect_error(wichtel_unglueck(n="t",k=2), "Der Funktion wurde für das Argument n mindestens 1 Buchstabe übergeben, bitte geben Sie ausschließlich Zahlen ein.")
})

# Auch möglich: testen auf negative werte für n,k,iterationen
# quasi alles auf das wir k testen können wir auch auf n testen und umgekehrt
# A3.5 --------------------------------------------------------------------
### Import der Daten -- kompletter Datensatz
#Originaler Datensatz, in Excel nur die Anführungszeichen entfernt


Capital_bikeshare_data_2022_with_NAs <- read.csv(file = "C:/Users/TGerner/Desktop/Capital_bikeshare_data_ohneAnfuherungszeichen.csv",
                                                 header = TRUE,
                                                 sep = ",",
                                                 dec = ".")
View(Capital_bikeshare_data_2022_with_NAs)

#Kontrolle ob Data.frame
class(Capital_bikeshare_data_2022_with_NAs) ## ist Data.Frame

#Entfernen NA´s des ganzen Datensatzes
anyNA(Capital_bikeshare_data_2022_with_NAs) # testen ob unsere Daten NAs enthalten sind - Ergebnis: TRUE
Bikeshare_ohne_NA <- na.omit(Capital_bikeshare_data_2022_with_NAs) # NAs entfernen durch überspringen der NA Zeilen und Übergabe an neue Datei
anyNA(Bikeshare_ohne_NA) # testen ob immer noch NAs enthalten sind - Ergebnis: FALSE
View(Bikeshare_ohne_NA) # anschauen des neuen Datensatzes ohne NAs 

#Daten auf Plausiblität der Werte überprüfen
# Station wird nicht auf Datenanomalien geprüft, da dies nicht sinnvoll erscheint
range(Bikeshare_ohne_NA$wind_speed) # negative Windgeschwindigkeiten gefunden -> Datenanomalie - oder ist das dann Rückenwind :)
range(Bikeshare_ohne_NA$count) # keine weiteren Datenanomalien gefunden für alle anderen Spalten
range(Bikeshare_ohne_NA$date)
range(Bikeshare_ohne_NA$precipitation)
range(Bikeshare_ohne_NA$mean_temperature)
range(Bikeshare_ohne_NA$max_temperature)
range(Bikeshare_ohne_NA$min_temperature)
range(Bikeshare_ohne_NA$snowfall)
range(Bikeshare_ohne_NA$snow_depth)

#Unplausible Daten entfernen
Bikeshare_ohne_NA<- Bikeshare_ohne_NA[Bikeshare_ohne_NA$wind_speed >= 0, ] # negative Windgeschwindkeiten werden entfernt
range(Bikeshare_ohne_NA$wind_speed) # Kontrolle Ergebnis: hat geklappt

# hier kommt stationsfilter für unsere Gruppe: 81 - 18th St & Wyoming Ave NW
daten_81 <- Bikeshare_ohne_NA %>% 
  filter(station == "18th St & Wyoming Ave NW")
range(daten_81$station)

View(daten_81) # Überprüfung des Datensatzes

#Konvertiere das Datum in ein Date-Objekt
daten_81$date <- as.Date(daten_81$date)

View(daten_81) # Dies ist der fertige Datensatz, den wir nun nutzen um Grafiken zu erstellen


#4.1 Erstellen Grafiken Zusammenhang zwischen der Anzahl ausgeliehener Fahrräder und...####
# 4.1 (2 Punkte) Stellen Sie den Zusammenhang zwischen der Anzahl ausgeliehener Fahrr¨ader und:
#   • der Temperatur;
# • der Niederschlagsmenge;
# • der Windgeschwindigkeit;
# • der Zeit
# grafisch dar, die in der Vorlesung besprochenen Grunds¨atze der Datenvisualisierung befolgend
# (diese sind auch in den nachfolgenden Aufgaben zu beachten). W¨ahlen Sie eine geeignete Darstellungsform, 
# wobei das R-Paket ggplot2 zu verwenden ist. (Hinweis: es sind 4 Grafiken zu erstellen.)

#Grafik count + temperature als Scatterplot - (Zusammenhang Anzahl ausgeliehener Fahrräder und Temperatur)
Grafik_CT <- ggplot(data = daten_81) +
  geom_point(aes(y = daten_81$count, x = daten_81$mean_temperature))+
  ylab("Anzahl Fahrräder") +
  xlab("Durchschnittstemperatur (in F)") +
  ggtitle("ausgeliehene Fahrräder abhängig von der Temperatur")+
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14)) # Änderung der Schriftgröße - X-Achsenüberschrift   

#Grafik Count + Precipitation als Scatterplot - (Zusammenhang Anzahl ausgeliehener Fahrräder und Niederschlag)
Grafik_CP <- ggplot(data = daten_81) +
  geom_point(aes(y = daten_81$count, x = daten_81$precipitation))+
  ylab("Anzahl Fahrräder") +
  xlab("Niederschlag") +
  ggtitle("ausgeliehene Fahrräder abhängig vom Niederschlag")+
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14)) # Änderung der Schriftgröße - X-Achsenüberschrift
##Niederschlag Ausreißer Text schreiben? - Ja, steht drin, ist noch im gültigen Bereich

#Grafik Count + wind_speed als Scatterplot - (Zusammenhang Anzahl ausgeliehener Fahrräder und Windgeschwindigkeit)
Grafik_CW <- ggplot(data = daten_81) +
  geom_point(aes(y = daten_81$count, x = daten_81$wind_speed))+
  ylab("Anzahl Fahrräder") +
  xlab("Windgeschwindigkeit (in mph)") +
  ggtitle("ausgeliehene Fahrräder abhängig von der Windgeschwindigkeit")+
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14)) # Änderung der Schriftgröße - X-Achsenüberschrift   


#Grafik Count + Date als Scatterplot - (Zusammenhang Anzahl ausgeliehener Fahrräder und Datum)
Grafik_CD <- ggplot(data = daten_81) +
  geom_point(aes( x = daten_81$date, y = daten_81$count, ))+
  ylab("Anzahl Fahrräder") +
  xlab("Monat") +
  ggtitle("ausgeliehene Fahrräder im Jahresverlauf")+
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14)) # Änderung der Schriftgröße - X-Achsenüberschrift    

#Zusammenfügen aller vier Grafiken in eine Übersicht
grid.arrange(Grafik_CT, Grafik_CP, Grafik_CW,
             Grafik_CD,
             nrow = 2, ncol = 2)

#4.2 Fahrradverleih Regen ja/nein ####

# 4.2 (2 Punkte) Stellen Sie den Zusammenhang zwischen der Anzahl ausgeliehener Fahrr¨ader und der
# Temperatur dar, und zwar getrennt f¨ur:
#   • Tage, an denen es geregnet hat;
# • Tage, an denen es nicht geregnet hat.
# (Hinweis: es sind 2 Grafiken zu erstellen.)

#Erstellen Grafik für Regentage + Anzahl ausgeliehener Fahrräder
Grafik_Regen_ja <- ggplot(data = filter(daten_81, precipitation > 0)) +
  geom_point(aes(x = count, y = mean_temperature)) +
  xlab("Anzahl ausgeliehener Fahrräder") +
  ylab("durchschnittliche Temperatur (in F)")+
  ggtitle("Regentag") +
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14))+ # Änderung der Schriftgröße - X-Achsenüberschrift 
  coord_cartesian(xlim = range(daten_81$count))

#Erstellen Grafik für Tage ohne Regen + Anzahl ausgeliehener Fahrräder
Grafik_Regen_nein <- ggplot(data = filter(daten_81, precipitation == 0)) +
  geom_point(aes(x = count, y = mean_temperature)) +
  xlab("Anzahl ausgeliehener Fahrräder") +
  ylab("durchschnittliche Temperatur (in F)")+
  ggtitle("Kein Regentag") +
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14))+ # Änderung der Schriftgröße - X-Achsenüberschrift 
  coord_cartesian(xlim = range(daten_81$count))

#Grafiken zusammenführen
grid.arrange(Grafik_Regen_ja, Grafik_Regen_nein,
             nrow = 1, ncol = 2)

#4.3 Verteilung darstellen- Anzahl Fahrräder,Temperatur, Windgeschwindigkeit, Niederschlag, 4 Grafiken####

# 4.3 (2 Punkte) Stellen Sie die Verteilung
# • der Anzahl ausgeliehener Fahrr¨ader;
# • der Temperatur;
# • der Niederschlagsmenge;
# • der Windgeschwindigkeit
# grafisch dar. W¨ahlen Sie eine geeignete Darstellungsform, wobei das R-Paket ggplot2 zu verwenden ist. 
# (Hinweis: es sind 4 Grafiken zu erstellen.)

#Erstellen der Grafiken OBACHT Y ACHSE NICHT IM GLEICHEN MAßSTAB
Grafik_1 <- ggplot(data = daten_81)+
  geom_density(aes(x = count))+ 
  xlab("Anzahl der ausgeliehenen Fahrräder") +
  ylab("Verteilung")+
  ggtitle("Verteilungsfunktion ausgeliehener Fahrräder")+
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14)) # Änderung der Schriftgröße - X-Achsenüberschrift 

Grafik_2 <- ggplot(data = daten_81)+
  geom_density(aes(x = mean_temperature))+
  xlab("durchschnittliche Temperatur (in F)")+
  ylab("Verteilung")+
  ggtitle("Verteilungsfunktion durchschnittlicher Temperatur")+
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14)) # Änderung der Schriftgröße - X-Achsenüberschrift 

Grafik_3 <- ggplot(data = daten_81)+
  geom_density(aes(x = wind_speed))+
  xlab("Windgeschwindigkeit (in mph)")+
  ylab("Verteilung")+
  ggtitle("Verteilungsfunktion Windgeschwindigkeit")+
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14)) # Änderung der Schriftgröße - X-Achsenüberschrift 

Grafik_4 <- ggplot(data = daten_81)+
  geom_density(aes(x = precipitation))+
  xlab("Niederschlag")+
  ylab("Verteilung")+
  ggtitle("Verteilungsfunktion Niederschlagsmenge")+
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14)) # Änderung der Schriftgröße - X-Achsenüberschrift 

# Grafiken Zusammenfügen
grid.arrange(Grafik_1, Grafik_2, Grafik_3, Grafik_4,
             nrow = 2, ncol =2) 


#4.4 Gleiche Grafiken, aber Kerndichteschätzer, getrennt Frühling, Sommer, Herbst, Winter, eine Grafik ####

# 4.4 (2 Punkte) Erweitern Sie Ihre Grafiken aus 4.3, indem Sie die Verteilung der Anzahl ausgeliehener
# Fahrr¨ader getrennt nach Jahreszeit, also f¨ur:
#   • den Fr¨uhling;
# • den Sommer;
# • den Herbst;
# • den Winter,
# in einer Grafik darstellen. Nutzen Sie daf¨ur sich ¨uberlagernde Kerndichtesch¨atzer und verschiedene, 
# transparente Farben. (Hinweis: es ist eine Grafik zu erstellen).

## Quelle Jahreszeitenwechsel https://www.linker.ch/eigenlink/jahreszeiten_beginn.htm
# Frühlingsanfang: 20.03.22
# Sommeranfang:    21.06.22
# Herbstanfang:    23.09.22
# Winteranfang:    21.12.22

# Aufteilen des Datensatzes nach Jahreszeiten 
Winter_21_22<- subset(daten_81, date < "2022-03-20")
Fruehling_22 <- subset(daten_81, date >= "2022-03-20" & date < "2022-06-21")
Sommer_22 <- subset(daten_81, date > "2022-06-21" & date < "2022-09-23")
Herbst_22 <- subset(daten_81, date > "2022-09-23" & date < "2022-12-21")

# Kontrolle
View(Winter_21)

# Erstellen der Grafik
Grafik_4.4 <- ggplot() +
  geom_density(data = Winter_21_22, aes(x = count, col = "Winter 2021/2022"))+
  geom_density(data = Fruehling_22, aes(x = count, col = "Frühling 2022"))+
  geom_density(data = Sommer_22, aes (x = count, col = "Sommer 2022"))+
  geom_density(data = Herbst_22, aes(x = count, col = "Herbst 2022"))+
  labs(color = "Legende")+
  xlab("Anzahl ausgeliehener Fahrräder")+ 
  ylab("Verteilung")+
  ggtitle("Verteilung nach Jahreszeiten")+
  theme(plot.title = element_text(size = 16, face = "bold"),  # Änderung der Schriftgröße - Überschrift + Fettdruck
        axis.text.x = element_text(size = 10),   # Änderung der Schriftgröße - X-Achsenbeschriftung
        axis.text.y = element_text(size = 10),   # Änderung der Schriftgröße - Y-Achsenbeschriftung
        axis.title.y.left  = element_text(size=14),  # Änderung der Schriftgröße - Y-Achsenüberschrift
        axis.title.x.bottom = element_text(size=14))+ # Änderung der Schriftgröße - X-Achsenüberschrift 
  theme(legend.text=element_text(size=10))+                                                         # Ändert Schriftgöße der Jahreszeiten
  scale_color_manual(values = c("orange", "darkgreen", "blue","purple"))+                         # Ändert Farbe der Funktionen
  guides(color = guide_legend(override.aes = list(fill = c("orange","darkgreen","blue", "purple")))) # Füllt die Kästen der Legende

print(Grafik_4.4)





#4.5 Grafik mit Plotly#### 

# 4.5 (2 Punkte) Erstellen Sie mithilfe des R-Pakets plotly einen 3D-Scatterplot. Auf der x-Achse
# soll die Temperatur, auf der y-Achse die Windgeschwindigkeit und auf der z-Achse die Anzahl
# ausgeliehener Fahrr¨ader abgebildet sein. Nutzen Sie f¨ur die Punkte des 3D-Scatterplots eine
# geeignete Farbskala, wobei die Farbe von der Anzahl ausgeliehener Fahrr¨ader abh¨angen soll.
# (Hinweis: es ist eine Grafik zu erstellen.)

## 3D Scatterplot erstellen mit dem Package "plotly"
Grafik_4.5 <- Meine_Grafik <- plot_ly(data = daten_81, x = ~mean_temperature, y = ~wind_speed, z = ~count, type = "scatter3d", 
                                      mode = "markers", marker = list(col = "green", size = 5, opacity = 0.5), color = ~count,
                                      text = ~paste("durchschn. Temperatur:", mean_temperature, ## Informationen hinzufügen, die beim Darüberfahren mit der Maus angezeigt werden
                                                    "<br> Windgeschw.:", wind_speed,
                                                    "<br> Anzahl:", count),
                                      hoverinfo = "text")

## Achsenbeschriftungen ändern, Überschrift hinzufügen, 

Grafik_4.5 <- Grafik_4.5 %>% layout(scene = list(xaxis = list(title = "durchschnittliche Temperatur (in F)"), 
                                                 yaxis = list(title = "Windgeschwindigkeit (in mph)"), 
                                                 zaxis = list(title = "Anzahl ausgeliehener Fahrräder")), 
                                    title = list(text="3D Scatterplot", font=list(weight="bold")))

print(Grafik_4.5)

