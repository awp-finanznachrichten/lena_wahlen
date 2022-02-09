#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/lena_wahlen/Bern_2022")

#Bibliotheken, Funktionen und vorhandene Daten laden
source("config.R")
source("functions_storyfinder.R")
source("functions_storybuilder.R")

#Wahlkreise
wahlkreise <- c("Jura bernois","Biel-Bienne - Seeland","Oberaargau","Emmental","Mittelland-Nord",
                "Bern","Mittelland-Süd","Thun","Oberland")

for (w in 1:length(wahlkreise)) {
  
wahlkreis <- wahlkreise[w]
#Sind Daten schon da?

#Listendaten filtern
liste_wahlkreis <- Listen_und_Parteien %>%
  filter(Wahlkreis == wahlkreise[w])

#Neue Daten von CSV scrapen (Tabelle als Alternative?)
link <- paste0("https://www.bewas.sites.be.ch/2018/2018-03-25/WAHL_GROSSRAT/csvResultatWahlkreis-",LETTERS[w],".csv")
new_data <- read.csv(link,sep =";",skip = 4)
new_data$No.liste <- as.numeric(new_data$No.liste)
new_data <- new_data[1:nrow(liste_wahlkreis),]

new_data <- new_data %>%
  select("No.liste","Sièges") %>%
  rename("Liste_Nummer" = "No.liste",
         "Sitze" = "Sièges")


#Daten zusammenführen
liste_wahlkreis <- left_join(liste_wahlkreis,new_data)
liste_wahlkreis$Sitze <- as.numeric(liste_wahlkreis$Sitze)

#Sitze aufsummieren nach Partei
anzahl_sitze_partei <- liste_wahlkreis %>%
  filter(Partei != "Diverse",
         Partei != "Aufrecht Schweiz") %>%
  group_by(Partei) %>%
  summarise(Sitze=sum(Sitze)
  )


#Diverse Listen Sitze
diverse_sitze <- liste_wahlkreis %>%
  filter(Partei == "Diverse",
         Sitze > 0)

#Aufrecht Schweiz Sitze
aufrecht_sitze <- liste_wahlkreis %>%
  filter(Partei == "Aufrecht Schweiz",
         Sitze > 0)

#Zusammenführen mit historischen Daten und Vergleich
sitze_wahlkreis_historisch <- Sitzverteilung_Historisch %>%
  filter(Wahlkreis == wahlkreise[w]) %>%
  gather()

anzahl_sitze_partei <- left_join(anzahl_sitze_partei,
                                 sitze_wahlkreis_historisch,
                                 by = c("Partei"="key")) %>%
  rename("Sitze_alt" = "value")

anzahl_sitze_partei$Sitze_alt <- as.numeric(anzahl_sitze_partei$Sitze_alt)
anzahl_sitze_partei$change <- anzahl_sitze_partei$Sitze-anzahl_sitze_partei$Sitze_alt
anzahl_sitze_partei <- na.omit(anzahl_sitze_partei)
anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$change),]
print(anzahl_sitze_partei)

#Neu gewählte und abgewählte Kandidaten scrapen
neu_gewaehlt_text <- ""
abgewaehlt_text <- ""

link <- paste0("https://www.bewas.sites.be.ch/2018/2018-03-25/WAHL_GROSSRAT/reportResultatWahlkreisRanglisteCsv-",LETTERS[w],".csv")
candidates_data <- read.csv(link,sep =";",skip = 2)
candidates_data$Liste.liste <- as.numeric(gsub(" .*","",candidates_data$Liste.liste))
candidates_data <- left_join(candidates_data,liste_wahlkreis,by=c(Liste.liste = "Liste_Nummer"))

candidates_neu_gewaehlt <- candidates_data %>%
  filter(Gew..elu.e == "*",
         Bish..Sort. == "")

candidates_abgewaehlt <- candidates_data %>%
  filter(Gew..elu.e == "",
         Bish..Sort. == "x")

###Storyfinder

#Gewinner und Verlierer  
winners <- get_winner(anzahl_sitze_partei)
losers <- get_losers(anzahl_sitze_partei)
nochange <- special_check_nochange(anzahl_sitze_partei)

#Sitzverteilung
sitzverteilung <- get_sitzverteilung(anzahl_sitze_partei)

#Sitzverteilung Diverse
sitzverteilung_diverse <- get_sitzverteilung_diverse(diverse_sitze)

#Sitzverteilung Aufrecht
sitzverteilung_aufrecht <- get_sitzverteilung_aufrecht(aufrecht_sitze)

#Neu Gewählt
neu_gewaehlt <- get_neu_gewaehlt(candidates_neu_gewaehlt)

#Abgewaehlt
abgewaehlt <- get_abgewaehlt(candidates_abgewaehlt)

storyblocks <- paste0(winners,losers,nochange,
                sitzverteilung,sitzverteilung_diverse,sitzverteilung_aufrecht,
                neu_gewaehlt, abgewaehlt)
print(storyblocks)

###Storybuilder

#Textbausteine holen
text <- get_textbausteine_de(storyblocks,Textbausteine)

#Kompliziertere Variablen erstellen
ListeGewinner <- get_liste_gewinner(anzahl_sitze_partei)
ListeVerlierer <- get_liste_verlierer(anzahl_sitze_partei)
ListeDiversemitSitze <- get_liste_diverse(diverse_sitze)

ListeSitzverteilung <- get_liste_sitzverteilung(anzahl_sitze_partei)
ListeParteienOut <- get_liste_parteienout(anzahl_sitze_partei)
ListeNeugewaehlt <- get_liste_neugewaehlt(candidates_neu_gewaehlt)
ListeAbgewaehlt <- get_liste_abgewaehlt(candidates_abgewaehlt)


#Variablen ersetzen
text <- replace_varables_de(text,wahlkreis,anzahl_sitze_partei,diverse_sitze,aufrecht_sitze,
                            ListeGewinner,ListeVerlierer,
                            ListeSitzverteilung,ListeDiversemitSitze,
                            ListeNeugewaehlt,ListeAbgewaehlt)


#Letzte Textanpassungen

if (anzahl_sitze_partei$Partei[1] == "Grüne") {
text <- gsub("grosse Gewinnerin. Sie holt ","grossen Gewinner. Sie holen ",text)
}  

if (anzahl_sitze_partei$Partei[nrow(anzahl_sitze_partei)] == "Grüne") {
text <- gsub("grosse Verliererin. Sie büsst ","grossen Verlierer. Sie büssen ",text)
}  

text <- gsub("Die Grüne ist ","Die Grünen sind ",text)
text <- gsub("hält die Grüne ","halten die Grünen ",text)
text <- gsub("hält die Grüne ","halten die Grünen ",text)
text <- gsub("die Grüne verliert ","die Grünen verlieren ",text)
text <- gsub("die Grüne hält ","die Grünen halten ",text)
text <- gsub("Die Grüne ","Die Grünen ",text)
text <- gsub("die Grüne ","die Grünen ",text)

text <- gsub("\n\ndie","\n\nDie",text)
text <- gsub(" 1 "," einen ",text)
text <- gsub(" 2 "," zwei ",text)
text <- gsub(" 3 "," drei ",text)
text <- gsub(" 4 "," vier ",text)
text <- gsub(" 5 "," fünf ",text)
text <- gsub(" 6 "," sechs ",text)
text <- gsub(" 7 "," sieben ",text)
text <- gsub(" 8 "," acht ",text)
text <- gsub(" 9 "," neun ",text)
text <- gsub(" 10 "," zehn ",text)
text <- gsub(" 11 "," elf ",text)
text <- gsub(" 12 "," zwölf ",text)

cat(text)
}

#Daten vorbereiten für Datawrapper

#Auf Github hochladen

#Datawrapper-Grafik aktualisieren