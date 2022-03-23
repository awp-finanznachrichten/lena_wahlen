#Cercle électoral

#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/lena_wahlen/Bern_2022")

#Bibliotheken, Funktionen und vorhandene Daten laden
source("config.R", encoding = "UTF-8")
source("functions_storyfinder.R", encoding = "UTF-8")
source("functions_storybuilder.R", encoding = "UTF-8")
source("functions_text_adaptation.R", encoding = "UTF-8")

#Wahlkreise
wahlkreise <- c("Jura bernois","Biel-Bienne - Seeland","Oberaargau","Emmental","Mittelland-Nord",
                "Bern","Mittelland-Süd","Thun","Oberland")

wahlkreise_fr <- c("Jura bernois","Bienne-Seeland","Haute-Argovie","Emmental","Mittelland septentrional",
                "Berne","Mittelland méridional","Thoune","Oberland")

####Dataframe für alle Daten
data_gesamt <- data.frame("Wahlkreis","Wahlkreis_fr","Storyboard","Text_de","Text_fr","Sitze_all")
colnames(data_gesamt) <- c("Wahlkreis","Wahlkreis_fr","Storyboard","Text_de","Text_fr","Sitze_all")


for (w in 1:length(wahlkreise)) {
wahlkreis <- wahlkreise[w]
wahlkreis_fr <- wahlkreise_fr[w]

#Sind Daten schon da?
link <- paste0("https://www.bewas.sites.be.ch/2018/2018-03-25/WAHL_GROSSRAT/csvResultatWahlkreis-",LETTERS[w],".csv")
check_csv1 <- tryCatch( {
  read.csv(link,sep =";",skip = 4) 
  }, error= function(e) {
    print(e)
  }    
)

link <- paste0("https://www.bewas.sites.be.ch/2018/2018-03-25/WAHL_GROSSRAT/reportResultatWahlkreisRanglisteCsv-",LETTERS[w],".csv")
check_csv2 <- tryCatch( {
  read.csv(link,sep =";",skip = 4) 
}, error= function(e) {
  print(e)
}    
)

fail_check1 <- grepl("Verbindung nicht",check_csv1[1])
fail_check2 <- grepl("Verbindung nicht",check_csv2[1])

if (fail_check1 == TRUE || fail_check2 == TRUE) {
storyboard <- NA
text <- paste0("Der Wahlkreis ist noch nicht ausgezählt")
text_fr <- paste0("Le cercle électoral n'a pas encore été comptée")

new_entry <- data.frame(wahlkreis,wahlkreis_fr,storyboard,text,text_fr,Sitzverteilung_Historisch$Total[w])
colnames(new_entry) <- c("Wahlkreis","Wahlkreis_fr","Storyboard","Text_de","Text_fr","Sitze_all")
data_gesamt <- rbind(data_gesamt,new_entry)

cat(text)
cat(text_fr)

} else {  

#Listendaten filtern
liste_wahlkreis <- Listen_und_Parteien %>%
  filter(Wahlkreis == wahlkreise[w])

###Neue Daten von CSV scrapen (Tabelle als Alternative?)
link <- paste0("https://www.bewas.sites.be.ch/2018/2018-03-25/WAHL_GROSSRAT/csvResultatWahlkreis-",LETTERS[w],".csv")
new_data <- read.csv(link,sep =";",skip = 4)
new_data$No.liste <- as.numeric(new_data$No.liste)
new_data <- new_data[1:nrow(liste_wahlkreis),]

new_data <- new_data %>%
  select("No.liste","Sièges") %>%
  rename("Liste_Nummer" = "No.liste",
         "Sitze" = "Sièges")

###Plan B: Get new data from Excel
  new_data <- liste_wahlkreis %>%
    select(Liste_Nummer,Sitze)

#Daten zusammenführen
liste_wahlkreis <- left_join(liste_wahlkreis,new_data)
liste_wahlkreis$Sitze <- as.numeric(liste_wahlkreis$Sitze)


#Neu gewählte und abgewählte Kandidaten scrapen
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



#Sitze aufsummieren nach Partei
anzahl_sitze_partei <- liste_wahlkreis %>%
  filter(Fraktion_de != "Diverse",
         Fraktion_de != "Aufrecht Schweiz") %>%
  group_by(Fraktion_de,Fraktion_fr) %>%
  summarise(Sitze=sum(Sitze)
  )


#Diverse Listen Sitze
diverse_sitze <- liste_wahlkreis %>%
  filter(Fraktion_de == "Diverse",
         Sitze > 0)

#Aufrecht Schweiz Sitze
aufrecht_sitze <- liste_wahlkreis %>%
  filter(Fraktion_de == "Aufrecht Schweiz",
         Sitze > 0)

#Zusammenführen mit historischen Daten und Vergleich
sitze_wahlkreis_historisch <- Sitzverteilung_Historisch %>%
  filter(Wahlkreis == wahlkreise[w]) %>%
  gather()

anzahl_sitze_partei <- left_join(anzahl_sitze_partei,
                                 sitze_wahlkreis_historisch,
                                 by = c("Fraktion_de"="key")) %>%
  rename("Sitze_alt" = "value")


anzahl_sitze_partei$Sitze_alt <- as.numeric(anzahl_sitze_partei$Sitze_alt)
anzahl_sitze_partei$change <- anzahl_sitze_partei$Sitze-anzahl_sitze_partei$Sitze_alt
anzahl_sitze_partei <- na.omit(anzahl_sitze_partei)
anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$change),]
print(anzahl_sitze_partei)


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

storyboard <- paste0(winners,losers,nochange,
                sitzverteilung,sitzverteilung_diverse,sitzverteilung_aufrecht,
                neu_gewaehlt, abgewaehlt)

###Storybuilder

#Textbausteine holen
text <- get_textbausteine_de(storyboard,Textbausteine)
text_fr <- get_textbausteine_fr(storyboard,Textbausteine)

#Kompliziertere Variablen erstellen
ListeGewinner <- get_liste_gewinner(anzahl_sitze_partei)
ListeVerlierer <- get_liste_verlierer(anzahl_sitze_partei)
ListeDiversemitSitze <- get_liste_diverse(diverse_sitze)

ListeSitzverteilung <- get_liste_sitzverteilung(anzahl_sitze_partei)
ListeParteienOut <- get_liste_parteienout(anzahl_sitze_partei)
ListeNeugewaehlt <- get_liste_neugewaehlt(candidates_neu_gewaehlt)
ListeAbgewaehlt <- get_liste_abgewaehlt(candidates_abgewaehlt)

ListeGewinner_fr <- get_liste_gewinner_fr(anzahl_sitze_partei)
ListeVerlierer_fr <- get_liste_verlierer_fr(anzahl_sitze_partei)
ListeDiversemitSitze_fr <- get_liste_diverse_fr(diverse_sitze)

ListeSitzverteilung_fr <- get_liste_sitzverteilung_fr(anzahl_sitze_partei)
ListeParteienOut_fr <- get_liste_parteienout_fr(anzahl_sitze_partei)
ListeNeugewaehlt_fr <- get_liste_neugewaehlt_fr(candidates_neu_gewaehlt)
ListeAbgewaehlt_fr <- get_liste_abgewaehlt_fr(candidates_abgewaehlt)



#Variablen ersetzen
text <- replace_varables_de(text,wahlkreis,anzahl_sitze_partei,diverse_sitze,aufrecht_sitze,
                            ListeGewinner,ListeVerlierer,
                            ListeSitzverteilung,ListeDiversemitSitze,
                            ListeNeugewaehlt,ListeAbgewaehlt)

text_fr <- replace_varables_fr(text_fr,wahlkreis_fr,anzahl_sitze_partei,diverse_sitze,aufrecht_sitze,
                            ListeGewinner_fr,ListeVerlierer_fr,
                            ListeSitzverteilung_fr,ListeDiversemitSitze_fr,
                            ListeNeugewaehlt_fr,ListeAbgewaehlt_fr)

#Letzte Textanpassungen
text <- green_cleanup(text,anzahl_sitze_partei)
text <- text_optimisation(text)

text_fr <- green_cleanup_fr(text_fr,anzahl_sitze_partei)
text_fr <- text_optimisation_fr(text_fr)

#Daten einfügen
new_entry <- data.frame(wahlkreis,wahlkreis_fr,storyboard,text,text_fr,Sitzverteilung_Historisch$Total[w])
colnames(new_entry) <- c("Wahlkreis","Wahlkreis_fr","Storyboard","Text_de","Text_fr","Sitze_all")
data_gesamt <- rbind(data_gesamt,new_entry)

cat(text)
cat(text_fr)

}
}


#Daten vorbereiten für Datawrapper
data_gesamt <- data_gesamt[-1,]

data_datawrapper <- data_gesamt
data_datawrapper$Wahlkreis[1] <- "Berner Jura"
data_datawrapper$Wahlkreis[2] <- "Biel-Seeland"

#Texte für Wahlkreise
data_datawrapper$text_wahlkreis_de <- paste0("Wahlkreis ",data_datawrapper$Wahlkreis,
                                           " (",data_datawrapper$Sitze_all," Sitze)")

data_datawrapper$text_wahlkreis_fr <- paste0("Cercle électoral de ",data_datawrapper$Wahlkreis_fr,
                                           " (",data_datawrapper$Sitze_all," sièges)")


#Farbe definieren
data_datawrapper$Color <- 0
for (r in 1:nrow(data_datawrapper) ) {
  if (is.na(data_datawrapper$Storyboard[r]) == FALSE) {
data_datawrapper$Color[r] <- r
}
}
  
#Anpassungen Wahlkreis
  
write.csv(data_datawrapper,"Output/Uebersicht_dw_new.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

#Auf Github hochladen
#git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
token <- read.csv("C:/Users/simon/OneDrive/Github_Token/token.txt",header=FALSE)[1,1]
git2r::cred_token(token)
gitadd()
gitcommit()
gitpush()

#Datawrapper-Grafik aktualisieren
datawrapper_auth("BMcG33cGBCp2FpqF1BSN5lHhKrw2W8Ait4AYbDEjkjVgCiWe07iqoX5pwHXdW36g", overwrite = TRUE)
dw_edit_chart("Gypmx",intro=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("Gypmx")

dw_edit_chart("mlkcf",intro=paste0("Etat: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
dw_publish_chart("mlkcf")


#Texte speichern
#library(xlsx)
#write.xlsx(data_gesamt,"LENA_Wahlen_Bern_Texte.xlsx",row.names = FALSE)
