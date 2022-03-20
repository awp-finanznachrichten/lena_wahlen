#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/lena_wahlen/Vaud_2022")

#Bibliotheken, Funktionen und vorhandene Daten laden
source("config.R", encoding = "UTF-8")
source("functions_storyfinder.R", encoding = "UTF-8")
source("functions_storybuilder.R", encoding = "UTF-8")
source("functions_text_adaptation.R", encoding = "UTF-8")

Gewaehlte_Vaud_2017$Wahlkreis[12] <-	"Pays-d'Enhaut"
Gewaehlte_Vaud_2017$Wahlkreis[13] <-	"Vevey"

#Wahlkreise
wahlkreise <- c("Aigle","Broye-Vully","Gros-de-Vaud","La Vallée","Yverdon",
                "Lausanne-Ville","Romanel","Lavaux-Oron","Morges","Nyon","Ouest lausannois",
                "Pays-d'Enhaut","Vevey")

codes_wahlkreise <- c("A2","A3","A4","A11","A12","A13","A14","A5","A6","A7","A8","A9","A10")

####Dataframe für alle Daten
data_gesamt <- data.frame("Wahlkreis","Storyboard","Text_de","Text_fr","Sitze_all")
colnames(data_gesamt) <- c("Wahlkreis","Storyboard","Text_de","Text_fr","Sitze_all")

#Dataframe bisherige
Bisherige <- data.frame("Wahlkreis","Gewaehlt")
colnames(Bisherige) <- c("Wahlkreis","Gewaehlt")

for (w in 1:length(wahlkreise)) {
wahlkreis <- wahlkreise[w]

#Sind Daten schon da?
url <- paste0("https://www.elections.vd.ch/votelec/app16/html/VDGC20220320-",codes_wahlkreise[w],"/Resultat/resultatsGenerauxResultatElection.html")
webpage <- read_html(url)
data_table <- html_text(html_nodes(webpage,"td"))

fail_check <- length(data_table) == 0

#fail_check <- c(FALSE,FALSE,FALSE,FALSE,FALSE,
#                  FALSE,FALSE,FALSE,FALSE,FALSE,
#                  FALSE,FALSE,FALSE)

if (fail_check == TRUE) { #fail_check[w]
  storyboard <- NA
  text <- paste0("Der Wahlkreis ist noch nicht ausgezählt")
  text_fr <- paste0("L'arrondissement n'a pas encore été comptée")
  
  sitze_wahlkreis_historisch <- Sitzverteilung_Historisch %>%
    filter(Wahlkreis == wahlkreise[w]) %>%
    gather()
  
  new_entry <- data.frame(wahlkreis,storyboard,text,text_fr,sitze_wahlkreis_historisch$value[nrow(sitze_wahlkreis_historisch)])
  colnames(new_entry) <- c("Wahlkreis","Storyboard","Text_de","Text_fr","Sitze_all")
  data_gesamt <- rbind(data_gesamt,new_entry)
  
  cat(text)
  cat(text_fr)
  
} else {  
  
  #Listendaten filtern
  liste_wahlkreis <- Listen_und_Parteien %>%
    filter(Wahlkreis == wahlkreise[w])

  ###Neue Daten von Website scrapen 
  url <- paste0("https://www.elections.vd.ch/votelec/app16/html/VDGC20220320-",codes_wahlkreise[w],"/Resultat/resultatsGenerauxResultatElection.html")
  webpage <- read_html(url)
  data_table <- html_text(html_nodes(webpage,"td"))

  #Create new Dataframe
  data_wahlkreis <- data.frame("Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze")
  colnames(data_wahlkreis) <- c("Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze")
  list_number <- 1
  
  for (i in seq(1,which(grepl("Total",data_table))-1,4)) {
    
    new_data <- data.frame(list_number,data_table[i],data_table[i+1],data_table[i+3])
    colnames(new_data) <- c("Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze")
    
    data_wahlkreis <- rbind(data_wahlkreis,new_data)
    list_number <- list_number+1
  }


  data_wahlkreis <- data_wahlkreis[-1,]
  data_wahlkreis$Sitze <- as.numeric(data_wahlkreis$Sitze)
  data_wahlkreis$Liste_Nummer <- as.numeric(data_wahlkreis$Liste_Nummer)

  new_data <- data_wahlkreis %>%
    select("Liste_Nummer","Sitze") 

###Plan B: Get new data from Excel
#  new_data <- liste_wahlkreis %>%
#    select(Liste_Nummer,Sitze)

  #Daten zusammenführen
  liste_wahlkreis <- left_join(liste_wahlkreis,new_data)
  liste_wahlkreis$Sitze <- as.numeric(liste_wahlkreis$Sitze)

###Neu Gewählte Personen
  neu_gewaehlt <- ""
  abgewaehlt <- ""
  ListeNeugewaehlt <- ""
  bisherige <- Gewaehlte_Vaud_2017[Gewaehlte_Vaud_2017$Wahlkreis == wahlkreis,]
  Liste_bisherige <- gsub("; ","|",bisherige$Gewaehlt)
  count_neu_gewaehlt <- 0

  for (i in seq(which(grepl("Total",data_table))+3,length(data_table),3) ) {
  if (grepl(data_table[i],Liste_bisherige) == FALSE) {
    ListeNeugewaehlt <- paste0(ListeNeugewaehlt,data_table[i]," (",data_table[i+1],"), ")
    count_neu_gewaehlt <- count_neu_gewaehlt + 1
  }
  }

  if (count_neu_gewaehlt > 1) {
    ListeNeugewaehlt <- substr(ListeNeugewaehlt,1,nchar(ListeNeugewaehlt)-2)
    ListeNeugewaehlt <- stri_replace_last(ListeNeugewaehlt,fixed=","," et")
    neu_gewaehlt <- "Neu_gewaehlt_mehrere;"
    } else if (count_neu_gewaehlt == 1) {
      ListeNeugewaehlt <- substr(ListeNeugewaehlt,1,nchar(ListeNeugewaehlt)-2)
      neu_gewaehlt <- "Neu_gewaehlt_1Person;"
    } else {
  neu_gewaehlt <- "Neu_gewaehlt_keine;"
  }  
  
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
  ListeAbgewaehlt <- ""
  
  ListeGewinner_fr <- get_liste_gewinner_fr(anzahl_sitze_partei)
  ListeVerlierer_fr <- get_liste_verlierer_fr(anzahl_sitze_partei)
  ListeDiversemitSitze_fr <- get_liste_diverse_fr(diverse_sitze)
  
  ListeSitzverteilung_fr <- get_liste_sitzverteilung_fr(anzahl_sitze_partei)
  ListeParteienOut_fr <- get_liste_parteienout_fr(anzahl_sitze_partei)
  ListeNeugewaehlt_fr <- ListeNeugewaehlt
  ListeAbgewaehlt_fr <- ""
  
  ListeNeugewaehlt <- gsub(" et "," und ",ListeNeugewaehlt)



  #Variablen ersetzen
  text <- replace_varables_de(text,wahlkreis,anzahl_sitze_partei,diverse_sitze,aufrecht_sitze,
                              ListeGewinner,ListeVerlierer,
                              ListeSitzverteilung,ListeDiversemitSitze,
                              ListeNeugewaehlt,ListeAbgewaehlt)
  
  text_fr <- replace_varables_fr(text_fr,wahlkreis,anzahl_sitze_partei,diverse_sitze,aufrecht_sitze,
                                 ListeGewinner_fr,ListeVerlierer_fr,
                                 ListeSitzverteilung_fr,ListeDiversemitSitze_fr,
                                 ListeNeugewaehlt_fr,ListeAbgewaehlt_fr)

  #Letzte Textanpassungen
  text <- green_cleanup(text,anzahl_sitze_partei)
  text <- text_optimisation(text)
  
  text_fr <- green_cleanup_fr(text_fr,anzahl_sitze_partei)
  text_fr <- text_optimisation_fr(text_fr)
  
  #Daten einfügen
  new_entry <- data.frame(wahlkreis,storyboard,text,text_fr,sitze_wahlkreis_historisch$value[nrow(sitze_wahlkreis_historisch)])
  colnames(new_entry) <- c("Wahlkreis","Storyboard","Text_de","Text_fr","Sitze_all")
  data_gesamt <- rbind(data_gesamt,new_entry)
  
  cat(text)
  cat(text_fr)
  
}
}

#Daten vorbereiten für Datawrapper
data_gesamt <- data_gesamt[-1,]

data_datawrapper <- data_gesamt
data_datawrapper$Wahlkreis_fr <- data_gesamt$Wahlkreis

#Suous-arrondissement mergen
new_entry <- data.frame("Jura-Nord vaudois",data_datawrapper$Storyboard[4],
                        paste0("<b>Sous-arrondissement de la Vallée (2 Sitze)</b><br>",data_datawrapper$Text_de[4],"<br><br>",
                               "<b>Sous-arrondissement d'Yverdon (15 Sitze)</b><br>",data_datawrapper$Text_de[5]),
                        paste0("<b>Sous-arrondissement de la Vallée (2 sièges)</b><br>",data_datawrapper$Text_fr[4],"<br><br>",
                               "<b>Sous-arrondissement d'Yverdon (15 sièges)</b><br>",data_datawrapper$Text_fr[5]),
                        as.numeric(data_datawrapper$Sitze_all[4])+as.numeric(data_datawrapper$Sitze_all[5]),
                        "Jura-Nord vaudois"
                        )
colnames(new_entry) <- c("Wahlkreis","Storyboard","Text_de","Text_fr","Sitze_all","Wahlkreis_fr")
data_datawrapper <- rbind(data_datawrapper,new_entry)

new_entry <- data.frame("Lausanne",data_datawrapper$Storyboard[6],
                        paste0("<b>Sous-arrondissement de Lausanne-Ville (26 Sitze)</b><br>",data_datawrapper$Text_de[6],"<br><br>",
                               "<b>Sous-arrondissement de Romanel (5 Sitze)</b><br>",data_datawrapper$Text_de[7]),
                        paste0("<b>Sous-arrondissement de Lausanne-Ville (26 sièges)</b><br>",data_datawrapper$Text_fr[6],"<br><br>",
                               "<b>Sous-arrondissement de Romanel (5 sièges)</b><br>",data_datawrapper$Text_fr[7]),
                        as.numeric(data_datawrapper$Sitze_all[6])+as.numeric(data_datawrapper$Sitze_all[7]),
                        "Lausanne")
colnames(new_entry) <- c("Wahlkreis","Storyboard","Text_de","Text_fr","Sitze_all","Wahlkreis_fr")
data_datawrapper <- rbind(data_datawrapper,new_entry)

new_entry <- data.frame("Riviera-Pays d'Enhaut",data_datawrapper$Storyboard[12],
                        paste0("<b>Sous-arrondissement du Pays-d'Enhaut (2 Sitze)</b><br>",data_datawrapper$Text_de[12],"<br><br>",
                               "<b>Sous-arrondissement de Vevey (14 Sitze)</b><br>",data_datawrapper$Text_de[13]),
                        paste0("<b>Sous-arrondissement du Pays-d'Enhaut (2 sièges)</b><br>",data_datawrapper$Text_fr[12],"<br><br>",
                               "<b>Sous-arrondissement de Vevey (14 sièges)</b><br>",data_datawrapper$Text_fr[13]),
                        as.numeric(data_datawrapper$Sitze_all[12])+as.numeric(data_datawrapper$Sitze_all[13]),
                        "Riviera-Pays d'Enhaut")

colnames(new_entry) <- c("Wahlkreis","Storyboard","Text_de","Text_fr","Sitze_all","Wahlkreis_fr")
data_datawrapper <- rbind(data_datawrapper,new_entry)

data_datawrapper <- data_datawrapper[-c(4:7,12:13),]

#Anpassung Wahlkreis
data_datawrapper$text_wahlkreis_fr <- paste0("Arrondissement de ",data_datawrapper$Wahlkreis_fr," (",data_datawrapper$Sitze_all," sièges)")
data_datawrapper$text_wahlkreis_fr <- str_replace_all(data_datawrapper$text_wahlkreis_fr,"de Jura-Nord","du Jura-Nord")
data_datawrapper$text_wahlkreis_fr <- str_replace_all(data_datawrapper$text_wahlkreis_fr,"de Gros-de-Vaud","du Gros-de-Vaud")
data_datawrapper$text_wahlkreis_fr <- str_replace_all(data_datawrapper$text_wahlkreis_fr,"Broye-Vully","la Broye et du Vully")
data_datawrapper$text_wahlkreis_fr <- str_replace_all(data_datawrapper$text_wahlkreis_fr,"de Ouest lausannois","de l'Ouest Lausannois")
data_datawrapper$text_wahlkreis_fr <- str_replace_all(data_datawrapper$text_wahlkreis_fr,"de Aigle","d'Aigle")
data_datawrapper$text_wahlkreis_fr  <- str_replace_all(data_datawrapper$text_wahlkreis_fr,"Riviera-Pays d'Enhaut","la Riviera et du Pays d’Enhaut")
data_datawrapper$text_wahlkreis_de <- gsub("sièges","Sitze",data_datawrapper$text_wahlkreis_fr)

data_datawrapper$Text_fr[7] <- gsub("Ils gagnent un siège supplémentaires","Ils décrochent un siège",data_datawrapper$Text_fr[7])
data_datawrapper$Text_fr[9] <- gsub("Ils gagnent deux sièges supplémentaires","Ils décrochent deux sièges",data_datawrapper$Text_fr[9])

#Farbe definieren
data_datawrapper$Color <- 0
for (r in 1:nrow(data_datawrapper) ) {
  if (is.na(data_datawrapper$Storyboard[r]) == FALSE) {
    data_datawrapper$Color[r] <- r
  }
}


write.csv(data_datawrapper,"Output/Uebersicht_dw_vaud.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

#Auf Github hochladen
#git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
token <- read.csv("C:/Users/simon/OneDrive/Github_Token/token.txt",header=FALSE)[1,1]
git2r::cred_token(token)
gitadd()
gitcommit()
gitpush()

#Datawrapper-Grafik aktualisieren
datawrapper_auth("BMcG33cGBCp2FpqF1BSN5lHhKrw2W8Ait4AYbDEjkjVgCiWe07iqoX5pwHXdW36g", overwrite = TRUE)
dw_edit_chart("nEP9l",intro=paste0("Etat: ",format(Sys.time(),"%d.%m.%Y %H.%M")))
dw_publish_chart("nEP9l")

dw_edit_chart("krbAA",intro=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("krbAA")

#Texte speichern
#library(xlsx)
#write.xlsx(data_gesamt,"LENA_Wahlen_Vaud_Texte.xlsx",row.names = FALSE)
