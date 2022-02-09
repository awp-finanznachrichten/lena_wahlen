#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/lena_wahlen/Bern_2022")

#Bibliotheken und Funktionen laden
source("config.R")

#Wahlkreise
wahlkreise <- c("Jura bernois","Biel-Bienne-Seeland","Oberaargau","Emmental","Mittelland-Nord",
                "Bern","Mittelland-Süd","Thun","Oberland")



####Dataframe für alle Daten
data_gesamt <- data.frame("Wahlkreis","Liste_Nummer","Liste_Name","Liste_Kurzname",0,0,0,"neu_gewaehlt","abgewaehlt")
colnames(data_gesamt) <- c("Wahlkreis","Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze_bisher","Sitze_neu","Sitze_change","neu_gewaehlt","abgewaehlt")

###Daten pro Wahlkreise

for (w in 1:length(wahlkreise)) {

##Scraping von Listen und bisherigen Sitzen
url <- paste0("https://www.bewas.sites.be.ch/2022/2022-03-27/WAHL_GROSSRAT/listeWahlkreisListen-",LETTERS[w],"-de.html")
webpage <- read_html(url)

data_table <- html_text(html_nodes(webpage,"td"))

#Create new Dataframe
data_wahlkreis <- data.frame("Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze_bisher")
colnames(data_wahlkreis) <- c("Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze_bisher")

for (i in seq(7,length(data_table),6)) {
new_data <- data.frame(data_table[i],data_table[i+1],
                       data_table[i+2],data_table[i+5])
colnames(new_data) <- c("Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze_bisher")

data_wahlkreis <- rbind(data_wahlkreis,new_data)
}

data_wahlkreis <- data_wahlkreis[-1,]
data_wahlkreis$Sitze_bisher <- as.numeric(data_wahlkreis$Sitze_bisher)


##Neue Daten laden von 2022 (Simulation)
#url <- paste0("https://www.bewas.sites.be.ch/2022/2022-03-27/WAHL_GROSSRAT/listeWahlkreisListen-",LETTERS[w],"-de.html")
#webpage <- read_html(url)

#data_table <- html_text(html_nodes(webpage,"td"))


#Create new Dataframe
data_wahlkreis_new <- data.frame("Liste_Nummer","Sitze_neu")
colnames(data_wahlkreis_new) <- c("Liste_Nummer","Sitze_neu")

for (i in seq(7,length(data_table),6)) {
  new_data <- data.frame(data_table[i],sample(0:3,1))
  colnames(new_data) <- c("Liste_Nummer","Sitze_neu")
  data_wahlkreis_new <- rbind(data_wahlkreis_new,new_data)
}

data_wahlkreis_new <- data_wahlkreis_new[-1,]
data_wahlkreis_new$Sitze_neu <- as.numeric(data_wahlkreis_new$Sitze_neu)


#Daten zusammenführen und vergleichen
data_wahlkreis <- merge(data_wahlkreis,data_wahlkreis_new)
data_wahlkreis$Sitze_change <- data_wahlkreis$Sitze_neu - data_wahlkreis$Sitze_bisher

#Neu gewählt und abgewählte Personen (aus Tabelle auf Website) -> CSV?
data_wahlkreis$neu_gewaehlt <- ""
data_wahlkreis$abgewaehlt <- ""

url <- paste0("https://www.bewas.sites.be.ch/2018/2018-03-25/WAHL_GROSSRAT/resultatWahlkreis-",LETTERS[w],"-de.html")

webpage <- read_html(url)
data_table <- html_text(html_nodes(webpage,"td"))

for (i in 1:(length(data_table)-1)) {

if ( (data_table[i] == "*") & (data_table[i+1] == "") ){
data_wahlkreis$neu_gewaehlt <- paste0(data_wahlkreis$neu_gewaehlt,data_table[i-5]," (",data_table[i-4],"); ")  
}
  
if ( (data_table[i] == "") & (data_table[i+1] == "x") ){
data_wahlkreis$abgewaehlt <- paste0(data_wahlkreis$abgewaehlt,data_table[i-5]," (",data_table[i-4],"); ")  
}    
  
}  

#Wahlkreisname hinzufügen und weitere Formatierungen
data_wahlkreis$Wahlkreis <- wahlkreise[w]
data_wahlkreis$Liste_Nummer <- as.numeric(data_wahlkreis$Liste_Nummer)
data_wahlkreis <- data_wahlkreis[order(data_wahlkreis$Liste_Nummer),]

#Add Data
data_gesamt <- rbind(data_gesamt,data_wahlkreis)

}

data_gesamt <- data_gesamt[-1,]

View(data_gesamt)

#Get CSV
link <- "https://www.bewas.sites.be.ch/2018/2018-03-25/WAHL_GROSSRAT/csvResultatWahlkreis-B.csv"
test <- read.csv(link,sep =";",skip = 4)
test$No.liste <- as.numeric(test$No.liste)
test <- test[1:26,]

liste_wahlkreis <- LENA_Listen_Kanton_Bern %>%
  filter(Wahlkreis == "Biel-Bienne-Seeland")

liste_wahlkreis <- left_join(liste_wahlkreis,test, by=c("Liste_Nummer" = "No.liste"))

View(liste_wahlkreis)
#Storyfinder


#Storybuider

#Output

