#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/lena_wahlen/Vaud_2022")

#Bibliotheken und Funktionen laden
source("config.R")

#Wahlkreise
wahlkreise <- c("Aigle","Broye-Vully","Gros-de-Vaud","Jura-Nord vaudois","Lausanne",
                "Lavaux-Oron","Morges","Nyon","Ouest lausannois","Riviera-Pays-d'Enhaut")

codes_wahlkreise <- c("A2","A3","A4","A501","A502","A5","A6","A7","A8","A503")

####Dataframe für alle Daten
data_gesamt <- data.frame("Jahr","Wahlkreis","Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze")
colnames(data_gesamt) <- c("Jahr","Wahlkreis","Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze")



###Daten pro Wahlkreise

for (w in 1:length(wahlkreise)) {

##Scraping von Listen und bisherigen Sitzen
url <- paste0("https://www.elections.vd.ch/votelec/app5/html/VDGC20170430-",codes_wahlkreise[w],"/Resultat/resultatsGenerauxResultatElection.html")
webpage <- read_html(url)

url
data_table <- html_text(html_nodes(webpage,"td"))

#Create new Dataframe
data_wahlkreis <- data.frame("Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze")
colnames(data_wahlkreis) <- c("Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze")
list_number <- 1

for (i in seq(1,which(grepl("Total",data_table))[1]-1,4)) {

new_data <- data.frame(list_number,data_table[i],data_table[i+1],data_table[i+3])
colnames(new_data) <- c("Liste_Nummer","Liste_Name","Liste_Kurzname","Sitze")

data_wahlkreis <- rbind(data_wahlkreis,new_data)
list_number <- list_number+1
}

data_wahlkreis <- data_wahlkreis[-1,]
data_wahlkreis$Sitze <- as.numeric(data_wahlkreis$Sitze)


#Create new Dataframe
#data_wahlkreis_new <- data.frame("Liste_Nummer","Sitze_neu")
#colnames(data_wahlkreis_new) <- c("Liste_Nummer","Sitze_neu")

#for (i in seq(7,length(data_table),6)) {
#  new_data <- data.frame(data_table[i],sample(0:3,1))
#  colnames(new_data) <- c("Liste_Nummer","Sitze_neu")
#  data_wahlkreis_new <- rbind(data_wahlkreis_new,new_data)
#}

#data_wahlkreis_new <- data_wahlkreis_new[-1,]
#data_wahlkreis_new$Sitze_neu <- as.numeric(data_wahlkreis_new$Sitze_neu)

#Daten zusammenführen und vergleichen
#data_wahlkreis <- merge(data_wahlkreis,data_wahlkreis_new)
#data_wahlkreis$Sitze_change <- data_wahlkreis$Sitze_neu - data_wahlkreis$Sitze_bisher

#Neu gewählt und abgewählte Personen (aus Tabelle auf Website) -> CSV?


#Wahlkreisname hinzufügen und weitere Formatierungen
data_wahlkreis$Wahlkreis <- wahlkreise[w]
data_wahlkreis$Jahr <- "2017"
data_wahlkreis$Liste_Nummer <- as.numeric(data_wahlkreis$Liste_Nummer)
data_wahlkreis <- data_wahlkreis[order(data_wahlkreis$Liste_Nummer),]

#Add Data
data_gesamt <- rbind(data_gesamt,data_wahlkreis)

print(data_wahlkreis)
}

data_gesamt <- data_gesamt[-1,]



#Storyfinder

#Storybuider

#Output

