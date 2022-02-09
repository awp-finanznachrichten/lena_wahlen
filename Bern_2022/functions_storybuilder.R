#Textbausteine zusammenstellen
get_textbausteine_de <- function(storyboard,Textbausteine) {
output <- ""
parts <- strsplit(storyboard,";")[[1]]

for (part in parts) {
storyblock <- Textbausteine %>%
  filter(Text_ID == part)

storyblock <- storyblock$Text_d

output <- paste0(output,storyblock,"<br><br>")
}
return(output)  
}  

#Liste der Gewinner bei mehr als zwei Gewinner
get_liste_gewinner <- function(anzahl_sitze_partei) {
  output <- ""  
  anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$change),]
  anzahl_sitze_partei <- anzahl_sitze_partei %>%
    filter(change > 0)
  
  if (nrow(anzahl_sitze_partei) > 1) {
  for (p in 1:nrow(anzahl_sitze_partei)){
    output <- paste0(output,"die <b>",anzahl_sitze_partei$Partei[p],"</b> holt ",anzahl_sitze_partei$change[p]," Sitze, ")  
    
  }  
  output <- gsub("1 Sitze","1 Sitz",output)
  output <- substr(output,1,nchar(output)-2)
  output <- stri_replace_last(output,fixed=","," und")
  }
  return(output)  
}  

#Liste der Verlierer bei mehr als zwei Gewinner
get_liste_verlierer <- function(anzahl_sitze_partei) {
  output <- ""  
  anzahl_sitze_partei <- anzahl_sitze_partei[order(anzahl_sitze_partei$change),]
  anzahl_sitze_partei <- anzahl_sitze_partei %>%
    filter(change < 0)

  if (nrow(anzahl_sitze_partei) > 1) {
    for (p in 1:nrow(anzahl_sitze_partei)){
      output <- paste0(output,"die <b>",anzahl_sitze_partei$Partei[p],"</b> verliert ",abs(anzahl_sitze_partei$change[p])," Sitze, ")  
      
    }  
    output <- gsub("1 Sitze","1 Sitz",output)
    output <- substr(output,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," und")
  }
  return(output)  
}  



#Textbaustein Sitzverteilung
get_liste_sitzverteilung <- function(anzahl_sitze_partei) {
output <- ""
anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$Sitze),]
anzahl_sitze_partei <- anzahl_sitze_partei %>%
  filter(Sitze > 0)

for (p in 1:nrow(anzahl_sitze_partei)){
output <- paste0(output,"die ",anzahl_sitze_partei$Partei[p]," ",anzahl_sitze_partei$Sitze[p]," Sitze, ")  
    
}  
output <- gsub("1 Sitze","1 Sitz",output)
output <- substr(output,1,nchar(output)-2)
output <- stri_replace_last(output,fixed=","," und")

return(output)
}  

#Liste Diverse
get_liste_diverse <- function(diverse_sitze) {
  output <- ""  
  if (nrow(diverse_sitze) > 1) {
    for (p in 1:nrow(diverse_sitze)){
      output <- paste0(output,"die Liste '",diverse_sitze$Liste_Name,"' holt ",diverse_sitze$Sitze," Sitze, ")  
      
    }  
    output <- gsub("1 Sitze","1 Sitz",output)
    output <- substr(output,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," und")
  }
  return(output)  
}  

#Liste von Parteien, die nicht mehr dabei sind
get_liste_parteienout <- function(anzahl_sitze_partei) {
  output <- ""  
  parteien_out <- anzahl_sitze_partei %>%
      filter(Sitze_alt > 0,
             Sitze == 0)
    
  if (nrow(parteien_out) > 1) {
    for (p in 1:nrow(parteien_out)) {
      output <- paste0(output,"die ",parteien_out$Partei[p],", ")  
      
    }  
  output <- substr( output ,1,nchar(output)-2)
  output <- stri_replace_last(output,fixed=","," und")
  }
  return(output)  
}  


#Liste der Neugewählten
get_liste_neugewaehlt <- function(candidates_neu_gewaehlt) {
  output <- ""
if (nrow(candidates_neu_gewaehlt) > 0){
  for (n in 1:nrow(candidates_neu_gewaehlt)) {
    output <- paste0(output,candidates_neu_gewaehlt$Name.nom[n],
                                " (",candidates_neu_gewaehlt$Partei[n],"), ")  
  }
  output <- substr(output,1,nchar(output)-2)
  output <- stri_replace_last(output,fixed=","," und")
}
return(output)  
}

#Liste der Abgewählten
get_liste_abgewaehlt <- function(candidates_abgewaehlt) {
output <- ""
if (nrow(candidates_abgewaehlt) > 0){
  for (n in 1:nrow(candidates_abgewaehlt)) {
    output <- paste0(output,candidates_abgewaehlt$Name.nom[n],
                              " (",candidates_abgewaehlt$Partei[n],"), ")  
  }
  output <- substr(output,1,nchar(output)-2)
  output <- stri_replace_last(output,fixed=","," und")
}
return(output)
}


#Variablen ersetzen
replace_varables_de <- function(text,wahlkreis,anzahl_sitze_partei,diverse_sitze,aufrecht_sitze,
                                ListeGewinner,ListeVerlierer,
                                ListeSitzverteilung,ListeDiversemitSitze,
                                ListeNeugewaehlt,ListeAbgewaehlt) {

anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$change),]

text <- gsub("#Wahlkreis",wahlkreis,text)

ParteiFirst <- anzahl_sitze_partei$Partei[1]
ParteiSecond <- anzahl_sitze_partei$Partei[2]
ParteiLast <- anzahl_sitze_partei$Partei[nrow(anzahl_sitze_partei)]
ParteiSecondLast <- anzahl_sitze_partei$Partei[nrow(anzahl_sitze_partei)-1]

ParteiFirst_Sitze <- anzahl_sitze_partei$change[1]
ParteiSecond_Sitze <- anzahl_sitze_partei$change[2]
ParteiLast_Sitze <- abs(anzahl_sitze_partei$change[nrow(anzahl_sitze_partei)])
ParteiSecondLast_Sitze <- abs(anzahl_sitze_partei$change[nrow(anzahl_sitze_partei)-1])

text <- gsub("#ParteiSecondLast_Sitze",ParteiSecondLast_Sitze,text)
text <- gsub("#ParteiFirst_Sitze",ParteiFirst_Sitze,text)
text <- gsub("#ParteiSecond_Sitze",ParteiSecond_Sitze,text)
text <- gsub("#ParteiLast_Sitze",ParteiLast_Sitze,text)

text <- gsub("#ParteiSecondLast",paste0("<b>",ParteiSecondLast,"</b>"),text)
text <- gsub("#ParteiFirst",paste0("<b>",ParteiFirst,"</b>"),text)
text <- gsub("#ParteiSecond",paste0("<b>",ParteiSecond,"</b>"),text)
text <- gsub("#ParteiLast",paste0("<b>",ParteiLast,"</b>"),text)


text <- gsub("#ListeGewinner",ListeGewinner,text)
text <- gsub("#ListeVerlierer",ListeVerlierer,text)

text <- gsub("#ListeSitzverteilung",ListeSitzverteilung,text)
text <- gsub("#ParteienOut",ListeParteienOut,text)
text <- gsub("#ListeDiversemitSitze",ListeDiversemitSitze,text)

text <- gsub("#ListeNeugewaehlt",ListeNeugewaehlt,text)
text <- gsub("#ListeAbgewaehlt",ListeAbgewaehlt,text)


if (grepl("#ListeDiverse",text) == TRUE) {
text <- gsub("#ListeDiverse",diverse_sitze$Liste_Name,text)
text <- gsub("#Diverse_Sitze",diverse_sitze$Sitze,text)
}


if (grepl("#Aufrecht_Sitze",text) == TRUE) {
  text <- gsub("#Aufrecht_Sitze",sum(aufrecht_sitze$Sitze),text)
}  
  
if (grepl("#ParteiOut",text) == TRUE) {
  parteien_out <- anzahl_sitze_partei %>%
    filter(Sitze_alt > 0,
           Sitze == 0)
  text <- gsub("#ParteiOut",parteien_out$Partei[1],text)
}  


return(text)
}  


