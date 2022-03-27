#Textbausteine zusammenstellen
get_textbausteine_de <- function(storyboard,Textbausteine) {
output <- ""
parts <- strsplit(storyboard,";")[[1]]

for (part in parts) {
storyblock <- Textbausteine %>%
  filter(Text_ID == part)

storyblock <- storyblock$Text_d[sample(1:nrow(storyblock),1)]

output <- paste0(output,storyblock,"<br><br>")
}
return(output)  
}  


#Textbausteine zusammenstellen fr
get_textbausteine_fr <- function(storyboard,Textbausteine) {
  output <- ""
  parts <- strsplit(storyboard,";")[[1]]
  
  for (part in parts) {
    storyblock <- Textbausteine %>%
      filter(Text_ID == part)
    
    storyblock <- storyblock$Text_f[sample(1:nrow(storyblock),1)]
    
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
    output <- paste0(output,"die <b>",anzahl_sitze_partei$Fraktion_de[p],"</b> holt ",anzahl_sitze_partei$change[p]," Sitze, ")  
    
  }  
  output <- gsub("1 Sitze","1 Sitz",output)
  output <- substr(output,1,nchar(output)-2)
  output <- stri_replace_last(output,fixed=","," und")
  }
  return(output)  
}  

#Liste der Gewinner bei mehr als zwei Gewinner fr
get_liste_gewinner_fr <- function(anzahl_sitze_partei) {
  output <- ""  
  anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$change),]
  anzahl_sitze_partei <- anzahl_sitze_partei %>%
    filter(change > 0)
  
  if (nrow(anzahl_sitze_partei) > 1) {
    for (p in 1:nrow(anzahl_sitze_partei)){
      output <- paste0(output,"le <b>",anzahl_sitze_partei$Fraktion_fr[p],"</b> obtient ",anzahl_sitze_partei$change[p]," sièges, ")  
      
    }  
    output <- gsub("1 sièges","1 siège",output)
    output <- substr(output,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," et")
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
      output <- paste0(output,"die <b>",anzahl_sitze_partei$Fraktion_de[p],"</b> verliert ",abs(anzahl_sitze_partei$change[p])," Sitze, ")  
      
    }  
    output <- gsub("1 Sitze","1 Sitz",output)
    output <- substr(output,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," und")
  }
  return(output)  
}  


#Liste der Verlierer bei mehr als zwei Gewinner
get_liste_verlierer_fr <- function(anzahl_sitze_partei) {
  output <- ""  
  anzahl_sitze_partei <- anzahl_sitze_partei[order(anzahl_sitze_partei$change),]
  anzahl_sitze_partei <- anzahl_sitze_partei %>%
    filter(change < 0)
  
  if (nrow(anzahl_sitze_partei) > 1) {
    for (p in 1:nrow(anzahl_sitze_partei)){
      output <- paste0(output,"le <b>",anzahl_sitze_partei$Fraktion_fr[p],"</b> qui perd ",abs(anzahl_sitze_partei$change[p])," sièges, ")  
      
    }  
    output <- gsub("1 sièges","1 siège",output)
    output <- substr(output,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," et")
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
output <- paste0(output,"die ",anzahl_sitze_partei$Fraktion_de[p]," ",anzahl_sitze_partei$Sitze[p]," Sitze, ")  
    
}  
output <- gsub("1 Sitze","1 Sitz",output)
output <- substr(output,1,nchar(output)-2)
output <- stri_replace_last(output,fixed=","," und")

return(output)
}  

#Textbaustein Sitzverteilung
get_liste_sitzverteilung_fr <- function(anzahl_sitze_partei) {
  output <- ""
  anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$Sitze),]
  anzahl_sitze_partei <- anzahl_sitze_partei %>%
    filter(Sitze > 0)
  
  for (p in 1:nrow(anzahl_sitze_partei)){
  if (p == 1) {
    output <- paste0(output,"Le ",anzahl_sitze_partei$Fraktion_fr[p]," détient désormais ",anzahl_sitze_partei$Sitze[p]," sièges, ")
  } else {
    output <- paste0(output,"le ",anzahl_sitze_partei$Fraktion_fr[p]," ",anzahl_sitze_partei$Sitze[p]," sièges, ")  
  }
  }
  output <- gsub("1 sièges","1 siège",output)
  output <- substr(output,1,nchar(output)-2)
  output <- stri_replace_last(output,fixed=","," et")
  
  return(output)
}  


#Liste Diverse
get_liste_diverse <- function(diverse_sitze) {
  output <- ""  
  if (nrow(diverse_sitze) > 1) {
    for (p in 1:nrow(diverse_sitze)){
      output <- paste0(output,"die Liste '",diverse_sitze$Liste_Name[p],"' holt ",diverse_sitze$Sitze[p]," Sitze, ")  
      
    }  
    output <- gsub("1 Sitze","1 Sitz",output)
    output <- substr(output,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," und")
  }
  return(output)  
}  

#Liste Diverse
get_liste_diverse_fr <- function(diverse_sitze) {
  output <- ""  
  if (nrow(diverse_sitze) > 1) {
    for (p in 1:nrow(diverse_sitze)){
      output <- paste0(output,"la liste '",diverse_sitze$Liste_Name[p],"' obtient ",diverse_sitze$Sitze[p]," sièges, ")  
      
    }  
    output <- gsub("1 sièges","1 siège",output)
    output <- substr(output,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," et")
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
      output <- paste0(output,"die ",parteien_out$Fraktion_de[p],", ")  
      
    }  
  output <- substr( output ,1,nchar(output)-2)
  output <- stri_replace_last(output,fixed=","," und")
  }
  return(output)  
}  

#Liste von Parteien, die nicht mehr dabei sind
get_liste_parteienout_fr <- function(anzahl_sitze_partei) {
  output <- ""  
  parteien_out <- anzahl_sitze_partei %>%
    filter(Sitze_alt > 0,
           Sitze == 0)
  
  if (nrow(parteien_out) > 1) {
    for (p in 1:nrow(parteien_out)) {
      output <- paste0(output,"le ",parteien_out$Fraktion_fr[p],", ")  
      
    }  
    output <- substr( output ,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," et")
  }
  return(output)  
}  


#Liste der Neugewählten
get_liste_neugewaehlt <- function(candidates_neu_gewaehlt) {
  output <- ""
if (nrow(candidates_neu_gewaehlt) > 0){
  for (n in 1:nrow(candidates_neu_gewaehlt)) {
    output <- paste0(output,candidates_neu_gewaehlt$Name...Nom[n],
                                " (",candidates_neu_gewaehlt$Fraktion_de[n],"), ")  
  }
  output <- substr(output,1,nchar(output)-2)
  output <- stri_replace_last(output,fixed=","," und")
}
return(output)  
}


#Liste der Neugewählten
get_liste_neugewaehlt_fr <- function(candidates_neu_gewaehlt) {
  output <- ""
  if (nrow(candidates_neu_gewaehlt) > 0){
    for (n in 1:nrow(candidates_neu_gewaehlt)) {
      output <- paste0(output,candidates_neu_gewaehlt$Name...Nom[n],
                       " (",candidates_neu_gewaehlt$Fraktion_fr[n],"), ")  
    }
    output <- substr(output,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," et")
  }
  return(output)  
}

#Liste der Abgewählten
get_liste_abgewaehlt <- function(candidates_abgewaehlt) {
output <- ""
if (nrow(candidates_abgewaehlt) > 0){
  for (n in 1:nrow(candidates_abgewaehlt)) {
    output <- paste0(output,candidates_abgewaehlt$Name...Nom[n],
                              " (",candidates_abgewaehlt$Fraktion_de[n],"), ")  
  }
  output <- substr(output,1,nchar(output)-2)
  output <- stri_replace_last(output,fixed=","," und")
}
return(output)
}

#Liste der Abgewählten
get_liste_abgewaehlt_fr <- function(candidates_abgewaehlt) {
  output <- ""
  if (nrow(candidates_abgewaehlt) > 0){
    for (n in 1:nrow(candidates_abgewaehlt)) {
      output <- paste0(output,candidates_abgewaehlt$Name...Nom[n],
                       " (",candidates_abgewaehlt$Fraktion_fr[n],"), ")  
    }
    output <- substr(output,1,nchar(output)-2)
    output <- stri_replace_last(output,fixed=","," et")
  }
  return(output)
}




#Variablen ersetzen
replace_varables_de <- function(text,wahlkreis,anzahl_sitze_partei,diverse_sitze,aufrecht_sitze,
                                ListeGewinner,ListeVerlierer,
                                ListeSitzverteilung,ListeDiversemitSitze,
                                ListeNeugewaehlt,ListeAbgewaehlt) {

anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$change),]

text <- gsub("#Wahlkreis_de",wahlkreis,text)

ParteiFirst <- anzahl_sitze_partei$Fraktion_de[1]
ParteiSecond <- anzahl_sitze_partei$Fraktion_de[2]
ParteiLast <- anzahl_sitze_partei$Fraktion_de[nrow(anzahl_sitze_partei)]
ParteiSecondLast <- anzahl_sitze_partei$Fraktion_de[nrow(anzahl_sitze_partei)-1]

ParteiFirst_Sitze <- anzahl_sitze_partei$change[1]
ParteiSecond_Sitze <- anzahl_sitze_partei$change[2]
ParteiLast_Sitze <- abs(anzahl_sitze_partei$change[nrow(anzahl_sitze_partei)])
ParteiSecondLast_Sitze <- abs(anzahl_sitze_partei$change[nrow(anzahl_sitze_partei)-1])

text <- gsub("#ParteiSecondLast_Sitze",ParteiSecondLast_Sitze,text)
text <- gsub("#ParteiFirst_Sitze",ParteiFirst_Sitze,text)
text <- gsub("#ParteiSecond_Sitze",ParteiSecond_Sitze,text)
text <- gsub("#ParteiLast_Sitze",ParteiLast_Sitze,text)

text <- gsub("#ParteiSecondLast_de",paste0("<b>",ParteiSecondLast,"</b>"),text)
text <- gsub("#ParteiFirst_de",paste0("<b>",ParteiFirst,"</b>"),text)
text <- gsub("#ParteiSecond_de",paste0("<b>",ParteiSecond,"</b>"),text)
text <- gsub("#ParteiLast_de",paste0("<b>",ParteiLast,"</b>"),text)


text <- gsub("#ListeGewinner_de",ListeGewinner,text)
text <- gsub("#ListeVerlierer_de",ListeVerlierer,text)

text <- gsub("#ListeSitzverteilung_de",ListeSitzverteilung,text)
text <- gsub("#ParteienOut_de",ListeParteienOut,text)
text <- gsub("#ListeDiversemitSitze_de",ListeDiversemitSitze,text)

text <- gsub("#ListeNeugewaehlt_de",ListeNeugewaehlt,text)
text <- gsub("#ListeAbgewaehlt_de",ListeAbgewaehlt,text)


if (grepl("#ListeDiverse",text) == TRUE) {
text <- gsub("#ListeDiverse_de",diverse_sitze$Liste_Name,text)
text <- gsub("#Diverse_Sitze",diverse_sitze$Sitze,text)
}


if (grepl("#Aufrecht_Sitze",text) == TRUE) {
  text <- gsub("#Aufrecht_Sitze",sum(aufrecht_sitze$Sitze),text)
}  
  
if (grepl("#ParteiOut",text) == TRUE) {
  parteien_out <- anzahl_sitze_partei %>%
    filter(Sitze_alt > 0,
           Sitze == 0)
  text <- gsub("#ParteiOut_de",parteien_out$Fraktion_de[1],text)
}  


return(text)
}  


#Variablen ersetzen
replace_varables_fr <- function(text,wahlkreis,anzahl_sitze_partei,diverse_sitze,aufrecht_sitze,
                                ListeGewinner,ListeVerlierer,
                                ListeSitzverteilung,ListeDiversemitSitze,
                                ListeNeugewaehlt,ListeAbgewaehlt) {
  
  anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$change),]
  
  text <- gsub("#Wahlkreis_fr",wahlkreis,text)
  
  ParteiFirst <- anzahl_sitze_partei$Fraktion_fr[1]
  ParteiSecond <- anzahl_sitze_partei$Fraktion_fr[2]
  ParteiLast <- anzahl_sitze_partei$Fraktion_fr[nrow(anzahl_sitze_partei)]
  ParteiSecondLast <- anzahl_sitze_partei$Fraktion_fr[nrow(anzahl_sitze_partei)-1]
  
  ParteiFirst_Sitze <- anzahl_sitze_partei$change[1]
  ParteiSecond_Sitze <- anzahl_sitze_partei$change[2]
  ParteiLast_Sitze <- abs(anzahl_sitze_partei$change[nrow(anzahl_sitze_partei)])
  ParteiSecondLast_Sitze <- abs(anzahl_sitze_partei$change[nrow(anzahl_sitze_partei)-1])
  
  text <- gsub("#ParteiSecondLast_Sitze",ParteiSecondLast_Sitze,text)
  text <- gsub("#ParteiFirst_Sitze",ParteiFirst_Sitze,text)
  text <- gsub("#ParteiSecond_Sitze",ParteiSecond_Sitze,text)
  text <- gsub("#ParteiLast_Sitze",ParteiLast_Sitze,text)
  
  text <- gsub("#ParteiSecondLast_fr",paste0("<b>",ParteiSecondLast,"</b>"),text)
  text <- gsub("#ParteiFirst_fr",paste0("<b>",ParteiFirst,"</b>"),text)
  text <- gsub("#ParteiSecond_fr",paste0("<b>",ParteiSecond,"</b>"),text)
  text <- gsub("#ParteiLast_fr",paste0("<b>",ParteiLast,"</b>"),text)
  
  
  text <- gsub("#ListeGewinner_fr",ListeGewinner,text)
  text <- gsub("#ListeVerlierer_fr",ListeVerlierer,text)
  
  text <- gsub("#ListeSitzverteilung_fr",ListeSitzverteilung,text)
  text <- gsub("#ParteienOut_fr",ListeParteienOut_fr,text)
  text <- gsub("#ListeDiversemitSitze_fr",ListeDiversemitSitze,text)
  
  text <- gsub("#ListeNeugewaehlt_fr",ListeNeugewaehlt,text)
  text <- gsub("#ListeAbgewaehlt_fr",ListeAbgewaehlt,text)
  
  
  if (grepl("#ListeDiverse",text) == TRUE) {
    text <- gsub("#ListeDiverse_fr",diverse_sitze$Liste_Name,text)
    text <- gsub("#Diverse_Sitze",diverse_sitze$Sitze,text)
  }
  
  
  if (grepl("#Aufrecht_Sitze",text) == TRUE) {
    text <- gsub("#Aufrecht_Sitze",sum(aufrecht_sitze$Sitze),text)
  }  
  
  if (grepl("#ParteiOut",text) == TRUE) {
    parteien_out <- anzahl_sitze_partei %>%
      filter(Sitze_alt > 0,
             Sitze == 0)
    text <- gsub("#ParteiOut_fr",parteien_out$Fraktion_fr[1],text)
  }  
  

  return(text)
}  


