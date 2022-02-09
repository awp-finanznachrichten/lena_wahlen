green_cleanup <- function(text, anzahl_sitze_partei) {

  if (anzahl_sitze_partei$Partei[1] == "Grüne") {
    text <- gsub("grosse Gewinnerin. Sie holt ","grossen Gewinner. Sie holen ",text)
  }  
  
  if (anzahl_sitze_partei$Partei[nrow(anzahl_sitze_partei)] == "Grüne") {
    text <- gsub("grosse Verliererin. Sie büsst ","grossen Verlierer. Sie büssen ",text)
  }  
  
  text <- gsub("Die <b>Grüne</b> ist ","Die <b>Grünen</b> sind ",text)
  text <- gsub("Die Grüne ist ","Die Grünen sind ",text)
  text <- gsub("hält die <b>Grüne</b> ","halten die <b>Grünen</b> ",text)
  text <- gsub("hält die Grüne ","halten die Grünen ",text)
  text <- gsub("die <b>Grüne</b> verliert ","die <b>Grünen</b> verlieren ",text)
  text <- gsub("die Grüne verliert ","die Grünen verlieren ",text)
  text <- gsub("die <b>Grüne</b> hält ","die <b>Grünen</b> halten ",text)
  text <- gsub("die Grüne hält ","die Grünen halten ",text)
  text <- gsub("Die <b>Grüne</b> ","Die <b>Grünen</b> ",text)
  text <- gsub("die Grüne ","die Grünen ",text)
  text <- gsub("Die <b>Grüne</b> ","Die <b>Grünen</b> ",text)
  text <- gsub("die Grüne ","die Grünen ",text)

return(text)  
  
}  

text_optimisation <- function(text) {
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
return(text)  
}  