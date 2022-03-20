green_cleanup <- function(text, anzahl_sitze_partei) {

  text <- gsub("Die Ensemble","Ensemble",text)
  text <- gsub("Die <b>Ensemble","<b>Ensemble",text)
  text <- gsub("die Ensemble","Ensemble",text)
  text <- gsub("die <b>Ensemble","<b>Ensemble",text)
  
  if (anzahl_sitze_partei$Fraktion_de[1] == "Grüne" ||
      anzahl_sitze_partei$Fraktion_de[1] == "Libres" ||
      anzahl_sitze_partei$Fraktion_de[1] == "Ensemble à gauche et POP"
      ) {
    text <- gsub("grosse Gewinnerin. Sie holt ","grossen Gewinner. Sie holen ",text)
    text <- gsub("Gewinnerin. Sie holt ","Gewinner. Sie holen ",text)
  }  
  
  if (anzahl_sitze_partei$Fraktion_de[nrow(anzahl_sitze_partei)] == "Grüne" ||
      anzahl_sitze_partei$Fraktion_de[nrow(anzahl_sitze_partei)] == "Libres" ||
      anzahl_sitze_partei$Fraktion_de[nrow(anzahl_sitze_partei)] == "Ensemble à gauche et POP" ) {
    text <- gsub("grosse Verliererin. Sie büsst ","grossen Verlierer. Sie büssen ",text)
    text <- gsub("Verliererin. Sie büsst ","Verlierer. Sie büssen ",text)
  }  
  
  text <- gsub("Die <b>Grüne</b> ist ","Die <b>Grünen</b> sind ",text)
  text <- gsub("Die Grüne ist ","Die Grünen sind ",text)
  text <- gsub("hält die <b>Grüne</b> ","halten die <b>Grünen</b> ",text)
  text <- gsub("hält die Grüne ","halten die Grünen ",text)
  text <- gsub("die <b>Grüne</b> verliert ","die <b>Grünen</b> verlieren ",text)
  text <- gsub("die Grüne verliert ","die Grünen verlieren ",text)
  text <- gsub("die <b>Grüne</b> hält ","die <b>Grünen</b> halten ",text)
  text <- gsub("die Grüne hält ","die Grünen halten ",text)
  text <- gsub("Die <b>Grüne</b>","Die <b>Grünen</b>",text)
  text <- gsub("Die Grüne","Die Grünen",text)
  text <- gsub("die <b>Grüne</b>","die <b>Grünen</b> ",text)
  text <- gsub("die Grüne","die Grünen",text)
  text <- gsub("die <b>Grünen</b> holt ","die <b>Grünen</b> holen ",text)
  
  text <- gsub("Die <b>Libres</b> ist ","Les <b>Libres</b> sind ",text)
  text <- gsub("Die Libres ist ","Les Libres sind ",text)
  text <- gsub("hält die <b>Libres</b> ","halten les <b>Libres</b> ",text)
  text <- gsub("hält die Libres ","halten les Libres ",text)
  text <- gsub("die <b>Libres</b> verliert ","les <b>Libres</b> verlieren ",text)
  text <- gsub("die Libres verliert ","les Libres verlieren ",text)
  text <- gsub("die <b>Libres</b> hält ","les <b>Libres</b> halten ",text)
  text <- gsub("die Libres hält ","les Libres halten ",text)
  text <- gsub("Die <b>Libres</b>","Les <b>Libres</b>",text)
  text <- gsub("Die Libres","Les Libres",text)
  text <- gsub("die <b>Libres</b>","les <b>Libres</b>",text)
  text <- gsub("die Libres","les Libres",text)
  text <- gsub("les <b>Libres</b> holt ","les <b>Libres</b> holen ",text)
  
  text <- gsub("<b>Ensemble à gauche et POP</b> ist ","<b>Ensemble à gauche et POP</b> sind ",text)
  text <- gsub("Ensemble à gauche et POP ist ","Ensemble à gauche et POP sind ",text)
  text <- gsub("hält <b>Ensemble à gauche et POP</b> ","halten <b>Ensemble à gauche et POP</b> ",text)
  text <- gsub("hält Ensemble à gauche et POP ","halten Ensemble à gauche et POP ",text)
  text <- gsub("<b>Ensemble à gauche et POP</b> verliert ","<b>Ensemble à gauche et POP</b> verlieren ",text)
  text <- gsub("Ensemble à gauche et POP verliert ","Ensemble à gauche et POP verlieren ",text)
  text <- gsub("<b>Ensemble à gauche et POP</b> hält ","<b>Ensemble à gauche et POP</b> halten ",text)
  text <- gsub("Ensemble à gauche et POP hält ","Ensemble à gauche et POP halten ",text)
  text <- gsub("<b>Ensemble à gauche et POP</b> holt ","<b>Ensemble à gauche et POP</b> holen ",text)

return(text)  
  
}  

green_cleanup_fr <- function(text, anzahl_sitze_partei) {

  if (anzahl_sitze_partei$Fraktion_fr[1] == "Verts" ||
      anzahl_sitze_partei$Fraktion_fr[1] == "Vert'libéraux" ||
      anzahl_sitze_partei$Fraktion_fr[1] == "Libres" ||
      anzahl_sitze_partei$Fraktion_fr[1] == "Ensemble à gauche et POP"
      ) {
    text <- gsub("est le grand gagnant","sont les grand gagnants",text)
    text <- gsub("Il remporte ","Ils remportent ",text)
    text <- gsub("termine premier ","terminent premiers ",text)
    text <- gsub("Il gagne ","Ils gagnent ",text)
    
  }  
  
  if (anzahl_sitze_partei$Fraktion_fr[nrow(anzahl_sitze_partei)] == "Verts" ||
      anzahl_sitze_partei$Fraktion_fr[nrow(anzahl_sitze_partei)] == "Vert'libéraux" ||
      anzahl_sitze_partei$Fraktion_fr[nrow(anzahl_sitze_partei)] == "Libres" ||
      anzahl_sitze_partei$Fraktion_fr[nrow(anzahl_sitze_partei)] == "Ensemble à gauche et POP" 
      ) {
    text <- gsub("est le grand battu","sont les grands battus",text)
    text <- gsub("Il perd ","Ils perdent ",text)
    text <- gsub("recule ","reculent ",text)
    text <- gsub("perd un siège[.]","predent un siège.",text)
  }  

  text <- gsub("Le Ensemble","Ensemble",text)
  text <- gsub("Le <b>Ensemble","<b>Ensemble",text)
  text <- gsub("le Ensemble","Ensemble",text)
  text <- gsub("le <b>Ensemble","<b>Ensemble",text)
  text <- gsub("Le UDC","L'UDC",text)
  text <- gsub("Le <b>UDC","<b>L'UDC",text)
  text <- gsub("le UDC","l'UDC",text)
  text <- gsub("le <b>UDC","<b>l'UDC",text)

  
  text <- gsub("Le <b>Verts</b> est ","Les <b>Verts</b> sont ",text)
  text <- gsub("Le Verts est ","Les Verts sont ",text)
  text <- gsub("Le <b>Vert'libéraux</b> est ","Les <b>Vert'libéraux</b> sont ",text)
  text <- gsub("Le Vert'libéraux est ","Les Vert'libéraux sont ",text)
  text <- gsub("Le <b>Libres</b> est ","Les <b>Libres</b> sont ",text)
  text <- gsub("Le Libres est ","Les Libres sont ",text)
  text <- gsub("<b>Ensemble à gauche et POP</b> est ","<b>Ensemble à gauche et POP</b> sont ",text)
  text <- gsub("Ensemble à gauche est ","Ensemble à gauche sont ",text)
  
  text <- gsub("Le Verts a perdu tous ses sièges","Les Verts perdent tous leurs sièges",text)
  text <- gsub("Le Vert'libéraux a perdu tous ses sièges","Les Vert'libéraux perdent tout leurs sièges",text)
  text <- gsub("Le Libres a perdu tous ses sièges","Les Libres perdent tout leurs sièges",text)
  text <- gsub("Ensemble à gauche et POP a perdu tous ses sièges","Ensemble à gauche et POP perdent tout leurs sièges",text)
  
  text <- gsub("le <b>Verts</b> qui perd ","les <b>Verts</b> qui perdent ",text)
  text <- gsub("le <b>Vert'libéraux</b> qui perd ","les <b>Vert'libéraux</b> qui perdent ",text)
  text <- gsub("le <b>Libres</b> qui perd ","les <b>Libres</b> qui perdent ",text)
  text <- gsub("<b>Ensemble à gauche et POP</b> qui perd ","<b>Ensemble à gauche et POP</b> qui perdent ",text)
  
  text <- gsub("le <b>Verts</b> obtient ","les <b>Verts</b> obtiennent ",text)
  text <- gsub("le <b>Vert'libéraux</b> obtient ","les <b>Vert'libéraux</b> obtiennent ",text)
  text <- gsub("le <b>Libres</b> obtient ","les <b>Libres</b> obtiennent ",text)
  text <- gsub("<b>Ensemble à gauche et POP</b> obtient ","<b>Ensemble à gauche et POP</b> obtiennent ",text)
  
  text <- gsub("Verts détient ","Verts détiennent ",text)
  text <- gsub("Vert'libéraux détient ","Vert'libéraux détiennent ",text)
  text <- gsub("Libres détient ","Libres détiennent ",text)
  text <- gsub("Ensemble à gauche et POP détient ","Ensemble à gauche et POP détiennent ",text)
  
  text <- gsub("Le <b>Verts</b>","Les <b>Verts</b>",text)
  text <- gsub("Le Verts","Les Verts",text)
  text <- gsub("le <b>Verts</b>","les <b>Verts</b>",text)
  text <- gsub("le Verts","les Verts",text)
  text <- gsub("Le <b>Vert'libéraux</b>","Les <b>Vert'libéraux</b>",text)
  text <- gsub("Le Vert'libéraux","Les Vert'libéraux",text)
  text <- gsub("le <b>Vert'libéraux</b>","les <b>Vert'libéraux</b>",text)
  text <- gsub("le Vert'libéraux","les Vert'libéraux",text)
  text <- gsub("Le <b>Libres</b>","Les <b>Libres</b>",text)
  text <- gsub("Le Libres","Les Libres",text)
  text <- gsub("le <b>Libres</b>","les <b>Libres</b>",text)
  text <- gsub("le Libres","les Libres",text)
  
  text <- gsub("sont le gagnant","sont les gagnants",text)
  text <- gsub("l'Ouest Lausannois[.] Ils gagnent un siège supplémentaire",
               "l'Ouest Lausannois. Ils décrochent un siège",
               text)
  

  return(text)  
  
}  


text_optimisation <- function(text) {
  text <- gsub("<br><br>die","<br><br>Die",text)
  text <- gsub(": die",": Die",text)
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
  
  text <- substring(text,1,nchar(text)-8)
return(text)  
}  


text_optimisation_fr <- function(text) {
  text <- gsub("<br><br>l","<br><br>L",text)
  text <- gsub(" 1 "," un ",text)
  text <- gsub(" 2 "," deux ",text)
  text <- gsub(" 3 "," trois ",text)
  text <- gsub(" 4 "," quatre ",text)
  text <- gsub(" 5 "," cinq ",text)
  text <- gsub(" 6 "," six ",text)
  text <- gsub(" 7 "," sept ",text)
  text <- gsub(" 8 "," huit ",text)
  text <- gsub(" 9 "," neuf ",text)
  text <- gsub(" 10 "," dix ",text)
  text <- gsub(" 11 "," onze ",text)
  text <- gsub(" 12 "," douze ",text)
  
  text <- str_replace_all(text,"arrondissement ","arrondissement de ")
  text <- str_replace_all(text,"de Jura-Nord","du Jura-Nord")
  text <- str_replace_all(text,"de Gros-de-Vaud","du Gros-de-Vaud")
  text <- str_replace_all(text,"de Ouest lausannois","de l'Ouest Lausannois")
  text <- str_replace_all(text,"Broye-Vully","la Broye et du Vully")
  text <- str_replace_all(text,"Riviera-Pays d'Enhaut","la Riviera et du Pays d’Enhaut")
  text <- str_replace_all(text,"l'arrondissement de La Vallée","le sous-arrondissement de la Vallée")
  text <- str_replace_all(text,"l'arrondissement de Yverdon","le sous-arrondissement d'Yverdon")
  text <- str_replace_all(text,"l'arrondissement de Lausanne-Ville","le sous-arrondissement de Lausanne-Ville")
  text <- str_replace_all(text,"l'arrondissement de Romanel","le sous-arrondissement de Romanel")
  text <- str_replace_all(text,"l'arrondissement de Vevey","le sous-arrondissement de Vevey")
  text <- str_replace_all(text,"l'arrondissement de Pays-d'Enhaut","le sous-arrondissement du Pays d'Enhaut")
  
  text <- str_replace_all(text,"de A","d'A")
  text <- str_replace_all(text,"de E","d'E")
  text <- str_replace_all(text,"de I","d'I") 
  text <- str_replace_all(text,"de O","d'O") 
  text <- str_replace_all(text,"de U","d'U")
  text <- str_replace_all(text,"de Yv","d'Yv")
  text <- str_replace_all(text,"de le ","du ")
  
  text <- str_replace_all(text,"da[.] [(]EàG[)]","EàG")
  
  text <- substring(text,1,nchar(text)-8)
  return(text)  
}  