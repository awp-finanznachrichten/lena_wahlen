get_winner <- function(anzahl_sitze_partei) {

output <- ""  
anzahl_sitze_partei <- anzahl_sitze_partei[order(-anzahl_sitze_partei$change),]   
   
  if (  (anzahl_sitze_partei$change[1] > 2) &
        (anzahl_sitze_partei$change[2] > 2) &
        (anzahl_sitze_partei$change[3] > 2) ) {
    output <- paste0(output,"GG_mehrereParteien;")
  } else if ( (anzahl_sitze_partei$change[1] > 2) &
              (anzahl_sitze_partei$change[2] > 2) ) {
    if (anzahl_sitze_partei$change[1] == anzahl_sitze_partei$change[2]) {
      output <- paste0(output,"GG_zweiParteien_gleich;") } else {
        output <- paste0(output,"GG_zweiParteien;")  
      }
  } else if ( (anzahl_sitze_partei$change[1] > 2) &
              (anzahl_sitze_partei$change[2] <= 2) ) {
    output <- "GG_einePartei;"  
  } else if (  (anzahl_sitze_partei$change[1] > 0) &
               (anzahl_sitze_partei$change[2] > 0) &
               (anzahl_sitze_partei$change[3] > 0) ) {
    output <- "G_mehrereParteien;" 
  } else if ( anzahl_sitze_partei$change[1] == 2) {
    if (anzahl_sitze_partei$change[2] == 2) {
      output <- paste0(output,"G_zweiParteien_2Sitze;")   
    } else if (anzahl_sitze_partei$change[2] == 1) {
      output <- paste0(output,"G_zweiParteien_2Sitze_1Sitz;")
    } else {
      output <- paste0(output,"G_einePartei_2Sitze;")
    }  
  } else if ( anzahl_sitze_partei$change[1] == 1) {
    if (anzahl_sitze_partei$change[2] == 1) {
      output <- paste0(output,"G_zweiParteien_1Sitz;")   
    } else {
      output <- paste0(output,"G_einePartei_1Sitz;")
    }  
  }
  return(output)  
}  

get_losers <- function(anzahl_sitze_partei) {
output <- ""
anzahl_sitze_partei <- anzahl_sitze_partei[order(anzahl_sitze_partei$change),]  

  if (  (anzahl_sitze_partei$change[1] < -2) &
        (anzahl_sitze_partei$change[2] < -2) &
        (anzahl_sitze_partei$change[3] < -2) ) {
    output <- paste0(output,"LL_mehrereParteien;")
  } else if ( (anzahl_sitze_partei$change[1] < -2) &
              (anzahl_sitze_partei$change[2] < -2) ) {
    if (anzahl_sitze_partei$change[1] == anzahl_sitze_partei$change[2]) {
      output <- paste0(output,"LL_zweiParteien_gleich;") } else {
        output <- paste0(output,"LL_zweiParteien;")  
      }
  } else if ( (anzahl_sitze_partei$change[1] < -2) &
              (anzahl_sitze_partei$change[2] >= -2) ) {
    output <- "LL_einePartei;"  
  } else if (  (anzahl_sitze_partei$change[1] < 0) &
               (anzahl_sitze_partei$change[2] < 0) &
               (anzahl_sitze_partei$change[3] < 0) ) {
    output <- "L_mehrereParteien;" 
  } else if ( anzahl_sitze_partei$change[1] == -2) {
    if (anzahl_sitze_partei$change[2] == -2) {
      output <- paste0(output,"L_zweiParteien_2Sitze;")   
    } else if (anzahl_sitze_partei$change[2] == -1) {
      output <- paste0(output,"L_zweiParteien_2Sitze_1Sitz;")
    } else {
      output <- paste0(output,"L_einePartei_2Sitze;")
    }  
  } else if ( anzahl_sitze_partei$change[1] == -1) {
    if (anzahl_sitze_partei$change[2] == -1) {
      output <- paste0(output,"L_zweiParteien_1Sitz;")   
    } else {
      output <- paste0(output,"L_einePartei_1Sitz;")
    }  
  }
  return(output)  
}  

special_check_nochange <- function(anzahl_sitze_partei) {
output <- ""
if ( (anzahl_sitze_partei$change[1] == 0) & (anzahl_sitze_partei$change[nrow(anzahl_sitze_partei)] == 0) ) {
  output <- "Special_alles_gleich;"
}
return(output)
}

get_sitzverteilung <- function(anzahl_sitze_partei) {
  output <- ""
  if ( (anzahl_sitze_partei$change[1] == 0) & (anzahl_sitze_partei$change[nrow(anzahl_sitze_partei)] == 0) ) {
    output <- "Special_Sitzverteilung_alles_gleich;"
  } else {
    output <- "Sitzverteilung;"  
  }
  
  anzahl_sitze_partei$seatsgone <- anzahl_sitze_partei$Sitze_alt > 0 & anzahl_sitze_partei$Sitze == 0
  if (sum(anzahl_sitze_partei$seatsgone) > 1) {
  output <- paste0(output,"Sitzverteilung_Zusatz_alle_Sitze_weg_mehrere;")  
  } else if (sum(anzahl_sitze_partei$seatsgone) == 1) {
  output <- paste0(output,"Sitzverteilung_Zusatz_alle_Sitze_weg;")    
  }
return(output)
}

get_sitzverteilung_diverse <- function(diverse_sitze) {
  output <- ""
  if (nrow(diverse_sitze) > 1) {
  output <- "Sitzverteilung_Zusatz_Diverse_mehrere;"  
  } else if (nrow(diverse_sitze) == 1) {
    if (diverse_sitze$Sitze > 1) {
    output <- "Sitzverteilung_Zusatz_Diverse_mehrereSitze;"  
    } else {
    output <- "Sitzverteilung_Zusatz_Diverse_1Sitz;"  
    }
  }  
return(output)
}

get_sitzverteilung_aufrecht <- function(aufrecht_sitze) {
  output <- ""
  if (sum(aufrecht_sitze$Sitze) > 1){
  output <-  "Sitzverteilung_Zusatz_Aufrecht_mehrereSitze;"
  } else if (sum(aufrecht_sitze$Sitze) == 1) {
  output <-   "Sitzverzeilung_Zusatz_Aufrecht_1Sitz;"
  } 
  return(output)
}

get_neu_gewaehlt <- function(candidates_neu_gewaehlt) {
  output <- ""
  if (nrow(candidates_neu_gewaehlt) > 1){
  output <- "Neu_gewaehlt_mehrere;"  
  } else if (nrow(candidates_neu_gewaehlt) == 1) {
  output <- "Neu_gewaehlt_1Person;" 
  } else { 
  output <- "Neu_gewaehlt_keine;"
  }
  return(output)
}

get_abgewaehlt <- function(candidates_abgewaehlt) {
  output <- ""
  if (nrow(candidates_abgewaehlt) > 1){
    output <- "Abgewaehlt_mehrere;"  
  } else if (nrow(candidates_abgewaehlt) == 1) {
    output <- "Abgewaehlt_1Person;" 
  } 
  return(output)
}
