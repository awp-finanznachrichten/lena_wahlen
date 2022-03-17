#Bibliotheken laden
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(stringi)
library(xml2)
library(rjson)
library(jsonlite)
library(readxl)
library(git2r)
library(DatawRappr)
library(rvest)
library(readxl)

cat("Benoetigte Bibliotheken geladen\n")

#Github-Funktionen
gitcommit <- function(msg = "commit from Rstudio", dir = getwd()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}

gitstatus <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git status"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

gitadd <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git add --all"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

gitpush <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

cat("Benoetigte Funktionen geladen\n")

Listen_und_Parteien <- read_excel("Daten/LENA_Listen_Kanton_Bern.xlsx", 
                                      sheet = "Listen_Parteien")

Sitzverteilung_Historisch <- read_excel("Daten/LENA_Listen_Kanton_Bern.xlsx", 
                                  sheet = "Sitzverteilung_2018")

Gemeinden_Wahlkreise <- read_excel("Daten/Bern_Wahlkreise.xlsx")

cat("Listen und Sitzverteilunge geladen\n")


Textbausteine <- read_excel("Daten/Textbausteine_LENA_Wahlen_Kantonal.xlsx")

cat("Texte geladen\n")



