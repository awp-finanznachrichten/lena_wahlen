##Scraping von Listen und bisherigen Sitzen
url <- paste0("https://www.bewas.sites.be.ch/2018/2018-03-25/WAHL_GROSSRAT/resultatUebersicht-de.html")
webpage <- read_html(url)

links <- html_attr(html_nodes(webpage,"a"),"href")
  


links_dataframe <- as.data.frame(links)

links_dataframe$links <- gsub("resultatGemeinde-","",links_dataframe$links)
links_dataframe$links <- gsub("-de.html","",links_dataframe$links)

View(links_dataframe)
