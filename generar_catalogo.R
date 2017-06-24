# Script para crear crear el catálogo de patrimonio de Campo de Criptana
# Author: Alfredo Sánchez Alberca (asalber@gmail.com)

# URL de la hoja de cálculo con los datos
url.data <- "https://docs.google.com/spreadsheets/d/1hWyDiPwVU1oUflqVSC6iSveP-NigMpZhh9e7RKXZpJk/pub?output=csv"
library(RCurl)
library(rmarkdown)
library(yaml)
library(dplyr)
library(knitr)


# Carga de los datos
data <- read.csv(text = getURL(url.data, .encoding = "UTF-8"), encoding = "UTF-8", header = T, stringsAsFactors = F)
data <- data %>% arrange(Título)
headers <- gsub(".", " ", names(data), fixed=T)

# Generar las páginas de los lugares
for (i in 1:nrow(data)) {
  record <- data[i,]
  file.name <- gsub(" ", "-", tolower(iconv(record$Título, to='ASCII//TRANSLIT')))
  file.name <- gsub("/", "-", file.name)
  # Descargar fotos
  url.photos <- trimws(unlist(strsplit(
    gsub("/view\\?usp=sharing", "",
         gsub("https://drive.google.com/file/d/", "https://drive.google.com/uc?export=download&id=", record$Fotos)), ",")))
  photos <- NULL
  if (length(url.photos)>0) {
    for (i in 1:length(url.photos)) {
      photos[i] <- paste("img/", file.name, "-", i, ".jpg", sep="")
      download.file(url.photos[i], photos[i], method="wget", mode="w")
    }
    banner = paste0("/", photos[1])
  } else {
    banner="/img/home-bg.jpg"
  }
  knit("record.Rmd", output = paste0("lugares/",file.name,".md"))
}


# Generar el índice 
setwd("./lugares")
knit("index.Rmd", output = paste0("index.md"))
