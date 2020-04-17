# Lectura de catalogo
library(tidyverse)
library(readxl)
library(lubridate)

url_dic <- "http://187.191.75.115/gobmx/salud/datos_abiertos/diccionario_datos_covid19.zip"


if(!file.exists("diccionario_datos_covid19.zip")) {
  download.file(url_dic, "diccionario_datos_covid19.zip", mode = "wb")
  unzip("diccionario_datos_covid19.zip")
}
