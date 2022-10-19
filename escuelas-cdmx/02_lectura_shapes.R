library(dplyr)
library(arrow)
library(rgdal)
library(tmap)


# Descarga #### 
sitio   <- "https://datos.cdmx.gob.mx/dataset/"
shape_url_publicas <- "f4be0114-49fd-4a4a-84ab-3d12b54d9435/resource/36f5678b-1b48-43c3-bd09-8ec7751aad12/download/escuelas_publicas.zip"
shape_url_privadas <- "6ab735f4-a764-449b-8e1e-f527f7fd304d/resource/f43e9251-e060-427d-a8ba-ca04008aee7d/download/escuelas_privadas.zip"

download.file(paste0(sitio, shape_url_publicas), mode = "wb", "escuelas_publicas.zip")
download.file(paste0(sitio, shape_url_privadas), mode = "wb", "escuelas_privadas.zip")

unzip(zipfile = "escuelas_publicas.zip")
unzip(zipfile = "escuelas_privadas.zip")


# Lectura ####
shape_publicas <- rgdal::readOGR(dsn = "escuelas_publicas/escuelas_publicas.shp")
shape_privadas <- rgdal::readOGR(dsn = "escuelas_privadas/escuelas_privadas.shp")

datos_nivel <- 
  read_parquet("escuelas_cdmx.parquet") %>% 
  select(id, nivel, sostenimiento) %>% 
  mutate(id = as.numeric(gsub(id, pattern = "p(u|r)_", replacement = "")))


# Union con shapes ####
shape_publicas@data <- 
  datos_nivel %>% 
  filter(sostenimiento == "Pública") %>% 
  left_join(shape_publicas@data, ., by = "id")

shape_privadas@data <- 
  datos_nivel %>% 
  filter(sostenimiento == "Privada") %>% 
  left_join(shape_privadas@data, ., by = "id")
  

# Mapas ####
paleta <- c("#BACC81", "#FBE7C6", "#A0E7E5", "#FFAEBC", "#DDDDDD")

tmap_mode("view")
tm_shape(shape_publicas) +
  tm_dots(
    title = "Escuelas públicas de educación básica - CDMX", 
    col = "nivel",
    palette = paleta,
    popup.vars = TRUE
  )

tmap_mode("view")
tm_shape(shape_privadas) +
  tm_dots(
    title = "Escuelas privadas de educación básica - CDMX", 
    col = "nivel.y",
    palette = paleta,
    popup.vars = TRUE
  )
