library(dplyr)
library(stringr)
library(rgdal)
library(tmap)
library(rvest)
library(arrow)

# Descarga ####
sitio   <- "https://datos.cdmx.gob.mx/dataset/"
csv_url <- "f4be0114-49fd-4a4a-84ab-3d12b54d9435/resource/2aace691-0dc3-4e80-9564-92d56a9e9022/download/escuelas-publicas.csv"
shape_url   <- "f4be0114-49fd-4a4a-84ab-3d12b54d9435/resource/36f5678b-1b48-43c3-bd09-8ec7751aad12/download/escuelas_publicas.zip"

# mode = "wb" es necesario para Windows
download.file(paste0(sitio, csv_url),   mode = "wb", "escuelas_publicas.csv")
download.file(paste0(sitio, shape_url), mode = "wb", "escuelas_publicas.zip")

unzip(zipfile = "escuelas_publicas.zip")

# Lectura y procesamiento ####
shapefile <- rgdal::readOGR(dsn = "escuelas_publicas/escuelas_publicas.shp")

datos_raw <- read.csv("escuelas_publicas.csv") %>% as_tibble()


# Se muestran codificaciones a "5. Otro", pero pueden ser de mayor especificidad
# Por ejemplo, categorizar a CAM y CAPEP como Educación Especial
datos_servicio <- 
  datos_raw %>% 
  select(id, nombre) %>% 
  mutate(servicio = case_when(
    str_detect(nombre, "CENDI|LACTANTE") ~ "1. Inicial",
    str_detect(nombre, "INTERVENCION TEMPRANA") ~ "5. Otro",
    str_detect(nombre, "CAPEP") ~ "5. Otro",
    str_detect(nombre, "PREESCOLAR") ~ "2. Preescolar",
    str_detect(nombre, "PRIMARIA PARA ADULTOS") ~ "5. Otro",
    str_detect(nombre, "PRIMARIA") ~ "3. Primaria",
    str_detect(nombre, "TELESECUNDARIA") ~ "4. Secundaria",
    str_detect(nombre, "SECUNDARIA TECNICA") ~ "4. Secundaria",
    str_detect(nombre, "SECUNDARIA PARA TRABAJADORES") ~ "5. Otro",
    str_detect(nombre, "SECUNDARIA") ~ "4. Secundaria",
    str_detect(nombre, "SEC") ~ "4. Secundaria",
    TRUE ~ "5. Otro"
  )) %>% 
  select(-nombre)


# Datos adicionales ####
datos_alcaldia <- 
  datos_raw %>% 
  select(id, domicilio_con_nombre) %>% 
  mutate(alcaldia = str_extract(domicilio_con_nombre, "(?<=DELEGACION).*?(?=, MEXICO)"),
         alcaldia = str_squish(alcaldia)) %>% 
  mutate(alcaldia = str_replace(alcaldia, "Ó", "O"),
         alcaldia = str_replace(alcaldia, "AZCAPOTZALCO OBREGON", "AZCAPOTZALCO")) %>% 
  filter(!is.na(alcaldia)) %>% 
  select(-domicilio_con_nombre)

datos_simple <- 
  inner_join(datos_raw, datos_servicio, by = "id") %>% 
  inner_join(., datos_alcaldia, by = "id") %>% 
  select(id, servicio, alcaldia)


# Poblacion #####
pop_cdmx <- read_html("https://es.wikipedia.org/wiki/Demarcaciones_territoriales_de_la_Ciudad_de_M%C3%A9xico")

poblacion <- 
  pop_cdmx %>% 
  html_table() %>% 
  .[[2]] %>% 
  slice(-c(1:3)) %>% 
  select(2:5) %>% 
  na.omit() %>% 
  `names<-`(c("alcaldia", "poblacion", "superficie", "densidad")) %>% 
  mutate_at(c("poblacion", "superficie", "densidad"), 
            ~as.numeric(str_remove_all(., " "))
  ) %>% 
  mutate(alcaldia = chartr(alcaldia, old = "Ááéó", new = "Aaeo"),
         alcaldia = toupper(alcaldia))


# Esportar datos ####
datos_raw %>% 
  select(id, latitud, longitud, geopoint) %>% 
  left_join(datos_simple, by = "id") %>% 
  write_parquet("esc_publicas_cdmx.parquet")

write_parquet(poblacion, "poblacion.parquet")


# Mapa ####
paleta <- c("#BACC81", "#FBE7C6", "#A0E7E5", "#FFAEBC", "#DDDDDD")

shapefile@data <- left_join(shapefile@data, datos_servicio, by = "id")

tmap_mode("view")
tm_shape(shapefile) +
  tm_dots(
    title = "Escuelas públicas de educación básica - CDMX", 
    col = "servicio",
    palette = paleta,
    popup.vars = TRUE
  )
