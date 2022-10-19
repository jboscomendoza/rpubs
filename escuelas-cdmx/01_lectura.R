library(dplyr)
library(stringr)
library(rvest)
library(arrow)

# Descarga ####
sitio <- "https://datos.cdmx.gob.mx/dataset/"
url_csv_publicas <- "f4be0114-49fd-4a4a-84ab-3d12b54d9435/resource/2aace691-0dc3-4e80-9564-92d56a9e9022/download/escuelas-publicas.csv"
url_csv_privadas <- "6ab735f4-a764-449b-8e1e-f527f7fd304d/resource/72f904fb-3d44-48f4-a376-6aa635d06acc/download/escuelas-privadas.csv"

# mode = "wb" es necesario para Windows
download.file(paste0(sitio, url_csv_publicas), mode = "wb", "escuelas_publicas.csv")
download.file(paste0(sitio, url_csv_privadas), mode = "wb", "escuelas_privadas.csv")


# Publicas - Lectura y procesamiento ####
datos_publicas <- read.csv("escuelas_publicas.csv") %>% as_tibble()


# Se muestran codificaciones a "5. Otro", pero pueden ser de mayor especificidad
# Por ejemplo, categorizar a CAM y CAPEP como Educación Especial
datos_nivel <- 
  datos_publicas %>% 
  select(id, nombre) %>% 
  mutate(nivel = case_when(
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

# Datos de alcaldia
datos_alcaldia <- 
  datos_publicas %>% 
  select(id, domicilio_con_nombre) %>% 
  mutate(alcaldia = str_extract(domicilio_con_nombre, "(?<=DELEGACION).*?(?=, MEXICO)"),
         alcaldia = str_squish(alcaldia)) %>% 
  mutate(alcaldia = str_replace(alcaldia, "Ó", "O"),
         alcaldia = str_replace(alcaldia, "AZCAPOTZALCO OBREGON", "AZCAPOTZALCO")) %>% 
  filter(!is.na(alcaldia)) %>% 
  select(-domicilio_con_nombre)


# Datos de nivel y alcaldia
datos_simple <- 
  inner_join(datos_publicas, datos_nivel, by = "id") %>% 
  inner_join(., datos_alcaldia, by = "id") %>% 
  select(id, nivel, alcaldia)

publicas <- 
  datos_publicas %>% 
  select(id, latitud, longitud, geopoint) %>% 
  left_join(datos_simple, by = "id") %>%
  mutate(sostenimiento = "Pública",
         id = paste0("pu_", id),
         ) %>% 
  rename("lat" = "latitud", "lon" = "longitud")


# Privadas - Lectura y procesamiento ####
datos_privadas <- 
  read.csv("escuelas_privadas.csv") %>% 
  as_tibble() %>% 
  select(id, lat, lon, coordenadas, nivel, alcaldia) %>% 
  rename("geopoint" = "coordenadas", 
         "nivel" = "nivel") %>% 
  as_tibble()

privadas <- 
  datos_privadas %>% 
  mutate(nivel = case_when(
    nivel %in% c("INICIAL", "Preescolar - Inicial *") ~ "1. Inicial",
    nivel %in% c("Preescolar") ~ "2. Preescolar",
    nivel %in% c("Primaria")   ~ "3. Primaria",
    nivel %in% c("Secundaria") ~ "4. Secundaria",
    nivel %in% c("Adultos") ~ "5. Otro",
    nivel %in% c("Especial - CAM") ~ "5. Otro",
    TRUE ~ nivel
  )) %>% 
  select(id, lat, lon, geopoint, nivel, alcaldia) %>% 
  mutate(sostenimiento = "Privada",
         id = paste0("pr_", id))


# Unir publicas y privadas ####
escuelas <- bind_rows(publicas, privadas)


# Esportar datos ####
write_parquet(escuelas, "escuelas_cdmx.parquet")


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

write_parquet(poblacion, "poblacion_cdmx.parquet")
