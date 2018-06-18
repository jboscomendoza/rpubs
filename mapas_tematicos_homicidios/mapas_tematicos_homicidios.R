library(tidyverse)
library(data.table)
library(tmap)
library(tmaptools)
library(Hmisc)

# https://datos.gob.mx/busca/dataset/mapa-de-division-politica-de-mexico
# http://secretariadoejecutivo.gob.mx/incidencia-delictiva/incidencia-delictiva-datos-abiertos.php
# http://www.conapo.gob.mx/es/CONAPO/Proyecciones_Datos

download.file("http://www.conapo.gob.mx/work/models/CONAPO/Proyecciones/Datos/Bases_de_Datos/Proyecciones_Municipios/R/baseprymunMX_r.zip", mode = "wb")

unzip("mapa_division_politica.zip")

mex <-
  read_shape("mapa_division_politica/division_politica_municipal/Municipios_2010_5A.shp",
             as.sf = TRUE)

mex

names(mex) <- tolower(names(mex))

mex <- mutate(mex, cve = paste0(cve_ent, cve_mun))

crimen <-
  fread("Municipal_Delitos_2015_2018_abr18.csv",
        drop = c("Entidad","Municipio", "Bien jurídico afectado",
                 "Modalidad"))

# Los nombres originales
names(crimen)

# Cambios
names(crimen) <-
  names(crimen) %>%
  tolower() %>%
  gsub("\\W+", "_", .) %>%
  gsub("año", "periodo", .)

# Los nombres modificados
names(crimen)

homicidio <-
  crimen[periodo == 2017 & subtipo_de_delito == "Homicidio doloso"]

homicidio <-
  homicidio %>%
  tbl_df() %>%
  group_by(clave_ent, cve_municipio) %>%
  select(enero:diciembre) %>%
  gather(key = "mes", value = "numero", enero:diciembre) %>%
  summarise(homicidios = sum(numero)) %>%
  ungroup()


homicidio <-
  homicidio %>%
  rename(cve_ent = clave_ent, cve_mun = cve_municipio) %>%
  mutate_at(c("cve_ent", "cve_mun"), as.character) %>%
  mutate(cve_ent = ifelse(nchar(cve_ent) == 1, paste0("0", cve_ent), cve_ent),
         cve_mun = ifelse(nchar(cve_mun) == 4, substr(cve_mun, 2, 4), substr(cve_mun, 3, 5)),
         cve = paste0(cve_ent, cve_mun))

homicidio_cve <-
  homicidio %>%
  group_by(cve) %>%
  summarise(homicidios = sum(homicidios)) %>%
  ungroup()

hist(homicidio_cve$homicidios)

homicidio_cve <-
  homicidio_cve %>%
  mutate(h_cat = cut2(homicidios, cuts = c(0, 1, 5, 10, 50, 100)))

unzip("baseprymunMX_r.zip")
load("baseprymunMX_r.rdata")

pob_cve <-
  baseprymunMX %>%
  filter(año == 2017) %>%
  group_by(cvegeo) %>%
  summarise(poblacion = sum(pob))

mex <- append_data(shp = mex, data = homicidio_cve,
                   key.shp = "cve",
                   key.data = "cve")

mex

mex <- append_data(shp = mex, data = pob_cve,
                   key.shp = "cve",
                   key.data = "cvegeo")

tm_shape(mex) +
  tm_fill(col = "homicidios")

paleta_rojo <- colorRampPalette(c("white", "red"))

tm_shape(mex) +
  tm_fill(col = "homicidios",
          breaks = c(0, 1, 10, 50, 100, 500, 1000),
          palette = paleta_rojo(6))

tm_shape(mex) +
  tm_fill(col = "h_cat",
          palette = paleta_rojo(6))

mex <-
  mex %>%
  mutate(tasa_hom = (homicidios / (poblacion / 1000)) )

hist(mex$tasa_hom)

mex <-
  mex %>%
  mutate(tasa_cat = cut2(tasa_hom, cuts = c(0, 0.001, .5, 1, 1.5, 2),
                         digits = 2))

tm_shape(mex) +
  tm_fill(col = "tasa_cat",
          palette = paleta_rojo(5))
