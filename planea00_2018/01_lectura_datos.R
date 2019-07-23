library(tidyverse)
library(haven)
library(labelled)

leer_datos <- function(ruta) {
  read_sav(ruta) %>% 
    map_if(is.labelled, to_factor) %>% 
    map(remove_labels) %>% 
    map_if(is.character, as_factor) %>%
    tbl_df()
}

leer_nombre_vars <- function(ruta) {
  read_sav(ruta) %>% 
    var_label()
}

archivos <- list.files(pattern = "\\.sav")

df_cuest <- map(archivos, leer_datos)
names(df_cuest) <- c("directores", "docentes", "padres")

nombre_vars <- map(archivos, leer_nombre_vars)
names(nombre_vars) <- c("directores", "docentes", "padres")
