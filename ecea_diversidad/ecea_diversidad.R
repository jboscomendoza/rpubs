# devtools::install_github("jboscomendoza/ineeR")
library(tidyverse)
library(haven)
library(labelled)
library(ineeR)

# Obtener datos
download.file(
  "http://www.inee.edu.mx/images/stories/2018/ECEA/Bases_de_datos_2016/EVOE_EMS_16_ECEA_ESTUDIANTES_ULTIMO_GRADO.sav",
  "ecea16_est.sav", mode = "wb")

ecea_est <- read_sav("ecea16_est.sav")

ecea_est_ref$nombre <-
  ecea_est %>%
  var_label() %>%
  reduce(rbind) %>%
  tbl_df() %>%
  rename(nombre = V1)

ecea_est_ref$var <-
  ecea_est %>%
  names() %>%
  reduce(rbind) %>%
  tbl_df() %>%
  rename(variable = V1)


ecea_est <-
  map_if(ecea_est, is.labelled, as_factor) %>%
  map_if(is.factor, as.character) %>%
  tbl_df()

# Obtener nombres de variables
ecea_est_ref <- list()

ecea_est_ref <-
  ecea_est_ref %>%
  reduce(bind_cols)

obten_id <- function(texto, tabla = ecea_est_ref, desc = FALSE) {
  tabla <- tabla %>%
    filter(grepl(texto, nombre))
  if(desc) {
    pull(tabla, nombre)
  } else {
    pull(tabla, variable)
  }
}

var_id <- list()

var_id$indigena <- obten_id("indígena")
var_id$afro     <- obten_id("afro")
var_id$discrim  <- obten_id("discrim")


# Recode
ecea_est <-
  ecea_est %>%
  mutate(e74 = ifelse(e74 %in% c("Sí", NA), e74, "No"))

# Obtener estimación
estimacion_porcentaje(tabla = ecea_est, variable = "e48h", prefijo = "w_fscr",
                      peso_final = "w_est", grupo = "e73") %>%
  filter(!grepl("Perdidos", Grupo)) %>%
  separate(Grupo, into = c("Ind", "Disc"), sep = "\\.") %>%
  filter(grepl("Sí", Disc)) %>%
  ggplot() +
  aes(Disc, Porcentaje, color = Ind) +
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = Intervalo_inferior, ymax = Intervalo_superior),
                position = "dodge")

estimacion_porcentaje(tabla = ecea_est, variable = "e48h", prefijo = "w_fscr",
                      peso_final = "w_est", grupo = "e74") %>%
  filter(!grepl("Perdidos", Grupo)) %>%
  separate(Grupo, into = c("Ind", "Disc"), sep = "\\.") %>%
  filter(grepl("Sí", Disc)) %>%
  ggplot() +
  aes(Disc, Porcentaje, color = Ind) +
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = Intervalo_inferior, ymax = Intervalo_superior),
                position = "dodge")
