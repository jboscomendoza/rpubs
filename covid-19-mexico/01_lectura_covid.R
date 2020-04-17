library(tidyverse)
library(readxl)
library(lubridate)

if(!dir.exists("data")) {
  dir.create("data")
}

get_reciente <- function() {
  list.files("data", full.names = TRUE) %>% 
    map(file.info) %>% 
    reduce(rbind) %>% 
    rownames_to_column(var = "archivo") %>% 
    filter(ctime == max(ctime)) %>% 
    pull(archivo)
}

url_dge <- "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"

download.file(url_dge, "datos_abiertos_covid19.zip", mode = "wb")
unzip("datos_abiertos_covid19.zip", exdir = "data")

covid <- 
  read.csv(get_reciente()) %>% 
  as_tibble() %>% 
  mutate_if(is.factor, as.character)

catalogo <- 
  excel_sheets("Catalogos_0412.xlsx") %>% 
  map(~read_excel(path = "Catalogos_0412.xlsx", sheet = .)) %>% 
  {
    nombres <- str_remove_all(excel_sheets("Catalogos_0412.xlsx"), ".*( |de )")
    names(.) <- nombres
    .
  }

si_no <- c(
  "INTUBADO", "NEUMONIA", "EMBARAZO", "HABLA_LENGUA_INDIG", "DIABETES", "EPOC", 
  "ASMA", "INMUSUPR", "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", 
  "RENAL_CRONICA", "TABAQUISMO", "OTRO_CASO", "MIGRANTE", "UCI"
)

covid <- 
  covid %>% 
  mutate_at(
    c("ORIGEN", "SECTOR", "SEXO", "TIPO_PACIENTE", "NACIONALIDAD", "RESULTADO",
      "ENTIDAD_UM", "ENTIDAD_NAC",  "PAIS_NACIONALIDAD", "PAIS_ORIGEN",
      si_no), 
    ~ifelse(. > 97, NA, .)
    ) %>% 
  mutate_at(
  "MUNICIPIO_RES",
  ~ifelse(. > 997, NA, .)
  ) %>% 
  mutate_at(
    "FECHA_DEF",
    ~ifelse(. == "9999-99-99", NA, .)
  )

covid <- 
  map(names(covid), function(x) {
    if (x %in% names(catalogo)) {
      actual <- catalogo[[x]]
      
      names(actual) <- c(x, "desc")
      
      left_join(covid[x], actual) %>% 
        select(desc) %>% 
        `names<-`(x)
    } else if (x %in% si_no) {
      case_when(
        covid[x] == 1 ~ "Sí",
        covid[x] == 2 ~ "No",
        covid[x] == 97 ~ "No aplica",
        covid[x] == 98 ~ "Se ignora",
        is.na(covid[x]) ~ "NA"
      ) %>% 
        data.frame() %>% 
        `names<-`(x)
    } else {
      covid[x]
    }
  }) %>% 
  reduce(cbind) %>% 
  as_tibble()

covid <- 
  covid %>% 
  mutate_at(c("FECHA_ACTUALIZACION", "FECHA_DEF", "FECHA_INGRESO", "FECHA_SINTOMAS"), ymd) %>% 
  map_if(is.character, factor) %>% 
  as_tibble()

write_rds(covid, "covid.rds")
