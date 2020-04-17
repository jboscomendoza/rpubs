# Fuente
# https://www.inegi.org.mx/app/ageeml/

library(tidyverse)
library(data.table)

fread("AGEEML_2020414202565.csv", encoding = "UTF-8") %>% 
  filter(Cve_Loc == 001) %>% 
  select(
    Cve_Ent, Nom_Ent, 
    Cve_Mun, Nom_Mun,
    Lat_Decimal, Lon_Decimal, Pob_Total
  ) %>% 
  as_tibble() %>% 
  {
    names(.) <- toupper(names(.))
    .
  } %>% 
  write_rds(path = "municipios.rds")
