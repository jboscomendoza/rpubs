library(tidyverse)
library(leaflet)

municipios <- read_rds("municipios.rds")
covid <- read_rds("covid.rds")

covid_loc <- 
  inner_join(covid, municipios,
    by = c("ENTIDAD_RES" = "CVE_ENT", "MUNICIPIO_RES" = "CVE_MUN")
  )

para_mapa <- 
  covid_loc %>% 
  split(covid_loc$RESULTADO) %>% 
  {
    .$Todo <-  covid_loc
    .
  } %>% 
  map2_df(
    c(as.character(unique(covid_loc$RESULTADO)), "Todos los análisis"), 
    function(tabla, tipo) {
      pob <-  tabla %>% 
        select(ENTIDAD_RES, MUNICIPIO_RES, POB_TOTAL) %>% 
        distinct()
      
      conteo <- 
        count(tabla, ENTIDAD_RES, NOM_ENT, MUNICIPIO_RES, NOM_MUN, 
              LAT_DECIMAL, LON_DECIMAL, name = "N")
      
      left_join(conteo, pob, by = c("ENTIDAD_RES", "MUNICIPIO_RES")) %>% 
        mutate(
          PROP = N / (as.numeric(POB_TOTAL) / 1000),
          PROP = round(PROP, 3),
          
          ESCALA_N = (N - min(N)) / (max(N) - min(N)),
          ESCALA_N = (ESCALA_N + .2) * 15,
          
          ESCALA_PROP = (PROP - min(PROP)) / (max(PROP) - min(PROP)),
          ESCALA_PROP = (ESCALA_PROP + .2) * 15,
          
          VARIABLE = tipo
        )
    })

write_rds(para_mapa, "para_mapa.rds")
