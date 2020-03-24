#install.packages(
#  c("tidyverse", "lubridate", "janitor", "readxl", "scales", "RColorBrewer")
#)

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(scales)
library(RColorBrewer)

hoy <- format(Sys.time(), "%Y-%m-%d")

hoy

fechas_2020 <-  
  tibble(
    fecha = seq.Date(from = ymd("2020-01-01"), to = ymd(hoy), by = "1 day")
  )

url_covid <- 
  paste0(
    "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", 
    hoy,  ".xlsx"
  )

archivo_covid <- paste0(hoy, "_covid19.xlsx")

download.file(
  url = url_covid, 
  destfile = archivo_covid, 
  mode = "wb"
)

download.file(
  url = "https://datacenter.prb.org/download/international/indicator/population/2019/csv", 
  destfile = "poblacion.csv", 
  mode = "wb"
)

read_excel(archivo_covid, n_max = 10) 

data_covid  <- 
  archivo_covid %>% 
  read_excel() %>%
  select(
    "fecha" = DateRep,
    "casos_nuevos" = Cases,
    "muertes_nuevas" = Deaths,
    "region" = `Countries and territories`,
    "geo_id" = GeoId
  ) %>% 
  mutate_at("fecha", ymd)

data_covid <- 
  data_covid %>% 
  group_by(geo_id) %>% 
  arrange(fecha, .by_group = TRUE) %>% 
  ungroup() %>% 
  split(., .$geo_id) %>% 
  map_df(
    ~right_join(., fechas_2020, by = "fecha") %>% 
      fill(region, geo_id, .direction = "down") %>% 
      filter(!is.na(geo_id)) %>% 
      mutate_at(c("casos_nuevos", "muertes_nuevas"), ~ifelse(is.na(.), 0, .)) %>% 
      mutate(
        casos_acumulados = cumsum(casos_nuevos),
        muertes_acumuladas = cumsum(muertes_nuevas)
      ) %>% 
      filter(casos_acumulados > 0) %>% 
      mutate(dia = row_number())
  )

data_covid

read_lines("poblacion.csv", n_max = 10)

read_csv("poblacion.csv", n_max = 10)

data_poblacion <- 
  read_csv("poblacion.csv", skip = 3) %>% 
  select(
    "geo_id" = FIPS,
    "nombre" = Name, 
    "tipo" = Type, 
    "periodo" = TimeFrame, 
    "pob_mill" = Data
  ) %>% 
  filter(tipo == "Country") %>% 
  select(geo_id, pob_mill) %>% 
  mutate(pob_raw = pob_mill * 10 ^ 3) 

data_combinados <- 
  inner_join(data_covid, data_poblacion, by = "geo_id")

data_combinados

data_combinados <- 
  data_combinados %>% 
  mutate(
    casos_por_mil_habitantes = casos_acumulados / pob_raw,
    muertes_por_mil_habitantes = muertes_acumuladas / pob_raw
  ) %>% 
  pivot_longer(
    cols = c("casos_nuevos", "muertes_nuevas", 
             "casos_acumulados", "muertes_acumuladas", 
             "casos_por_mil_habitantes", "muertes_por_mil_habitantes"), 
    names_to = "tipo", values_to = "valor"
  ) %>% 
  mutate_at(c("tipo", "region"), 
            ~tools::toTitleCase(.) %>% 
              str_replace_all("_", " "))

dia_hoy <- 
  data_combinados %>% 
  filter(geo_id == "MX" & fecha == hoy) %>% 
  head(1) %>% 
  pull(dia)

dia_hoy

tema_plot <- 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x =  element_blank(),
    strip.background = element_rect(fill = "#eeeeee", color = NA) 
  ) 

data_combinados %>% 
  filter(geo_id %in% c("MX", "CN", "IT", "US")) %>% 
  ggplot() +
  aes(x = fecha, y = valor, color = region) +
  geom_line(size = 0.7) +
  scale_x_date(date_labels = "%B") + 
  scale_y_continuous(expand = c(0, 0), labels = comma_format()) +
  scale_color_manual(name = "País", values = brewer.pal(4, "Spectral")) +
  facet_wrap("tipo", scales = "free_y") +
  labs(x = "Fecha", y = "Conteo") +
  tema_plot +
  theme(legend.position = "top") 

data_combinados %>% 
  filter(geo_id %in% c("MX", "CN", "IT", "US")) %>%
  ggplot() +
  aes(dia, valor, color = region) +
  geom_line(size = 0.7) +
  geom_vline(xintercept = dia_hoy, color = "red", lty = 2) +
  scale_x_continuous(breaks = seq(0, 100, by = 15)) + 
  scale_y_continuous(expand = c(0, 0), labels = comma_format()) +
  scale_color_manual(name = "País", values = brewer.pal(4, "Spectral")) +
  facet_wrap("tipo", scales = "free") +
  labs(x = "Días desde el primer caso", y = "Conteo") +
  tema_plot + 
  theme(legend.position = "top") 

data_combinados %>% 
  mutate(resaltado = ifelse(geo_id == "IT", TRUE, FALSE)) %>% 
  ggplot() +
  aes(fecha, valor, group = region, color = resaltado) +
  geom_line(size = 0.7, alpha = .3) +
  scale_x_date(date_labels = "%B") + 
  scale_y_continuous(expand = c(0, 0), labels = comma_format()) +
  scale_color_manual(name = "region", values = c("black", "red")) +
  facet_wrap("tipo", scales = "free") +
  labs(x = "Fecha", y = "Conteo") +
  tema_plot +
  theme(legend.position = "none") 

data_combinados %>% 
  mutate(resaltado = ifelse(geo_id == "IT", TRUE, FALSE)) %>% 
  ggplot() +
  aes(dia, valor, group = region, color = resaltado) +
  geom_line(size = 0.7, alpha = .25) +
  geom_vline(xintercept = dia_hoy, color = "red", lty = 2) +
  scale_x_continuous(breaks = seq(0, 100, by = 15)) + 
  scale_y_continuous(expand = c(0, 0), labels = comma_format()) +
  scale_color_manual(name = "region", values = c("black", "red")) +
  facet_wrap("tipo", scales = "free_y") +
  labs(x = "Días desde el primer caso", y = "Conteo") +
  tema_plot +
  theme(legend.position = "none") 
