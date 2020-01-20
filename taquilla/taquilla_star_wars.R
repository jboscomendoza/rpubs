# Paquetes necesarios ----
library(janitor)
library(tidyverse)
library(rvest)

# urls de Box Office Mojo ----
ligas <- 
  list(
    ep_7 = c("Ep. 7: The Force Awakens",
             "https://www.boxofficemojo.com/release/rl2691925505"),
    ep_8 = c("Ep. 8: The Last Jedi",
             "https://www.boxofficemojo.com/release/rl2708702721"),
    ep_9 = c("Ep. 9: The Rise of Skywalker",
             "https://www.boxofficemojo.com/release/rl3305145857"),
    rone = c("Rogue One",
             "https://www.boxofficemojo.com/release/rl2557707777"),
    solo = c("Solo", 
             "https://www.boxofficemojo.com/release/rl1954383361")
  )

# Nombres de columnas de salida
columnas <- c(
  "Fecha", "DiaSemana", "Rango", "Diario", "CambioDiario", "CambioSemanal",
  "Cines", "Promedio", "Acumulado", "Dia", "Estimado", "Pelicula"
)

# Scrapping ----
salida <- 
  map_df(ligas, function(x) {
    read_html(x[[2]]) %>% 
      html_node(., ".mojo-body-table") %>% 
      html_table(trim = TRUE) %>% 
      tbl_df() %>% 
      mutate(Pelicula = x[[1]])
  }) %>% 
  clean_names()

# Procesamiento ----
new_sw <- 
  salida %>% 
  mutate_at(c("daily", "avg", "to_date", "theaters"), 
            ~str_remove_all(., "\\D") %>% 
              as.numeric()
  ) %>% 
  mutate_at(c("percent_yd", "percent_lw"), 
            ~str_remove_all(., "%|<|\\+") %>% 
              ifelse(. == "-", 0, .) %>% 
              as.numeric()
  ) %>% 
  mutate_at("date", 
            ~str_remove(salida$date, "(?<=20\\d{2}).*") %>% 
              str_remove_all(",")
  ) %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  `names<-`(columnas)

# Exportar ----
write.csv(new_sw, file = "new_sw.csv", row.names = FALSE, 
          fileEncoding = "utf-8")
