# Paquetes necesarios ----
library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)

# urls de Box Office Mojo ----
ligas_mcu <- 
  list(
    c("Iron Man",
      "https://www.boxofficemojo.com/release/rl1482327553"),
    c("The Incredible Hulk",
      "https://www.boxofficemojo.com/release/rl2791015937"),
    c("Iron Man 2",
      "https://www.boxofficemojo.com/release/rl1515881985"),
    c("Thor",
      "https://www.boxofficemojo.com/release/rl3094644225"),
    c("Captain America: The First Avenger",
      "https://www.boxofficemojo.com/release/rl1900578305"),
    c("Marvel's The Avengers",
      "https://www.boxofficemojo.com/release/rl709199361"),
    c("Iron Man 3",
      "https://www.boxofficemojo.com/release/rl1532659201"),
    c("Thor: The Dark World",
      "https://www.boxofficemojo.com/release/rl3111421441"),
    c("Captain America: The Winter Soldier",
      "https://www.boxofficemojo.com/release/rl3194193409"),
    c("Guardians of the Galaxy",
      "https://www.boxofficemojo.com/release/rl3177416193"),
    c("Avengers: Age of Ultron",
      "https://www.boxofficemojo.com/release/rl675644929"),
    c("Ant-Man",
      "https://www.boxofficemojo.com/release/rl88245761"),
    c("Captain America: Civil War",
      "https://www.boxofficemojo.com/release/rl3210970625"),
    c("Doctor Strange",
      "https://www.boxofficemojo.com/release/rl3076752897"),
    c("Guardians of the Galaxy Vol. 2",
      "https://www.boxofficemojo.com/release/rl2976089601"),
    c("Spider-Man: Homecoming",
      "https://www.boxofficemojo.com/release/rl863208961"),
    c("Thor: Ragnarok",
      "https://www.boxofficemojo.com/release/rl2959312385"),
    c("Black Panther",
      "https://www.boxofficemojo.com/release/rl2992866817"),
    c("Avengers: Infinity War",
      "https://www.boxofficemojo.com/release/rl3043198465"),
    c("Ant-Man and the Wasp",
      "https://www.boxofficemojo.com/release/rl2088535553"),
    c("Captain Marvel",
      "https://www.boxofficemojo.com/release/rl3009644033"),
    c("Avengers: Endgame",
      "https://www.boxofficemojo.com/release/rl3059975681"),
    c("Spider-Man: Far From Home",
      "https://www.boxofficemojo.com/release/rl3791750657")
  )


# Nombres de columnas de salida_mcu
columnas <- c(
  "Fecha", "DiaSemana", "Rango", "Diario", "CambioDiario", "CambioSemanal",
  "Cines", "Promedio", "Acumulado", "Dia", "Estimado", "Pelicula"
)

# Scrapping ----
salida_mcu <- 
  map_df(ligas_mcu, function(x) {
    read_html(x[[2]]) %>% 
      html_node(., ".mojo-body-table") %>% 
      html_table(trim = TRUE) %>% 
      tbl_df() %>% 
      mutate(Pelicula = x[[1]]) %>% 
      # Cambiar dia - a 0
      mutate(Day = ifelse(Day == "-", 0, as.numeric(Day))) 
  }) %>% 
  clean_names()

# Procesamiento ----
mcu <- 
  salida_mcu %>% 
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
            ~str_remove(salida_mcu$date, "(?<=20\\d{2}).*") %>% 
              str_remove_all(",")
  ) %>% 
  mutate(date = mdy(date)) %>% 
  `names<-`(columnas) %>% 
  group_by(Pelicula) %>% 
  mutate(
    Year = year(Fecha),
    Estreno = first(Fecha),
    YearEstreno = year(Estreno),
  )

# Exportar ----
write.csv(mcu, file = "mcu.csv", row.names = FALSE, 
          fileEncoding = "utf-8")
