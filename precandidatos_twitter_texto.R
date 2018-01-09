# Paquetes necesarios ----
library(twitteR)
library(tidyverse)
library(tidytext)
library(lubridate)
library(scales)
library(tm)

# Carpeta para exportar gráficos ----
dir.create("plots")

# Autenticación en Twitter ----
consumer_key    <- "HEcfJtIeeivscKRHnpugc8hMt"
  consumer_secret <- "FHfQsOyJ5iv6taqJaBYwhIRsFN550hxsGnviOZCy5vHxkJ3StV"
  access_token    <- "13195602-hN9TSbZocvxrVDIuoFq825aEYUUqAcRMwroBpWJ0x"
  access_secret   <- "B6hS6PyG5XgyugDz4uPx2xy3PPdnfOh7ozCG2OkwEKEFb"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Tema para gráficos ----
windowsFonts(sans = "Euphemia")

tema_plot <-
  theme(text = element_text(family = "sans", size = 10),
        panel.border = element_rect(color = "#cccccc", fill = NA),
        panel.background = element_rect(fill = "white"),
        panel.grid.major =  element_line(color = "#dddddd"),
        panel.grid.minor =  element_line(color = "#eeeeee"),
        axis.ticks = element_line(colour = "#cccccc"),
        strip.background = element_rect(color = "#cccccc", fill = "#eeeeee"),
        legend.position = "top")

colores <- c(
  "#cca711", # Jaime Rodríguez Calderón
  "#29b255", # José Antonio Meade
  "#b5261e", # Andrés Manuel López Obrador
  "#47bec6", # Margarita Zavala
  "#3660be"  # Ricardo Anaya
)

meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct",
           "Nov", "Dic")
dias_sem <- c("Lun", "Mar", "Mie", "Jue", "Vie", "Sab", "Dom")

# Obtener actividad de usuarios ----
candidatos <- list("lopezobrador_", "Mzavalagc","RicardoAnayaC",
                   "JoseAMeadeK", "JaimeRdzNL")

tuits <- list()

tuits <-
  map(candidatos, function(x){
    userTimeline(user = x, n = 3200, includeRts = T, excludeReplies = F) %>%
      twListToDF()
  })

tuits_df <-
  tuits %>%
  do.call(args = ., what = rbind) %>%
  tbl_df %>%
  mutate(text = gsub("[^[:graph:]]", " ", text)) %>%
  mutate(text = tolower(text)) %>%
  mutate(created = with_tz(created, "America/Mexico_City")) %>%
  separate(created, into = c("fecha", "horadia"), sep = " ") %>%
  mutate(periodo = year(fecha),
         mes = month(fecha, label = T, abbr = F),
         dia = as.numeric(day(fecha)),
         dia_sem = wday(fecha, label = T, abbr = F, week_start = 1),
         dia_per = yday(fecha),
         fecha = as.Date(fecha),
         Hora = substr(horadia, 1, 2)) %>%
  rename(Precandidato = screenName,
         Favoritos = favoriteCount, RTs = retweetCount) %>%
  mutate(Tipo = case_when(
    isRetweet == TRUE ~"RT",
    !is.na(replyToSN) ~"Respuesta",
    TRUE ~"Tuit"
  )) %>%
  filter(periodo == 2017)

# tuits ----
tuits_tokens <-
  tuits_df %>%
  filter(Tipo == "Tuit") %>%
  #unnest_tokens(input = text, output = Palabra, token = "regex", pattern = "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))") %>%
  unnest_tokens(input = text, output = Palabra, token = "words") %>%
  select(Precandidato, Palabra, Favoritos, RTs, statusSource, periodo:Hora) %>%
  mutate(statusSource = gsub("<(.*)>+?", "", statusSource))

tuits_tokens %>%
  group_by(Precandidato) %>%
  count(statusSource) %>%
  mutate(prop = n / sum(n)) %>%
  filter(prop > 0.02) %>%
  ggplot() +
  aes(Precandidato, prop, fill = statusSource) +
  geom_col(color = "black")

tuits_tokens %>%
  filter(!Palabra %in% stopwords("es")) %>%
  filter(!Palabra %in% c("t.co", "https")) %>%
  group_by(Precandidato) %>%
  count(Palabra) %>%
  top_n(10, wt = n) %>%
  ggplot() +
  aes(reorder(Palabra, n), n, fill = Precandidato) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Palabra", y = "Conteo") +
  coord_flip() +
  facet_wrap(~Precandidato, scales = "free") +
  tema_plot

tuits_tokens %>%
  filter(!Palabra %in% stopwords("es")) %>%
  filter(!Palabra %in% c("t.co", "https")) %>%
  filter(mes > "agosto") %>%
  group_by(Precandidato) %>%
  count(Palabra) %>%
  top_n(10, wt = n) %>%
  ggplot() +
  aes(reorder(Palabra, n), n, fill = Precandidato) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Palabra", y = "Conteo") +
  coord_flip() +
  facet_wrap(~Precandidato, scales = "free") +
  tema_plot

