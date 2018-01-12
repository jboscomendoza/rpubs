# Paquetes necesarios ----
library(twitteR)
library(tidyverse)
library(tidytext)
library(lubridate)
library(scales)
library(grid)
library(tm)
library(igraph)
library(ggraph)
library(wordcloud)

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

# funciones
quitar_tilde <- function(texto) {
  chartr(texto, old = "ÁÉÍÓÚÑáéíóúñ", new = "AEIOUNaeioun")
}

# Obtener actividad de usuarios ----
candidatos <- list("JaimeRdzNL", "JoseAMeadeK", "lopezobrador_",
                   "Mzavalagc","RicardoAnayaC")

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

# Léxico
afinn <- read_csv("lexico_afinn.en.es.csv")

# tuits ----
tuits_tokens <-
  tuits_df %>%
  filter(Tipo == "Tuit") %>%
  #unnest_tokens(input = text, output = Palabra, token = "regex", pattern = "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))") %>%
  unnest_tokens(input = text, output = Palabra, token = "words") %>%
  select(Precandidato, Palabra, Favoritos, RTs, statusSource, periodo:Hora) %>%
  mutate(statusSource = gsub("<(.*)>+?", "", statusSource)) %>%
  filter(!Palabra %in% stopwords("es")) %>%
  filter(!Palabra %in% c("t.co", "https", "vía", "youtube", "solo"))

tuits_tokens %>%
  group_by(Precandidato) %>%
  count(statusSource) %>%
  mutate(prop = n / sum(n)) %>%
  filter(prop > 0.02) %>%
  ggplot() +
  aes(Precandidato, prop, fill = statusSource) +
  geom_col(color = "black")


# Frecuencia de palabras
png("plots/tokens_conteo.png", width = 800, height = 800, res = 150)
tuits_tokens %>%
  group_by(Precandidato) %>%
  count(Palabra) %>%
  top_n(10, wt = n) %>%
  ggplot() +
  aes(reorder(Palabra, n), n, fill = Precandidato) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = colores) +
  labs(x = "Palabra", y = "Conteo") +
  coord_flip() +
  facet_wrap(~Precandidato, scales = "free", nrow = 3) +
  tema_plot +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6))
dev.off()

# Nubes de palabras ----
map(1:5, function(x) {
  pre_actual  <- candidatos[[x]]
  col_actual  <- colores[[x]]
  nom_archivo <- paste0("plots/wordcloud_", pre_actual, ".png")
  mis_cols    <- colorRampPalette(colors = c("#cccccc", col_actual))


  png(nom_archivo, width = 800, height = 600, res = 150)
  tuits_tokens %>%
    filter(Precandidato == pre_actual) %>%
    mutate_at("Palabra", quitar_tilde) %>%
    pull(Palabra) %>%
    wordcloud(max.words = 50, colors = mis_cols(8), scale = c(3, .4), random.order = F)
  dev.off()
  })

# Bigramas
tuits_bigram <-
  tuits_df %>%
  filter(Tipo == "Tuit") %>%
  mutate_at("text", quitar_tilde) %>%
  mutate_at("text", removeWords, words = stopwords("es")) %>%
  mutate_at("text", removeWords, words = c("t.co", "https", "vía", "youtube", "solo")) %>%
  #unnest_tokens(input = text, output = Palabra, token = "regex", pattern = "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))") %>%
  unnest_tokens(input = text, output = Palabra, token = "ngrams", n = 2) %>%
  select(Precandidato, Palabra, Favoritos, RTs, statusSource, periodo:Hora) %>%
  mutate(statusSource = gsub("<(.*)>+?", "", statusSource))

png("plots/bigramas_frecuencia.png", width = 800, height = 800, res = 150)
tuits_bigram %>%
  group_by(Precandidato) %>%
  count(Palabra) %>%
  mutate(Palabra = reorder(Palabra, n)) %>%
  top_n(10, wt = n) %>%
  ggplot() +
  aes(Palabra, n, fill = Precandidato) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = colores) +
  labs(x = "Bigramas", y = "Conteo") +
  facet_wrap(~Precandidato, scales = "free", nrow = 3) +
  tema_plot +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6))
dev.off()

# Afinn ---
inner_join(tuits_tokens, afinn, by = "Palabra") %>%
  group_by(Precandidato, dia_per) %>%
  summarize(Sentimiento = sum(Puntuacion)) %>%
  ggplot() +
  aes(dia_per, Sentimiento, fill = Precandidato)  +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = colores) +
  facet_grid(Precandidato~.) +
  tema_plot +
  theme(legend.position = "none")

png("plots/afinn_tendencia.png", width = 800, height = 800, res = 150)
inner_join(tuits_tokens, afinn, by = "Palabra") %>%
  group_by(Precandidato, dia_per) %>%
  summarize(Sentimiento = sum(Puntuacion)) %>%
  mutate(Sentimiento = zoo::rollmean(Sentimiento, 3, fill = NA, align = "right")) %>%
  ggplot() +
  aes(dia_per, Sentimiento, color = Precandidato) +
  geom_hline(yintercept = 0, color = "#999999") +
  geom_line(aes(group = Precandidato), size = .4) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Día del año") +
  tema_plot +
  facet_grid(Precandidato~.) +
  theme(legend.position = "none") +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6))
dev.off()

# Palabras positivas, negativas ---
map(list("Positivo", "Negativo"), function(valor) {

  nom_archivo <- paste0("plots/afinn_palabras_", valor, ".png")

  mi_plot <-
  inner_join(tuits_tokens, afinn, by = "Palabra") %>%
    mutate(Puntuacion = ifelse(Puntuacion > 0, "Positivo", "Negativo")) %>%
    filter(Puntuacion == valor) %>%
    group_by(Precandidato) %>%
    count(Palabra, Puntuacion) %>%
    group_by(Precandidato, Puntuacion) %>%
    arrange(desc(n)) %>%
    top_n(10, n) %>%
    ggplot() +
    aes(Palabra, n, fill = Precandidato) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = colores) +
    facet_wrap(~Precandidato, scales = "free", nrow = 3) +
    tema_plot +
    theme(legend.position = "none",
          axis.text = element_text(size = 6),
          strip.text = element_text(size = 6))

  png(nom_archivo, width = 800, height = 800, res = 150)
  print(mi_plot)
  dev.off()

})

# Balance negativo, positivo ----
png("plots/afinn_.png", width = 800, height = 600, res = 150)
inner_join(tuits_tokens, afinn, by = "Palabra") %>%
  mutate(Puntuacion = ifelse(Puntuacion > 0, "Positivo", "Negativo")) %>%
  group_by(Precandidato) %>%
  count(Puntuacion) %>%
  mutate(Prop = n / sum(n)) %>%
  ggplot() +
  aes(Precandidato, Prop, fill = Puntuacion) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(Prop, 2) * 100, "%")),
            position = position_dodge(width = .9), vjust = -.4, size = 2) +
  scale_y_continuous(expand = c(0, 0), labels = percent, limits = c(0, 1)) +
  scale_fill_manual(values = c("#37cc66", "#cc374f")) +
  labs(y = "Proporción") +
  tema_plot
dev.off()

# Red de palabras ----
map(1:5, function(x) {
  nom_pre <- candidatos[[x]]
  col_pre <- colores[[x]]

    bigramas <-
    tuits_bigram %>%
    filter(Precandidato == nom_pre) %>%
    count(Palabra)

  umbral <- round(nrow(bigramas) * .00075)

  bigramas %>%
    filter(n > umbral) %>%
    separate(Palabra, into = c("Palabra1", "Palabra2"), sep = " ") %>%
    graph_from_data_frame() %>%
    ggraph(graph = ., layout = "fr") +
    geom_edge_link(show.legend = ,
                   arrow = arrow(type = "closed", length = unit(2.3, units = "mm"))) +
    geom_node_point(color = col_pre, size = 2) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void() +
    theme(text = element_text(family = "sans"))

})
