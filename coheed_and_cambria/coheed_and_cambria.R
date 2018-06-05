library(rvest)
library(httr)
library(xml2)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(lubridate)
library(scales)

musicbrainz_html <- read_html("https://musicbrainz.org/release/5e5dad52-a3cf-4cf7-a222-6f4bca6b17ef")

musicbrainz_html

musicbrainz_html %>%
  html_nodes(css = "tbody tr") %>%
  html_text()

musicbrainz_html %>%
  html_nodes(css = "tbody tr") %>%
  html_text() %>%
  str_split(pattern = "\\n", simplify = T) %>%
  data.frame() %>%
  tbl_df() %>%
  slice(-1) %>%
  select(song = X5) %>%
  mutate_all(trimws)

musicbrainz_html %>%
  html_nodes(css = ".releaseheader h1") %>%
  html_text()

musicbrainz_html %>%
  html_nodes(css = ".release-date") %>%
  html_text()

obtener_canciones <- function(musicbrainz_url) {
  mi_html <-
    musicbrainz_url %>%
    read_html()

  nombre_album <-
    mi_html %>%
    html_nodes(css = ".releaseheader h1") %>%
    html_text()

  fecha_album <-
    mi_html %>%
    html_nodes(css = ".release-date") %>%
    html_text()

  canciones <-
    mi_html %>%
    html_nodes(css = "tbody tr") %>%
    html_text() %>%
    str_split(pattern = "\\n", simplify = T) %>%
    data.frame() %>%
    tbl_df() %>%
    slice(-1) %>%
    select(cancion = X5) %>%
    mutate_all(trimws)

  canciones %>%
    mutate(album = nombre_album, fecha = fecha_album)
}

obtener_canciones("https://musicbrainz.org/release/50714cf9-0f08-4632-b7ed-ea33cd05cc92")

lista_urls <-
  c(
    "https://musicbrainz.org/release/b3074be9-5d8e-4996-a68d-c8f824a2a6e6",
    "https://musicbrainz.org/release/a4bbe913-9728-44af-9edc-83f2080038cb",
    "https://musicbrainz.org/release/c80821ec-61bd-398a-9f93-60daa0387b52",
    "https://musicbrainz.org/release/88b19eac-a7bd-4f46-ba0b-dff3a1f27057",
    "https://musicbrainz.org/release/5e5dad52-a3cf-4cf7-a222-6f4bca6b17ef",
    "https://musicbrainz.org/release/50714cf9-0f08-4632-b7ed-ea33cd05cc92",
    "https://musicbrainz.org/release/a87344ff-39ab-4889-a834-51db7b828ae8",
    "https://musicbrainz.org/release/b9c9bc5d-24fc-4340-a941-c5e1ab0ff011"
  )

coheed_cambria <-
  map(lista_urls, obtener_canciones) %>%
  reduce(bind_rows)

coheed_cambria

mi_api_key <- # Tu API key

url_prueba <- paste0(
  "https://orion.apiseeds.com/api/music/lyric/",
  "Coheed and Cambria/",
  "Everything Evil",
  "?apikey=",
  mi_api_key
)

url_prueba

everything_evil <-  GET(url = url_prueba)

content(everything_evil, as = "text", encoding = "UTF-8")

cc_letras_lista <-
  map(coheed_cambria[["cancion"]], function(x){
    ruta <-  paste0(
      "https://orion.apiseeds.com/api/music/lyric/Coheed and Cambria/",
      x,
      "?apikey=",
      mi_api_key
    )

    GET(url = ruta)
  })

content(cc_letras_lista[[4]], as = "text", encoding = "UTF-8")

content(cc_letras_lista[[4]], as = "text", encoding = "UTF-8") %>%
  fromJSON()

everything_evil_json <-
  content(cc_letras_lista[[4]], as = "text", encoding = "UTF-8") %>%
  fromJSON()

everything_evil_json$result$track$text

extraer_letra <- function(contenido){
  if(!is.na(contenido)) {
    cont_json <- fromJSON(contenido)
    c(cancion = cont_json$result$track$name,
      letra = cont_json$result$track$text) %>%
      gsub("[[:cntrl:]]", " ", .) %>%
      gsub("\\[.*?\\]", " ", .) %>%
      trimws()
  } else {
    c(cancion = NA, letra = NA)
  }
}

content(cc_letras_lista[[1]], as = "text", encoding = "UTF-8")
content(cc_letras_lista[[10]], as = "text", encoding = "UTF-8")

mis_letras_df <-
  cc_letras_lista %>%
  map(~content(., as = "text", encoding = "UTF-8")) %>%
  map(~ifelse(grepl("error|Bad Request|html", .), NA, .)) %>%
  map(extraer_letra) %>%
  do.call(what = bind_rows)

mis_letras_df

mis_letras_df <-
  cc_letras_lista %>%
  map(~content(., as = "text", encoding = "UTF-8")) %>%
  map(~ifelse(grepl("error|Bad Request|html", .), NA, .)) %>%
  map(function(x) {
    if(!is.na(x)) {
      y <- fromJSON(x)
      c(cancion = y$result$track$name,
        letra = y$result$track$text) %>%
        gsub("[[:cntrl:]]", " ", .) %>%
        gsub("\\[.*?\\]", " ", .) %>%
        trimws()
    } else {
      c(cancion = NA, letra = NA)
    }
  }) %>%
  do.call(what = bind_rows)

coheed_cambria_df <-
  coheed_cambria %>%
  left_join(., mis_letras_df, by = "cancion")

coheed_cambria_df

coheed_cambria_df %>%
  filter(!is.na(letra)) %>%
  count(album)

coheed_cambria_df <-
  coheed_cambria_df %>%
  mutate(album = case_when(
    album == "Good Apollo I’m Burning Star IV, Volume One: From Fear Through the Eyes of Madness" ~ "From Fear Through the Eyes of Madness",
    album == "Good Apollo I’m Burning Star IV, Volume Two: No World for Tomorrow" ~ "No World for Tomorrow",
    TRUE ~ as.character(album)    )
  )

coheed_cambria_df <-
  coheed_cambria_df %>%
  mutate(fecha = ymd(fecha),
         album = reorder(as.factor(album), fecha))

coheed_cambria_tokens <-
  coheed_cambria_df %>%
  unnest_tokens(input = "letra", output = "word") %>%
  inner_join(., get_sentiments(lexicon = "nrc"), by = "word") %>%
  filter(!sentiment %in% c("positive", "negative", "trust", "surprise", "anticipation"))

coheed_cambria_tokens

coheed_cambria_tokens %>%
  group_by(sentiment) %>%
  count(word, sort = T) %>%
  top_n(15) %>%
  ggplot() +
  aes(word, n, fill = sentiment) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  theme(legend.position = "none")

coheed_cambria_tokens <-
  coheed_cambria_tokens %>%
  filter(!word %in% c("words", "boy", "mother", "god", "lines"))

coheed_cambria_tokens %>%
  group_by(fecha, album) %>%
  count(sentiment) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot() +
  aes(album, prop, fill = sentiment) +
  geom_col(position = "stack", color = "black") +
  coord_flip()  +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal()

coheed_cambria_tokens %>%
  group_by(fecha, album) %>%
  count(sentiment) %>%
  ggplot() +
  aes(album, n, fill = sentiment) +
  geom_col(position = "stack", color = "black") +
  coord_flip()  +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Palabras") +
  theme_minimal()

coheed_cambria_tokens %>%
  group_by(fecha, album) %>%
  count(sentiment) %>%
  mutate(prop = n / sum(n)) %>%
  top_n(1, wt = prop) %>%
  ggplot() +
  aes(album, prop, fill = sentiment) +
  geom_col(position = "stack", color = "black") +
  coord_flip()  +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal()

coheed_cambria_tokens %>%
  group_by(fecha, album) %>%
  count(sentiment) %>%
  mutate(prop = n / sum(n)) %>%
  top_n(-1, wt = prop) %>%
  ggplot() +
  aes(album, prop, fill = sentiment) +
  geom_col(position = "stack", color = "black") +
  coord_flip()  +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal()

coheed_cambria_tokens %>%
  group_by(fecha, album) %>%
  count(sentiment) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(album = reorder(album, fecha)) %>%
  ggplot() +
  aes(album, prop, color = sentiment) +
  geom_point() +
  geom_line(aes(group = sentiment)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4),
        text = element_text(family = "serif")) +
  labs(title = "Coheed and Cambria\nSentimientos a través del tiempo",
       x = "Disco", y = "Porporción", color = "Sentimiento") +
  scale_y_continuous(labels = percent_format())

coheed_cambria_tokens %>%
  group_by(fecha, album) %>%
  count(sentiment, sort = T) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot() +
  aes(album, prop, color = sentiment, alpha = prop) +
  geom_point(aes(size = prop), fill = "white", stroke = 1, shape = 21) +
  geom_text(aes(label = sentiment, size = prop), vjust = -.9, family = "serif") +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text =  element_text(family = "serif")) +
  coord_flip() +
  labs(title = "Coheed and Cambria \nSentimientos en las letras",
       x = "Disco",
       y = "Proporción del sentimiento")

coheed_cambria_tokens %>%
  group_by(album, cancion) %>%
  count(sentiment) %>%
  mutate(prop = n / sum(n)) %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ggplot() +
  aes(sentiment, prop, color = sentiment) +
  geom_point() +
  geom_text(aes(label = paste0(cancion, "\n", album)),
            vjust = -.3, size = 3) +
  scale_y_continuous(limits = c(0.15, 0.6)) +
  theme_minimal() +
  theme(legend.position = "none")

graficar_cancion <- function(sentimiento, cantidad = 7) {
  coheed_cambria_tokens %>%
    group_by(album, cancion) %>%
    count(sentiment) %>%
    mutate(prop = n / sum(n)) %>%
    group_by(sentiment) %>%
    top_n(cantidad) %>%
    filter(sentiment == sentimiento) %>%
    mutate(cancion = paste0(cancion, "\n(", album, ")"),
           cancion = reorder(cancion, prop)) %>%
    ggplot() +
    aes(cancion, prop) +
    geom_col(position = "dodge", fill = "#bb88ff") +
    theme_minimal() +
    theme(legend.position = "none", text = element_text(family = "serif")) +
    coord_flip() +
    labs(title = paste0("Coheed and Cambria\nCanciones con más ", sentimiento),
         x = "Canción (Disco)", y = "Proporcion") +
    scale_y_continuous(limits = c(0, .6), expand = c(0, 0), label = percent_format())
}

graficar_cancion("sadness", 5)

unique(coheed_cambria_tokens$sentiment) %>%
  map(graficar_cancion)
