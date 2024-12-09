# install.packages(c("tidyverse", "httr2"))
library(tidyverse)
library(httr2)


# Obten una API key de last.fm en:
# https://www.last.fm/api
api_key <- ""

lastfm_root <- "http://ws.audioscrobbler.com/2.0/"

usuario <- "zegim"
periodo <- "12month"
limite_artistas  <- "50"
limite_canciones <- "50"


# Artistas más reproducidos ####
method_artistas <- "user.gettopartists"

request_artistas <- paste0(
  lastfm_root,
  "?method=", method_artistas,
  "&user=", usuario,
  "&api_key=", api_key,
  "&period=", periodo,
  "&limit=", limite_artistas,
  "&format=json"
)

contenido_artistas <- 
  request(request_artistas) %>% 
  req_perform() %>% 
  resp_body_json()

df_artistas <- 
  map_df(contenido_artistas$topartists$artist, function(x_artist) {
  tibble(
    "artista" = x_artist[["name"]],
    "reproducciones" = as.numeric(x_artist[["playcount"]]))
})


# Canciones más reproducidas ####
method_canciones <- "user.gettoptracks"

request_canciones <- paste0(
  lastfm_root,
  "?method=", method_canciones,
  "&user=", usuario,
  "&api_key=", api_key,
  "&period=", periodo,
  "&limit=", limite_canciones,
  "&format=json"
)

contenido_canciones <- 
  request(request_canciones) %>% 
  req_perform() %>% 
  resp_body_json()

df_canciones <- 
  map_df(contenido_canciones$toptracks$track, function(x){
    tibble(
      "cancion" = x[["name"]],
      "artista" = x[["artist"]][["name"]],
      "reproducciones" = as.numeric(x[["playcount"]]),
      "duracion" = as.numeric(x[["duration"]])
    )
  })

df_canciones <- 
  df_canciones %>% 
  mutate(
    duracion = ifelse(duracion == 0, NA, duracion),
    duracion = ifelse(is.na(duracion), median(duracion, na.rm = TRUE), duracion)
  )


# Tags principales ####
artistas <- df_artistas$artista

# Función para obtener tags por artista
get_artist_tags <- function(artist, api_key, limit=15) {
  method_tags <- "artist.getTopTags"
  request_tags <- paste0(
    lastfm_root,
    "?method=", method_tags,
    "&artist=", str_replace_all(artist, " ", "+"),
    "&api_key=", api_key,
    "&format=json"
  )

  contenido_tags <- 
    request(request_tags) %>% 
    req_perform() %>% 
    resp_body_json()
  
  if (is.null(contenido_tags[["error"]])) {
    map_df(contenido_tags$toptags$tag[1:limit], function(tag) {
      tibble("artista" = artist, "tag" = tag[["name"]])
    })
  } else {
    NULL
  }
}


df_tags <- map_df(artistas, function(artista) {
  Sys.sleep(0.01)
  get_artist_tags(artista, api_key)
})

df_tags <- 
  df_tags %>%  
  mutate(tag = str_to_lower(tag))

# Artistas con más tiempo reproducido
df_tiempo <-
  df_canciones %>%
  mutate(tiempo = reproducciones * duracion) %>%
  group_by(artista) %>%
  summarise(tiempo = sum(tiempo)) %>%
  ungroup() %>%
  arrange(desc(tiempo)) %>%
  mutate(
    minutos = tiempo /60,
    horas = minutos / 60
  )


# Plots ####
# Tags principales
df_tags %>%
  mutate(tag = str_to_lower(tag)) %>%
  filter(!tag %in% c(NA, "seen live")) %>%
  count(tag, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  mutate(tag = reorder(tag, n, decreasing = FALSE)) %>%
  ggplot() +
  aes(tag, n) +
  coord_flip() +
  geom_col(fill = "#967aa1") +
  geom_text(
    aes(label = n),
    hjust = 1.5,
    color = "#ffffff",
  ) +
  geom_text(
    aes(label = tag, y = .2),
    hjust = "left",
    color = "#ffffff",
    size = 2.5
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Tags principales", x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

ggsave(
  "lastfm_tags.png", 
  bg = "#ffffff",
  units = "px", height = 400, width = 600, 
  dpi = 300, scale = 2
)


# Artistas más reproducidos
df_artistas %>%
  top_n(10, wt = reproducciones) %>%
  mutate(artista = reorder(artista, reproducciones, decreasing = FALSE)) %>%
  ggplot() +
  aes(artista, reproducciones) +
  coord_flip() +
  geom_col(fill = "#1a759f") +
  geom_text(
    aes(label = reproducciones),
    hjust = 1.5,
    color = "#ffffff",
  ) +
  geom_text(
    aes(label = artista, y = 5),
    hjust = "left",
    color = "#ffffff",
    size = 2.5
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Artistas más reproducidos", x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

ggsave(
  "lastfm_artistas.png", 
  bg = "#ffffff",
  units = "px", height = 400, width = 600, 
  dpi = 300, scale = 2
  )


# Canciones más reproducidas
df_canciones %>%
  top_n(10, wt = reproducciones) %>%
  mutate(cancion = paste0(cancion, " [", artista, "]")) %>%
  mutate(cancion = reorder(cancion, reproducciones, decreasing = FALSE)) %>%
  ggplot() +
  aes(cancion, reproducciones) +
  coord_flip() +
  geom_col(fill = "#55a630") +
  geom_text(
    aes(label = reproducciones),
    hjust = 1.5,
    color = "#ffffff",
  ) +
  geom_text(
    aes(label = cancion, y = 0.5),
    hjust = "left",
    color = "#ffffff", 
    size = 2.5
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Canciones más reproducidas", x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

ggsave(
  "lastfm_canciones.png", 
  bg = "#ffffff",
  units = "px", height = 400, width = 600, 
  dpi = 300, scale = 2
)


# Tiempo reproducido
df_tiempo %>%
  mutate(
    artista = reorder(artista, tiempo, decreasing = FALSE),
    minutos = round(minutos)
    ) %>%
  top_n(10, wt = tiempo) %>%
  ggplot() +
  aes(artista, round(minutos)) +
  coord_flip() +
  geom_col(fill = "#b5838d") +
  geom_text(
    aes(label = minutos),
    hjust = "right",
    nudge_y = -3,
    color = "#ffffff",
  ) +
  geom_text(
    aes(label = artista, y = 3),
    hjust = "left",
    color = "#ffffff",
    size = 2.5, 
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Artistas con más tiempo reproducido\n(minutos)", x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

ggsave(
  "lastfm_tiempo.png", 
  bg = "#ffffff",
  units = "px", height = 400, width = 600, 
  dpi = 300, scale = 2
)
