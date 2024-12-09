library(tidyverse)
library(httr)


# Obten una API key de last.fm en:
# https://www.last.fm/api
api_key <- ""
lastfm_root <- "http://ws.audioscrobbler.com/2.0/"
user <- "zegim"


# Top Artists ####
met_top_artists <- "user.gettopartists"

req_top_artists <- paste0(
  lastfm_root,
  "?method=", met_top_artists,
  "&user=", user,
  "&api_key=", api_key,
  "&format=json",
  "&period=12month",
  "&limit=30"
)

res_top_artists <- GET(req_top_artists)
status_code(res_top_artists)

con_top_artists <- content(res_top_artists)
df_top_artists <- map_df(con_top_artists$topartists$artist, function(x_artist) {
  tibble(
    "artist" = x_artist[["name"]],
    "playcount" = as.numeric(x_artist[["playcount"]]))
})


# Top Tracks ####
met_top_tracks <- "user.gettoptracks"

req_top_tracks <- paste0(
  lastfm_root,
  "?method=", met_top_tracks,
  "&user=", user,
  "&api_key=", api_key,
  "&format=json",
  "&period=12month",
  "&limit=50"
)

res_top_tracks <- GET(req_top_tracks)
status_code(res_top_tracks)

con_top_tracks <- content(res_top_tracks)
df_top_tracks <- map_df(con_top_tracks$toptracks$track, function(x){
  tibble(
    "track" = x[["name"]],
    "artist" = x[["artist"]][["name"]],
    "playcount" = as.numeric(x[["playcount"]]),
    "duration" = as.numeric(x[["duration"]])
  )
})

# Get top tags ####
artists <- df_top_artists$name

get_artist_tags <- function(artist, api_key, limit=15) {
  r_artist_tags <- paste0(
    lastfm_root,
    "?method=artist.getTopTags",
    "&artist=", str_replace_all(artist, " ", "+"),
    "&api_key=", api_key,
    "&format=json"
  )

  get_artist_tags <- GET(r_artist_tags)
  if (status_code(get_artist_tags) == 200) {
    con_artist_tags <- content(get_artist_tags)
    map_df(con_artist_tags$toptags$tag[1:limit], function(x) {
      tibble("artist" = artist, "tag" = x[["name"]])
    })
  } else {
    NULL
  }
}

df_artists_tags <- map_df(artists, function(x) {
  Sys.sleep(0.05)
  get_artist_tags(x, api_key)
})

# Artistas con más tiempo reproducido
df_time_played <-
  df_top_tracks %>%
  mutate(tiempo = playcount * duration) %>%
  group_by(artist) %>%
  summarise(tiempo = sum(tiempo)) %>%
  ungroup() %>%
  arrange(desc(tiempo)) %>%
  mutate(
    minutos = tiempo /60,
    horas = minutos / 60
    )


# Plots ####
# Tags principales
df_artists_tags %>%
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
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Tags principales", x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Artistas más reproducidos
df_top_artists %>%
  top_n(10, wt = playcount) %>%
  mutate(artist = reorder(artist, playcount, decreasing = FALSE)) %>%
  ggplot() +
  aes(artist, playcount) +
  coord_flip() +
  geom_col(fill = "#1a759f") +
  geom_text(
    aes(label = playcount),
    hjust = 1.5,
    color = "#ffffff",
  ) +
  geom_text(
    aes(label = artist, y = 5),
    hjust = "left",
    color = "#ffffff",
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Artistas más reproducidos", x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Canciones más reproducidas
df_top_tracks %>%
  top_n(10, wt = playcount) %>%
  mutate(cancion = paste0(track, " [", artist, "]")) %>%
  mutate(cancion = reorder(cancion, playcount, decreasing = FALSE)) %>%
  ggplot() +
  aes(cancion, playcount) +
  coord_flip() +
  geom_col(fill = "#55a630") +
  geom_text(
    aes(label = playcount),
    hjust = 1.5,
    color = "#ffffff",
  ) +
  geom_text(
    aes(label = cancion, y = 0.5),
    hjust = "left",
    color = "#ffffff",
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Canciones más reproducidas", x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Tiempo reproducido
df_time_played %>%
  mutate(
    artist = reorder(artist, tiempo, decreasing = FALSE),
    minutos = round(minutos)
    ) %>%
  top_n(10, wt = tiempo) %>%
  ggplot() +
  aes(artist, round(minutos)) +
  coord_flip() +
  geom_col(fill = "#967aa1") +
  geom_text(
    aes(label = minutos),
    hjust = "right",
    nudge_y = -3,
    color = "#ffffff",
  ) +
  geom_text(
    aes(label = artist, y = 3),
    hjust = "left",
    color = "#ffffff",
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Artistas con más tiempo reproducido", x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )
