# Paquetes necesarios ----
library(twitteR)
library(tidyverse)
library(lubridate)

# Carpeta para exportar gráficos ----
dir.create("plots")

# Autenticación en Twitter ----
consumer_key    <- # Tu Consumer Key
consumer_secret <- # Tu Consumer Secret
access_token    <- # Tu Access Token
access_secret   <- # Tu Access Secret

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
  rename(Candidato = screenName) %>%
  mutate(Tipo = case_when(
    isRetweet == TRUE ~"RT",
    !is.na(replyToSN) ~"Respuesta",
    TRUE ~"Tuit"
  )) %>%
  filter(periodo == 2017)

# Tuits, Respuestas y RTs ----
png("plots/tuits_total.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  count(Candidato, Tipo) %>%
  ggplot() +
  aes(Candidato, n, fill = Tipo) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2500),
                     labels = scales::comma) +
  scale_fill_manual(values = c("#1da1f2", "#17bf63","#66757f")) +
  tema_plot +
  labs(x = "Precandidato", y = "Conteo")
dev.off()

# Proporcion Tuits (No usada) ---
png("plots/tuits_prop.png", width = 800, height = 600, units = "px",
    res = 120)
tuits_df %>%
  count(Candidato, Tipo) %>%
  group_by(Candidato) %>%
  mutate(Prop = n / sum(n)) %>%
  ggplot() +
  aes(Candidato, Prop, fill = Tipo) +
  geom_col() +
  scale_fill_manual(values = c("#1da1f2", "#17bf63","#66757f")) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  tema_plot +
  labs(x = "Precandidato", y = "Proporción")
dev.off()

# Tuits por día (No usado) ----
png("plots/tuits_dia_cont.png", width = 800, height = 700, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  count(Candidato, dia_per, Tipo) %>%
  ggplot() +
  aes(dia_per, n, color = Candidato) +
  geom_point(size = 1, alpha = .25) +
  scale_color_manual(values = colores) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  facet_wrap(~Candidato, nrow = 3) +
  tema_plot +
  theme(legend.position = "none") +
  labs(x = "Día del año", y = "Conteo")
dev.off()

# Tutis por dia - Línea de tendencia ----
png("plots/tuits_dia_superpuesto.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  count(Candidato, dia_per, Tipo) %>%
  ggplot() +
  aes(dia_per, n, color = Candidato) +
  geom_smooth(fill = NA, size = .75) +
  scale_color_manual(values = colores) +
  scale_x_continuous(expand = c(0, 0)) +
  tema_plot +
  labs(x = "Día del año", y = "Conteo")
dev.off()

# Tuits por Hora - Conteo (No usado) ----
png("plots/tuits_hora_conteo.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  mutate(Hora = substr(Hora, 1 , 2)) %>%
  count(Candidato, Hora, Tipo) %>%
  ggplot() +
  aes(Hora, n, color = Candidato) +
  geom_point() +
  geom_line(aes(group = Candidato)) +
  scale_color_manual(values = colores) +
  tema_plot +

  labs(x = "Hora del día", y = "Conteo")
dev.off()

# Tuits por Hora - Porcentaje ----
png("plots/tuits_hora_porcentaje.png", width = 800, height = 700, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  count(Candidato, Hora, Tipo) %>%
  group_by(Candidato) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot() +
  aes(Hora, prop, color = Candidato) +
  geom_point() +
  geom_line(aes(group = Candidato)) +
  scale_y_continuous(label = scales::percent) +
  scale_color_manual(values = colores) +
  facet_wrap(~Candidato, nrow = 3) +
  tema_plot +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Hora del día", y = "Proporción")
dev.off()

# Tuits por día de la semana - Conteo (No usado) ----
png("plots/tuits_semana_conteo.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  count(Candidato, dia_sem, Tipo) %>%
  group_by(Candidato) %>%
  ggplot() +
  aes(dia_sem, n, color = Candidato) +
  geom_point() +
  geom_line(aes(group = Candidato)) +
  scale_x_discrete(labels = dias_sem) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = colores) +
  tema_plot +
  labs(x = "Día de la semana", y = "Conteo")
dev.off()

# Tuits por día de la semana - Porcentaje ----
png("plots/tuits_semana_porcentaje.png", width = 800, height = 700, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  count(Candidato, dia_sem, Tipo) %>%
  group_by(Candidato) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot() +
  aes(dia_sem, prop, color = Candidato) +
  geom_point() +
  geom_line(aes(group = Candidato)) +
  scale_x_discrete(labels = dias_sem) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = colores) +
  facet_wrap(~Candidato, nrow = 3) +
  tema_plot +
  theme(legend.position = "none") +
  labs(x = "Día de la semana", y = "Proporción")
dev.off()

# Tuits por mes - conteo (No usado) ----
png("plots/tuits_mes_conteo.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  count(Candidato, mes, Tipo) %>%
  group_by(Candidato) %>%
  ggplot() +
  aes(mes, n, color = Candidato) +
  geom_point() +
  geom_line(aes(group = Candidato)) +
  scale_x_discrete(labels = meses) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = colores) +
  tema_plot +
  labs(x = "Mes", y = "Conteo")
dev.off()

# Tuits por mes - Porcentaje ----
png("plots/tuits_mes_porcentaje.png", width = 800, height = 700, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  count(Candidato, mes, Tipo) %>%
  group_by(Candidato) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot() +
  aes(mes, prop, color = Candidato) +
  geom_point() +
  geom_line(aes(group = Candidato)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = meses) +
  scale_color_manual(values = colores) +
  facet_wrap(~Candidato, nrow = 3) +
  tema_plot +
  theme(legend.position = "none") +
  labs(x = "Mes", y = "Proporción")
dev.off()
