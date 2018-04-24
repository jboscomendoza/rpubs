# Paquetes necesarios ----
library(twitteR)
library(tidyverse)
library(lubridate)
library(scales)

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
  rename(Precandidato = screenName,
         Favoritos = favoriteCount, RTs = retweetCount) %>%
  mutate(Tipo = case_when(
    isRetweet == TRUE ~"RT",
    !is.na(replyToSN) ~"Respuesta",
    TRUE ~"Tuit"
  )) %>%
  filter(periodo == 2017)

# Interacciones ----
png("plots/int_total.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  gather(Interaccion, Conteo, RTs, Favoritos) %>%
  group_by(Precandidato, Interaccion) %>%
  summarise(Total = sum(Conteo)) %>%
  ggplot() +
  aes(Precandidato, Total, fill = Interaccion) +
  geom_col(position = "dodge") +
  geom_text(aes(label = comma(Total)), position = position_dodge(1),
            vjust = -.5, size = 3, family = "sans") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1200000),
                     labels = comma) +
  scale_fill_manual(values = c("#e0245e", "#17bf63", "#1da1f2", "#66757f"),
                    name = "Interacción") +
  tema_plot
dev.off()

# Interaccion promedio ----
png("plots/int_promedio.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato) %>%
  summarize(Favoritos = mean(Favoritos), RTs = mean(RTs)) %>%
  gather(Interaccion, Promedio, Favoritos, RTs) %>%
  ggplot() +
  aes(Precandidato, Promedio, fill = Interaccion) +
  geom_col(position = "dodge") +
  geom_text(aes(label = comma(round(Promedio))), position = position_dodge(1),
            vjust = -.5, size = 3, family = "sans") +
  scale_fill_manual(values = c("#e0245e", "#17bf63", "#1da1f2", "#66757f"),
                    name = "Interacción") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3600),
                     labels = scales::comma) +
  tema_plot
dev.off()

# Interacciones por día del año ----
png("plots/int_dia_conteo.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, dia_per) %>%
  gather(key = Interaccion, value = Conteo, Favoritos, RTs) %>%
  ggplot() +
  aes(dia_per, Conteo, color = Precandidato) +
  geom_smooth(size = .75, fill = NA) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
  scale_y_continuous(breaks = seq(0, 5000, by = 1000), labels = comma) +
  facet_wrap(~Interaccion) +
  coord_cartesian(ylim = c(0, 5500)) +
  labs(x = "Día del año") +
  tema_plot
dev.off()

# Interacciones por día del año - Promedio ----
png("plots/int_dia_promedio.png", width = 800, height = 600, units = "px", res =120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, dia_per) %>%
  summarize(Favoritos = mean(Favoritos), RTs = mean(RTs) ) %>%
  gather(key = Interaccion, value = Promedio, Favoritos, RTs) %>%
  ggplot() +
  aes(dia_per, Promedio, color = Precandidato, fill = Precandidato) +
  geom_smooth(size = .75, fill = NA) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
  scale_y_continuous(breaks = seq(0, 5000, by = 1000), labels = comma) +
  facet_wrap(~Interaccion) +
  coord_cartesian(ylim = c(0, 5500)) +
  labs(x = "Día del año") +
  tema_plot
dev.off()

# Interacciones por mes - Conteo ----
png("plots/int_mes_conteo.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, mes) %>%
  summarize(Favoritos = sum(Favoritos), RTs = sum(RTs)) %>%
  gather(key = Interaccion, value = Conteo, RTs, Favoritos) %>%
  ggplot() +
  aes(mes, Conteo, color = Precandidato) +
  geom_point() +
  geom_line(aes(group = Precandidato)) +
  scale_color_manual(values = colores) +
  scale_x_discrete(labels = meses) +
  scale_y_continuous(labels = comma,
                     breaks = seq(from = 0, to = 250000, by = 50000)) +
  facet_wrap(~Interaccion) +
  labs(x = "Mes") +
  tema_plot
dev.off()

# Interacciones por mes - Promedio ----
png("plots/int_mes_promedio.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, mes) %>%
  summarize(RTs = mean(RTs), Favoritos = mean(Favoritos)) %>%
  gather(Interaccion, Promedio, RTs, Favoritos) %>%
  group_by(Precandidato, Interaccion) %>%
  ggplot() +
  aes(mes, Promedio, color = Precandidato) +
  geom_point() +
  geom_line(aes(group = Precandidato)) +
  scale_color_manual(values = colores) +
  scale_x_discrete(labels = meses) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~Interaccion) +
  labs(x = "Mes") +
  tema_plot
dev.off()

# interacciones por mes - porcentaje ----
png("plots/int_mes_porcentaje.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, mes) %>%
  summarize(RTs = sum(RTs), Favoritos = sum(Favoritos)) %>%
  gather(Interaccion, Conteo, RTs, Favoritos) %>%
  group_by(Precandidato, Interaccion) %>%
  mutate(Proporcion = Conteo / sum(Conteo)) %>%
  ggplot() +
  aes(mes, Proporcion, color = Precandidato) +
  geom_point() +
  geom_line(aes(group = Precandidato)) +
  scale_color_manual(values = colores) +
  scale_x_discrete(labels = meses) +
  scale_y_continuous(labels = percent) +
  labs(x = "Mes") +
  facet_wrap(~Interaccion) +
  tema_plot
dev.off()

# Interacciones por día semana - Promedio ----
png("plots/int_diasem_promedio.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, dia_sem) %>%
  summarize(RTs = mean(RTs), Favoritos = mean(Favoritos)) %>%
  gather(Interaccion, Promedio, RTs, Favoritos) %>%
  group_by(Precandidato, Interaccion) %>%
  ggplot() +
  aes(dia_sem, Promedio, color = Precandidato) +
  geom_point() +
  geom_line(aes(group = Precandidato)) +
  scale_color_manual(values = colores) +
  scale_x_discrete(labels = dias_sem) +
  scale_y_continuous(labels = comma) +
  labs(x = "Día de la semana") +
  facet_wrap(~Interaccion) +
  tema_plot
dev.off()

# Interacciones por día semana - Porcentaje ----
png("plots/int_diasem_porcentaje.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, dia_sem) %>%
  summarize(RTs = sum(RTs), Favoritos = sum(Favoritos)) %>%
  gather(Interaccion, Conteo, RTs, Favoritos) %>%
  group_by(Precandidato, Interaccion) %>%
  mutate(Proporcion = Conteo / sum(Conteo)) %>%
  ggplot() +
  aes(dia_sem, Proporcion, color = Precandidato) +
  geom_point() +
  geom_line(aes(group = Precandidato)) +
  scale_color_manual(values = colores) +
  scale_x_discrete(labels = dias_sem) +
  scale_y_continuous(labels = percent) +
  labs(x = "Día de la semana") +
  facet_wrap(~Interaccion) +
  tema_plot
dev.off()

# Conteo de interaccion - Hora ----
png("plots/int_hora_conteo.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, Hora) %>%
  summarize(Favoritos = sum(Favoritos), RTs = sum(RTs)) %>%
  gather(key = Interaccion, value = Conteo, Favoritos, RTs) %>%
  ggplot() +
  aes(Hora, Conteo, color = Precandidato) +
  geom_point() +
  geom_line(aes(group = Precandidato)) +
  scale_color_manual(values = colores) +
  facet_wrap(~Interaccion) +
  scale_y_continuous(labels = comma) +
  labs(x = "Hora del día") +
  tema_plot +
  theme(axis.text.x = element_text(size = 6))
dev.off()

# Porcentaje de interaccion - Hora ----
png("plots/int_hora_porcentaje.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, Hora) %>%
  summarize(Favoritos = sum(Favoritos), RTs = sum(RTs)) %>%
  gather(key = Interaccion, value = Conteo, Favoritos, RTs) %>%
  group_by(Precandidato, Interaccion) %>%
  mutate(Proporcion = Conteo / sum(Conteo)) %>%
  ggplot() +
  aes(Hora, Proporcion, color = Precandidato) +
  geom_point() +
  geom_line(aes(group = Precandidato)) +
  scale_color_manual(values = colores) +
  scale_y_continuous(labels = percent) +
  facet_wrap(~Interaccion) +
  labs(x = "Hora del día") +
  tema_plot +
  theme(axis.text.x = element_text(size = 6))
dev.off()

# Promedio de interacciones - Hora ---
png("plots/int_hora_promedio.png", width = 800, height = 600, units = "px", res = 120)
tuits_df %>%
  filter(Tipo == "Tuit") %>%
  group_by(Precandidato, Hora) %>%
  summarize(Favoritos = mean(Favoritos), RTs = mean(RTs)) %>%
  gather(key = Interaccion, value = Promedio, Favoritos, RTs) %>%
  ggplot() +
  aes(Hora, Promedio, color = Precandidato) +
  geom_point() +
  geom_line(aes(group = Precandidato)) +
  scale_color_manual(values = colores) +
  scale_y_continuous(labels = comma,
                     breaks = seq(from = 0, to = 5000, by = 1000)) +
  facet_wrap(~Interaccion) +
  labs(x = "Hora del día") +
  tema_plot +
  theme(axis.text.x = element_text(size = 6))
dev.off()
