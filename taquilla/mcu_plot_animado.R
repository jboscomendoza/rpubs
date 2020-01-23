library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
library(scales)

mcu <- read_csv("mcu.csv")

mcu_procesado <- 
  mcu %>% 
  select(Fecha, Pelicula, Diario) %>% 
  mutate(
    Fecha = ymd( paste(year(Fecha), month(Fecha), "01", sep = "-" ))
  ) %>% 
  group_by(Fecha) %>% 
  mutate(Reciente = last(Pelicula),
         Total = sum(Diario)) %>% 
  ungroup() %>% 
  arrange(Fecha) %>% 
  select(-c(Diario)) %>% 
  distinct() %>% 
  mutate(Acumulado = cumsum(Total))

fechas_extra <- 
  tibble(
  Fecha = seq.Date(ymd("2008-05-01"), ymd("2019-12-01"), by = "month")
  )

mcu_para_plot <- 
  left_join(fechas_extra, mcu_procesado, by = "Fecha") %>% 
  fill(-Fecha) %>% 
  mutate(
    Fecha_texto = tools::toTitleCase(
      paste(
        month(Fecha, label = TRUE, abbr = FALSE), 
        year(Fecha), sep = "\n"
      )
    )
  ) 

plot_animado <- 
  mcu_para_plot %>% 
  ggplot() +
  aes(Fecha, Acumulado) +
  geom_point() +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = 
                  paste(dollar(Acumulado), Reciente, sep = "\n")
  ), vjust = -0.25) +
  scale_x_date(
    date_labels = "%Y", 
    breaks = seq.Date(ymd("2008-03-01"), ymd("2020-12-01"), by = "year"),
    limits = c(ymd("2007-12-01"), 
               ymd("2021-5-01"))
  ) +
  scale_y_continuous(
    labels = dollar_format(),
                     limits = c(0, 15000000000),
                     breaks = seq(0, 15000000000, by = 2500000000)
    ) +
  labs(x = "Año", y = "Taquilla de Estados Unidos (USD)") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
transition_reveal(Fecha)


anim_save(
  filename = "mcu.gif", 
  animation = animate(plot_animado, duration = 10, 
                      start_pause = 5, end_pause = 5, 
                      renderer = gifski_renderer(), 
                      height = 400, width = 600)
)
