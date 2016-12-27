library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Archivo usado, descarga manual al archivo "muertes_07-16.xls"
# https://drive.google.com/open?id=0B5vY_A3KPmP5b2FHV3MxVnpmWEE
# La información se obtuvo de las páginas contenidas en este portal de Wikipedia en Inglés (27 dic. 2016)
# https://en.wikipedia.org/wiki/Lists_of_deaths_by_year

muertes <-
  lapply(2007:2016, function(hoja){
  read_excel("muertes_07-16.xlsx", sheet = as.character(hoja), col_names = "persona") %>%
    mutate(mes = ifelse(persona %in% month.name, persona, NA),
           mes = ordered(mes, month.name),
           periodo = hoja) %>%
    fill(mes) %>%
    mutate(persona = gsub("[[:digit:] ]", "", persona),
           persona = ifelse(persona %in% month.name, NA, persona)) %>%
    na.omit
}
) %>%
  do.call(bind_rows, .) %>% 
  mutate(periodo = as.factor(periodo))

muertes %>% 
  group_by(periodo) %>% 
  tally() %>% 
  ggplot(aes(periodo, n)) + 
  geom_bar(stat = "identity", fill = "violet", color = "black") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Muertes célebres por año", subtitle = "Fuente: en.wikipedia.org. Consultado el 27/12/2016)",
       x = "Año", y = "Muertes") +
  theme_minimal()

muertes %>% 
  group_by(periodo, mes) %>% 
  tally() %>% 
  ggplot(aes(mes, n)) + 
  geom_bar(stat = "identity", fill = "violet") +
  facet_wrap(~periodo) +
labs(title = "Muertes célebres por mes, diez años", subtitle = "Fuente: en.wikipedia.org. Consultado el 27/12/2016)",
     x = "Mes", y = "Muertes") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

muertes %>% 
  group_by(periodo, mes) %>% 
  tally() %>% 
  ggplot(aes(mes, n)) + 
  geom_boxplot(fill = "violet") +
  labs(title = "Muertes célebres por mes, diez años", subtitle = "Fuente: en.wikipedia.org. Consultado el 27/23/2016)",
       x = "Mes", y = "Muertes") +
  theme_minimal()

muertes2 <- 
  muertes %>% 
  group_by(periodo, mes) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  mutate(fecha = paste(periodo, mes, " - "))
  
  muertes2$fecha <- ordered(muertes2$fecha, muertes2$fecha[order(muertes2$n, decreasing = T)])

muertes2 %>% 
  ggplot(aes(fct_rev(fecha), n)) +
  geom_bar(stat = "identity",  color = "black", aes(fill = periodo)) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Meses más fatales para celebridades", subtitle = "Fuente: en.wikipedia.org. Consultado el 27/23/2016)",
       x = "Año y mes", y = "Número de muertes")
