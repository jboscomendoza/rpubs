library(tidyverse)
library(ineeR)

dir.create("plots")
dir.create("output")

planea <- 
  read_rds("planea.rds") %>% 
  map(~mutate_at(., "NACIONAL", ~"Nacional"))

nombre_vars <- read_rds("nombre_vars.rds")

asignaturas <- c("LYC", "PM") 
grupo_nombres <- c("NACIONAL", "SERV", "RURALIDAD", "SEXO", "EDAD_AC")
no_validos <- c("No identificada", "Respuesta múltiple", "Respuesta omitida")

# Puntajes
df_variables <- 
  expand.grid(
    variable = asignaturas, 
    grupo = grupo_nombres, 
    stringsAsFactors = FALSE
  )

resultados <- 
  pmap(df_variables, 
       function(variable, grupo) {
         media_pv(x = planea[[variable]], variable = variable, 
                  w_final = "W_FSTUWT", w_rep = "W_FSTR", grupo = grupo)
       }) %>% 
  map(~filter(., !Grupo %in% no_validos))

resultados <- 
seq_along(resultados) %>% 
  matrix(nrow = 2) %>% 
  data.frame() %>% 
  map(function(x) {
    bind_rows(
      pluck(resultados, x[[1]]),
      pluck(resultados, x[[2]])
    )
  })

names(resultados) <- grupo_nombres

plot_resultados <- function(tabla) {
  tabla %>% 
    ggplot() +
    aes(Grupo, Media, color = Grupo) +
    geom_point() +
    geom_text(aes(label = round(Media, 1)), 
              position = position_nudge(x = .3)) + 
    geom_errorbar(aes(ymin = Lim_inf, ymax = Lim_sup), width = .25) +
    scale_y_continuous(limits = c(300, 650)) +
    facet_wrap("Variable") +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor.y = element_blank())
}

graf_resultados <- map(resultados, plot_resultados)

map(grupo_nombres, function(x) {
  nombre <- paste0("plots/", x, ".png")
  png(nombre, width = 1000, height = 600, res = 150)
  print(graf_resultados[[x]])
  dev.off()
})


write_rds(resultados, "output/resultados.rds")
write_rds(graf_resultados, "output/graf_resultados.rds")


# Niveles de logro
variables_prop <- c("LYCNVL1", "PMNVL1")

df_variables_prop <- 
  expand.grid(
    variable = variables_prop,
    grupo = grupo_nombres, 
    stringsAsFactors = FALSE
  )

proporcion <- 
  pmap(df_variables_prop, 
       function(variable, grupo) {
         tabla <- str_remove(variable, "NVL1")
         prop_pob(x = planea[[tabla]], variable = variable, 
                  w_final = "W_FSTUWT", w_rep = "W_FSTR", grupo = grupo)
       }) %>% 
  map(~filter(., !Grupo %in% no_validos))

proporcion <- 
  seq_along(proporcion) %>% 
  matrix(nrow = 2) %>% 
  data.frame() %>% 
  map(function(x) {
    bind_rows(
      pluck(proporcion, x[[1]]),
      pluck(proporcion, x[[2]])
    )
  }) %>% 
  map(separate, col = "Grupo", into = c("Grupo", "Nivel"), sep = "\\.")

names(proporcion) <- grupo_nombres
##
proporcion$NACIONAL %>% 
  mutate(
    Variable = str_remove_all(Variable, "NVL1")) %>% 
  ggplot() +
  aes(Grupo, Porcentaje, fill = Nivel, color = Nivel) +
  geom_col(color = "#333333") + 
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap("Variable") +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

