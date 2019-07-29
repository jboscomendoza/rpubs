library(tidyverse)
library(ineeR)

dir.create("plots")

planea <- read_rds("planea.rds")

variable_nombres <- c("LYC", "PM") 
grupo_nombres <- c("NACIONAL", "SERV", "RURALIDAD", "SEXO", "EDAD_AC")
no_validos <- c("No identificada", "Respuesta múltiple", "Respuesta omitida")

# Puntajes
df_variables <- 
  expand.grid(
    variable = variable_nombres, 
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

graf_resultados <- map(resultados, graf_media)

map(grupo_nombres, function(x) {
  nombre <- paste0("plots/", x, ".png")
  png(nombre, width = 1000, height = 600, res = 150)
  print(graf_resultados[[x]])
  dev.off()
})

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
