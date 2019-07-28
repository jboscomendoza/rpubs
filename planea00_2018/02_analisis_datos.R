library(tidyverse)
library(ineeR)

planea <- read_rds("planea.rds")

variable_nombres <- c("LYC", "PM") 
grupo_nombres <- c("NACIONAL", "SERV", "RURALIDAD", "SEXO", "EDAD_AC")
  
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
       })

resultados <- 
  map(resultados, 
    ~filter(., !Grupo %in% c("No identificada", "Respuesta múltiple", 
                             "Respuesta omitida")))

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

names(resultados) <- variable_nom
