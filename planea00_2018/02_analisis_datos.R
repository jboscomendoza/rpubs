library(tidyverse)
library(ineeR)

planea <- read_rds("planea.rds")

df_variables <- 
  expand.grid(
    variable = c("LYC", "PM"), 
    grupo = c("NACIONAL", "SERV", "RURALIDAD", "SEXO", "EDAD_AC"), 
    stringsAsFactors = FALSE
  )

resultados <- 
  pmap(df_variables, 
       function(variable, grupo) {
         media_pv(x = planea[[variable]], variable = variable, 
                  w_final = "W_FSTUWT", w_rep = "W_FSTR", grupo = grupo)
       })

names(resultados) <- 
  pmap(df_variables, 
       function(variable, grupo) {
         paste(variable, grupo, sep = "_")
       }) %>% 
  reduce(c)

resultados
