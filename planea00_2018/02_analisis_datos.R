library(tidyverse)
library(ineeR)

planea <- read_rds("planea.rds")

media_pv(x = planea$lyc, variable = "LYC", 
         w_final = "W_FSTUWT", w_rep = "W_FSTR")

media_pv(x = planea$lyc, variable = "LYC", 
         w_final = "W_FSTUWT", w_rep = "W_FSTR", grupo = "SERV")

media_pv(x = planea$lyc, variable = "LYC", 
         w_final = "W_FSTUWT", w_rep = "W_FSTR", grupo = "RURALIDAD")

media_pv(x = planea$lyc, variable = "LYC", 
         w_final = "W_FSTUWT", w_rep = "W_FSTR", grupo = "SEXO")

media_pv(x = planea$lyc, variable = "LYC", 
         w_final = "W_FSTUWT", w_rep = "W_FSTR", grupo = "EDAD_AC")
