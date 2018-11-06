# Paquetes
library(tidyverse)

pokemon <- read_csv("pokemon.csv")

pokemon$abilities[pokemon$name == "Pikachu"]

pokemon %>% 
  select(name, abilities) %>% 
  mutate_at("abilities", function(x) {
    
    gsub("(\\[|\\'|\\]|,)+", "_", x) %>% 
      gsub("^_|_$", "", .) %>% 
      gsub("_ _", "_", .) %>% 
      trimws()
  } 
  ) %>% 
  separate(col = "abilities", into = c(letters[1:6]), sep = "_") %>% 
  mutate_at(letters[1:6], as.factor) %>% 
  gather(orden, nombre, a:f)

