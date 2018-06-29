library(tidyverse)
library(psych)

download.file("", destfile = )

unzip("superhero-set.zip")

dc_marvel <-
  read_csv("heroes_information.csv") %>%
  select(name, Publisher) %>%
  filter(Publisher %in% c("DC Comics", "Marvel Comics"))

# Resultados
dc_marvel

heroe_poderes <- read_csv("super_hero_powers.csv")

# Resultados
heroe_poderes

heroe_poderes <-
  heroe_poderes %>%
  filter(hero_names %in% dc_marvel$name)

names(heroe_poderes) <-
  names(heroe_poderes) %>%
  tolower() %>%
  gsub("\\W+", "_", .)

# Personajes
heroe <- select(heroe_poderes, hero_names)

# Poderes
poderes <- select(heroe_poderes, -hero_names)

poderes <-
  map_df(poderes, ~ifelse(. == "True", 1, 0))

sum(poderes$sonic_scream)


plot(density(index_poderes), col = "#6666ee", main = "Frecuencia de poderes")

# Más comunes
head(sort(index_poderes, decreasing = TRUE), 5)

# Menos comunes
head(sort(index_poderes), 5)

poderes <- poderes[(index_poderes > 4 & index_poderes < 150)]

# Tamaño nuevo de poderes
dim(poderes)

# Histograma nuevo de poderes
map_dbl(poderes, sum) %>%
  density() %>%
  plot(col = "#6666ee", main = "Frecuencia de poderes")

index_heroe <- rowSums(poderes)

# Resutlado
index_heroe

plot(density(index_heroe), "Distribución de heroe")

heroe <- heroe[index_heroe > 0 & index_heroe < 30, ]

# Tambien lo tenemos que hacer con poderes para que coincidan los tamaños de las tablas
poderes <- poderes[index_heroe > 0 & index_heroe < 30, ]

# Nueva distribución
rowSums(poderes) %>%
  density() %>%
  plot(main = "Distribución de héroe - Nuevo")

poderes_vss <- vss(poderes)

# Nuestro resultado
poderes_vss

poderes_pca <- pca(r = poderes, nfactors = 8)

cor(poderes_pca$weights) %>% round(2)

poderes_loadings <-
  poderes_pca$weights %>%
  data.frame() %>%
  rownames_to_column("poder") %>%
  tbl_df()

# Nuestro resultado
poderes_loadings

names(poderes_loadings[-1]) %>%
  map(function(x){
    poderes_loadings %>%
      select(poder, factor = x) %>%
      arrange(desc(factor))
  })

poderes_nombres <- c("super_ojos", "divino", "psiquico", "spider_man",
                     "acuatico", "energia", "ladrillo", "vigilante")

# Asignamos nombres
names(poderes_loadings) <- c("poder", poderes_nombres)

poderes_scores <-
  ((poderes_pca$scores * 100) + 500) %>%
  tbl_df() %>%
  bind_cols(heroe, .)

# Asignación de nombre
names(poderes_scores) <- c("heroe", poderes_nombres)

# Resultado
poderes_scores

names(poderes_scores[-1]) %>%
  map(function(x){
    poderes_scores %>%
      select(heroe, Score = x) %>%
      arrange(desc(Score))
  }) %>% {
    names(.) <- poderes_nombres
    .
  }

poderes_nombres <- c("superman", "omnipotente", "psiquico", "spiderman",
                     "animal", "energia", "titan", "vigilante")

# Renombramos las columnas de poderes_scores
names(poderes_scores) <- c("heroe", poderes_nombres)

filter(poderes_scores, heroe == "Colossus")

filter(poderes_scores, heroe == "Black Lightning")

obten_tipo <- function(nombre, cuantos = 3) {
  poderes_scores %>%
    filter(heroe == nombre) %>%
    gather(componente, score, superman:vigilante) %>%
    arrange(desc(score)) %>%
    top_n(wt = score, n = cuantos) %>%
    mutate(diferencia = score - first(score))
}

c("X-23", "Punisher", "Stargirl", "Swamp Thing") %>%
  map(obten_tipo)
