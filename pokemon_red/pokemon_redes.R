# Paquetes
library(tidyverse)
library(igraph)
library(ggraph)

pokemon <- read_csv("pokemon.csv")

pokemon %>%
  filter(is.na(type2)) %>%
  select(tipo = type1, starts_with("against")) %>%
  gather("enemigo", "modificador", against_bug:against_water) %>%
  mutate(enemigo = gsub("against_", "", enemigo)) %>%
  group_by(tipo, enemigo) %>%
  summarize(modificador = mean(modificador)) %>%
  filter(modificador > 1) %>%
  select(enemigo, tipo, modificador) %>%
  graph_from_data_frame() %>%
  plot(layout = layout_with_graphopt, edge.arrow.size = 0.2)

# Fuerza de los tipos de pokemon
poke_fuerza <-
  pokemon %>%
  filter(is.na(type2)) %>%
  select(tipo = type1, starts_with("against")) %>%
  gather("enemigo", "modificador", against_bug:against_water) %>%
  mutate(enemigo = gsub("against_", "", enemigo),
         enemigo = gsub("fight", "fighting", enemigo)) %>%
  distinct() %>%
  filter(modificador != 1) %>%
  mutate(Relacion = ifelse(modificador < 1, "Debil", "Fuerte")) %>%
  select(enemigo, tipo, modificador, Relacion) %>%
  graph_from_data_frame()

poke_fuerza%>%
  plot(layout = layout_with_graphopt, edge.arrow.size = 0.15)


poke_fuerza%>%
  ggraph() +
  geom_edge_link() +
  #geom_node_label(aes(label = name)) +
  geom_node_point() +
  theme_graph()

poke_fuerza%>%
  ggraph(layout = "linear") +
  geom_edge_arc() +
  geom_node_label(aes(label = name)) +
  theme_graph()

poke_fuerza %>%
  ggraph(layout = "circle") +
  geom_edge_link() +
  geom_node_label(aes(label = name)) +
  theme_graph()


plot(poke_fuerza, layout = layout.circle(poke_fuerza),
     edge.arrow.size = .1,
     vertex.size = 28, vertex.color = "#ffffff",
     edge.color = "#333300", vertex.label.color = "black",
     vertex.shape = "sphere")



# Layout con arcos
poke_fuerza %>%
  ggraph(layout = "linear") +
  geom_edge_arc(aes(color = Relacion, end_cap = label_rect(node2.name)),
                arrow = arrow(type = "closed", length = unit(1.5, "mm"))) +
  geom_node_point() +
  geom_node_label(aes(label = name)) +
  facet_grid(Relacion~.) +
  theme_void() +
  theme(legend.position = "none")

# Layout en circulo
poke_fuerza %>%
  ggraph(layout = "circle") +
  geom_edge_link(aes(color = Relacion),
                 arrow = arrow(type = "closed", length = unit(1.5, "mm"))) +
  geom_node_label(aes(label = name), vjust = 1) +
  facet_wrap("Relacion") +
  theme_void() +
  theme(legend.position = "none") +
  scale_edge_color_manual(values = c("#ddbbbb", "#00ccbb"))

# Layout normal
poke_fuerza %>%
  ggraph() +
  geom_edge_loop(aes(color = Relacion)) +
  geom_edge_fan(arrow = arrow(type = "closed", length = unit(1.5, "mm")),
                aes(end_cap = label_rect(node2.name), color = Relacion)) +
  geom_node_label(aes(label = name)) +
  theme_void() +
  theme(legend.position = "top") +
  scale_edge_color_manual(values = c("#ddbbbb", "#00ccbb"))


pokemon %>%
  count(type1, type2) %>%
  filter(n > 3) %>%
  na.omit() %>%
  graph_from_data_frame() %>%
  ggraph(layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(color = n, end_cap = label_rect(node2.name)),
                 arrow = arrow(length = unit(1.5, "mm"))) +
  geom_node_label(aes(label = name)) +
  theme_void()


pokemon$abilities[pokemon$name == "Starmie"]

pokemon_habilidades <-
  pokemon %>%
  select(name, type1, type2, abilities) %>%
  mutate_at("abilities", function(x) {
    gsub("(\\[|\\'|\\]|,)+", "_", x) %>%
      gsub("^_|_$", "", .) %>%
      gsub("_ _", "_", .) %>%
      trimws()
  }) %>%
  separate(col = "abilities", into = c(letters[1:6]), sep = "_") %>%
  mutate_at(letters[1:6], as.factor) %>%
  gather("Orden_H", "Habilidad", a:f) %>%
  gather("Orden_T", "Tipo", type1, type2) %>%
  select(Tipo, Habilidad, name) %>%
  na.omit()

pokemon_habilidades %>%
  count(Tipo, Habilidad) %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point()

pokemon_habilidades %>%
  count(Tipo, Habilidad) %>%
  group_by(Tipo) %>%
  mutate(prop = n / sum(n)) %>%
  filter(prop >= .05) %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link(aes(end_cap = circle(radius = 2, unit = "mm"),
                     color = prop, alpha = prop, edge_width = prop),
                 arrow = arrow(type = "open", length = unit(1.5, "mm")),
                 show.legend = FALSE) +
  scale_edge_color_continuous(low = "red", high = "blue") +
  scale_edge_width(range = c(.01, 1.3)) +
  scale_edge_alpha(range = c(.3, 1)) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



