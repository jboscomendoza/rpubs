# Paquetes ----
library(tidyverse)
library(haven)
library(ineeR)
library(forcats)

# Para instalar ineeR
devtools::install_github("https://github.com/jboscomendoza/ineeR")

# Fuentes -----
windowsFonts(plotina = windowsFont("Cambria Math"))

# Datos ----
download.file("http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/Planea06_2015_Alumnos.zip", destfile = "Planea06_2015_Alumnos.sav")

p06 <-
  haven::read_sav("Planea06_2015_Alumnos.sav") %>%
  select(ALUMNO:W_FSTUWT, AB001, AB002, AB003, RFAB,RFABNVL, PV1LYC:PV5LYC,
         PV1MAT:PV5MAT, W_FSTR1:W_FSTR100) %>%
  mutate(
    AB001 = recode(as.numeric(AB001),
                   `1` = "Español", `2` = "Indígena", `3` = "Extranjera"),
    AB002 = recode(as.numeric(AB002),
                   `1` = "Sí", `2` = "No", `3` = "No se habla"),
    AB003 = recode(as.numeric(AB003),
                   `1` = "Indígena", `2` = "No indígena", `3` = "No sé"),
    SERV = recode(as.character(SERV),
                  "CCO" = "Curso comunitario", "IND" = "Indígena",
                  "GRP" = "General", "PRV" = "Privada"),
    I_MULTIGRADO = recode(as.numeric(I_MULTIGRADO),
                          `0` = "No multigrado", `1` = "Multigrado",
                          `9` = "No identificado"),
    MARGINC = recode(as.numeric(MARGINC),
                     `0` = "No identificada", `1` = "Alta o Muy alta",
                     `2` = "Media", `3` = "Baja o Muy baja"),
    RFABNVL = recode(as.numeric(RFABNVL),
                     `1` = "Nivel 1", `2` = "Nivel 2", `3` = "Nivel 3",
                     `4` = "Nivel 4")
  )

# Recodes ----
# Construimos nuestras categorías a partir de las respuestas a las preguntas AB001 y AB003
p06$cat[p06$AB001 == "Indígena" & p06$AB003 == "Indígena"] <-  "Indígena Hablante"
p06$cat[p06$AB001 != "Indígena" & p06$AB003 == "Indígena" & !is.na(p06$AB001)] <-  "Indígena No hablante"
p06$cat[p06$AB001 == "Indígena" & p06$AB003 != "Indígena" & !is.na(p06$AB003)] <-  "No indígena Hablante"
p06$cat[p06$AB001 != "Indígena" & p06$AB003 != "Indígena" & !is.na(p06$AB003) & !is.na(p06$AB001)] <-  "No indígena No hablante"

# Todos los resultados
perc <- list()
punt <- list()

# Porcentajes ----
perc <-
  list(
    cat = estimacion_porcentaje(tabla = p06,
                                variable = "cat",
                                prefijo = "W_FSTR",
                                peso_final = "W_FSTUWT") %>%
      mutate(Grupo = rownames(.)),

    cat_serv = p06 %>%
      split(p06$cat) %>% map(function(Modalidad){
        estimacion_porcentaje(tabla = Modalidad,
                              variable = "SERV",
                              prefijo = "W_FSTR",
                              peso_final = "W_FSTUWT")
      }) %>%
      invoke(.f = rbind) %>%
      mutate(Grupo = rownames(.)) %>%
      separate(Grupo, into = c("Grupo", "Modalidad"), sep = "\\."),

    cat_marg = p06 %>%
      split(p06$cat) %>% map(function(Marginacion){
        estimacion_porcentaje(tabla = Marginacion,
                              variable = "MARGINC",
                              prefijo = "W_FSTR",
                              peso_final = "W_FSTUWT")
      }) %>%
      invoke(.f = rbind) %>%
      mutate(Grupo = rownames(.)) %>%
      separate(Grupo, into = c("Grupo", "Marginacion"), sep = "\\."),

    cat_mult = p06 %>%
      split(p06$cat) %>% map(function(Multigrado){
        estimacion_porcentaje(tabla = Multigrado,
                              variable = "I_MULTIGRADO",
                              prefijo = "W_FSTR",
                              peso_final = "W_FSTUWT")
      }) %>%
      invoke(.f = rbind) %>%
      mutate(Grupo = rownames(.)) %>%
      separate(Grupo, into = c("Grupo", "Tipo"), sep = "\\."),

    cat_rfab = p06 %>%
      split(p06$cat) %>% map(function(Rfab){
        estimacion_porcentaje(tabla = Rfab,
                              variable = "RFABNVL",
                              prefijo = "W_FSTR",
                              peso_final = "W_FSTUWT")
      }) %>%
      invoke(.f = rbind) %>%
      mutate(Grupo = rownames(.)) %>%
      separate(Grupo, into = c("Grupo", "RFAB"), sep = "\\.")

    )

# Porcentajes ---.
# Perc Plot Grupo

perc$cat %>%
  ggplot() +
  aes(Grupo, Porcentaje, fill = Grupo, color = Grupo) +
  geom_text(aes(label = Porcentaje, family = "serif"),
            size = 3.4, vjust = -.35,
            position = position_dodge(width = .9),
            show.legend = FALSE) +
  geom_bar(stat = "identity", position = "dodge", alpha = .6) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95)) +
  theme_minimal() +
  theme(text = element_text(family = "plotina"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        legend.key.width = unit(5, units = "mm"),
        legend.key.height = unit(5, units = "mm"),
        legend.position = "non2") +
  ggsave(filename = "cat.png", units = "in",
         width = 8, height = 3, dpi = 150)


p06 %>% count(cat) %>% rename(Grupo = cat) %>% na.omit()

# Perc Plot Servicio x Grupo
perc$cat_serv %>%
  mutate(Modalidad = Modalidad %>%
           factor %>%
           fct_relevel(perc$Modalidad,
                       c("General", "Indígena", "Curso comunitario", "Privada"))) %>%
  ggplot() +
  aes(Grupo, Porcentaje, fill = Modalidad, color = Modalidad) +
  geom_text(aes(label = Porcentaje, family = "serif"),
            size = 3.4, vjust = -.35,
            position = position_dodge(width = .9),
            show.legend = FALSE) +
  geom_bar(stat = "identity", position = "dodge", alpha = .6) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95)) +
  theme_minimal() +
  theme(text = element_text(family = "plotina"),
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 8.5),
    legend.text = element_text(size = 8.5),
    legend.key.width = unit(5, units = "mm"),
    legend.key.height = unit(5, units = "mm")
  ) +
  ggsave(filename = "cat_serv.png", units = "in",
         width = 8, height = 3, dpi = 150)

p06 %>% count(cat, SERV) %>% rename(Grupo = cat, Modalidad = SERV) %>% na.omit()

# Plot grupo x multigrado
perc$cat_mult %>%
  mutate(
    Tipo = Tipo %>%
      factor %>%
      fct_relevel(
        perc$Tipo,
        c("Multigrado", "No multigrado", "No identificado"))
  ) %>%
  ggplot() +
  aes(Grupo, Porcentaje, fill = Tipo, color = Tipo) +
  geom_text(aes(label = Porcentaje, family = "serif"),
            size = 3.4, vjust = -.35,
            position = position_dodge(width = .9),
            show.legend = FALSE) +
  geom_bar(stat = "identity", position = "dodge", alpha = .6) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95)) +
  theme_minimal() +
  theme(text = element_text(family = "plotina"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        legend.key.width = unit(5, units = "mm"),
        legend.key.height = unit(5, units = "mm")
  ) +
  ggsave(filename = "cat_mult.png", units = "in",
         width = 8, height = 3, dpi = 150)


p06 %>% count(cat, I_MULTIGRADO) %>% rename(Grupo = cat, Tipo = I_MULTIGRADO) %>% na.omit() %>% data.frame

# Perc Plot Marginacion
perc$cat_marg %>%
  mutate(
    Marginacion = Marginacion %>%
      factor %>%
      fct_relevel(
        perc$cat_marg$`Marginacion`,
        c("Alta o Muy alta", "Media", "Baja o Muy baja", "No identificada"))
  ) %>%
  ggplot() +
  aes(Grupo, Porcentaje, fill = Marginacion, color = Marginacion) +
  geom_text(aes(label = Porcentaje, family = "serif"),
            size = 3.4, vjust = -.35,
            position = position_dodge(width = .9),
            show.legend = FALSE) +
  geom_bar(stat = "identity", position = "dodge", alpha = .6) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95)) +
  theme_minimal() +
  theme(text = element_text(family = "plotina"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        legend.key.width = unit(5, units = "mm"),
        legend.key.height = unit(5, units = "mm")
  ) +
  ggsave(filename = "cat_marg.png", units = "in",
         width = 8, height = 3, dpi = 150)


p06 %>% count(cat, MARGINC) %>% rename(Grupo = cat, Marginacion = MARGINC) %>% na.omit() %>% data.frame

# Perc Plot x RFAB
perc$cat_rfab %>%
  ggplot() +
  aes(Grupo, Porcentaje, fill = RFAB, color = RFAB) +
  geom_text(aes(label = Porcentaje, family = "serif"),
            size = 3.4, vjust = -.35,
            position = position_dodge(width = .9),
            show.legend = FALSE) +
  geom_bar(stat = "identity", position = "dodge", alpha = .6) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95)) +
  theme_minimal() +
  theme(text = element_text(family = "plotina"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        legend.key.width = unit(5, units = "mm"),
        legend.key.height = unit(5, units = "mm")
  ) +
  ggsave(filename = "cat_rfab.png", units = "in",
         width = 8, height = 3, dpi = 150)


p06 %>% count(cat, RFABNVL) %>% rename(Grupo = cat, RFAB = RFABNVL) %>% na.omit() %>% data.frame

# Puntajes ----
punt <-
  list(
    lyc = puntaje_plausible(tabla = p06, sufijo = "LYC", prefijo = "W_FSTR",
                            peso_final = "W_FSTUWT", grupo = "cat"),
    mat = puntaje_plausible(tabla = p06, sufijo = "MAT", prefijo = "W_FSTR",
                            peso_final = "W_FSTUWT", grupo = "cat")
  )

# Plot LYC
punt$lyc %>%
  ggplot() +
  aes(Grupo, Media, color = Grupo) +
  geom_point() +
  geom_text(aes(label = round(Media, 2), family = "plotina"), hjust = -.25,
            size = 3.4) +
  geom_errorbar(aes(ymax = Intervalo_superior, ymin = Intervalo_inferior),
                width = 0.05, alpha = .65) +
  scale_y_continuous(limits = c(400, 520)) +
  theme_minimal() +
  theme(text = element_text(family = "plotina"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        legend.key.width = unit(5, units = "mm"),
        legend.key.height = unit(5, units = "mm"),
        legend.position = "none"
  ) +
  labs(y = "Media en Lenguaje y Comunicación")
  ggsave(filename = "punt_lyc.png", units = "in",
         width = 8, height = 3, dpi = 150)

# Plot Mat
punt$mat %>%
  ggplot() +
  aes(Grupo, Media, color = Grupo) +
  geom_point() +
  geom_text(aes(label = round(Media, 2), family = "plotina"), hjust = -.25,
            size = 3.4) +
  geom_errorbar(aes(ymax = Intervalo_superior, ymin = Intervalo_inferior),
                width = 0.05, alpha = .65) +
  scale_y_continuous(limits = c(400, 520)) +
  theme_minimal() +
  theme(text = element_text(family = "plotina"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        legend.key.width = unit(5, units = "mm"),
        legend.key.height = unit(5, units = "mm"),
        legend.position = "none"
  ) +
  labs(y = "Media en Matemáticas")
ggsave(filename = "punt_mat.png", units = "in",
       width = 8, height = 3, dpi = 150)


# Conteos -----
count(p06, cat)
count(p06, SERV, cat)
count(p06, cat, RFABNVL)
count(p06, cat, MARGINC)
count(p06, cat, TAM_LOC_PRI) %>% View

count(p06, SERV, cat) %>% mutate(perc = n / sum(n)) %>%
  ggplot() + aes(cat, n,  fill = SERV %>% factor) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 400) + scale_y_log10()

count(p06, cat, RFABNVL) %>% mutate(perc = n / sum(n)) %>%
  ggplot() + aes(cat, perc,  fill = RFABNVL %>% factor %>% fct_rev) + geom_bar(stat = "identity")

