# Source
source("cct_localidades.R")

if(!dir.exists("resumen")) dir.create("resumen")

df_sec$NOM_ENT[df_sec$NOM_ENT == "Veracruz de Ignacio de la Llave"] <- "Veracruz"
df_sec$NOM_ENT[df_sec$NOM_ENT == "Michoacán de Ocampo"] <- "Michoacán"

df_sec <-
  df_sec %>%
  rename(Modalidad = SUBNIVEL, Entidad = NOM_ENT)

df_sec$Modalidad <-
  recode(df_sec$Modalidad,
         "TELESECUNDARIA" = "Telesecundaria",
         "SECUNDARIA GENERAL" = "Secundaria General",
         "SECUNDARIA TECNICA" = "Secundaria Tecnica",
         "SECUNDARIA COMUNITARIA" = "Secundaria Comunitaria")

# Localidades indígenas con secundaria (LIS) a nivel nacional
df_sec %>%
  distinct(ENT, MUN, LOC) %>%
  tally()

# Secundarias en localidades indígena por estado
df_sec %>%
  group_by(Entidad) %>%
  distinct(MUN, LOC) %>%
  tally()

df_sec %>%
  group_by(Entidad) %>%
  distinct(MUN, LOC) %>%
  tally() %>%
  ggplot(aes(fct_rev(Entidad), n)) +
  geom_point() + geom_text(aes(label = n), vjust = -.5) +
  coord_flip() +
  theme_minimal()

# Secundarias en localidades indígenas por estado, por modalidad
df_sec %>%
  group_by(Entidad, Modalidad) %>%
  tally()

df_sec %>%
  group_by(Entidad, Modalidad) %>%
  tally() %>%
  ggplot(aes(Entidad, n, fill = Modalidad)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = n),
            position = position_stack(vjust = .5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .35),
        legend.position = "top")

# Estudiantes inscritos secundarias en localidades indígenas, por estado (CEMABE, 2015)
df_sec %>%
  group_by(Entidad) %>%
  summarise(Estudiantes = sum(ALUMNOS))

df_sec %>%
  group_by(Entidad) %>%
  summarise(Estudiantes = sum(ALUMNOS)) %>%
  ggplot(aes(fct_rev(Entidad), Estudiantes)) +
  geom_bar(stat = "identity", color = "white", fill = "grey") +
  geom_text(aes(label = Estudiantes),
            size = 6,
            vjust = -.5) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .35),
        legend.position = "top")

# Estudiantes inscritos secundarias en localidades indígenas, por estado, por modalidad (CEMABE, 2015)
df_sec %>%
  group_by(Entidad, Modalidad) %>%
  summarise(Estudiantes = sum(ALUMNOS))

df_sec %>%
  group_by(Entidad, Modalidad) %>%
  summarise(Estudiantes = sum(ALUMNOS)) %>%
  ggplot(aes(fct_rev(Entidad), Estudiantes, fill = Modalidad)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = Estudiantes),
            position = position_stack(vjust = .5),
            size = 6) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .35),
        legend.position = "top")

# Resumen estadístico de estudiantes inscritos en secundarias en localidades indígenas, por estado
df_sec %>%
  group_by(Entidad) %>%
  summarise(
    Media = mean(ALUMNOS, na.rm = T),
    SD = sd(ALUMNOS, na.rm = T),
    Minimo = min(ALUMNOS),
    Q1 = quantile(ALUMNOS, .25, na.rm = T),
    Mediana = median(ALUMNOS),
    Q3 = quantile(ALUMNOS, .75, na.rm = T),
    Maximo = max(ALUMNOS),
    Planteles = length(Entidad)
  )

# Resumen por estao y modalidad
# Resumen estadístico de estudiantes inscritos en secundarias en localidades indígenas, por estado
df_sec %>%
  group_by(Entidad, Modalidad) %>%
  summarise(
    Media = mean(ALUMNOS, na.rm = T),
    SD = sd(ALUMNOS, na.rm = T),
    Minimo = min(ALUMNOS),
    Q1 = quantile(ALUMNOS, .25, na.rm = T),
    Mediana = median(ALUMNOS),
    Q3 = quantile(ALUMNOS, .75, na.rm = T),
    Maximo = max(ALUMNOS),
    Planteles = length(Entidad)
  )
