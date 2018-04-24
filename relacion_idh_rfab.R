# Paquetes necesarios ----
# Para instalar ineeR desde github:
# devtools::install_github("jboscomendoza/ineeR")
library(tidyverse)
library(readxl)
library(ineeR)
library(haven)
library(viridis)
library(forcats)
library(PerformanceAnalytics)
library(corrr)
library(ggrepel)

# Fuente para graficas ----
windowsFonts(plotina = windowsFont("Cambria Math"))

# Origen de los archivos ----
# http://www.inee.edu.mx/index.php/planea/bases-de-datos-planea
# http://www.mx.undp.org/content/mexico/es/home/library/poverty/indice-de-desarrollo-humano-para-las-entidades-federativas--mexi.html
#
lista_archivos <-
  list(
    c("lyc_06.xlsx", "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_6prim_Resultados_de_Logro_LyC_20160713.xlsx"),
    c("mat_06.xlsx", "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_6prim_Resultados_de_Logro_Mat_20160713.xlsx"),
    c("lyc_09.xlsx", "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_3sec_Resultados_de_Logro_LyC_20160713.xlsx"),
    c("mat_09.xlsx", "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_3sec_Resultados_de_Logro_Mat_20160713.xlsx"),
    c("idh_entidad_2015.xlsx", "http://www.mx.undp.org/content/dam/mexico/docs/Publicaciones/PublicacionesReduccionPobreza/InformesDesarrolloHumano/PNUDMx_Base_IDH_EF_Web_VF.xlsx"),
    c("cuest_06.xlsx", "http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/PLANEA_6prim_2015_Cuest_Contexto_Alumnos_20161214.xlsx"),
    c("cuest_09.xlsx", "http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/PLANEA_3sec_2015_Cuest_Contexto_Alumnos_20161214.xlsx"),
    c("Planea06_2015_Alumnos.zip", "http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/Planea06_2015_Alumnos.zip"),
    c("Planea09_2015_Alumnos.zip", "http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/Planea09_2015_Alumnos.zip")
  )

map(lista_archivos, function(cada_archivo){
  download.file(destfile = cada_archivo[[1]], url = cada_archivo[[2]], mode = "wb")
})

map(c("Planea06_2015_Alumnos.zip", "Planea06_2015_Alumnos.zip"), unzip)

# Funciones de ayuda para leer tablas ----
leer_tabla <- function(tabla_original, rfab_grado){
  tabla_nueva <- tabla_original %>%
    read_sav() %>%
    select(RFAB, RFABNVL, MARGINC, NOM_ENT, W_FSTUWT, W_FSTR1:W_FSTR100) %>%
    mutate(
      RFABNVL = recode(as.character(RFABNVL), `1` = "I", `2` = "II",
                       `3` = "III", `4` = "IV"),
      MARGINC = recode(as.character(MARGINC), `0` = "No identificada",
                       `1` = "Alta o Muy alta", `2` = "Media",
                       `3` = "Baja o Muy baja")) %>%
    media_poblacion(variable = "RFAB", prefijo = "W_FSTR",
                    peso_final = "W_FSTUWT", grupo = "NOM_ENT") %>%
    select(Grupo, Media)

  gc()

  names(tabla_nueva) <- c("entidad", rfab_grado)

  tabla_nueva
}

leer_tabla_marg <- function(tabla_original_marg){
  tabla_nueva_marg <- tabla_original_marg %>%
    read_sav() %>%
    select(RFABNVL, MARGINC, NOM_ENT, W_FSTUWT, W_FSTR1:W_FSTR100) %>%
    mutate(
      RFABNVL = recode(as.character(RFABNVL), `1` = "I", `2` = "II",
                       `3` = "III", `4` = "IV"),
      MARGINC = recode(as.character(MARGINC), `0` = "No identificada",
                       `1` = "Alta o Muy alta", `2` = "Media",
                       `3` = "Baja o Muy baja"))
  gc()

  split(tabla_nueva_marg, tabla_nueva_marg$MARGINC) %>%
    map(function(x){
      estimacion_porcentaje(x, variable = "RFABNVL", peso_final = "W_FSTUWT",
                            prefijo = "W_FSTR")
    }) %>%
    invoke(.f = rbind) %>%
    mutate(Grupo = rownames(.)) %>%
    separate(Grupo, into = c("Marginacion", "Nivel_RFAB"), sep = "\\.") %>%
    mutate(Marginacion = fct_relevel(Marginacion,
                                     "Alta o Muy alta", "Media",
                                     "Baja o Muy baja"))
}

# Creación de objetos ----
# Indice de desarrollo humano
idh_14 <- read_excel("idh_entidad_2015.xlsx", col_names = F, range = "B10:Z41") %>%
  select(entidad = X__1, esperanza_vida = X__4, esc_promedio = X__7 ,
         esc_esperada = X__10, inbpc = X__13, salud = X__16, educacion = X__19,
         ingreso = X__22, idh = X__25) %>%
  mutate(entidad = entidad %>%
           toupper() %>%
           chartr(old = "ÁÉÍÓÚ", new = "AEIOU") %>%
           recode("COAHUILA DE ZARAGOZA" = "COAHUILA",
                  "ESTADO DE MEXICO" = "MEXICO",
                  "MICHOACAN DE OCAMPO" = "MICHOACAN",
                  "VERACRUZ DE IGNACIO DE LA LLAVE" = "VERACRUZ"))

# Recursos asociados al bienestar 2015
rfab_p15 <-
  list(
    c("Planea06_2015_Alumnos.sav", "rfab_06"),
    c("Planea09_2015_Alumnos.sav", "rfab_09")
  ) %>%
  map(function(rfabs){
    leer_tabla(rfabs[[1]], rfabs[[2]])
  }) %>%
  reduce(.f = full_join)

rfab_p15[23, "entidad"] <- "QUINTANA ROO"

rfab_nacional <-
  list(
    c("Planea06_2015_Alumnos.sav"),
    c("Planea09_2015_Alumnos.sav")
  ) %>%
  map(function(cada_tabla) {
    read_sav(cada_tabla) %>%
      media_poblacion(variable = "RFAB", prefijo = "W_FSTR",
                      peso_final = "W_FSTUWT")  %>%
      select(Media)
  }) %>%
  bind_cols()

names(rfab_nacional) <- c("rfab_06", "rfab_09")

# Tabla final ----
tabla_rfab_idh <-
  list(idh_14, rfab_p15) %>%
  reduce(.f = full_join, by = "entidad")

# correlaciones ----
# Matriz de correlaciones
png(filename = "cors.png", width = 800, height = 600, units = "px")
tabla_rfab_idh %>%
  select(rfab_06:rfab_09, idh, salud:ingreso) %>%
  chart.Correlation(method = "pearson", histogram = F)
dev.off()

# Red de correlacion
png(filename = "cors_red.png", width = 800, height = 600, units = "px")
tabla_rfab_idh %>%
  select(rfab_06:rfab_09, idh, salud:ingreso) %>%
  correlate(use = "pairwise") %>%
  network_plot(min_cor = 0.1, colors =  viridis(n = 6))
dev.off()

# RFAB por entidad
c("rfab_06", "rfab_09") %>%
  map(function(grado){
    ggplot() +
      aes(tabla_rfab_idh[[grado]], reorder(tabla_rfab_idh[["entidad"]], tabla_rfab_idh[[grado]])) +
      geom_point() +
      geom_text(aes(label = round(tabla_rfab_idh[[grado]], 2),
                    family = "plotina"),
                size = 2.5, nudge_x = 1.25) +
      geom_vline(xintercept = rfab_nacional[[grado]], alpha = .25) +
      theme_minimal() +
      theme(text = element_text(family = "plotina")) +
      scale_x_continuous(limits = c(30, 60)) +
      labs(x = paste0("RFAB - ", substr(grado, nchar(grado)-1, nchar(grado))),
                     y = "Entidad")
    ggsave(filename = paste0("rfab_ent_", grado, ".png"),
           units = "in",
           width = 6, height = 5, dpi = 150)
  })

# IDH por entidad
tabla_rfab_idh %>%
  ggplot() +
  aes(idh, reorder(entidad, idh)) +
  geom_point() +
  geom_text(aes(label = round(idh, 2)), family = "plotina",
            size = 2.5, hjust = -.3) +
  geom_vline(xintercept = median(tabla_rfab_idh[["idh"]], na.rm = T),
             alpha = .25) +
  labs(x = "IDH", y = "Entidad") +
  scale_x_continuous(limits = c(.65, .85)) +
  theme_minimal() +
  theme(text = element_text(family = "plotina")) +
  ggsave(filename = "idh_ent.png",
       units = "in",
       width = 6, height = 5, dpi = 150)

# Cruce RFAB e IDH
c("rfab_06", "rfab_09") %>%
  map(function(grado){
    ggplot() +
      aes(tabla_rfab_idh[[grado]], tabla_rfab_idh[["idh"]]) +
      geom_point() +
      #geom_smooth(fill = NA, method = "lm", alpha = .25, weight = .1) +

      geom_line(stat="smooth",
                method = "lm",
                color = "blue",
                alpha = 0.3) +

      geom_text_repel(aes(label = tabla_rfab_idh[["entidad"]], family = "plotina"),
                      size = 2) +
      geom_vline(xintercept = rfab_nacional[[grado]], alpha = .25) +
      geom_hline(yintercept = mean(tabla_rfab_idh[["idh"]], na.rm = T), alpha = .25) +
      theme_minimal() +
      theme(text = element_text(family = "plotina")) +
      labs(x = paste0("RFAB - ", substr(grado, nchar(grado)-1, nchar(grado))),
           y = "IDH") +
      ggsave(filename = paste0("idh_", grado, ".png"),
             units = "in",
             width = 6, height = 5, dpi = 150)
  })

