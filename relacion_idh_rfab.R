# Paquetes necesarios ----
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

# Descarga de archivos ----
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
    c("cuest_09.xlsx", "http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/PLANEA_3sec_2015_Cuest_Contexto_Alumnos_20161214.xlsx")
  )

map(lista_archivos, function(cada_archivo){
  download.file(destfile = cada_archivo[[1]], url = cada_archivo[[2]])
})

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
p15_rfab <-
  list(
    c("D:/jmendoza.INEE/Desktop/r_wd/planea_db/Planea06_2015_Alumnos.sav",
      "rfab_06"),
    c("D:/jmendoza.INEE/Desktop/r_wd/planea_db/Planea09_2015_Alumnos.sav",
      "rfab_09")
  ) %>%
  map(function(rfabs){
    leer_tabla(rfabs[[1]], rfabs[[2]])
  }) %>%
  reduce(.f = full_join)

p15_rfab[23, "entidad"] <- "QUINTANA ROO"

# Puntajes de planea
p15_puntajes <-
  list(
    c("lyc_06.xlsx", "lyc06"), c("mat_06.xlsx", "mat06"),
    c("lyc_09.xlsx", "lyc09"), c("mat_09.xlsx", "mat09")
  ) %>%
  map(function(tabla_puntaje){
    tabla_resumen <-
      read_excel(tabla_puntaje[[1]], sheet = "6", range = "A5:C280" ,
                 col_names = F) %>%
      filter(!X__1 %in% c("Tipo de escuela", "Marginación",
                          "Rural-Urbano", NA)) %>%
      select(X__1, X__3) %>%
      filter(!grepl(x = X__1, pattern = "[[:punct:]]")) %>%
      mutate(X__3 = ifelse(is.na(X__3), 0, as.numeric(X__3)))

    names(tabla_resumen) <- c("entidad", tabla_puntaje[[2]])

    data.frame(tabla_resumen)
  }) %>%
  reduce(.f = full_join) %>%
  mutate(entidad = chartr("ÁÉÍÓÚ", "AEIOU", toupper(entidad))) %>%
  arrange(entidad)

# Recursos familiares cruzados con niveles de marginacion
rfabxmarg_06 <-
  leer_tabla_marg("D:/jmendoza.INEE/Desktop/r_wd/planea_db/Planea06_2015_Alumnos.sav")
rfabxmarg_09 <-
  leer_tabla_marg("D:/jmendoza.INEE/Desktop/r_wd/planea_db/Planea09_2015_Alumnos.sav")

# Tabla final ----
idh <-
  list(idh_14, p15_rfab, p15_puntajes) %>%
  reduce(.f = full_join, by = "entidad")

# correlaciones ----

png(filename = "cors.png", width = 800, height = 600, units = "px")
idh %>% select(rfab_06:rfab_09, idh, salud:ingreso) %>%
  chart.Correlation(method = "pearson", histogram = F)
dev.off()

idh %>% select(rfab_06:rfab_09, salud:mat09) %>%
  correlate(use = "pairwise") %>%
  network_plot(min_cor = 0.1, colors =  viridis(n = 6))

c("rfab_06", "rfab_09") %>%
  map(function(grado){
    ggplot() +
      aes(idh[[grado]], reorder(idh[["entidad"]], idh[[grado]])) +
      geom_point() +
      geom_text(aes(label = round(idh[[grado]], 2), 
                    family = "plotina"),
                  size = 2.5, nudge_x = 1.25) +
      geom_vline(xintercept = mean(idh[[grado]], na.rm = T), alpha = .25) +
      theme_minimal() +
      theme(text = element_text(family = "plotina")) +
      scale_x_continuous(limits = c(30, 60)) +
      labs(x = "RFAB", y = "Entidad")
      ggsave(filename = paste0("rfab_ent_", grado, ".png"),
             units = "in",
             width = 6, height = 5, dpi = 150)
    
  })

c("rfab_06", "rfab_09") %>%
  map(function(grado){
    ggplot() +
      aes(idh[[grado]], idh[["idh"]]) +
      geom_point() +
      geom_text_repel(aes(label = idh[["entidad"]], family = "plotina"),
                      size = 2) +
      geom_vline(xintercept = mean(idh[[grado]], na.rm = T), alpha = .25) +
      geom_hline(yintercept = mean(idh[["idh"]], na.rm = T), alpha = .25) +
      theme_minimal() +
      theme(text = element_text(family = "plotina")) +
      labs(x = "RFAB", y = "IDH") +
      ggsave(filename = paste0("idh_", grado, ".png"),
             units = "in",
             width = 6, height = 5, dpi = 150)
  })



# RFAB con nivel de marginacion de la localidad  ----

list(
  list(rfabxmarg_06, "06"),
  list(rfabxmarg_09, "09")
) %>%
  map(function(para_grafica){
    para_grafica[[1]] %>%
      filter(Marginacion != "No identificada") %>%
      ggplot() +
      aes(Marginacion, Porcentaje, fill = fct_rev(Nivel_RFAB)) +
      geom_bar(stat = "identity", alpha = .5) +
      geom_text(aes(label = Porcentaje, family = "plotina"),
                position = position_stack(vjust = .5), size = 2.7) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_viridis(discrete = T, direction = -1, name = "Nivel de RFAB") +
      labs(x = "Nivel de marginación de la localidad") +
      theme_minimal() +
      theme(text = element_text(family = "plotina")) +
      ggsave(filename = paste0("rfab_marg_", para_grafica[[2]], ".png"),
             units = "in",
             width = 6, height = 5, dpi = 150)
  })
