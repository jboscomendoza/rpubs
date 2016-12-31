# Fuentes de información consultadas
#
# Investigación sobre gasto en educación en México.
# Documento usado como referencia conceptual y metodológica.
# http://ciep.mx/gasto-publico-en-educacion-por-entidad-federativa/
#
# Gasto educativo por entidad, de acuerdo al Cuestionario Financiamiento
# Educativo Estatal, a cargo de la Secretaría de Educación Pública.
# Se realizaron dos consultas.
# Una con los parámetros:
# Etapa: Aprobado; Periodo: 2015; Fuente; Federal; Nivel Educativo: Básico
# Otra con los parámetros:
# Etapa: Ejercido; Periodo: 2015; Fuente; Federal; Nivel Educativo: Básico
# Por cada consulta, del reporte generado, se han usado los datos de la columna
# "Total Primaria". Los datos de la primera columna corresponden a la columna
# "Aprobado", y los de la segunda a la columna "Ejecido" del documento
# "documento presupuesto_2015.xlsx".
# http://www.planeacion.sep.gob.mx/cfee/
# http://www.planeacion.sep.gob.mx/cfee/Reports/Default.aspx
#
# Población de México por entidad en el 2015
# Se ha copiado el contenido de la tabla presentada en el siguiente enlace al
# documento "población.xlsx", cambiando el nombre de las entidades a su forma
# más breve (ej. "Michoacán", en lugar de "Michoacán de Ocampo").
# http://cuentame.inegi.org.mx/poblacion/habitantes.aspx?tema=P
#
# Desarrollo humano
# "Informe sobre Desarrollo Humano México 2016 | Desigualdad y movilidad"
# Elaborado por el Programa de las Naciones Unidas para el Desarrollo. Se
# recuperan los indicadores de desarrollo de la fecha de su estimación más
# reciente, del del 2010.
# http://www.mx.undp.org/content/mexico/es/home/library/poverty/informe-nacional-sobre-desarrollo-humano-mexico-2016.html
#
# Logro educativo.
# Resultados de Sexto de Primaria del Plan Nacional para la Evaluación de los
# Aprendizajes, aplicación 2015, a cargo del Instituto Nacional para la
# Evaluación de la Educación.
# http://www.inee.edu.mx/index.php/planea

# Archivos a descargar ----
download.file(
  url = "http://www.mx.undp.org/content/dam/mexico/docs/Publicaciones/PublicacionesReduccionPobreza/InformesDesarrolloHumano/idhmovilidadsocial2016/PNUDMx_INDH2016_Base_IDH.xlsx",
  destfile = "PNUDMx_INDH2016_Base_IDH.xlsx",
  mode = "wb")

download.file(
  url = "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_6prim_Resultados_de_Logro_LyC_20160713.xlsx",
  destfile = "lyc_6.xlsx",
  mode = "wb")

download.file(
  url = "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_6prim_Resultados_de_Logro_Mat_20160713.xlsx",
  destfile = "mat_6.xlsx",
  mode = "wb")

# Paquetes ----
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(forcats)

# ----
docs_nombres <-
  c("lyc_6.xlsx", "mat_6.xlsx")

no_tilde <- function(texto){
 chartr("áéíóú",
        "aeiou",
        trimws(as.character(texto))
        )
}

# Entidades ----
entidades <-
  data.frame(
    entidad = c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
      "Chiapas", "Chihuahua", "Coahuila", "Colima", "Distrito Federal",
      "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Mexico",
      "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
      "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa", "Sonora",
      "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas"
    ),
    abreviatura = c(
      "AGU", "BCN", "BCS", "CAM", "CHP", "CHH", "COA", "COL", "CDMX", "DUR",
      "GUA", "GRO", "HID", "JAL", "MEX", "MIC", "MOR", "NAY", "NLE", "OAX",
      "PUE", "QUE", "ROO", "SLP", "SIN", "SON", "TAB", "TAM", "TLA", "VER",
      "YUC", "ZAC")) %>%
  lapply(., as.character) %>%
  tbl_df()

# ----
planea_sexto <-
  bind_cols(
    lapply(docs_nombres, function(medias){
      read_excel(medias, sheet = "6", skip = 4, col_names = F) %>%
        slice(1:214) %>%
        select(entidad = X0,
               media_estatal = X2,
               media_hombres = X8,
               media_mujeres = X14) %>%
        filter(!entidad %in% c("Tipo de escuela", NA, "Marginación",
                               "Rural-Urbano", "(EE): Error Estándar.")) %>%
        mutate(media_menero =
                 as.numeric(media_hombres) - as.numeric(media_mujeres)) %>%
        group_by(entidad) %>%
        mutate_all(funs(as.numeric)) %>%
        ungroup() %>%
        mutate(asignatura = substr(medias, 1, 5))
    }) %>%
      do.call(bind_rows, .),
    lapply(docs_nombres, function(niveles){
      read_excel("PLANEA_6prim_Resultados_de_Logro_LyC_20160713.xlsx",
                 sheet = "7", skip = 5, col_names = F) %>%
        slice(1:214) %>%
        select(entidad = X0,
               nivel_1 = X2,
               nivel_2 = X6,
               nivel_3 = X10,
               nivel_4 = X14,
               al_menos_2 = X18,
               al_menos_3 = X22) %>%
        filter(!entidad %in% c("Tipo de escuela", NA, "Marginación",
                               "Rural-Urbano", "(EE): Error Estándar.")) %>%
        lapply(., function(columna){
          gsub(pattern = "\\*", replacement = "", x = columna)
        }) %>%
        tbl_df() %>%
        group_by(entidad) %>%
        mutate_all(funs(as.numeric)) %>%
        ungroup()
    }) %>%
      do.call(bind_rows, .) %>%
      select(-entidad)
  ) %>%
  mutate(entidad = no_tilde(entidad))


tabla_2015 <-
  read_excel("PNUDMx_INDH2016_Base_IDH.xlsx",
             col_names = F,
             skip = 8) %>%
  select(entidad = X1,
         indice_salud = X10,
         indice_educacion = X19,
         indice_ingreso = X28,
         indice_desarrollo_humano = X37) %>%
  slice(1:32) %>%
  mutate(entidad = ifelse(entidad == "Estado de México",
                          "Mexico",
                          no_tilde(entidad))) %>%
  left_join(.,
            planea_sexto,
            by = "entidad") %>%
  left_join(
    read_excel("2015_presupuesto.xlsx") %>%
      clean_names() %>%
      mutate(entidad = ifelse(entidad == "Estado de México",
                              "México",
                              no_tilde(entidad))),
    by = "entidad"
  ) %>%
  left_join(
    read_excel("2015_poblacion.xlsx") %>%
      mutate(entidad = no_tilde(entidad)),
    by = "entidad"
  ) %>%
  mutate(
    autorizado_per_capita = autorizado / poblacion,
    ejercido_per_capita = ejercido / poblacion,
    balance_gasto = autorizado_per_capita - ejercido_per_capita
  ) %>%
  left_join(., entidades,
            by = "entidad")

# Plots ----
tabla_2015 %>%
  ggplot() +
  aes(fct_rev(entidad), ejercido_per_capita) +
  geom_point() +
  coord_flip()

# Plots ----

variable_resultado <- names(tabla_2015[, c(6, 11, 14, 16)])
variable_contexto  <- names(tabla_2015[, c(3:5, 22, 23)])

scatter_plot <- function(variable_x, variable_y) {
  tabla_2015 %>%
    ggplot() +
    aes_string(variable_x, variable_y) +
    geom_ribbon(stat = "smooth",
                method = "loess",
                alpha = .2,
                color = NA,
                fill = "#3366FF") +
    geom_point() +
    geom_text(aes(label = abreviatura),
              color = "#333333",
              vjust = -.8,
              size = 3,
              alpha = .75) +
    geom_hline(yintercept = mean(tabla_2015[[variable_y]], na.rm = T)) +
    geom_vline(xintercept = mean(tabla_2015[[variable_x]], na.rm = T)) +
    theme_bw() +
    theme(legend.position = "none")
}

plots_2015 <-
  lapply(variable_resultado, function(variable_x){
    lapply(variable_contexto, function(variable_y){
      scatter_plot(variable_x, variable_y)
    })
  })

names(plots_2015) <- variable_resultado

plots_2015 <-
  lapply(plots_2015, function(grupo){
    names(grupo) <- variable_contexto
    grupo
  })
