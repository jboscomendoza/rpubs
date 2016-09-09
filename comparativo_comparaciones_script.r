library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(ggplot2)
library(viridis)

download.file(url = "http://www.inee.edu.mx/images/stories/documentos_pdf/Bases_Datos/EXCALE06_2009/excale-06_2009_resultados_de_logro_espanol.xls", destfile = "esp_09.xls", mode = "wb")

download.file(url = "http://www.inee.edu.mx/images/stories/documentos_pdf/Bases_Datos/EXCALE06_2009/excale-06_2009_resultados_de_logro_matematicas.xls", destfile = "mat_09.xls", mode = "wb")

download.file(url = "http://www.inee.edu.mx/images/stories/2016/EXCALE06cambios/Excale-06_2013_Resultados_Logro_Espanol.xlsx", destfile = "esp_13.xlsx", mode = "wb")

download.file(url = "http://www.inee.edu.mx/images/stories/documentos_pdf/Bases_Datos/EXCALE06_2013/Excale-06_2013_Resultados_Logro_Matematicas.xlsx", destfile = "mat_13.xlsx", mode = "wb")

download.file(url = "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_6prim_Resultados_de_Logro_LyC_20160713.xlsx", destfile = "esp_15.xlsx", mode = "wb")

download.file(url = "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_6prim_Resultados_de_Logro_Mat_20160713.xlsx", destfile = "mat_15.xlsx", mode = "wb")


mat_09 <-
    read_excel(path = "mat_09.xls", sheet = "3.8", skip = 3, col_names = F)[1:149, c(1:2)] %>%
    filter(!(X1 %in% c("Rural público", "Urbano público", "Indígena", "Privado", NA))) %>%
    rename("Estado" = X1, "mat_2009" = X3)

esp_09 <-
    read_excel(path = "esp_09.xls", sheet = "2.8", skip = 3, col_names = F)[1:149, c(1:2)] %>%
    filter(!(X1 %in% c("Rural público", "Urbano público", "Indígena", "Privado", NA))) %>%
    rename("Estado" = X1, "esp_2009" = X3)

mat_13 <-
    read_excel(path = "mat_13.xlsx", sheet = "3.8", skip = 3, col_names = F)[1:154, c(1,3)] %>%
    filter(!(X0 %in% c("Rural público", "Urbano público", "Indígena", "Privado", NA))) %>%
    rename("Estado" = X0, "mat_2013" = X2)

esp_13 <-
    read_excel(path = "esp_13.xlsx", sheet = "2.8", skip = 3, col_names = F)[1:154, c(1,3)] %>%
    filter(!(X0 %in% c("Rural público", "Urbano público", "Indígena", "Privado", NA))) %>%
    rename("Estado" = X0, "esp_2013" = X2)

mat_15 <-
    read_excel(path = "mat_15.xlsx", sheet = "6", skip = 4, col_names = F)[1:211, c(1,3)] %>%
    filter(!(X0 %in% c("Tipo de escuela", "Marginación", "Rural-Urbano", NA))) %>%
    rename("Estado" = X0, "mat_2015" = X2)

esp_15 <-
    read_excel(path = "esp_15.xlsx", sheet = "6", skip = 4, col_names = F)[1:211, c(1,3)] %>%
    filter(!(X0 %in% c("Tipo de escuela", "Marginación", "Rural-Urbano", NA))) %>%
    rename("Estado" = X0, "esp_2015" = X2)

comparativo <-
    full_join(mat_09, esp_09) %>%
    full_join(
        full_join(mat_13, esp_13)
    ) %>%
    full_join(
        full_join(mat_15, esp_15)
    )

comparativo <-
    map_at(comparativo, 2:7, as.numeric) %>% tbl_df %>%
    mutate(
        mat_dif = mat_2015 - mat_2009,
        esp_dif = esp_2015 - esp_2009,
        mat_status = ifelse(mat_dif > 0, "Incremento", "Decremento"),
        esp_status = ifelse(esp_dif > 0, "Incremento", "Decremento")
    )

comparativo <-
    comparativo %>%
    gather(asignatura, media, mat_2009:esp_2015) %>%
    separate(asignatura, c("asignatura", "aplicacion"))

comparativo %>%
    filter(asignatura == "esp") %>%
    ggplot(aes(aplicacion, media, color = esp_status)) +
    geom_line(aes(group = Estado)) +
    geom_text(aes(label = ifelse(aplicacion == 2009, Estado, ""), hjust = 1)) +
    geom_text(aes(label = ifelse(aplicacion == 2015, Estado, ""), hjust = 0)) +
    scale_color_manual(values = c("#3355cc", "#ff5533")) +
    theme_minimal()


comparativo %>%
    filter(asignatura == "mat") %>%
    ggplot(aes(aplicacion, media, color = mat_status)) +
    geom_line(aes(group = Estado)) +
    geom_text(aes(label = ifelse(aplicacion == 2009, Estado, ""), hjust = 1)) +
    geom_text(aes(label = ifelse(aplicacion == 2015, Estado, ""), hjust = 0)) +
    scale_color_manual(values = c("#3355cc", "#ff5533")) +
    theme_minimal()

comparativo %>%
    arrange(desc(mat_dif)) %>%
    distinct(Estado, .keep_all = T) %>%
    transform(Estado = reorder(Estado, mat_dif)) %>%
    na.omit() %>%
    ggplot(aes(Estado, mat_dif, fill = mat_dif)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = ifelse(mat_dif > 0, round(mat_dif, 1), "")), hjust = 0) +
    geom_text(aes(label = ifelse(mat_dif < 0, round(mat_dif, 1), "")), hjust = 1) +
    coord_flip() +
    scale_fill_viridis() +
    theme_minimal()

comparativo %>%
    arrange(desc(esp_dif)) %>%
    distinct(Estado, .keep_all = T) %>%
    transform(Estado = reorder(Estado, esp_dif)) %>%
    na.omit() %>%
    ggplot(aes(Estado, esp_dif, fill = esp_dif)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = ifelse(esp_dif > 0, round(esp_dif, 1), "")), hjust = 0) +
    geom_text(aes(label = ifelse(esp_dif < 0, round(esp_dif, 1), "")), hjust = 1) +
    coord_flip() +
    scale_fill_viridis() +
    theme_minimal()
