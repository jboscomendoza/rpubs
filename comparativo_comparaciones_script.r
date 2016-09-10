library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(ggplot2)

var_archivos <-
    list(
     esp_09 = "http://www.inee.edu.mx/images/stories/documentos_pdf/Bases_Datos/EXCALE06_2009/excale-06_2009_resultados_de_logro_espanol.xls",
     mat_09 = "http://www.inee.edu.mx/images/stories/documentos_pdf/Bases_Datos/EXCALE06_2009/excale-06_2009_resultados_de_logro_matematicas.xls",
     esp_13 = "http://www.inee.edu.mx/images/stories/2016/EXCALE06cambios/Excale-06_2013_Resultados_Logro_Espanol.xlsx",
     mat_13 = "http://www.inee.edu.mx/images/stories/documentos_pdf/Bases_Datos/EXCALE06_2013/Excale-06_2013_Resultados_Logro_Matematicas.xlsx",
     esp_15 = "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_6prim_Resultados_de_Logro_LyC_20160713.xlsx",
     mat_15 = "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_6prim_Resultados_de_Logro_Mat_20160713.xlsx"
    )

download.file(url = archivos[["esp_09"]], destfile = "esp_09.xls", mode = "wb")
download.file(url = archivos[["mat_09"]], destfile = "mat_09.xls", mode = "wb")
download.file(url = archivos[["esp_13"]], destfile = "esp_13.xlsx", mode = "wb")
download.file(url = archivos[["mat_13"]], destfile = "mat_13.xlsx", mode = "wb")
download.file(url = archivos[["esp_15"]], destfile = "esp_15.xlsx", mode = "wb")
download.file(url = archivos[["mat_15"]], destfile = "mat_15.xlsx", mode = "wb")

var_omitir <- c("Rural público", "Urbano público", "Indígena", "Privado", "Tipo de escuela", "Marginación", "Rural-Urbano", NA)

mat_09 <-
    read_excel(path = "mat_09.xls", sheet = "3.8", skip = 3, col_names = F)[1:149, c(1:2)] %>%
    filter(!(X1 %in% var_omitir)) %>%
    rename("estado" = X1, "mat_2009" = X3)

esp_09 <-
    read_excel(path = "esp_09.xls", sheet = "2.8", skip = 3, col_names = F)[1:149, c(1:2)] %>%
    filter(!(X1 %in% var_omitir)) %>%
    rename("estado" = X1, "esp_2009" = X3)

mat_13 <-
    read_excel(path = "mat_13.xlsx", sheet = "3.8", skip = 3, col_names = F)[1:154, c(1,3)] %>%
    filter(!(X0 %in% var_omitir)) %>%
    rename("estado" = X0, "mat_2013" = X2)

esp_13 <-
    read_excel(path = "esp_13.xlsx", sheet = "2.8", skip = 3, col_names = F)[1:154, c(1,3)] %>%
    filter(!(X0 %in% var_omitir)) %>%
    rename("estado" = X0, "esp_2013" = X2)

mat_15 <-
    read_excel(path = "mat_15.xlsx", sheet = "6", skip = 4, col_names = F)[1:211, c(1,3)] %>%
    filter(!(X0 %in% var_omitir)) %>%
    rename("estado" = X0, "mat_2015" = X2)

esp_15 <-
    read_excel(path = "esp_15.xlsx", sheet = "6", skip = 4, col_names = F)[1:211, c(1,3)] %>%
    filter(!(X0 %in% var_omitir)) %>%
    rename("estado" = X0, "esp_2015" = X2)

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
    geom_line(aes(group = estado)) +
    geom_text(aes(label = ifelse(aplicacion == 2009, estado, ""), hjust = 1)) +
    geom_text(aes(label = ifelse(aplicacion == 2015, estado, ""), hjust = 0)) +
    theme_minimal()

comparativo %>%
    filter(asignatura == "mat") %>%
    ggplot(aes(aplicacion, media, color = mat_status)) +
    geom_line(aes(group = estado)) +
    geom_text(aes(label = ifelse(aplicacion == 2009, estado, ""), hjust = 1)) +
    geom_text(aes(label = ifelse(aplicacion == 2015, estado, ""), hjust = 0)) +
    theme_minimal()

comparativo %>%
    arrange(desc(mat_dif)) %>%
    distinct(estado, .keep_all = T) %>%
    transform(estado = reorder(estado, mat_dif)) %>%
    na.omit() %>%
    ggplot(aes(estado, mat_dif)) +
    geom_bar(stat = "identity", aes(fill = mat_status)) +
    geom_text(aes(label = ifelse(mat_dif > 0, round(mat_dif, 1), "")), hjust = 0) +
    geom_text(aes(label = ifelse(mat_dif < 0, round(mat_dif, 1), "")), hjust = 1) +
    coord_flip() +
    theme_minimal()

comparativo %>%
    arrange(desc(esp_dif)) %>%
    distinct(estado, .keep_all = T) %>%
    transform(estado = reorder(estado, esp_dif)) %>%
    na.omit() %>%
    ggplot(aes(estado, esp_dif)) +
    geom_bar(stat = "identity", aes(fill = esp_status)) +
    geom_text(aes(label = ifelse(esp_dif > 0, round(esp_dif, 1), "")), hjust = 0) +
    geom_text(aes(label = ifelse(esp_dif < 0, round(esp_dif, 1), "")), hjust = 1) +
    coord_flip() +
    theme_minimal()

comparativo %>%
    gather(tipo, valor, mat_dif, esp_dif) %>%
    distinct(valor, .keep_all = T) %>%
    mutate(diferencia = "Asignatura") %>%
    ggplot(aes(diferencia, valor)) +
    geom_boxplot(aes(fill = tipo)) +
    theme_minimal()
