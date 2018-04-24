library(tidyverse)
library(haven)
library(difR)
library(readxl)

dir.create("sav")
dir.create("plot")
dir.create("plot/icc")
dir.create("plot/resumen")
dir.create("xlsx")

# Descarga de archivos ----
# Tabla de datos
download.file(url = "http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/Planea06_2015_Alumnos.zip",
              destfile = "sav/p15_06.zip")

unzip(zipfile = "sav/p15_06.zip", exdir = "sav")

# Contenido de la prueba
download.file(url = "http://www.inee.edu.mx/images/stories/2016/planea/resultados-agosto-2016/PLANEA_6prim_Resultados_de_Logro_LyC_20160713.xlsx",
              destfile = "xlsx/esp_sexto.xlsx", mode = "wb")

sexto_cont <-
  read_excel(path = "xlsx/esp_sexto.xlsx", sheet = "9", range = "A6:B151",
             col_names = c("cont", "curr")) %>%
  mutate(clave = ifelse(grepl(x = cont, pattern = "^PE"), cont, NA),
         cont  = ifelse(!grepl(x = cont, pattern = "^PE"), cont, NA)) %>%
  fill(cont) %>%
  filter(!is.na(clave)) %>%
  dplyr::select(clave, cont, curr)

# Nombre de las variables y recodificación
# PE***: 3 en adelante, perdido. 7 == No en el cuadernillo
# AB001: 1 == Español, 2 == Lengua indigena, 3 en adelante, Lengua extranjera,
#        recodificado a NA
sexto <-
  read_sav("sav/Planea06_2015_Alumnos.sav") %>%
  dplyr::select(starts_with("PE"), AB001, ID_LYC_INSTR) %>%
  mutate_at(vars(starts_with("PE"), AB001), as.numeric) %>%
  mutate(ID_LYC_INSTR = as.factor(ID_LYC_INSTR)) %>%
  mutate(AB001 = ifelse(AB001 > 2, NA, AB001)) %>%
  mutate_at(vars(starts_with("PE")), function(val_item) {
    ifelse(va_item > 1 & va_item < 7, NA, val_item)
  }) %>%
  split(.$ID_LYC_INSTR) %>%
  map(function(forma) {
    forma <- dplyr::select(forma, -ID_LYC_INSTR)
    forma <- forma[colMeans(forma, na.rm =T) != 7]
    forma[forma == 7] <- NA
    forma[!is.na(x$AB001), ]
  })

# DIF MH, Lord, Raju ----
# Lista para almacenar resultados.
# Estructura: lista_sexto[[nombredemetodo]][[nombredeforma]]
lista_sexto <- list()

# Parámetros: Grupo focal = 2 (Lengua indígena), se realiza purificación
# se ajusta el valor de p con el método de Holm. Lord y Raju ajustan con un
# modelo de un parámetro (Rasch)
lista_sexto$MH <-
  map(sexto, function(forma){
    difMH(Data = forma, group = "AB001", focal.name = 2,
          purify = TRUE, p.adjust.method = "holm")
  })

lista_sexto$Lord <-
  map(sexto, function(forma){
    difLord(Data = forma, group = "AB001", focal.name = 2,
            model = "1PL", purify = TRUE, p.adjust.method = "holm")
  })

lista_sexto$Raju <-
  map(sexto, function(forma){
    difRaju(Data = forma, group = "AB001", focal.name = 2,
            model = "1PL", purify = TRUE, p.adjust.method = "holm")
  })

lista_sexto$Logistic <-
  map(sexto, function(forma){
    difLogistic(Data = as.data.frame(forma), group = "AB001",
                focal.name = 2, purify = TRUE, p.adjust.method = "holm")

  })

# Plots DIF ----
# Resultados por forma
map(names(lista_sexto), function(metodo) {
  map(names(lista_sexto[[metodo]]), function(forma){
    png(filename = paste0("plot/", metodo, "_", forma, ".png"))
    plot(lista_sexto[[metodo]][[forma]])
    dev.off()
  })
})

# Plots icc ----
# Curva característica del ítem, resultados del método de Lord
# que son aproximadamente equivalentes al método de Raju
map(lista_sexto$Lord, function(forma){
  item_num <- forma$names[forma$DIFitems]
  map(item_num, function(reactivo){
    png(filename = paste0("plot/icc/", reactivo, "_lord.png"))
    plot.Lord(x = forma, plot = "itemCurve", item = reactivo)
    dev.off()
  })
})

# Curva característica del ítem, resultados del método Logístico
map(lista_sexto$Logistic, function(forma){
  item_num <- forma$names[forma$DIFitems]
  map(item_num, function(reactivo){
    png(filename = paste0("plot/icc/", reactivo, "_logistic.png"))
    plot.Logistic(x = forma, plot = "itemCurve", item = reactivo)
    dev.off()
  })
})



# Resultados reporte ----
resumen <-
  map(names(lista_sexto), function(metodo){
    map(names(lista_sexto[[metodo]]), function(forma){
      res <- lista_sexto[[metodo]][[forma]]
      sexto_cont %>%
        filter(clave %in% res$names[res$DIFitems]) %>%
        mutate(metodo = metodo, forma = forma) %>%
        dplyr::select(metodo, forma, clave, clave, cont, curr)
    })
  }) %>%
  do.call(what = bind_rows)

write.csv(x = resumen, file = "xlsx/resumen.csv" ,fileEncoding = "latin1")

# Plots  ---
# sexto %>%
#   map(names) %>%
#   do.call(what = c) %>%
#   table()
# Cada reactivo aparece en dos formas

# Reactivos identificados por método ----
png(filename = "plot/resumen/00_items_identificados.png",
    width = 1440, height = 900, res = 150)
resumen %>%
  distinct(metodo, clave) %>%
  count(metodo) %>%
  ggplot() +
  aes(metodo, n) +
  geom_col(alpha = .65) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  labs(title = "Reactivos identificados por método",
       x = "Método", y = "Cantidad")
dev.off()

# Reactivos identificados, por número de veces
png(filename = "plot/resumen/01_items_identificados_veces.png",
    width = 1440, height = 900, res = 150)
resumen %>%
  count(metodo, clave, sort = T) %>%
  count(metodo, n) %>%
  ggplot() +
  aes(metodo, nn, fill = factor(n)) +
  geom_col(alpha = .65, position = "dodge") +
  geom_text(aes(label = nn),
            position = position_dodge(0.9), vjust = -0.45) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
  scale_fill_discrete(name = "Número de formas") +
  theme_minimal() +
  labs(title = "Reactivos identificados por método \nNúmero de formas en que fueron identificados",
       subtitle = "(Cada reactivo aparece en dos formas)",
       x = "Método", y = "Cantidad")
dev.off()

# Reactivos identificados, por contenido
png(filename = "plot/resumen/02_items_identificados_contenido.png",
    width = 1440, height = 900, res = 150)
resumen %>%
  distinct(metodo, clave, cont) %>%
  count(metodo, cont) %>%
  ggplot() +
  aes(metodo, n, fill = cont) +
  geom_col(alpha = .65, position = "dodge") +
  geom_text(aes(label = n),
            position = position_dodge(0.9), vjust = -0.45) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20)) +
  scale_fill_discrete(name = "Tipo de contenido") +
  theme_minimal() +
  labs(title = "Reactivos identificados por método \nTipo de contenido al que pertenecen",
       subtitle = "(El instrumento tiene tres tipos de contenido)",
       x = "Método", y = "Cantidad")
dev.off()

# Casos por forma ----
png(filename = "plot/resumen/03_casos_forma.png",
    width = 1440, height = 900, res = 150)
lapply(names(sexto), function(nombre) {
  sexto[[nombre]] %>%
    count(AB001) %>%
    mutate(Forma = nombre)
}) %>%
  invoke(.f = bind_rows) %>%
  group_by(Forma) %>%
  mutate(AB001 = recode(AB001,
                        "1" = "Español (Referencia)",
                        "2" = "Lengua indígena (Focal)"),
         Prop = (n / sum(n) * 100 )) %>%
  rename(Grupo = AB001) %>%
  ggplot() + aes(Forma, Prop, fill = Grupo) +
  geom_col(alpha = .65, position = "dodge") +
  geom_text(aes(label = round(Prop, 2)),
            position = position_dodge(0.9), vjust = -0.45) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
  labs(title = "Proporción. Grupo Focal y Grupo de Referencia",
       subtitle = "Por forma",
       y = "Proporción") +
  theme_minimal()
dev.off()

