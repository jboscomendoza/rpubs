library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

# Datos
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", "wine.data")

# Información
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.names", "wine.names")

readLines("wine.data", n = 10)

vino <- read.table("wine.data", sep = ",", header = FALSE)

vino

readLines("wine.names", n = 10)

file.copy(from = "wine.names", to = "wine_names.txt")

file.show("wine_names.txt")

summary(vino)

nombres <-
  readLines("wine_names.txt")[58:70] %>%
  gsub("[[:cntrl:]].*\\)", "", .) %>%
  trimws() %>%
  tolower() %>%
  gsub(" |/", "_", .) %>%
  # Agregamos el nombre "tipo", para nuestra primera columna con los tipos de vino
  c("tipo", .)

names(vino) <- nombres

vino <- vino %>%
  mutate_at("tipo", factor)

set.seed(1649)
vino_entrenamiento <- sample_frac(vino, .7)

vino_prueba <- setdiff(vino, vino_entrenamiento)

arbol_1 <- rpart(formula = tipo ~ ., data = vino_entrenamiento)

arbol_1

rpart.plot(arbol_1)

prediccion_1 <- predict(arbol_1, newdata = vino_prueba, type = "class")

confusionMatrix(prediccion_1, vino_prueba[["tipo"]])

set.seed(7439)
vino_entrenamiento_2 <- sample_frac(vino, .7)

vino_prueba_2 <- setdiff(vino, vino_entrenamiento)

arbol_2 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_2)

prediccion_2 <- predict(arbol_2, newdata = vino_prueba_2, type = "class")

rpart.plot(arbol_2)

confusionMatrix(prediccion_2, vino_prueba_2[["tipo"]])

set.seed(8476)
vino_entrenamiento_3 <- sample_frac(vino, .7)

vino_prueba_3 <- setdiff(vino, vino_entrenamiento)

arbol_3 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_3)

prediccion_3 <- predict(arbol_3, newdata = vino_prueba_3, type = "class")

rpart.plot(arbol_3)

confusionMatrix(prediccion_3, vino_prueba_3[["tipo"]])

crear_sets <- function(datos, proporcion = .7) {
  sets <- list()

  sets[["entrenamiento"]] <- sample_frac(datos, proporcion)
  sets[["prueba"]] <- setdiff(datos, sets[["entrenamiento"]])

  sets
}

entrenar_arbol <- function(sets, objetivo, predictores = ".", mi_cp = .01) {
  if(length(predictores > 1)) {
    predictores <- paste0(predictores, collapse = "+")
  }
  mi_formula <- paste0(objetivo, " ~ ", predictores) %>% as.formula()

  arbol <- list()
  arbol[["modelo"]] <-
    rpart(data = sets[["entrenamiento"]], formula = mi_formula,
          control = rpart.control(cp = mi_cp, xval = 35, minsplit = 5))
  arbol[["prediccion"]] <- predict(arbol[["modelo"]], sets[["prueba"]], type = "class")
  arbol[["referencia"]] <- sets[["prueba"]][[objetivo]]

  arbol
}

obtener_diagnostico <- function(arbol, objetivo, mi_cp = 0.01) {
  diagnostico <- list()
  diagnostico[["matriz"]] <- confusionMatrix(data = arbol[["prediccion"]],
                                             reference = arbol[["referencia"]])

  cp <- with(arbol[["modelo"]], cptable[which.min(cptable[, "xerror"]), "CP"])
  cp_original <- mi_cp
  podar <- if(cp < mi_cp) "SI" else "NO"
  diagnostico[["mincp"]] <- data.frame("CP mínimo" = cp, "CP original" = cp_original, "Podar" = podar)

  diagnostico
}

crear_arbol <- function(datos, objetivo, predictores = ".", mi_cp = 0.01) {
  resultado <- list()
  resultado[["sets"]] <- crear_sets(datos)
  resultado[["arbol"]] <- entrenar_arbol(resultado[["sets"]], objetivo, predictores, mi_cp)
  resultado[["diagnostico"]] <- obtener_diagnostico(resultado[["arbol"]], objetivo, mi_cp)

  resultado
}

set.seed(1986)
unarbol <- crear_arbol(vino, "tipo", mi_cp = 0.005)

unarbol[["diagnostico"]]
