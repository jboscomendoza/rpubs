library(tidyverse)
library(tidytext)
library(naivebayes)
library(tm)
library(caret)

download.file(url = "https://raw.githubusercontent.com/jboscomendoza/rpubs/master/bayes_twitter/tuits_bayes.csv", destfile = "tuits.csv")

tuits_df <-
read.csv("tuits_bayes.csv", stringsAsFactors = F, fileEncoding = "latin1") %>%
tbl_df

quitar_url <- function(texto) {
gsub("\\<http\\S*\\>|[0-9]", " ", texto)
}

data.frame(
"id" = 1:3,
"texto" = c("este es un texto de ejemplo", "texto distinto, distinto contenido", "conjunto de palabras nuevo"),
stringsAsFactors = F
)

data.frame(
"id" = 1:3,
"texto" = c("este es un texto de ejemplo", "texto distinto, distinto contenido", "conjunto de palabras nuevo"),
stringsAsFactors = F
) %>%
unnest_tokens(input = "texto", output = "palabra") %>%
count(id, palabra) %>%
spread(key = palabra, value = n)

tuits_df

tuits_df %>%
unnest_tokens(input = "text", output = "palabra") %>%
count(screen_name, status_id, palabra) %>%
spread(key = palabra, value = n)

crear_matriz <- function(tabla) {
tabla %>%
mutate(text = quitar_url(text)) %>%
unnest_tokens(input = "text", output = "palabra") %>%
count(screen_name, status_id, palabra) %>%
spread(key = palabra, value = n) %>%
select(-status_id)
}

ejemplo_matriz <-
tuits_df %>%
mutate(screen_name = ifelse(screen_name == "MSFTMexico", screen_name, "Otro"),
screen_name = as.factor(screen_name)) %>%
crear_matriz

elegir_usuario <- function(nombres, usuario) {
as.factor(ifelse(nombres %in% usuario, nombres, "Otro"))
}

set.seed(2001)
ejemplo_entrenamiento <- sample_frac(ejemplo_matriz, .7)
ejemplo_prueba <- setdiff(ejemplo_matriz, ejemplo_entrenamiento)

crear_sets <- function(tabla, prop = .7) {
lista_sets <- list()
lista_sets$train <- sample_frac(tabla, prop)
lista_sets$test  <- setdiff(tabla, lista_sets[["train"]])

lista_sets
}

ejemplo_modelo <- naive_bayes(formula = screen_name ~ .,  data = ejemplo_entrenamiento)

ejemplo_prediccion <- predict(ejemplo_modelo, ejemplo_prueba)

head(ejemplo_prediccion, 25)

confusionMatrix(ejemplo_prediccion, ejemplo_prueba[["screen_name"]])

obtener_bayes <- function(lista_sets, objetivo = "screen_name") {
bayes_formula<- as.formula(paste0(objetivo, "~ .") )
bayes <- list()

bayes$modelo <- naive_bayes(formula = bayes_formula, data = lista_sets[["train"]])
bayes$prediccion   <- predict(object = bayes$modelo, newdata = lista_sets[["test"]])

bayes
}

mat_conf <- function(resultado, set_test) {
confusionMatrix(resultado[["prediccion"]], set_test[["test"]][["screen_name"]])
}

ejemplo_conf <- confusionMatrix(ejemplo_prediccion, ejemplo_prueba[["screen_name"]])
plot(ejemplo_conf[["table"]])

plot_conf <- function(resultados_bayes) {
plot(resultados_bayes[["confusion"]][["table"]],
col = c("#00BBFF", "#FF6A00"),
main = resultados_bayes[["confusion"]][["positive"]])
}

hacer_bayes <- function(tabla, usuario) {
  ingenuo <- list()

  ingenuo[["matriz"]] <-
    tabla %>%
    mutate(screen_name = elegir_usuario(screen_name, usuario)) %>%
    crear_matriz()

  ingenuo[["sets"]] <- crear_sets(ingenuo[["matriz"]])

  ingenuo[["resultado"]] <- obtener_bayes(ingenuo[["sets"]])

  ingenuo[["confusion"]] <- list()

  ingenuo[["confusion"]] <- mat_conf(ingenuo[["resultado"]], ingenuo[["sets"]])

  ingenuo
}

set.seed(1988)
bayes_cmll <- hacer_bayes(tuits_df, "CMLL_OFICIAL")

bayes_cmll[["matriz"]]

bayes_cmll[["sets"]][["train"]]
bayes_cmll[["sets"]][["test"]]

bayes_cmll[["resultado"]][["modelo"]]
bayes_cmll[["resultado"]][["prediccion"]]

bayes_cmll[["confusion"]]

lista_usuarios <- list(lopezobrador_ = "lopezobrador_",
                       MSFTMexico = "MSFTMexico",
                       UNAM_MX  = "UNAM_MX",
                       CMLL_OFICIAL = "CMLL_OFICIAL")

lista_bayes <- map(lista_usuarios, hacer_bayes, tabla = tuits_df)
