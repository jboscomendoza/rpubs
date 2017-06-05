# Paquetes ----
library(tidyverse)
library(haven)
library(ineeR)
library(rpart)
library(rpart.plot)

# Descargas ----
download.file("http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/Planea06_2015_Alumnos.zip",
              "Planea06_2015_Alumnos.zip")

download.file("http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/Estructura_de_tablas_publicas_planea.xlsx",
              "Estructura_de_tablas_publicas_planea.xls",
              mode = "wb")

unzip("Planea06_2015_Alumnos.zip")

p15 <-
  read_sav("Planea06_2015_Alumnos.sav") %>%
  select(NOM_ENT, SERV, MARGINC, TAM_LOC_PRI, I_MULTIGRADO, ID_LYC_INSTR,
         ID_MAT_INSTR, SEXO, EDAD, W_FSTUWT, AB001, AB002, AB003:AB082,
         LYC = PV1LYC, LYCNVL = LYCNVL1, MAT = PV1MAT, MATNVL = MATNVL1, RFAB)

gc()

p15 <-
  p15 %>%
  map_if(is.character, as.factor) %>%
  map_if(is.labelled, as.factor) %>%
  tbl_df()

# Modelo simple ----
png(filename = "modelo_simple_lyc.png")
p15 %>%
  select(-c(LYCNVL, MAT, MATNVL)) %>%
  rpart(formula = LYC ~ ., data = ., method = "anova") %>%
  rpart.plot()
dev.off()

png(filename = "modelo_simple_mat.png")
p15 %>%
  select(-c(LYCNVL, LYC, MATNVL)) %>%
  rpart(formula = MAT ~ ., data = ., method = "anova") %>%
  rpart.plot()
dev.off()


# Modelo excluyendo escuelas privadas ----
png(filename = "modelo_noprv_mat.png")
p15 %>%
  select(-c(LYCNVL, LYC, MATNVL)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = MAT ~ ., data = ., method = "anova") %>%
  rpart.plot()
dev.off()

png(filename = "modelo_noprv_lyc.png")
p15 %>%
  select(-c(LYCNVL, MAT, MATNVL)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = LYC ~ ., data = ., method = "anova") %>%
  rpart.plot()
dev.off()

# Modelo con pesos ----

png(filename = "modelo_noprv_peso_mat.png")
p15 %>%
  select(-c(LYCNVL, LYC, MATNVL)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = MAT ~ ., data = ., method = "anova", weights = W_FSTUWT) %>%
  rpart.plot()
dev.off()

png(filename = "modelo_noprv_peso_lyc.png")
p15 %>%
  select(-c(LYCNVL, MAT, MATNVL)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = LYC ~ ., data = ., method = "anova", weights = W_FSTUWT) %>%
  rpart.plot()
dev.off()

#                         #
# Usar siempre los pesos. #
#                         #

# Modelo con cp bajo----
png(filename = "modelo_noprv_peso_cpbajo_mat.png")
p15 %>%
  select(-c(LYCNVL, LYC, MATNVL, NOM_ENT)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = MAT ~ ., data = ., method = "anova", weights = W_FSTUWT,
        control = rpart.control(cp = 0.005)) %>%
  rpart.plot()
dev.off()

png(filename = "modelo_noprv_peso_cpbajo_lyc.png")
p15 %>%
  select(-c(LYCNVL, MAT, MATNVL, NOM_ENT)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = LYC ~ ., data = ., method = "anova", weights = W_FSTUWT,
        control = rpart.control(cp = 0.005)) %>%
  rpart.plot()
dev.off()


# Modelo con surrogate de porcentaje correcto y cp bajo
png(filename = "modelo_noprv_peso_cpbajo_surrogate_mat.png")
p15 %>%
  select(-c(LYCNVL, LYC, MATNVL, NOM_ENT)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = MAT ~ . - W_FSTUWT, data = ., method = "anova",
        weights = W_FSTUWT,
        control = rpart.control(cp = 0.005, surrogatestyle = 1)) %>%
  rpart.plot()
dev.off()

png(filename = "modelo_noprv_peso_cpbajo_surrogate_lyc.png")
p15 %>%
  select(-c(LYCNVL, MAT, MATNVL, NOM_ENT)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = LYC ~ . - W_FSTUWT, data = ., method = "anova",
        weights = W_FSTUWT,
        control = rpart.control(cp = 0.005, surrogatestyle = 1)) %>%
  rpart.plot(type = 3, extra = 101)
dev.off()

# Modelo por nivel de logro ----
png(filename = "modelo_nivel_peso_cpbajo_surrogate_lyc.png")
p15 %>%
  select(-c(LYC, MAT, MATNVL, NOM_ENT)) %>%
  rpart(formula = LYCNVL ~ . - W_FSTUWT,
        data = .,
        weights = W_FSTUWT,
        control = rpart.control(cp = 0.005, surrogatestyle = 1)) %>%
  rpart.plot(type = 3, extra = 104)
dev.off()


png(filename = "modelo_nivel_peso_cpbajo_surrogate_mat.png")
p15 %>%
  select(-c(LYC, MAT, LYCNVL, NOM_ENT)) %>%
  rpart(formula = MATNVL ~ . - W_FSTUWT,
        data = .,
        weights = W_FSTUWT,
        control = rpart.control(cp = 0.005, surrogatestyle = 1)) %>%
  rpart.plot(type = 3, extra = 104)
dev.off()



# Modelo por nivel de logro, sin privada ----
png(filename = "modelo_nivel_peso_noprv_cpbajo_surrogate_lyc.png")
p15 %>%
  select(-c(LYC, MAT, MATNVL, NOM_ENT)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = LYCNVL ~ . - W_FSTUWT,
        data = .,
        weights = W_FSTUWT,
        control = rpart.control(cp = 0.005, surrogatestyle = 1)) %>%
  rpart.plot(type = 3, extra = 104)
dev.off()


png(filename = "modelo_nivel_peso_noprv_cpbajo_surrogate_mat.png")
p15 %>%
  select(-c(LYC, MAT, LYCNVL, NOM_ENT)) %>%
  filter(SERV != "PRV") %>%
  rpart(formula = MATNVL ~ . - W_FSTUWT,
        data = .,
        weights = W_FSTUWT,
        control = rpart.control(cp = 0.005, surrogatestyle = 1)) %>%
  rpart.plot(type = 3, extra = 104)
dev.off()


# Modelo predictivo ----

set.seed(1024)
p15_train <- sample_frac(p15, 0.80)
p15_test <- setdiff(p15, p15_train)

p15_fit_lyc <-
  p15_train %>%
  select(-c(LYC, MAT, MATNVL, NOM_ENT)) %>%
  #filter(SERV != "PRV") %>%
  rpart(formula = LYCNVL ~ . - W_FSTUWT,
        data = .,
        weights = W_FSTUWT,
        control = rpart.control(cp = 0.01, surrogatestyle = 2))

p15_test$pred <- predict(p15_fit_lyc, p15_test, type = "class")

mean(p15_test$pred == p15_test$LYCNVL)
addmargins(table(p15_test$pred))
addmargins(table(p15_test$pred, p15_test$LYCNVL))

png(filename = "modelo_nivel_peso_noprv_cpbajo_surrogate_lyc.png")

dev.off()

# Modelo predictivo - Alcanzan al menos el nivel basico ----
# Variable que identifica si alcanza al menos básico
p15 <-
  p15 %>%
  mutate(LYC_G = ifelse(as.numeric(LYCNVL) > 1, "Basico", "Debajo_basico"),
         MAT_G = ifelse(as.numeric(MATNVL) > 1, "Basico", "Debajo_basico"))

# Set de entrenamiento y prueba
p15_train <- sample_frac(p15, 0.80)
p15_test <- setdiff(p15, p15_train)


# Ajuste del modelo
p15_fit_lyc <-
  p15_train %>%
  select(-c(LYC, MAT, MATNVL, LYCNVL, NOM_ENT, MAT_G)) %>%
  #filter(SERV != "PRV") %>%
  rpart(formula = LYC_G ~ . - W_FSTUWT,
        data = .,
        weights = W_FSTUWT,
        parms = list(split = "gini"),
        control = rpart.control(cp = 0.001, surrogatestyle = 2,
                                minsplit = 5000, minbucket = 1500,
                                maxdepth = 6))

# Visualizacion del modelo
rpart.plot(p15_fit_lyc, type = 4, extra = 104)

# Creacion de columna con predicciones
p15_test$pred <- predict(p15_fit_lyc, p15_test, type = "class")

# Evaluacion de las predicciones
list(
  "tabla_verdad" = addmargins(table(p15_test$pred, p15_test$LYC_G)),
  "tabla_verdad_prop" = prop.table(table(p15_test$pred, p15_test$LYC_G), 1) * 100,
  "prop_correcto" = mean(p15_test$pred == p15_test$LYC_G)
)

# Modelo podado
## Calculo del valor de cp a usar
p15_pruned_lyc <-
  prune(tree = p15_fit_lyc,
        cp = p15_fit_lyc$cptable[which.min(p15_fit_lyc$cptable[, "xerror"])])

# Visualizacion del modelo podado
rpart.plot(p15_pruned_lyc, type = 4, extra = 104)

# Creacion de prediccion podada
p15_test$pred_pruned <- predict(p15_fit_lyc, p15_test, type = "class")

# Evaluacion de las predicciones
list(
  "tabla_verdad" = addmargins(table(p15_test$pred_pruned, p15_test$LYC_G)),
  "tabla_verdad_prop" = prop.table(table(p15_test$pred_pruned, p15_test$LYC_G), 1) * 100,
  "prop_correcto"= mean(p15_test$pred_pruned == p15_test$LYC_G)
)

# Mate ----
#
p15_train <- sample_frac(p15, 0.80)
p15_test <- setdiff(p15, p15_train)


# Ajuste del modelo
p15_fit_mat <-
  p15_train %>%
  select(-c(LYC, MAT, MATNVL, LYCNVL, LYC_G, NOM_ENT)) %>%
  #filter(SERV != "PRV") %>%
  rpart(formula = MAT_G ~ . - W_FSTUWT,
        data = .,
        weights = W_FSTUWT,
        parms = list(split = "gini"),
        control = rpart.control(cp = 0.001, surrogatestyle = 2,
                                minsplit = 4500, minbucket = 1500,
                                maxdepth = 5))

# Visualizacion del modelo
rpart.plot(p15_fit_mat, type = 4, extra = 104)

# Creacion de columna con predicciones
p15_test$pred <- predict(p15_fit_mat, p15_test, type = "class")

# Evaluacion de las predicciones
list(
  "tabla_verdad" = addmargins(table(p15_test$pred, p15_test$MAT_G)),
  "tabla_verdad_prop" = prop.table(table(p15_test$pred, p15_test$MAT_G), 1) * 100,
  "prop_correcto" = mean(p15_test$pred == p15_test$MAT_G)
)

# Modelo podado
## Calculo del valor de cp a usar
p15_pruned_mat <-
  prune(tree = p15_fit_mat,
        cp = p15_fit_mat$cptable[which.min(p15_fit_mat$cptable[, "xerror"])])

# Visualizacion del modelo podado
rpart.plot(p15_pruned_mat, type = 4, extra = 104)

# Creacion de prediccion podada
p15_test$pred_pruned <- predict(p15_fit_mat, p15_test, type = "class")

# Evaluacion de las predicciones
list(
  "tabla_verdad" = addmargins(table(p15_test$pred_pruned, p15_test$MAT_G)),
  "tabla_verdad_prop" = prop.table(table(p15_test$pred_pruned, p15_test$MAT_G), 1) * 100,
  "prop_correcto"= mean(p15_test$pred_pruned == p15_test$MAT_G)
)



# Podemos mejorar el modelo transformando grupos de reactivos, que se supone forman una escala
# En una medida resumen, de modo que aporten mayor información sin redundancia. Además, podemos introducir las variables que han demostrado mayor contribución, excluyendo las demás, o al revés... Mejor probemos al revés.

# Mejor como una funcion
#

arbol_p15 <- function(tabla, var_objetivo){
  p15_train <- sample_frac(tabla, 0.80)
  p15_test <- setdiff(tabla, p15_train)

  var_formula <-
    paste0(var_objetivo, "~ .") %>%
    as.formula()

  p15_fit <-
    p15_train %>%
    rpart(formula = var_formula,
          data = .,
          weights = W_FSTUWT,
          parms = list(split = "gini"),
          control = rpart.control(cp = 0.001, surrogatestyle = 2,
                                  minsplit = 4500, minbucket = 1500,
                                  maxdepth = 5))
  gc()

  p15_test$pred <-
    predict(p15_fit, p15_test, type = "class")

  resultados <-
    list(
      "t_verdad" = addmargins(table(p15_test$pred,
                                    p15_test[[var_objetivo]])),
      "t_verdad_prop" = prop.table(
        table(p15_test$pred, p15_test[[var_objetivo]]), 1
      ) * 100,
      "prop_correcto" = mean(p15_test$pred == p15_test[[var_objetivo]])
    )

  p15_pruned <-
    prune(tree = p15_fit,
          cp = p15_fit$cptable[which.min(p15_fit$cptable[, "xerror"])])

  gc()

  p15_test$pred_pruned <- predict(p15_fit, p15_test, type = "class")

  resultados <-
    c(resultados,
      list(
        "t_verdad_prune" = addmargins(table(p15_test$pred_pruned,
                                            p15_test[[var_objetivo]])),
        "t_verdad_prop_prune" = prop.table(table(p15_test$pred_pruned,
                                                 p15_test[[var_objetivo]]), 1) * 100,
        "prop_correcto_prune"= mean(p15_test$pred_pruned == p15_test[[var_objetivo]])
      ))

  resultados
}

prop_correcto_prune <-
  map(1:10, function(x) {
    tabla_resultado <-
      arbol_p15(tabla = p15 %>% select(-c(LYC, MATNVL, MAT, LYCNVL, LYC_G, NOM_ENT)),
                arb_formula = MAT_G ~ . -W_FSTUWT)
    gc()

    tabla_resultado$prop_correcto_prune

  })


# Vamos a hacer escalas tomando los valores de las preguntas
# Más sofisticado, determinar componentes principales o factores, determinar el
# peso de cada variable y a partir de ello determinar una puntuación por
# escala

p15_escalas <-
  p15 %>%
  select(starts_with("AB")) %>%
  map(function(x) {
    x <- as.numeric(x)
    ifelse(is.na(x), 0, x)
  }) %>%
  tbl_df %>%
  transmute(
    indigena = AB001 + AB003,
    clases_lengua = AB002,
    discapacidad = AB004 + AB005 + AB006 + AB007 + AB008 + AB009,
    preescolar = AB010,
    reprobacion = AB011,
    migracion = AB012,
    expectativa = AB013,
    trabajo = AB017 + AB018,
    violencia = AB045 + AB046 + AB047 + AB048,
    conv_doc = AB050 + AB051 + AB052 + AB053 + AB054 + AB055 + AB056,
    esco_pat = AB057 + AB058,
    clases = AB069 + AB070 + AB071 + AB072 + AB073,
    act_caro = AB074 + AB075 + AB076 + AB077,
    act_pop = AB078 + AB079 + AB080,
    apoyo_pat = AB081 + AB082
  )

memes_dank <-
  bind_cols(
    p15 %>% select(SERV:I_MULTIGRADO, SEXO, EDAD, MAT_G),
    p15_escalas
  ) %>%
  filter(SERV != "PRV") %>%
  rpart(MAT_G ~ ., data = .,
        control = rpart.control(cp = 0.001, minsplit = 6000, minbucket = 2000,
                                maxdepth = 6))

rpart.plot(memes_dank, extra = 104)

bind_cols(
  p15 %>% select(SERV:I_MULTIGRADO, SEXO, EDAD, MAT_G, W_FSTUWT),
  p15_escalas
) %>%
  arbol_p15(var_objetivo = "MAT_G")

bind_cols(
  p15 %>% select(SERV:I_MULTIGRADO, SEXO, EDAD, LYC_G, W_FSTUWT),
  p15_escalas
) %>%
  arbol_p15(var_objetivo = "LYC_G")


library(randomForest)
arbol_aleatorio <-
  bind_cols(
    p15 %>% select(SERV:I_MULTIGRADO, SEXO, EDAD, LYC_G, RFAB),
    p15_escalas
  ) %>%
  map(function(x) {
    ifelse(is.na(x), 0, x)}
  ) %>%
  data.frame %>%
  sample_frac(.8) %>%
  randomForest(LYC_G ~ ., data = .)

arbol_aleatorio

varImpPlot(arbol_aleatorio,
           sort = T,
           n.var = 15,
           main = "Top 10 - Variable Importance")

# Probemos con los pesos factoriales para determinar los atributos -----

library(psych)

p15_vss <-
  vss(p15 %>%
        select(starts_with("AB")) %>%
        mutate_all(.funs = as.numeric),
      rotate = "varimax",
      n = 12)

p15_paralell <-
  fa.parallel(p15 %>%
        select(starts_with("AB")) %>%
          mutate_all(.funs = "as.numeric"),
        cor = "mixed")


library(tidyverse)
library(psych)
library(rpart)
library(rpart.plot)

# Fa con todas las variables, ocho factores de acuerdo a resultado de vss ----
# Sin embargo, después
# Varimax, para buscar ortogonalidad en los datos
# matriz de correlacion mixta, aunque sea más lento, es más preciso
# Con peso muestral e imputando la mediana

p15_fa <- list()

p15_fa$fa_8 <-
  p15 %>%
  select(starts_with("AB")) %>%
  mutate_all(.funs = as.numeric) %>%
  fa(nfactors = 8, rotate = "varimax",
     cor = "mixed", weight = p15$W_FSTUWT, impute = "median")

# Quitando items con cargas debajo de 0.25
p15_fa$fa_8_v2 <-
  p15 %>%
  select(starts_with("AB"),
         -c(AB010, AB012, AB014, AB016, AB063, AB064)) %>%
  mutate_all(.funs = as.numeric) %>%
  fa(nfactors = 8, rotate = "varimax",
     cor = "mixed", weight = p15$W_FSTUWT, impute = "median")

# quitando item sobre id de poblacion indigena
p15_fa$fa_8_v3 <-
  p15 %>%
  select(starts_with("AB"),
         -c(AB010, AB012, AB014, AB016, AB063, AB064),
         -c(AB001:AB003)) %>%
  mutate_all(.funs = as.numeric) %>%
  fa(nfactors = 8, rotate = "varimax",
     cor = "mixed", weight = p15$W_FSTUWT, impute = "median")

# Subimos la exigencia a carga de ,3 o mayor
p15_fa$fa_8_v4 <-
  p15 %>%
  select(starts_with("AB"),
         -c(AB010, AB012, AB014, AB016, AB063, AB064),
         -c(AB001:AB003),
         -c(AB011, AB013, AB015, AB033, AB060, AB071)) %>%
  mutate_all(.funs = as.numeric) %>%
  fa(nfactors = 8, rotate = "varimax",
     cor = "mixed", weight = p15$W_FSTUWT, impute = "median")

# Aunque ocho parece ser un numero razonable de factores, no son del todo claros
# intentaremos con 10, desde el principio.
# De una vez quitaremos los reactivos sobre población indígena
p15_fa$fa_10 <-
  p15 %>%
  select(starts_with("AB"),
         -c(AB001:AB003)) %>%
  mutate_all(.funs = as.numeric) %>%
  fa(nfactors = 10, rotate = "varimax",
     cor = "mixed", weight = p15$W_FSTUWT, impute = "median")

# Quitamos lo que tuvo carga menor a 0.3
p15_fa$fa_10_v2 <-
  p15 %>%
  select(starts_with("AB"),
         -c(AB001:AB003),
         -c(AB010, AB012:AB016, AB020, AB033, AB060, AB063, AB064)) %>%
  mutate_all(.funs = as.numeric) %>%
  fa(nfactors = 10, rotate = "varimax",
     cor = "mixed", weight = p15$W_FSTUWT, impute = "median")

# Quitamos un reactivo que no tiene carga suficiente y uno que conceptualmente
# no debe estar en el factor al que carga más
p15_fa$fa_10_v3 <-
  p15 %>%
  select(starts_with("AB"),
         -c(AB001:AB003),
         -c(AB010, AB012:AB016, AB020, AB033, AB060, AB063, AB064),
         -c(AB011, AB019)) %>%
  mutate_all(.funs = as.numeric) %>%
  fa(nfactors = 10, rotate = "varimax",
     cor = "poly", weight = p15$W_FSTUWT, impute = "median")

# Mandemos puntuaciones a su propio df
p15_fa_scores <-
  p15_fa$fa_10_v3$scores %>%
  tbl_df %>%
  mutate_all(.funs = function(x) (x * 10) + 50) %>%
  rename(conv_esco = MR1, bien_hoga = MR2, disc_fisi = MR5, acti_cult = MR9,
         viol_esco = MR4, bien_basi = MR6, lect_apoy = MR8, clas_extr = MR10,
         estu_expe = MR3, trab_esco = MR7)

#Hagamos una prueba
p15_with_scores <-
  bind_cols(
    select(p15, LYC_G, SEXO, EDAD, SERV, MARGINC,
           AB001:AB003, AB010, AB012:AB016, AB020, AB033, AB060, AB063, AB064,
           AB011, AB019,
           W_FSTUWT),
    p15_fa_scores
  )

p15_with_scores_train <- sample_frac(p15_with_scores, .8)
p15_with_scores_test  <- setdiff(p15_with_scores, p15_with_scores_train)

p15_with_scores_fit <-
  rpart(LYC_G ~ . -W_FSTUWT,
        data = p15_with_scores_train, weights = W_FSTUWT,
        control = rpart.control(cp = 0.0005, maxdepth = 5,
                                minsplit = 6000, minbucket = 2500))

rpart.plot(p15_with_scores_fit)

p15_with_scores_test$predict <-
  predict(p15_with_scores_fit, p15_with_scores_test, type = "class")

mean(p15_with_scores_test$predict == p15_with_scores_test$LYC_G)

prop.table(table(p15_with_scores_test$predict,
                 p15_with_scores_test$LYC_G), 1) * 100

png("p15_fac.png", width = 1440, height = 900)
rpart.plot(p15_with_scores_fit, extra = 104)
dev.off()

file.show("p15_fac.png")

# Ahora toca usar XGboost


