# Paquetes necesarios ----
# Lectura y manipulacion de datos
library(tidyverse)
library(haven)
# Arboles de clasificacion
library(rpart)
library(rpart.plot)
# Random Forest
#library(randomForest)
# Para analisis factorial
library(psych)

# Descarga de los archivos necesarios ----
download.file("http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/Planea06_2015_Alumnos.zip",
              "Planea06_2015_Alumnos.zip")

download.file("http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/Estructura_de_tablas_publicas_planea.xlsx",
              "Estructura_de_tablas_publicas_planea.xls",
              mode = "wb")

unzip("Planea06_2015_Alumnos.zip")

# Lectura de la base de datos ----
p15 <-
  read_sav("Planea06_2015_Alumnos.sav") %>%
  select(SERV, MARGINC, TAM_LOC_PRI, I_MULTIGRADO, ID_LYC_INSTR,
         ID_MAT_INSTR, SEXO, EDAD, W_FSTUWT, AB001, AB002, AB003:AB082,
         LYC_III = LYCNVLIII1, MAT_III = MATNVLIII1, RFAB) %>%
  map_if(is.character, as.factor) %>%
  map_if(is.labelled,  as.factor) %>%
  tbl_df() %>%
  mutate(
    LYC_III = recode(LYC_III, "0" = "No", "1" = "Si"),
    MAT_III = recode(MAT_III, "0" = "No", "1" = "Si")
    )

# Limpiamos factores
p15$MAT_III[p15$MAT_III == "NaN"] <- NA
p15$MAT_III <- factor(p15$MAT_III,  levels = c("No", "Si"))

# Eliminamos todos los casos sin ninguna respuesta a las preguntas de contexto
p15 <- p15[rowSums(is.na(p15)) != ncol(p15[, 11:92]), ]

gc()

# Funcion para probar modelos de rpart -----
arbol_p15 <- function(tabla, var_objetivo, cp_usado = 0.001, tipo = "class"){
  p15_train <- sample_frac(tabla, 0.80)
  p15_test <- setdiff(tabla, p15_train)

  prob_prior <-
    if(var_objetivo == "MAT_III") {
      c(0.7725, 1 - 0.7725)
    } else {
      c(0.8068, 1 - 0.8068)
    }

  var_formula <-
    paste0(var_objetivo, "~ .") %>%
    as.formula()

  p15_fit <-
    p15_train %>%
    rpart(formula = var_formula,
          data = .,
          weights = W_FSTUWT,
          parms = list(split = "information", prior = prob_prior),
          control = rpart.control(cp = cp_usado, surrogatestyle = 0,
                                  minsplit = 4500, minbucket = 1500))
  gc()

  p15_test$pred <-
    predict(p15_fit, p15_test, type = tipo)

  resultados <-
    list(
      "m_confusion" = addmargins(table(p15_test$pred,
                                    p15_test[[var_objetivo]])),
      "m_confusion_prop" = prop.table(
        table(p15_test$pred, p15_test[[var_objetivo]]), 1
      ) * 100,
      "prop_correcto" = mean(p15_test$pred == p15_test[[var_objetivo]],
                             na.rm = T),
      "modelo" = p15_fit
    )

  p15_pruned <-
    prune(tree = p15_fit,
          cp = p15_fit$cptable[which.min(p15_fit$cptable[, "xerror"])])

  gc()

  p15_test$pred_pruned <- predict(p15_fit, p15_test, type = tipo)

  resultados <-
    c(resultados,
      list(
        "m_confusion_prune" = addmargins(table(p15_test$pred_pruned,
                                            p15_test[[var_objetivo]])),
        "m_confusion_prop_prune" = prop.table(table(p15_test$pred_pruned,
                                                 p15_test[[var_objetivo]]), 1) * 100,
        "prop_correcto_prune"= mean(p15_test$pred_pruned == p15_test[[var_objetivo]],
                                    na.rm = T),
        "modelo_prune" = p15_pruned
      ))

  resultados
}

# rpart - modelo simple----
p15_rpart <- list()

p15_rpart$simple_lyc <-
  select(p15, -c(MAT_III)) %>%
  arbol_p15(tabla = ., var_objetivo = "LYC_III")

p15_rpart$simple_mat <-
  select(p15, -c(LYC_III)) %>%
  arbol_p15(tabla = ., var_objetivo = "MAT_III")

# rpart - modelo excluyendo escuelas privadas ----
p15_rpart$nopriv_lyc <-
  select(p15, -c(MAT_III)) %>%
  filter(SERV != "PRV") %>%
  arbol_p15(tabla = ., var_objetivo = "LYC_III")

p15_rpart$nopriv_mat <-
  select(p15, -c(LYC_III)) %>%
  filter(SERV != "PRV") %>%
  arbol_p15(tabla = ., var_objetivo = "MAT_III")

# p15 - constuccion de escalas simples ----
# En una medida resumen, de modo que aporten mayor información sin redundancia.
# Sumamos el valor de las respuesta para cada pregunta para crear una puntuacion
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

# rpart - modelo con escalas simples ----
p15_rpart$esimple_lyc <-
  bind_cols(
    select(p15, -c(MAT_III, AB001:AB013, AB017, AB018, AB045:AB058, AB069:AB082)),
    p15_escalas
  ) %>%
  arbol_p15(var_objetivo = "LYC_III")

p15_rpart$esimple_mat <-
  bind_cols(
    select(p15, -c(LYC_III, AB001:AB013, AB017, AB018, AB045:AB058, AB069:AB082)),
    p15_escalas
  ) %>%
  arbol_p15(var_objetivo = "MAT_III")

# Fa con todas las variables, ocho factores de acuerdo a resultado de vss ----
library(psych)

# VSS para estimar número de factores
p15_vss <-
  vss(p15 %>%
        select(starts_with("AB")) %>%
        mutate_all(.funs = as.numeric),
      rotate = "varimax",
      n = 12)

# Ahora sí, FA con los ocho factores que encuentra fa
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

# Subimos la exigencia a carga de 0.3 o mayor
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

# Escala factorial ----

p15_rpart$esfact_lyc <-
  bind_cols(
    select(p15, SERV:W_FSTUWT, RFAB, LYC_III,
           AB001:AB003, AB010, AB012:AB016, AB020, AB033, AB060, AB063, AB064,
           AB011, AB019),
    p15_fa_scores
  ) %>%
  arbol_p15(var_objetivo = "LYC_III")

p15_rpart$esfact_mat <-
  bind_cols(
    select(p15, SERV:W_FSTUWT, RFAB, MAT_III,
           AB001:AB003, AB010, AB012:AB016, AB020, AB033, AB060, AB063, AB064,
           AB011, AB019),
    p15_fa_scores
  ) %>%
  arbol_p15(var_objetivo = "MAT_III")

# Escala factorial, sin privadas ----
p15_rpart$esfactnoprv_lyc <-
  bind_cols(
    select(p15, SERV:W_FSTUWT, RFAB, LYC_III,
           AB001:AB003, AB010, AB012:AB016, AB020, AB033, AB060, AB063, AB064,
           AB011, AB019),
    p15_fa_scores
  ) %>%
  filter(SERV != "PRV") %>%
  arbol_p15(var_objetivo = "LYC_III")

p15_rpart$esfactnoprv_mat <-
  bind_cols(
    select(p15, SERV:W_FSTUWT, RFAB, MAT_III,
           AB001:AB003, AB010, AB012:AB016, AB020, AB033, AB060, AB063, AB064,
           AB011, AB019),
    p15_fa_scores
  ) %>%
  filter(SERV != "PRV") %>%
  arbol_p15(var_objetivo = "MAT_III")

# cross_val <- map(1:10, function(x) {
#   bind_cols(
#     select(p15, SERV:W_FSTUWT, RFAB, MAT_III,
#            AB001:AB003, AB010, AB012:AB016, AB020, AB033, AB060, AB063, AB064,
#            AB011, AB019),
#     p15_fa_scores
#   ) %>%
#     filter(SERV != "PRV") %>%
#     arbol_p15(var_objetivo = "MAT_III") %>%
#     {.$prop_correcto_prune}
# })

# Gráficas de los árboles ----
# Exportemos las gráficas de los modelos
map(names(p15_rpart), function(modelo_arbol) {
  png(paste0("p15_rpart_", modelo_arbol, ".png"))
  rpart.plot(p15_rpart[[modelo_arbol]][["modelo_prune"]], extra = 104)
  dev.off()
})

# Ahora toca usar XGboost, o Naive Bayes

# Random Forest ----
# No está de más probar este, pero quizás es mejor dejarlo para el final,
# después de definir las variables
# library(randomForest)
# arbol_aleatorio <-
#   bind_cols(
#     p15 %>% select(SERV:I_MULTIGRADO, SEXO, EDAD, LYC_G, RFAB),
#     p15_escalas
#   ) %>%
#   map(function(x) {
#     ifelse(is.na(x), 0, x)}
#   ) %>%
#   data.frame %>%
#   sample_frac(.8) %>%
#   randomForest(LYC_G ~ ., data = .)
#
# arbol_aleatorio
#
# varImpPlot(arbol_aleatorio,
#            sort = T,
#            n.var = 15,
#            main = "Importancia de las variables")

