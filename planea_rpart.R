library(tidyverse)
library(haven)
library(ineeR)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

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

# Modelo predictivo Sobre basico ----

set.seed(1024)
p15_train <- sample_frac(p15, 0.80)
p15_test <- setdiff(p15, p15_train)

p15_train <-
  p15_train %>%
  mutate(LYC_G = ifelse(as.numeric(LYCNVL) > 1, "BASICO", "NO_BASICO"),
         MAT_G = ifelse(as.numeric(MATNVL) > 1, "BASICO", "NO_BASICO"))


p15_test <-
  p15_test %>%
  mutate(LYC_G = ifelse(as.numeric(LYCNVL) > 1, "BASICO", "NO_BASICO"),
         MAT_G = ifelse(as.numeric(MATNVL) > 1, "BASICO", "NO_BASICO"))

p15_fit_lyc <-
  p15_train %>%
  select(-c(LYC, MAT, MATNVL, LYCNVL, NOM_ENT, MAT_G)) %>%
  #filter(SERV != "PRV") %>%
  rpart(formula = LYC_G ~ . - W_FSTUWT,
        data = .,
        weights = W_FSTUWT,
        control = rpart.control(cp = 0.05, surrogatestyle = 2))

p15_test$pred <- predict(p15_fit_lyc, p15_test, type = "class")

mean(p15_test$pred == p15_test$LYC_G)
addmargins(table(p15_test$pred))
addmargins(table(p15_test$pred, p15_test$LYC_G))

rpart.plot(p15_fit_lyc)

