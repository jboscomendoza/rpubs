library(tidyverse)
library(haven)
library(ineeR)
library(rpart)
library(rpart.plot)

download.file("http://www.inee.edu.mx/images/stories/2015/planea/bases_de_datos/agosto-2016/bases-de-datos/enero-2017/Planea06_2015_Alumnos.zip", "Planea06_2015_Alumnos.zip")

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

set.seed(1024)
p15 %>%
  select(-c(LYCNVL, MAT, MATNVL)) %>%
  sample_n(50000) %>%
  rpart(formula = LYC ~ ., data = ., method = "anova") %>%
  rpart.plot()

p15 %>%
  select(-c(LYC, MAT, MATNVL)) %>%
  filter(SERV != "PRV") %>%
  sample_n(50000) %>%
  rpart(formula = LYC ~ ., data = ., method = "anova") %>%
  rpart.plot()

png(filename = "plot.png")
p15 %>%
  select(-c(LYCNVL, MAT, MATNVL, NOM_ENT)) %>%
  sample_n(10000) %>%
  rpart(formula = LYC ~ ., data = ., method = "anova", weights = W_FSTUWT,
        control = rpart.control(cp = 0.005)) %>%
  rpart.plot()

p15 %>%
  select(-c(LYCNVL, LYC, MATNVL, NOM_ENT)) %>%
  sample_n(10000) %>%
  rpart(formula = MAT ~ ., data = ., method = "anova", weights = W_FSTUWT,
        control = rpart.control(cp = 0.005)) %>%
  rpart.plot()

dev.off()
