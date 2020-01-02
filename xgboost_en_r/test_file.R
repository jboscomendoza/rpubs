library(tidyverse)
library(xgboost)

original <- read.delim("agaricus-lepiota.data", sep = ",", header = FALSE)

original

read_lines("agaricus-lepiota.names", skip_empty_rows = TRUE, ) %>% 
  tibble(renglones = .) %>% 
  mutate(renglones = str_squish(renglones)) %>% 
  filter(str_detect(renglones, "\\d{1,2}\\. "))

read.table("agaricus-lepiota.data", sep = ",", col)


nombres <- 
  c(
    "target", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", 
    "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape",
    "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", 
    "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", 
    "veil_color", "ring_number", "ring_type", "spore_print_color", "population",
    "habitat"
  )

original %>% 
  `names<-`(nombres) %>% 
  tbl_df()


data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')

original_01 <- 
  original %>% 
  map_df(~as.numeric(.) - 1)

set.seed(1986)
original_train <- sample_frac(original_01, size = .8)
original_test <- setdiff(original_01, original_train)

original_test_mat <- 
  xgb.DMatrix(as.matrix(original_test[-1]), 
              label = original_test$V1)
  
original_train_mat <- 
    xgb.DMatrix(as.matrix(original_train[-1]), 
                label = original_train$V1)

modelo <- xgboost(original_train_mat, nrounds = 100,
                  booster = "gbtree",
                  objective = "binary:logistic",
                  eval_metric = "error")

predict(newdata = (original_test_mat), object = modelo, type = "class")
predict(type = "class")