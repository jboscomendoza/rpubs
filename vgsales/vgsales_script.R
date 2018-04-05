library(tidyverse)
library(scales)

theme_graf <- 
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 14),
        axis.text = element_text(size = 12), 
        panel.grid.minor = element_blank(),
        legend.position = "top")

#Descargar de
# https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings/downloads/Video_Games_Sales_as_at_22_Dec_2016.csv/2

unzip("Video_Games_Sales_as_at_22_Dec_2016.csv.zip")

vgsales <- read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

summary(vgsales)

table(vgsales["User_Score"])
table(vgsales["Year_of_Release"])

vgsales <-
  vgsales %>%
  mutate(
    User_Score = ifelse(User_Score == "tbd", NA, as.numeric(User_Score)),
    User_Score = User_Score * 10,
    Year_of_Release = ifelse(Year_of_Release == "N/A", NA, Year_of_Release),
    Year_of_Release = as.numeric(as.character(Year_of_Release))
  ) %>%
  mutate_if(is.integer, as.numeric)

# Total perdidos
sum(is.na(vgsales[["Year_of_Release"]]))

# Proporcion perdidos
(sum(is.na(vgsales[["Year_of_Release"]])) / length(vgsales[["Year_of_Release"]])) * 100

vgsales %>%
  filter(is.na(Year_of_Release))

vgsales <-
  vgsales %>%
  group_by(Name) %>%
  mutate(
    Versions = n(),
    Imputed_Year = round(median(Year_of_Release, na.rm = T)),
    Year_of_Release = ifelse(is.na(Year_of_Release), Imputed_Year, Year_of_Release),
    Year_of_Release = ifelse(is.nan(Year_of_Release), NA, Year_of_Release)
  ) %>%
  ungroup()

sport_years <-
  vgsales %>%
  filter(is.na(Year_of_Release)) %>%
  mutate(year_foo = str_extract(Name, "\\d+$")) %>%
  filter(!is.na(year_foo) & Genre %in% c("Racing", "Sports")) %>%
  mutate(
    year_foo = ifelse(grepl("2K", Name), paste0(200, year_foo), year_foo),
    year_foo = ifelse(nchar(year_foo) < 2, NA, year_foo),
    year_foo = ifelse(year_foo %in% 85:99, paste0(19, year_foo), year_foo),
    year_foo = ifelse(year_foo %in% paste0("0", 1:9), paste0(20, year_foo), year_foo),
    year_foo = ifelse(year_foo %in% paste0("1", 1:9), paste0(20, year_foo), year_foo),
    year_foo = ifelse(nchar(year_foo) < 4, NA, year_foo),
    year_foo = as.numeric(year_foo) - 1
  ) %>%
  filter(!is.na(year_foo)) %>%
  select(Name, year_foo)

vgsales[vgsales[["Name"]] %in% sport_years[["Name"]], "Year_of_Release"] <-
  sport_years[["year_foo"]]

# Total
sum(is.na(vgsales[["Year_of_Release"]]))
# Porcentaje
sum(is.na(vgsales[["Year_of_Release"]])) / length(vgsales[["Year_of_Release"]])

vgsales <-
  vgsales %>%
  group_by(Platform) %>%
  mutate(
    Year_of_Release = ifelse(Year_of_Release > 2017, 2017, Year_of_Release),
    Year_of_Release = ifelse(is.na(Year_of_Release), round(median(Year_of_Release, na.rm = T)), Year_of_Release)
  ) %>%
  ungroup

vgsales %>%
  group_by(Year_of_Release) %>% 
  mutate(Total = n()) %>% 
  filter(Name %in% originales) %>%
  transmute(Imputado = length(Year_of_Release), Proporcion = (Imputado / Total) * 100) %>% 
  ungroup() %>% 
  arrange(Year_of_Release) %>% 
  distinct()

vgsales <- 
  vgsales %>% 
  filter(Year_of_Release %in% 1996:2016)

vgsales %>%
  ggplot() +
  aes(Global_Sales) +
  geom_density() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 85, by = 5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_graf

quantile(vgsales[["Global_Sales"]], probs = seq(0, 1, by = .1))

quantile(vgsales[["Global_Sales"]], probs = seq(.9, 1, by = .01))

vgsales %>%
  #filter(Global_Sales < 3.5198) %>%
  filter(Global_Sales < 1.1500) %>%
  ggplot() +
  aes(Global_Sales) +
  geom_density() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 4, by = .5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_graf

vgsales %>%
  mutate(Tipo = case_when(
    Global_Sales >= 3.5198 ~ "3% superior",
    Global_Sales >= 1.15 ~ "10% superior",
    TRUE ~  "Resto de ventas"
  )) %>%
  count(Tipo) %>%
  ggplot() +
  aes(Tipo, n, fill = Tipo) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 20000, by = 2000),
                     label = comma_format())  +
  scale_x_discrete(expand = c(0, 0)) +
  theme_graf +
  theme(legend.position = "none")

vgsales %>%
  mutate(Tipo = case_when(
    Global_Sales >= 3.5198 ~ "3% superior",
    Global_Sales >= 1.15 ~ "10% superior",
    TRUE ~  "Resto de ventas"
  )) %>%
  group_by(Tipo) %>%
  summarize(Ventas = sum(Global_Sales)) %>%
  ungroup() %>%
  mutate(Prop = Ventas / sum(Ventas)) %>%
  ggplot() +
  aes(Tipo, Prop, fill = Tipo)  +
  geom_col() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .45),
                     labels = percent_format()) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_graf +
  theme(legend.position = "none")

vgsales <-
  vgsales %>%
  group_by(Year_of_Release) %>%
  mutate(
    Tipo = Hmisc::cut2(Global_Sales,
                       quantile(Global_Sales, probs = c(.9, .97))),
    Tipo = as.numeric(as.factor(Tipo)),
    Tipo = case_when(
      Tipo == 3 ~ "3% superior",
      Tipo == 2 ~ "10% superior",
      Tipo == 1 ~ "Resto de ventas"
    ),
    Tipo = factor(Tipo, levels = c("3% superior", "10% superior", "Resto de ventas"))
  )

vgsales %>% 
  count(Year_of_Release, Tipo) %>% 
  ggplot() +
  aes(Year_of_Release, n, fill = Tipo) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1996, 2016, by = 2)) +
  theme_graf

vgsales %>%
  group_by(Year_of_Release) %>%
  summarize(Ventas = sum(Global_Sales)) %>%
  ggplot() +
  aes(Year_of_Release, Ventas)  +
  geom_line() +
  geom_point(shape = 19) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1996, 2016, by = 2)) +
  theme_graf

vgsales %>%
  group_by(Year_of_Release, Tipo) %>%
  summarize(Ventas = sum(Global_Sales)) %>%
  ggplot() +
  aes(Year_of_Release, Ventas, fill = Tipo)  +
  geom_area() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1996, 2016, by = 2)) +
  theme_graf

vgsales %>%
  group_by(Year_of_Release, Tipo) %>%
  summarize(Ventas = sum(Global_Sales)) %>%
  group_by(Year_of_Release) %>%
  mutate(Prop = Ventas / sum(Ventas)) %>%
  ggplot() +
  aes(Year_of_Release, Prop, fill = Tipo) +
  geom_area()  +
  scale_y_continuous(expand = c(0, 0), labels = percent_format(),
                     breaks = seq(0, 1, by = .1)) +
  scale_x_continuous(limits = c(1996, 2017), breaks = seq(1996, 2016, by = 2)) +
  theme_graf

vgsales %>%
  group_by(Year_of_Release, Tipo) %>%
  transmute(Ventas = sum(Global_Sales), Conteo = n()) %>%
  distinct() %>%
  group_by(Year_of_Release) %>%
  mutate(Prop = Ventas / sum(Ventas)) %>%
  ggplot() +
  aes(Year_of_Release, Prop, fill = Tipo) +
  geom_col(width = 1) +
  geom_text(aes(label = round(Conteo, 2)), position = position_stack(.5), size = 3) +
  scale_y_continuous(expand = c(0, 0), labels = percent_format()) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1996, 2016, by = 2)) +
  theme_graf

vgsales %>%
  group_by(Year_of_Release) %>%
  summarise(Media_Ventas = mean(Global_Sales)) %>%
  ggplot() +
  aes(Year_of_Release, Media_Ventas) +
  geom_line() +
  geom_point(shape = 19) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 2, by = .1)) +
  scale_x_continuous(breaks = seq(1996, 2016, by = 2)) +
  theme_graf

vgsales %>%
  group_by(Year_of_Release, Tipo) %>%
  summarise(Media_Ventas = mean(Global_Sales)) %>%
  ggplot() +
  aes(Year_of_Release, Media_Ventas, color = Tipo) +
  geom_line() +
  geom_point(shape = 19) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(breaks = seq(1996, 2016, by = 2)) +
  theme_graf

vgsales %>%
  group_by(Year_of_Release, Tipo) %>%
  summarise(Media_Ventas = mean(Global_Sales)) %>%
  group_by(Tipo) %>% 
  mutate(
    Media_Ventas = (Media_Ventas - (min(Media_Ventas))) / (max(Media_Ventas) - min(Media_Ventas))
  ) %>%
  ggplot() +
  aes(Year_of_Release, Media_Ventas, color = Tipo) +
  geom_line() +
  geom_point(shape = 19) +
  scale_x_continuous(breaks = seq(1996, 2016, by = 2)) +
  theme_graf
