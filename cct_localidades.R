library(dplyr)
library(data.table)
library(readxl)
library(ggplot2)
library(ggthemes)
library(forcats)

# Archivos ----
file_cct <- "D:/jmendoza.INEE/Desktop/r_wd/catalogo_centros_trabajo/cct.csv"
file_loc <- "D:/jmendoza.INEE/Desktop/r_wd/pueblos_hablantes/catalogo_de_localidades_indigenas_2010.xlsx"
file_cem <- "D:/jmendoza.INEE/Desktop/r_wd/cemabe/TR_CENTROS.csv"

# Censo Centros de Trabajo ----
df_cct <-
  fread(file_cct) %>%
  select(ENT = entidad, MUN = municipio, LOC = localidad, CCT = clavecct,
         AMBITO = ambito, NIVEL = nnivel, SUBNIVEL = nsubnivel,
         SERVICIO = nservicio, TURNO = nturno) %>%
  mutate_at(c("ENT", "MUN", "LOC"), as.numeric)

# Catalogo localidades indigenas ----
df_loc <-
  read_excel(file_loc, sheet = 2) %>%
  filter(
    NOM_ENT != "Estados Unidos Mexicanos",
    !NOM_LOC %in% c("Total Estatal", "Total Municipal")
  ) %>%
  select(-GM_2010) %>%
  rename(POB_TOT = POBTOT, NOM_TIPO = NOMTIPO, TIPO_LOC = TIPOLOC) %>%
  mutate(POB_PROP = POB_INDI / POB_TOT) %>%
  mutate_at(c("ENT", "MUN", "LOC"), as.numeric)

# Cemabe ----
df_cem <-
  fread(file_cem) %>%
  select(CCT = CLAVE_CT, NOMBRE = NOMBRECT, ALUMNOS = P166, CAPACIDAD = P167) %>%
  mutate(CCT = gsub("[[:digit:]]$", "", CCT))

gc()

# Localidades indigenas con secundaria ----
df_sec <-
  right_join(df_cct, df_loc, by = c("ENT", "MUN", "LOC")) %>%
  filter(
    NIVEL == "SECUNDARIA",
    SUBNIVEL != "SECUNDARIA PARA TRABAJADORES",
    !TURNO %in% c("NOCTURNO", "VESPERTINO"),
    TIPO_LOC == "Loc. de 40% y más"
  ) %>%
  inner_join(., df_cem, by = "CCT") %>%
  arrange(ENT, MUN, LOC)

df_sec$ALUMNOS[df_sec$ALUMNOS > 9000] <- NA



# Guardar tabla de datos ----
saveRDS(df_sec, "df_sec.RDS")
