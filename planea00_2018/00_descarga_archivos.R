library(tidyverse)

# Instalacion ineeR
devtools::install_github(repo = "jboscomendoza/ineeR")

# Enlace de descarga desde el sitio del INEE
# Una copia del zip se encuentra en este mismo repositorio
rutas <- list()

rutas$data_lyc <- "https://www.inee.edu.mx/wp-content/uploads/2019/07/Planea00_2018_Alumnos_LyC.zip"
rutas$data_mat <- "https://www.inee.edu.mx/wp-content/uploads/2019/07/Planea00_2018_Alumnos_PM.zip"
rutas$cuest_padres <-  "https://www.inee.edu.mx/wp-content/uploads/2019/07/PLANEA00_2018_Padres.zip"
rutas$cuest_docentes <-  "https://www.inee.edu.mx/wp-content/uploads/2019/07/PLANEA00_2018_Docentes.zip"
rutas$cuest_directores <-  "https://www.inee.edu.mx/wp-content/uploads/2019/07/PLANEA00_2018_Directores.zip"

archivos <- map(rutas, str_extract, pattern = "(PLANEA|Planea).*")

map2(rutas, archivos, function(ruta, archivo) {
  download.file(url = ruta, destfile = archivo, mode = "wb")  
})

map(archivos, unzip)
