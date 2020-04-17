library(shiny)
library(readr)
library(leaflet)


ui <- fluidPage(
  includeCSS("styles.css"),
  titlePanel("Casos de COVID-19 en México"),
  sidebarPanel(
    selectInput(
      inputId = "tipo", "Resultado del diagnóstico",
      c("Positivo COVID-19" = "Positivo SARS-CoV-2",
        "Negativo COVID-19" = "No positivo SARS-CoV-2",
        "Resultado pendiente" =  "Resultado pendiente",
        "Todos los análisis"  = "Todos los análisis")
    ),
    selectInput(
      inputId = "relacion", "Tipo de conteo",
      c("Total" = "N",
        "Por 1,000 habitantes"  = "PROP")
    ),
    br(),
    p("Fuente:",
      tags$br(),
      tags$a(href = "https://www.gob.mx/salud/documentos/datos-abiertos-152127", 
             "Secretaría de Salud. Dirección General de Epidemiología.")
    ),
  )
  ,
  mainPanel(
    leafletOutput("out_mapa")
  )
)


server <- function(input, output) {
  data_mapa <- read_rds("para_mapa.rds")
  
  mapa <- renderLeaflet({
    valor_label <-  input$relacion
    valor_radius <- paste0("ESCALA_", input$relacion)
    
    para_mapa <- data_mapa[data_mapa[["VARIABLE"]] == input$tipo, ]
    
    leaflet(para_mapa) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      addCircleMarkers(
        lat = ~LAT_DECIMAL,
        lng = ~LON_DECIMAL,
        label = paste0(para_mapa$NOM_MUN, ": ", para_mapa[[valor_label]]),
        radius = para_mapa[[valor_radius]],
        color = "black", weight = 1,
        fillColor = "#ee4481", fillOpacity = 0.8, 
        labelOptions = labelOptions(noHide = FALSE, direction = "top")
      )
  })
  
  output$out_mapa = (mapa)
}


shinyApp(ui, server)
