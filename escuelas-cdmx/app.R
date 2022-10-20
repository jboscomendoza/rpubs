library(shiny)
library(arrow)
library(dplyr)
library(ggplot2)
library(plotly)

escuelas <- 
  read_parquet("escuelas_cdmx.parquet") %>% 
  filter(!is.na(nivel))

nom <- list()

nom$niv <- sort(unique(escuelas$nivel))
nom$sos <- sort(unique(escuelas$sostenimiento))
nom$alc <- sort(unique(escuelas$alcaldia))

# UI ####
ui <- fluidPage(
  title = "Escuela de Educación Básica en la CDMX",
  titlePanel("Escuelas de Educación Básica en la CDMX"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          12,
          checkboxGroupInput(
            inputId = "check_niv", label = "Nivel", 
            choices = nom$niv, selected = nom$niv,
          )
        )
      ),
      fluidRow(
        column(
          12,
          checkboxGroupInput(
            inputId = "check_sos", label = "Sostenimiento", 
            choices = nom$sos, selected = nom$sos,
          )
        )
      ),
      fluidRow(
        column(
          12, 
          checkboxGroupInput(
            inputId = "check_alc", label = "Alcaldías", 
            choices = nom$alc, selected = nom$alc
          )
        )
      )
    ), 
    mainPanel(
      fluidRow(
        column(
          12, 
          plotlyOutput("escuelas")
        )
      ),
      fluidRow(
        column(
          12, 
          plotOutput("conteo")
        )
      )
    )
  )
)

seleccion <- function(datos, input) {
  datos %>% 
    filter(nivel %in% input$check_niv) %>% 
    filter(alcaldia %in% input$check_alc) %>%
    filter(sostenimiento %in% input$check_sos)
}


# Server #####
server <- function(input, output) {

  output$escuelas <- renderPlotly(
    escuelas %>% 
      seleccion(input) %>% 
      count(alcaldia, nivel) %>% 
      plot_ly(type = "bar", x = ~n, y = ~alcaldia, color = ~nivel) %>% 
      layout(barmode = "stack")
  )
  
  output$conteo <- renderPlot(
    escuelas %>% 
      count(alcaldia, nivel, sostenimiento) %>% 
      group_by(alcaldia, nivel) %>% 
      mutate(prop = n / sum(n)) %>% 
      seleccion(input) %>% 
      ggplot() +
      aes(prop, alcaldia, fill = sostenimiento) +
      geom_col() +
      geom_vline(xintercept = .5, color = "white", size = 1) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = c("#dd66dd", "#66dddd")) +
      facet_wrap("nivel") 
      
  )
  
}

# Run ####
shinyApp(ui = ui, server = server)
