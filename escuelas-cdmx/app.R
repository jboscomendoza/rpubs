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
check_niv <- checkboxGroupInput(
  inputId = "check_niv", label = "Nivel", 
  choices = nom$niv, selected = nom$niv,
)

check_sos <- checkboxGroupInput(
  inputId = "check_sos", label = "Sostenimiento", 
  choices = nom$sos, selected = nom$sos,
)

check_alc <- selectInput(
  multiple = FALSE, inputId = "check_alc", label = "Alcaldía", 
  choices = nom$alc
)


check_alc_vsa <- selectInput(
  multiple = FALSE, inputId = "check_alc_vsa", label = "Alcaldía comparativa", 
  choices = nom$alc, selected = "ALVARO OBREGON"
)

check_alc_vsb <- selectInput(
  multiple = FALSE, inputId = "check_alc_vsb", label = "Alcaldía comparativa", 
  choices = nom$alc, selected = "AZCAPOTZALCO"
)


ui <- fluidPage(
  title = "Escuela de Educación Básica en la CDMX",
  titlePanel("Escuelas de Educación Básica en la CDMX"),
  sidebarLayout(
    sidebarPanel(width = 3,
      fluidRow(
        column(6, check_niv),
        column(6, check_sos)
      )
    ), 
    mainPanel(width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "General",
          fluidRow(column(6, plotlyOutput("escuelas_n")),
                   column(6, plotlyOutput("escuelas_prop")))
        ),
        tabPanel(
          "Por alcaldía",
          fluidRow(column(12, check_alc)),
          fluidRow(column(6, plotlyOutput("alcaldias_n")),
                   column(6, plotlyOutput("alcaldias_prop")))
        ),
        tabPanel(
          "Comparativo",
          fluidRow(column(6, check_alc_vsa),
                   column(6, check_alc_vsb)),
          fluidRow(column(12, plotlyOutput("comparativo_n")))
        )
      )
    )
  )
)


# Server #####
seleccion <- function(datos, input) {
  datos %>% 
    filter(nivel %in% input$check_niv) %>% 
    filter(alcaldia %in% input$check_alc) %>%
    filter(sostenimiento %in% input$check_sos)
}

create_plot_prop <- function(datos) {
  plot_ly(datos, type = "bar", x = ~Prop, y = ~nivel, 
          color = ~sostenimiento, colors = c("#2a9d8f", "#e76f51", "#e9c46a")) %>% 
    layout(
      barmode = "stack",
      xaxis = list(
        title = "Proporción (%)",
        range = list(0, 100)
      ),
      yaxis = list(
        title = "Nivel"
      ))
}

create_plot_n <- function(datos) {
  
  maximo <- max(datos$n) * 1.05
  
  plot_ly(datos, type = "bar", x = ~n, y = ~nivel, 
          marker = list(color = "#a2d2ff")) %>% 
    layout(
      xaxis = list(
        title = "Conteo (n)",
        range = list(0, maximo)
      ),
      yaxis = list(
        title = "Nivel"
      ))
}


server <- function(input, output) {
  
  escuelas_prop<- reactive({
    escuelas %>% 
      count(alcaldia, nivel, sostenimiento) %>% 
      group_by(alcaldia, nivel) %>% 
      mutate(prop = n / sum(n)) %>% 
      seleccion(input)
  })
  
  output$escuelas_n <- renderPlotly(
    escuelas %>% 
      count(nivel) %>% 
      mutate(CDMX = "CDMX") %>% 
      create_plot_n()
  )
  
  output$escuelas_prop <- renderPlotly(
    escuelas %>% 
      group_by(nivel) %>% 
      count(sostenimiento) %>% 
      mutate(
        Prop = n / sum(n),
        Prop = round(Prop * 100, 1)
      ) %>% 
      filter(nivel %in% input$check_niv) %>% 
      filter(sostenimiento %in% input$check_sos) %>% 
      create_plot_prop()
  )
  
  output$alcaldias_n <- renderPlotly(
    escuelas %>% 
      seleccion(input) %>% 
      count(nivel) %>% 
      mutate(CDMX = "CDMX") %>% 
      create_plot_n()
      
  )
  
  output$alcaldias_prop <- renderPlotly(
    escuelas %>% 
      seleccion(input) %>% 
      group_by(nivel) %>% 
      count(sostenimiento) %>% 
      mutate(
        Prop = n / sum(n),
        Prop = round(Prop * 100, 1)
      ) %>% 
      filter(nivel %in% input$check_niv) %>% 
      filter(sostenimiento %in% input$check_sos) %>% 
      create_plot_prop()
  )

  output$comparativo_n <- renderPlotly(
    escuelas %>% 
      filter(alcaldia %in% c(input$check_alc_vsa, input$check_alc_vsb)) %>%
      filter(nivel %in% input$check_niv & sostenimiento %in% input$check_sos) %>% 
      count(alcaldia, nivel) %>% 
      plot_ly(type = "scatter", x = ~nivel, y = ~n, color = ~alcaldia) %>% 
      layout(
        xaxis = list(
          title = "Conteo (n)"
          #range = list(0, maximo)
        ),
        yaxis = list(
          title = "Nivel"
        ))
  )
  
}

# Run ####
shinyApp(ui = ui, server = server)
