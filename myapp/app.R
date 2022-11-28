library(shiny)
library(shinythemes)
library(DT)
library(rgdal)
library(leaflet)
library(mapview)
library(shinycssloaders)
library(shinyWidgets)
library(data.table)
library(ggplot2)
library(kableExtra)
library(tidyverse)
library(knitr)
library(sqldf)
library(reticulate)
library(FSinR)
library(caret)
library(hash)
library(plotly)
library(lubridate)
library(dplyr)

load("calculo_indice.RData")
load("data_corregida.RData")

# sidebarLayout(
#  sidebarPanel(
#    selectInput(
#      inputId = "TipoAccidente",
#      label = "Tipo de Accidente",
#      choices = c("Choque","Atropello","Caida de Ocupante","Volcamiento","Incendio","Otro")
#    ),
#    selectInput(
#      inputId = "Mes",
#      label = "Mes",
#      choices = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
#    ),
#    selectInput(
#      inputId = "Ano",
#      label = "Ano",
#      choices = 2014:2022
#    ),
#  )

ui <- fluidPage(navbarPage(
  "Predicción de incidentes viales en Medellín",
  tabPanel("Agrupamiento",
           fluidRow(
             id = 'header1',
             column(12, ),
             mainPanel(
               leafletOutput(
                 outputId = "mapaBarrios",
                 width = 1000,
                 height = 700
               ),
             )
             
           )),
  tabPanel(
    "Historico de datos",
    fluidRow(
      id = 'header1',
      column(12, ),
      sidebarPanel(fluidRow(
        column(
          3,
          style = "background-color: #e3e3e3;",
          selectInput(
            inputId = "clase",
            label = "Clase",
            choices = c(
              "Todas",
              "Atropello",
              "Caída de Ocupante",
              "Choque",
              "Incendio",
              "Volcamiento",
              "Otro"
            )
          )
        ),
        column(
          3,
          style = "background-color: #e3e3e3;",
          dateInput(
            inputId = "fecha_ini",
            label = "Fecha inicial",
            value = "2014-07-04",
            min = "2014-07-04",
            max = "2020-08-31",
            format = "dd-mm-yyyy"
          ),
        ),
        column(
          3,
          style = "background-color: #e3e3e3;",
          dateInput(
            inputId = "fecha_fin",
            label = "Fecha final",
            value = "2020-08-31",
            min = "2014-07-04",
            max = "2020-08-31",
            format = "yyyy-mm-dd"
          ),
        ),
        
      ),),
      fluidRow(id = 'modelos',
               column(
                 3,
                 dataTableOutput("tablaData") %>% withSpinner(color = "#0dc5c1")
               ), )
    )
  ),
  tabPanel(
    "Predicción de la accidentalidad",
    fluidRow(
      id = 'header1',
      column(12, ),
      sidebarPanel(fluidRow(
        column(
          3,
          style = "background-color: #e3e3e3;",
          selectInput(
            inputId = "clase",
            label = "Clase",
            choices = c(
              "Atropello",
              "Caída de Ocupante",
              "Choque",
              "Incendio",
              "Volcamiento",
              "Otro"
            )
          )
        ),
        column(
          3,
          style = "background-color: #e3e3e3;",
          dateInput(
            inputId = "fecha_ini",
            label = "Fecha inicial",
            value = "2014-07-04",
            min = "2014-07-04",
            max = "2020-08-31",
            format = "dd-mm-yyyy"
          ),
        ),
        column(
          3,
          style = "background-color: #e3e3e3;",
          dateInput(
            inputId = "fecha_fin",
            label = "Fecha final",
            value = "2020-08-31",
            min = "2014-07-04",
            max = "2020-08-31",
            format = "yyyy-mm-dd"
          ),
        ),
        
      ),),
      fluidRow(id = 'modelos',
               column(
                 3,
                 dataTableOutput("tablaPrediccion") %>% withSpinner(color = "#0dc5c1")
               ), )
    )
  ),
),
)


server <- function(input, output) {
  barrios = readOGR("www/Limite_Barrio_Vereda_Catastral.shp", layer = "Limite_Barrio_Vereda_Catastral")
  nombres_barrios = iconv(barrios@data$NOMBRE, "UTF-8", "ISO_8859-1")
  
  color <-
    colorFactor(
      palette = c("#0d6122", "#ffa333", "#d90000", "#ff0000"),
      levels = c("Bajo", "Medio", "Alto", "Muy Alto")
    )
  pal <- colorFactor(
    palette = c("#0d6122", "#ffa333", "#d90000", "#ff0000"),
    domain = c("Bajo", "Medio", "Alto", "Muy Alto")
  )
  
  # Create the map
  output$mapaBarrios <- renderLeaflet({
    leaflet() %>%
      addPolygons(
        data = barrios,
        weight = 1,
        color = "white",
        fillColor = color(calculo_indice$Riesgo),
        fillOpacity = 0.8,
        
        label = paste0(
          "<p style='font-size:20px'> <strong>Barrio: </strong>",
          calculo_indice$BARRIO,
          "<br><strong>Riesgo: </strong>",
          calculo_indice$Riesgo,
          "<br><strong>Comuna: </strong>",
          calculo_indice$COMUNA
        ) %>% lapply(htmltools::HTML)
        
        
      ) %>%
      addTiles()
    
  })
  
  output$tablaData <- renderDataTable({
    data_corregida
    if (input$clase == "Todas") {
      data <- data_corregida
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"), ]
    }
    else if (input$clase == "Atropello") {
      data <- data_corregida[data_corregida$CLASE_ACCIDENTE == "Atropello", ]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"), ]
    }
    
    else if (input$clase == "Caída de Ocupante") {
      data <- data_corregida[data_corregida$CLASE_ACCIDENTE == "Caída de Ocupante", ]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"), ]
    }
    
    else if (input$clase == "Choque") {
      data <- data_corregida[data_corregida$CLASE_ACCIDENTE == "Choque", ]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"), ]
    }
    
    else if (input$clase == "Volcamiento") {
      data <- data_corregida[data_corregida$CLASE_ACCIDENTE == "Volcamiento", ]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"), ]
    }
    
    else if (input$clase == "Otro") {
      data <- data_corregida[data_corregida$CLASE_ACCIDENTE == "Otro", ]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"), ]
    }
    
    else if (input$clase == "Incendio") {
      data <- data_corregida[data_corregida$CLASE_ACCIDENTE == "Incendio", ]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"), ]
    }
  })
}

shinyApp(ui = ui, server = server)