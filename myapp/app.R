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

ui <- fluidPage(
  titlePanel(p("Prediccion de incidentes viales en Medellin", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "TipoAccidente",
        label = "Tipo de Accidente",
        choices = c("Choque","Atropello","Caida de Ocupante","Volcamiento","Incendio","Otro")
      ),
      selectInput(
        inputId = "Mes",
        label = "Mes",
        choices = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
      ),
      selectInput(
        inputId = "Ano",
        label = "Ano",
        choices = 2014:2022
      ),
    ),
    
    
    mainPanel(
      leafletOutput(outputId = "mapaBarrios", width=1000, height=700),
    )
  )
)
        

server <- function(input, output) {
  
  barrios=readOGR("www/Limite_Barrio_Vereda_Catastral.shp",layer="Limite_Barrio_Vereda_Catastral")
  nombres_barrios=iconv(barrios@data$NOMBRE,"UTF-8","ISO_8859-1")
  
  colorMaker <- colorFactor(palette = c("#0d6122", "#ffa333", "#d90000","#ff0000"), 
                            levels = c("Bajo", "Medio", "Alto", "Muy Alto"))
  pal <- colorFactor(
    palette = c("#0d6122", "#ffa333", "#d90000","#ff0000"),
    domain = c("Bajo", "Medio", "Alto", "Muy Alto")
  )
  
  # Create the map
  output$mapaBarrios <- renderLeaflet({
    leaflet() %>%
      addPolygons(data=barrios,
                  weight = 1,
                  color = "white",
                  fillColor =colorMaker(calculo_indice$Riesgo),
                  fillOpacity = 0.8,
                  
                  label=paste0("<p style='font-size:20px'> <strong>Barrio: </strong>",calculo_indice$BARRIO,
                               "<br><strong>Riesgo: </strong>",calculo_indice$Riesgo,
                               "<br><strong>Comuna: </strong>",calculo_indice$COMUNA
                  )%>% lapply(htmltools::HTML),
                  
                  
      )%>%
      addTiles()
    
  })
}

shinyApp(ui = ui, server = server)