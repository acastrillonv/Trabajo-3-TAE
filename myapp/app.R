library(leaflet)
library(dplyr)

# datos <- readRDS("datos.rds")

# See above for the definitions of ui and server
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
      leafletOutput(outputId = "map", width=1000, height=700),
    )
  )
)
        

server <- function(input, output) {
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>% 
    addProviderTiles("OpenStreetMap.Mapnik",options = providerTileOptions(noWrap = FALSE)) %>%
    setView(lng = -75.56, lat = 6.24, zoom = 13) # %>%
    # addCircles(lng = ~LONGITUD, lat = ~LATITUD, weight = 2, 
      #         radius = 20)
    
  })
}

shinyApp(ui = ui, server = server)