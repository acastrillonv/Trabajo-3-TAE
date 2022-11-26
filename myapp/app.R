library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(fdm2id)
library(flexclust)

# See above for the definitions of ui and server
ui <- fluidPage(tabPanel("Interactive map",
                      div(class="outer",
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                          ),
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%")
                                        
                          )
                          
                         
                      )
             )
        

server <- function(input, output) {
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.TonerLite",options = providerTileOptions(noWrap = FALSE)) %>% setView(lng = -75.56, lat = 6.24, zoom = 12)
    
  })
}

shinyApp(ui = ui, server = server)