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
load("media_X_tr.RData")
load("sd_X_tr.RData")
load("ACP_c1.RData")
load("ACP_c2.RData")
load("ACP_c3.RData")
load("ACP_c4.RData")
load("ACP_c6.RData")
load("modelo_otro.RData")
load("modelo_volcamiento.RData")
load("modelo_ca.RData")
load("modelo_choque.RData")
load("modelo_atropello.RData")

#fecha festivos
festivos_2021 <-
  c(
    "2021-01-01",
    "2021-01-11",
    "2021-03-22",
    "2021-04-01",
    "2021-04-02",
    "2021-04-04",
    "2021-05-01",
    "2021-05-17",
    "2021-06-07",
    "2021-07-05",
    "2021-07-20",
    "2021-08-07",
    "2021-08-15",
    "2021-10-18",
    "2021-11-08",
    "2021-11-15",
    "2021-12-08",
    "2021-12-25"
  )

festivos_2022 <-
  c(
    "2022-01-01",
    "2022-01-10",
    "2022-03-21",
    "2022-04-14",
    "2022-04-15",
    "2022-04-17",
    "2022-05-01",
    "2022-05-30",
    "2022-06-20",
    "2022-06-27",
    "2022-07-04",
    "2022-07-20",
    "2022-08-07",
    "2022-08-17",
    "2022-10-17",
    "2022-11-07",
    "2022-11-14",
    "2022-12-08",
    "2022-12-25"
  )
fecha_festivos2 <- as.Date(c(festivos_2021, festivos_2022))
#Dias que son quincenas

iden_quincena <- function(p) {
  if (p == 15) {
    valorNuevo <- "Si"
  }
  else if (p == 30) {
    valorNuevo <- "Si"
  }
  else if (p == 31) {
    valorNuevo <- "Si"
  }
  else{
    valorNuevo <- "No"
  }
  return(valorNuevo)
}

iden_weekend <- function(p) {
  if (p == "domingo") {
    valorNuevo <- "Si"
  }
  else if (p == "sábado") {
    valorNuevo <- "Si"
  }
  else{
    valorNuevo <- "No"
  }
  return(valorNuevo)
}

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

ui <- fluidPage(
  navbarPage(
    "Predicción de incidentes viales en Medellín",
    tabPanel("Agrupamiento",
             fluidRow(
               id = 'header1',
               column(12,),
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
        column(12,),
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
              format = "yyyy-mm-dd"
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
          
        ), ),
        fluidRow(id = 'modelos',
                 column(
                   3,
                   dataTableOutput("tablaData") %>% withSpinner(color = "#0dc5c1")
                 ),)
      )
    ),
    tabPanel(
      "Predicción de la accidentalidad",
      fluidRow(
        id = 'header1',
        column(12,),
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
              value = "2021-01-01",
              min = "2021-01-01",
              max = "2022-12-31",
              format = "yyyy-mm-dd"
            ),
          ),
          column(
            3,
            style = "background-color: #e3e3e3;",
            dateInput(
              inputId = "fecha_fin",
              label = "Fecha final",
              value = "2022-12-31",
              min = "2021-01-01",
              max = "2022-12-31",
              format = "yyyy-mm-dd"
            ),
          ),
          
        ), ),
        fluidRow(id = 'modelos',
                 column(
                   3,
                   dataTableOutput("tablaPrediccion") %>% withSpinner(color = "#0dc5c1")
                 ),)
      )
    ),
  ),
)


server <- function(input, output) {
  barrios = readOGR("www/Limite_Barrio_Vereda_Catastral.shp", layer = "Limite_Barrio_Vereda_Catastral")
  
  color <-
    colorFactor(
      palette = c("#d90000",  "#ffa333", "#006400"),
      levels = c("Alta", "Media", "Leve")
    )
  pal <- colorFactor(
    palette = c("#d90000",  "#ffa333", "#006400"),
    domain = c("Alta", "Media", "Leve")
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
          barrios$NOMBRE_BAR,
          "<br><strong>Riesgo: </strong>",
          calculo_indice$Riesgo,
          "<br><strong>Comuna: </strong>",
          barrios$NOMBRE_COM
        ) %>% lapply(htmltools::HTML)
        
        
      ) %>%
      addTiles()
    
  })
  
  output$tablaData <- renderDataTable({
    if (input$clase == "Todas") {
      data <- data_corregida
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"),]
    }
    else if (input$clase == "Atropello") {
      data <-
        data_corregida[data_corregida$CLASE_ACCIDENTE == "Atropello",]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"),]
    }
    
    else if (input$clase == "Caída de Ocupante") {
      data <-
        data_corregida[data_corregida$CLASE_ACCIDENTE == "Caída de Ocupante",]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"),]
    }
    
    else if (input$clase == "Choque") {
      data <- data_corregida[data_corregida$CLASE_ACCIDENTE == "Choque",]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"),]
    }
    
    else if (input$clase == "Volcamiento") {
      data <-
        data_corregida[data_corregida$CLASE_ACCIDENTE == "Volcamiento",]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"),]
    }
    
    else if (input$clase == "Otro") {
      data <- data_corregida[data_corregida$CLASE_ACCIDENTE == "Otro",]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"),]
    }
    
    else if (input$clase == "Incendio") {
      data <-
        data_corregida[data_corregida$CLASE_ACCIDENTE == "Incendio",]
      data <-
        data[data$FECHA_ACCIDENTE >= as.Date(input$fecha_ini, format = "%d/%m/%Y") &
               data$FECHA_ACCIDENTE <= as.Date(input$fecha_fin, format = "%d/%m/%Y"),]
    }
  })
  
  output$tablaPrediccion <- renderDataTable({
    df_Pred <-
      as.data.frame(seq(
        from = as.Date(input$fecha_ini, format = "%d/%m/%Y"),
        to = as.Date(input$fecha_fin, format = "%d/%m/%Y"),
        by = 1
      ))
    names(df_Pred) <- c("Fecha")
    df_Pred$Dia <- day(df_Pred$Fecha)
    df_Pred$Semana <- week(df_Pred$Fecha)
    df_Pred$Quincena <- sapply(df_Pred$Dia, iden_quincena)
    df_Pred <-
      mutate(df_Pred, Festivo = ifelse((Fecha %in% fecha_festivos2), "Si", "No"))
    df_Pred$DiaDeLaSemana <- weekdays(df_Pred$Fecha)
    df_Pred$Finde <- sapply(df_Pred$DiaDeLaSemana, iden_weekend)
    df_Pred$MES <- month(df_Pred$Fecha)
    
    df_Pred <- df_Pred %>%
      mutate(
        MES = recode(
          MES,
          "1" = "Enero",
          "2" = "Febrero",
          "3" = "Marzo",
          "4" = "Abril",
          "5" = "Mayo",
          "6" = "Junio",
          "7" = "Julio",
          "8" = "Agosto",
          "9" = "Septiembre",
          "10" = "Octubre",
          "11" = "Noviembre",
          "12" = "Diciembre",
        )
      )
    
    df_Pred$Dia <- as.numeric(df_Pred$Dia)
    df_Pred$Semana <- as.numeric(df_Pred$Semana)
    df_Pred$DiaDeLaSemana <- as.factor(df_Pred$DiaDeLaSemana)
    df_Pred$Festivo <- as.factor(df_Pred$Festivo)
    df_Pred$Finde <- as.factor(df_Pred$Finde)
    df_Pred$MES <- as.factor(df_Pred$MES)
    df_Pred$Quincena <- as.factor(df_Pred$Quincena)
    
    ##Estandarizacion variables categorica
    variables_cat <- sapply(df_Pred, is.factor)
    data_cat <- df_Pred[variables_cat]
    data_cat <- subset(data_cat, select = -c(DiaDeLaSemana))
    onehotencoding <- dummyVars(~ ., data = data_cat)
    data_cat_dummy <-
      as.data.frame(predict(onehotencoding, data_cat))
    
    df_Pred <- subset(df_Pred, select = -c(DiaDeLaSemana, MES))
    df_Pred <- cbind(df_Pred, data_cat_dummy)
    Fecha_P <- df_Pred$Fecha
    df_Pred <-
      subset(df_Pred , select = -c(Quincena, Festivo, Finde, Fecha))
    ##Se centra la información usando la media y la desviacion de los datos con los que fue entrenado el modelo.
    df_Pred_std <-
      scale(df_Pred, center = media_X_tr, scale = sd_X_tr)
    df_Pred_std <- as.data.frame(df_Pred_std)
    
    if (input$clase == "Atropello") {
      ACP <- predict(ACP_c1, newdata = df_Pred_std)
      df_P_acp <- data.frame(ACP[, 1:6])
      predicciones <- predict(modelo_atropello, newdata = df_P_acp)
      predicciones <- as.data.frame(predicciones)
      predicciones$Fecha <- Fecha_P
    }
    
    else if (input$clase == "Caída de Ocupante") {
      ACP <- predict(ACP_c3, newdata = df_Pred_std)
      df_P_acp <- data.frame(ACP[, 1:2])
      predicciones <- predict(modelo_ca, newdata = df_P_acp)
      predicciones <- as.data.frame(predicciones)
      predicciones$Fecha <- Fecha_P
    }
    
    else if (input$clase == "Choque") {
      ACP <- predict(ACP_c2, newdata = df_Pred_std)
      df_P_acp <- data.frame(ACP[, 1:2])
      predicciones <- predict(modelo_choque, newdata = df_P_acp)
      predicciones <- as.data.frame(predicciones)
      predicciones$Fecha <- Fecha_P
    }
    
    else if (input$clase == "Volcamiento") {
      ACP <- predict(ACP_c4, newdata = df_Pred_std)
      df_P_acp <- data.frame(ACP[, 1:3])
      predicciones <-
        predict(modelo_volcamiento, newdata = df_P_acp)
      predicciones <- as.data.frame(predicciones)
      predicciones$Fecha <- Fecha_P
    }
    
    else if (input$clase == "Otro") {
      ACP <- predict(ACP_c6, newdata = df_Pred_std)
      df_P_acp <- data.frame(ACP[, 1:2])
      predicciones <- predict(modelo_otro, newdata = df_P_acp)
      predicciones <- as.data.frame(predicciones)
      predicciones$Fecha <- Fecha_P
    }
  })
}

shinyApp(ui = ui, server = server)