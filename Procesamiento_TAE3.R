rm(list = ls())

library(tidyverse)
library(lubridate) 
library(dplyr)

datos_originales <- read.csv("incidentes_viales.csv", sep = ";", header = T)

apply(is.na(datos_originales),2,sum) # total de na's por cada variable

# SELECCÓN DE LAS VARIABLES

names(datos_originales)

names(datos_originales)[6] <- "DESING"

datos <- select(datos_originales, CLASE_ACCIDENTE, DESING, FECHA_ACCIDENTES, DIRECCION,
                GRAVEDAD_ACCIDENTE, NUMCOMUNA, BARRIO, COMUNA, LOCATION, X, Y..)       

# CLASE_ACCIDENTE

tabla_CLASE_ACCIDENTE <- datos %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)
tabla_CLASE_ACCIDENTE

datos <- datos %>% mutate(CLASE_ACCIDENTE = ifelse(str_detect(datos$CLASE_ACCIDENTE, "^Ca(i|í|Ã)"),
                                                   "Caida Ocupante",CLASE_ACCIDENTE), 
                          CLASE_ACCIDENTE = ifelse(CLASE_ACCIDENTE %in% c("Incendio", ""), "Otro", CLASE_ACCIDENTE))

datos %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)

# DESING

datos %>% group_by(DESING) %>% summarise(n = n()) %>% arrange(-n)
datos <- datos %>% mutate(DESING = ifelse(DESING ==  "" , "Otro", DESING),
                          DESING = ifelse(DESING %in% c("Pont\\xF3n","Pontón") , "Ponton", DESING))
datos %>% group_by(DESING) %>% summarise(n = n())

summary(datos$FECHA_ACCIDENTE)

# FECHA_ACCIDENTES

FECHA_ACCIDENTE <- ymd(substr(datos$FECHA_ACCIDENTES, 1,10)) 
x <- wday(FECHA_ACCIDENTE)
HOUR  <- round(hour(hm(substr(datos$FECHA_ACCIDENTES, 12,16)))+minute(hm(substr(datos$FECHA_ACCIDENTES, 12,16)))/60, 3)
DAY <- ifelse(x == 1, "DOMINGO",
              ifelse(x == 2, "LUNES",
                     ifelse(x == 3, "MARTES",
                            ifelse(x == 4, "MIERCOLES",
                                   ifelse(x == 5, "JUEVES",
                                          ifelse(x == 6, "VIERNES","SABADO"))))))
WEEK_YEAR <- week(FECHA_ACCIDENTE)
MONTH <- as.factor(month(FECHA_ACCIDENTE))
YEAR <- year(FECHA_ACCIDENTE)
DAY_MONTH <- mday(FECHA_ACCIDENTE)
DAY_YEAR <- yday(FECHA_ACCIDENTE)
datos <- datos %>% mutate(FECHA_ACCIDENTE, HOUR, DAY, WEEK_YEAR, MONTH, YEAR, DAY_MONTH, DAY_YEAR)

datos <- select(datos, FECHA_ACCIDENTE, HOUR, DAY, WEEK_YEAR, MONTH, YEAR, DAY_MONTH, DAY_YEAR, CLASE_ACCIDENTE, DESING, 
                GRAVEDAD_ACCIDENTE, NUMCOMUNA, BARRIO, COMUNA, LOCATION, X, Y.., DIRECCION)       

# GRAVEDAD_ACCIDENTE 

datos %>% group_by(GRAVEDAD_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)
datos <- datos %>% mutate(GRAVEDAD_ACCIDENTE = ifelse(GRAVEDAD_ACCIDENTE=="Solo da\\xF1os","Solo daños",GRAVEDAD_ACCIDENTE),
                          GRAVEDAD_ACCIDENTE = ifelse(GRAVEDAD_ACCIDENTE ==  "" , "Otro", GRAVEDAD_ACCIDENTE))
datos %>% group_by(GRAVEDAD_ACCIDENTE) %>% summarise(n = n())

#### falta----------------------------------------------------------------------------------------------------------------------------

# NUMCOMUNA

datos %>% group_by(NUMCOMUNA) %>% summarise(n = n())  
datos <- datos %>% mutate(NUMCOMUNA = ifelse(NUMCOMUNA == "01", 1, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == "02", 2, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == "03", 3, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == "04", 4, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == "05", 5, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == "06", 6, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == "07", 7, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == "08", 8, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == "09", 9, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == 50, 5, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == 60, 6, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == 70, 7, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == 80, 8, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == 90, 9, NUMCOMUNA),
                          NUMCOMUNA = ifelse(NUMCOMUNA == "SN", 13, NUMCOMUNA))
(View(datos %>% group_by(NUMCOMUNA) %>% summarise(n = n())))

# COMUNA

datos %>% group_by(COMUNA) %>% summarise(n = n())  
datos <- datos %>% mutate(COMUNA = ifelse(str_detect(datos$COMUNA, "^Sebast(\xE1|á)"),
                                          "Corregimiento de San Sebastián de Palmitas",COMUNA),
                          COMUNA = ifelse(str_detect(datos$COMUNA, "^Cris(\xF3|ó)"),
                                          "Corregimiento de San Cristóbal",COMUNA))

(View(datos %>% group_by(COMUNA) %>% summarise(n = n())))


datos <- filter(datos, COMUNA != "Corregimiento de Altavista")
datos <- filter(datos, COMUNA != "Corregimiento de San Antonio de Prado")
datos <- filter(datos, COMUNA != "Corregimiento de San Cristóbal")
datos <- filter(datos, COMUNA != "Corregimiento de San Sebastián de Palmitas")
datos <- filter(datos, COMUNA != "Corregimiento de Santa Elena")


#datos <- filter(datos, COMUNA != "Corregimiento de San Crist\xF3bal")


#View(filter(datos, COMUNA != "Corregimiento de San Crist\xF3bal"))

#starts_with("Corregimiento")

#datos %>% group_by(COMUNA) %>% summarise(n = n())  
#datos <- datos %>% mutate(COMUNA = ifelse(COMUNA == select(starts_with("Corregimiento")), "CorregimientO",COMUNA))

#-------------------------------------------------------------------------------


datos <- datos %>% mutate(COMUNA = ifelse(COMUNA %in% c("Corregimiento de San Crist\xF3bal",
                                                        "Corregimiento de San Sebasti\xE1n de Palmitas") , "Corregimiento", COMUNA))
View(datos %>% group_by(COMUNA) %>% summarise(n = n()))
                          
                       
