rm(list = ls())

library(tidyverse)
library(lubridate) 
library(dplyr)

datos <- read.csv("incidentes_viales.csv", sep = ";", header = T)

apply(is.na(datos),2,sum) # total de na's por cada variable

# SELECCÓN DE LAS VARIABLES

names(datos)

names(datos)[6] <- "DESING"

datos <- select(datos, CLASE_ACCIDENTE, DESING, FECHA_ACCIDENTES,
                GRAVEDAD_ACCIDENTE, NUMCOMUNA, BARRIO, COMUNA, LOCATION, X, Y)       

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

View(datos)

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
                GRAVEDAD_ACCIDENTE, NUMCOMUNA, BARRIO, COMUNA, LOCATION, X, Y)       

View(datos)
