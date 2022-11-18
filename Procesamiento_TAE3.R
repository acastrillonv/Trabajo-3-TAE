rm(list = ls())

library(tidyverse)
library(lubridate) 
library(dplyr)

datos <- read.csv("incidentes_viales.csv", sep = ";", header = T)

apply(is.na(datos),2,sum) # total de na's por cada variable

# SELECCÓN DE LAS VARIABLES

names(datos)

names(datos)[6] <- "DESING"

datos <- select(datos, CLASE_ACCIDENTE, DESING, FECHA_ACCIDENTE,
                GRAVEDAD_ACCIDENTE, NUMCOMUNA, BARRIO, COMUNA, LOCATION, X, Y)       

# CLASE_ACCIDENTE

tabla_CLASE_ACCIDENTE <- datos %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)
tabla_CLASE_ACCIDENTE

datos <- datos %>% mutate(CLASE_ACCIDENTE = ifelse(str_detect(datos$CLASE_ACCIDENTE, "^Ca(i|í|Ã)"),
                                                   "Caída Ocupante",CLASE_ACCIDENTE), 
                          CLASE_ACCIDENTE = ifelse(CLASE_ACCIDENTE == "", "Otro", CLASE_ACCIDENTE),
                          CLASE_ACCIDENTE = ifelse(CLASE_ACCIDENTE == "Incendio", "Otro", CLASE_ACCIDENTE))

datos %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)

# DESING