rm(list = ls())
 
library(tidyverse)
library(lubridate) 
library(dplyr)
datos <- read.csv("incidentes_viales.csv", sep = ";", header = T)
apply(is.na(datos),2,sum) # total de na's por variable
