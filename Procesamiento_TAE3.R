rm(list = ls())

library(tidyverse)
library(lubridate) 
library(dplyr)

data <- read.csv("incidentes_viales.csv", sep = ";", header = T)

View(data)
names(data)
apply(is.na(data),2,sum) # total de na's por cada variable

# SELECCÓN DE LAS VARIABLES

names(data)

names(data)[6] <- "DESING"


#data <- select(data, CLASE_ACCIDENTE, DESING, FECHA_ACCIDENTES, DIRECCION, GRAVEDAD_ACCIDENTE, NUMCOMUNA, BARRIO, COMUNA, LOCATION, X, Y..)       

# CLASE_ACCIDENTE

tabla_CLASE_ACCIDENTE <- data %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)
tabla_CLASE_ACCIDENTE

data <- data %>% mutate(CLASE_ACCIDENTE = ifelse(str_detect(data$CLASE_ACCIDENTE, "^Ca(i|í|Ã)"),
                                                   "Caida Ocupante",CLASE_ACCIDENTE), 
                          CLASE_ACCIDENTE = ifelse(CLASE_ACCIDENTE %in% c("Incendio", ""), "Otro", CLASE_ACCIDENTE))

data %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)

# DESING

data %>% group_by(DESING) %>% summarise(n = n()) %>% arrange(-n)
data <- data %>% mutate(DESING = ifelse(DESING ==  "" , "Otro", DESING),
                          DESING = ifelse(DESING %in% c("Pont\\xF3n","Pontón") , "Ponton", DESING))
data %>% group_by(DESING) %>% summarise(n = n())

summary(data$FECHA_ACCIDENTE)

# FECHA_ACCIDENTES

FECHA_ACCIDENTE <- ymd(substr(data$FECHA_ACCIDENTES, 1,10)) 
x <- wday(FECHA_ACCIDENTE)
HOUR  <- round(hour(hm(substr(data$FECHA_ACCIDENTES, 12,16)))+minute(hm(substr(data$FECHA_ACCIDENTES, 12,16)))/60, 3)
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
data <- data %>% mutate(FECHA_ACCIDENTE, HOUR, DAY, WEEK_YEAR, MONTH, YEAR, DAY_MONTH, DAY_YEAR)



#data <- select(data, FECHA_ACCIDENTE, HOUR, DAY, WEEK_YEAR, MONTH, YEAR, DAY_MONTH, DAY_YEAR, CLASE_ACCIDENTE, DESING, GRAVEDAD_ACCIDENTE, NUMCOMUNA, BARRIO, COMUNA, LOCATION, X, Y.., DIRECCION)       

# GRAVEDAD_ACCIDENTE 

data %>% group_by(GRAVEDAD_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)
data <- data %>% mutate(GRAVEDAD_ACCIDENTE = ifelse(GRAVEDAD_ACCIDENTE=="Solo da\\xF1os","Solo daños",GRAVEDAD_ACCIDENTE),
                          GRAVEDAD_ACCIDENTE = ifelse(GRAVEDAD_ACCIDENTE ==  "" , "Otro", GRAVEDAD_ACCIDENTE))
data %>% group_by(GRAVEDAD_ACCIDENTE) %>% summarise(n = n())

#### falta----------------------------------------------------------------------------------------------------------------------------

# NUMCOMUNA




data %>% group_by(NUMCOMUNA) %>% summarise(n = n())  
data <- data %>% mutate(NUMCOMUNA = ifelse(NUMCOMUNA == "01", 1, NUMCOMUNA),
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
View(data %>% group_by(COMUNA) %>% summarise(n = n()))




# BARRIOS

View(data %>% group_by(BARRIO) %>% summarise(n = n()))

data %>% 
  mutate(BARRIO = ifelse(BARRIO=="Aldea Pablo V","Aldea Pablo VI",BARRIO),
         BARRIO = ifelse(BARRIO=="Alejandr\\xEDa","Alejandría",BARRIO),
         BARRIO = ifelse(BARRIO=="Alejandro Echavarr\\xEDa","Alejandro Echavarría",BARRIO),
         BARRIO = ifelse(BARRIO=="Alfonso L\\xF3pez","Alfonso López",BARRIO),
         BARRIO = ifelse(BARRIO=="Altos de El Poblado","Altos del Poblado",BARRIO),
         BARRIO = ifelse(BARRIO=="Andaluc\\xEDa","Andalucía",BARRIO),
         BARRIO = ifelse(BARRIO=="Antonio Nari\\xF1o","Antonio Nariño",BARRIO),
         BARRIO = ifelse(BARRIO=="Asomadera N","Asomadera No. 1",BARRIO),
         BARRIO = ifelse(BARRIO=="Aures N","Aures No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Aures No. 2","Aures No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="B. Cerro  El Volador","B. Cerro El Volador",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrio Caycedo","Barrio Caicedo",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrio Col\\xF3n","Barrio Colombia",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrio Crist\\xF3bal","Barrio Cristóbal",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrio de Jes\\xFAs","Barrio de Jesús",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrios de Jesús","Barrio de Jesús",BARRIO),
         BARRIO = ifelse(BARRIO=="Batall\\xF3n Girardot","Batallón Girardot",BARRIO),
         BARRIO = ifelse(BARRIO=="Bel\\xE9n","Belén",BARRIO),
         BARRIO = ifelse(BARRIO=="Belalc\\xE1zar","Belalcázar",BARRIO),
         BARRIO = ifelse(BARRIO=="Berl\\xEDn","Berlin",BARRIO),
         BARRIO = ifelse(BARRIO=="Berlín","Berlin",BARRIO),
         BARRIO = ifelse(BARRIO=="Bombon\\xE1 No. 1","Bomboná No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Bomboná No. 1","Bomboná No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Bombon\\xE1 No.1","Bomboná No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Bombon\\xE1 No. 2","Bomboná No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Bomboná No. 2","Bomboná No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Boyac\\xE1","Boyacá",BARRIO),
         BARRIO = ifelse(BARRIO=="C\\xF3rdoba","Córdoba",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Vald\\xE9s No. 1","Campo Valdés No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Valdés No. 1","Campo Valdés No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Vald\\xE9s No. 2","Campo Valdés No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Vald\\xE9s No.2","Campo Valdés No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Valdés No. 2","Campo Valdés No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Valdés N","Campo Valdés No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Catalu\\xF1a","Cataluña",BARRIO),
         BARRIO = ifelse(BARRIO=="Coraz\\xF3n de Jes\\xFAs","Corazón de Jesús",BARRIO),
         BARRIO = ifelse(BARRIO=="Diego Echavarr\\xEDa","Diego Echavarría",BARRIO),
         BARRIO = ifelse(BARRIO=="El Coraz\\xF3n","El Corazón",BARRIO),
         BARRIO = ifelse(BARRIO=="El Rinc\\xF3n","El Rincón",BARRIO),
         BARRIO = ifelse(BARRIO=="El Vel\\xF3dromo","El Velódromo",BARRIO),
         BARRIO = ifelse(BARRIO=="Estaci\\xF3n Villa","Estación Villa",BARRIO),
         BARRIO = ifelse(BARRIO=="F\\xE1tima","Fátima",BARRIO),
         BARRIO = ifelse(BARRIO=="H\\xE9ctor Abad G\\xF3mez","Héctor Abad Gómez",BARRIO),
         BARRIO = ifelse(BARRIO=="Hospital San Vicente de Pa\\xFAl","Hospital San Vicente de Paúl",BARRIO),
         BARRIO = ifelse(BARRIO=="Jard\\xEDn Bot\\xE1nico","Jardín Botánico",BARRIO),
         BARRIO = ifelse(BARRIO=="Inst",NA,BARRIO),
         BARRIO = ifelse(BARRIO=="Jes\\xFAs Nazareno","Jesús Nazareno",BARRIO),
         BARRIO = ifelse(BARRIO=="La Am\\xE9rica","LA América",BARRIO),
         BARRIO = ifelse(BARRIO=="L\\xF3pez de Mesa","López de Mesa",BARRIO),
         BARRIO = ifelse(BARRIO=="La Loma de los Bernal","La Loma de Los Bernal",BARRIO),
         BARRIO = ifelse(BARRIO=="La Mansi\\xF3n","La Mansión",BARRIO),
         BARRIO = ifelse(BARRIO=="La mota","La Mota",BARRIO),
         BARRIO = ifelse(BARRIO=="La Pi\\xF1uela","La Pilarica",BARRIO),
         BARRIO = ifelse(BARRIO=="Los \\xC1ngeles","Los Alcázares",BARRIO),
         BARRIO = ifelse(BARRIO=="Los Alc\\xE1zares","Los Alcázares",BARRIO),
         BARRIO = ifelse(BARRIO=="Manrique Central No. 1","Manrique Central No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Manrique Central N","Manrique Central No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Manrique Central No. 2","Manrique Central No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Mar\\xEDa Cano Carambolas","María Cano Carambolas",BARRIO),
         BARRIO = ifelse(BARRIO=="Mosc\\xFA No. 1","Moscú No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Moscú No. 1","Moscú No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Mosc\\xFA No. 2","Moscú No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Mosc\\xFA No.2","Moscú No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Moscú No. 2","Moscú No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Moscú","Moscú No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Nueva Villa de Aburr\\xE1","Nueva Villa de Aburrá",BARRIO),
         BARRIO = ifelse(BARRIO=="Nueva Villa de la Iguan\\xE1","Nueva Villa de la Iguaná",BARRIO),
         BARRIO = ifelse(BARRIO=="Nueva Villa de La Iguaná","Nueva Villa de la Iguaná",BARRIO),
         BARRIO = ifelse(BARRIO=="Play\\xF3n de Los Comuneros","Playón de los Comuneros",BARRIO),
         BARRIO = ifelse(BARRIO=="Playón de Los Comuneros","Playón de los Comuneros",BARRIO),
         BARRIO = ifelse(BARRIO=="San Germ\\xE1n","San Germán",BARRIO),
         BARRIO = ifelse(BARRIO=="San Joaqu\\xEDn","San Joaquín",BARRIO),
         BARRIO = ifelse(BARRIO=="San Jos\\xE9 la Cima No. 1","San José la Cima No. 1",BARRIO),
         BARRIO = ifelse(BARRIO=="San Jos\\xE9 la Cima No.2","San José la Cima No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="San Mart\\xEDn de Porres","San Martín de Porres",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa F","Santa Fé",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa F\\xE9","Santa Fé",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa In\\xE9s","Santa Inés",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa Luc\\xEDa","Santa Lucía",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa M\\xF3nica","Santa Margarita",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa Mar\\xEDa de los \\xC1ngeles","Santa María de los Ángeles",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa Mar\\xEDa de Los \\xC1ngeles","Santa María de los Ángeles",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa Maria de los Angeles","Santa María de los Ángeles",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa María de Los Ángeles","Santa María de los Ángeles",BARRIO),
         BARRIO = ifelse(BARRIO=="Santo Domingo Savio No. 1","Santo Domingo Savio No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Sim\\xF3n Bol\\xEDvar","",BARRIO),
         BARRIO = ifelse(BARRIO=="Simon Bolivar","Simón Bolívar",BARRIO),
         BARRIO = ifelse(BARRIO=="Sin Nombre","El Pesebre",BARRIO),
         BARRIO = ifelse(BARRIO=="Versalles No. 1","Versalles No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Versalles No. 2","Versalles No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Villa Lilliam","Villa Lilian",BARRIO),
         BARRIO = ifelse(BARRIO=="Villa Liliam","Villa Lilian",BARRIO),
         BARRIO = ifelse(BARRIO=="Bomboná" ,"Bomboná No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Bomboná" ,"Bomboná No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Versalles N" ,"Versalles No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Versalles N","Versalles No.1",BARRIO)
  )-> data


# COMUNA

View(data %>% group_by(COMUNA) %>% summarise(n = n()))
#data <- data %>% mutate(COMUNA = ifelse(str_detect(data$COMUNA, "^Sebast(\xE1|á)"),
#                                          "Corregimiento de San Sebastián de Palmitas",COMUNA),
#                          COMUNA = ifelse(str_detect(data$COMUNA, "^Cris(\xF3|ó)"),
#                                          "Corregimiento de San Cristóbal",COMUNA))


data <- filter(data, COMUNA != "Corregimiento de Altavista")
data <- filter(data, COMUNA != "Corregimiento de San Antonio de Prado")
data <- filter(data, COMUNA != "Corregimiento de San Cristóbal")
data <- filter(data, COMUNA != "Corregimiento de San Sebastián de Palmitas")
data <- filter(data, COMUNA != "Corregimiento de San Crist\\xF3bal")
data <- filter(data, COMUNA != "Corregimiento de San Sebasti\\xE1n de Palmitas")
#data <- filter(data, DIRECCION != "CL 1 CR 3")
#data <- filter(data, DIRECCION != "CL 62 CR 131") #pertenece al corregimiento de san cristobal





#---------------------------------------------------------------------------------------
#(View(data %>% group_by(NUMCOMUNA) %>% summarise(n = n())))
#View(data <- filter(data, NUMCOMUNA == "In"))
#data <- filter(data, COMUNA != "Corregimiento de San Crist\xF3bal")
#View(filter(data, COMUNA != "Corregimiento de San Crist\xF3bal"))

#starts_with("Corregimiento")

#data %>% group_by(COMUNA) %>% summarise(n = n())  
#data <- data %>% mutate(COMUNA = ifelse(COMUNA == select(starts_with("Corregimiento")), "CorregimientO",COMUNA))
#---------------------------------------------------------------------------------------
