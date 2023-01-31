library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
require(sf)
library(arrow)
library(plyr)

###################################
####### Shapefile
###################################

### Catalunya Shapefile
catalunya <- read.xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Tables/provinciesCat.xlsx",
                       sheetName = "provinciesCat")
catalunya$id <- row.names(catalunya) 

catalunya <- catalunya[, c(4, 1, 2, 3)]
catalunya <- data.frame(catalunya)
numeric <- as.numeric(catalunya$id)
catalunya <- cbind(numeric, catalunya)
catalunya$id <- NULL
names(catalunya)[1] <- "id"

catalunya <- catalunya %>% mutate(NOMPROV =
                                    case_when(id > 0   & id < 313 ~ "Barcelona", 
                                              id > 312 & id < 535 ~ "Girona",
                                              id >= 535 & id < 767 ~ "Lleida",
                                              id > 766 & id < 952 ~ "Tarragona")
)

#947 Municipis a Catalunya https://www.idescat.cat/codis/?id=50&n=11&lang=es

#1-312 Barcelona
#313-535 Lleida
#536-766 - Girona
#767-951 - Tarragona

shape_catalunya <- read_sf("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/divisions-administratives-v2r1-20220101/divisions-administratives-v2r1-municipis-50000-20220101.shp")
numeric_shape_codi_muni <- as.numeric(shape_catalunya$CODIMUNI)
names(numeric_shape_codi_muni)[1] <- "CODIMUNI"
numeric_shape_codi_muni <- data.frame(numeric_shape_codi_muni)
shape_catalunya <- cbind(numeric_shape_codi_muni, shape_catalunya)
shape_catalunya$CODIMUNI <- NULL
names(shape_catalunya)[1] <- "CODIMUNI"

catalunya_noms_oficials <- catalunya
names(catalunya_noms_oficials)[3] <- "CODIMUNI"
catalunya_noms_oficials <- catalunya_noms_oficials[-c(1,313,535, 767),]

catalunya_noms_oficials <- merge(shape_catalunya, catalunya_noms_oficials, by = "CODIMUNI")
catalunya_noms_oficials$geometry <- NULL
catalunya_noms_oficials <- catalunya_noms_oficials[,-c(4, 14)]


write.xlsx(catalunya_noms_oficials, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/catalunya_noms_oficials.xlsx")
