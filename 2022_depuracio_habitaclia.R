library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
#require(sf)
library(arrow)
library(plyr)
require(data.table)

##########################################
##### Adding NOMMUNI, CODIMUNI & Counting
##########################################

habit_oferta_2022 <- read.csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Habitaclia_oferta/tractament_habit_oferta_2022.csv")
habit_oferta_2022 <- fread("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Habitaclia_oferta/tractament_habit_oferta_2022.csv")
#catalunya_noms <- read.xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/Catalunya_noms_oficials.xlsx",
                            #sheetName = "Sheet1")
#data_series_habitaclia_2022 <- merge(habit_oferta_2022, catalunya_noms, by ="NOMMUNI")
#count(data_series_habitaclia_2022, "NOMMUNI")
####################
########### cleaning
####################

habit_oferta_2022 <- habit_oferta_2022 %>% 
  filter(!grepl(c('Madrid'), c(NOMMUNI)))
#data_series_habitaclia_2022 <- data_series_habitaclia_2022 %>% 
  #filter(!grepl(c('Undefined'), c(transaction_type)))
habit_oferta_2022 <- habit_oferta_2022 %>% 
  filter(!grepl(c('Font-Romeu'), c(NOMMUNI)))

count(habit_oferta_2022, "transaction_type")

##########################################
##### Adding NOMMUNI, CODIMUNI & Counting
##########################################

habit_oferta_2022 <- habit_oferta_2022[,-c(1, 15:35)]
habit_oferta_2022 <- habit_oferta_2022[order(habit_oferta_2022$date),]
habit_oferta_2022$data <- habit_oferta_2022$date

library(lubridate)
#transform data to month names
month <- data.frame(habit_oferta_2022$date <-month(ymd(habit_oferta_2022$date)))
names(month)[1] <- "mes"
test1 <- cbind(habit_oferta_2022, month)
names(habit_oferta_2022)[1] <- "mes"


library(data.table)
test2 <-  unique(setDT(test1), by = c("property_id", "mes"))
test2$property_id_1 <- test2$property_id
count(test2, "mes")
counting <- test2 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id_1"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test2 <- merge(test2, counting, by = "property_id_1")
test2$property_id_1 <- NULL
test2$property_id <- as.character(test2$property_id)
test2 <- test2[order(test2$property_id),]
count(test2, "numero_mesos_anunci_apareix")

test2$property_subtype_1 <- test2$property_subtype
names(test2)[23] <- "tipologia"

################################
#### Counting total property_id
################################

dsf1 <- test2 %>% group_by(property_id) %>% tally()
names(dsf1)[2] <- "property_id_total"
dsf2 <- merge(test2, dsf1, by= "property_id")


### Tipologias Plurifamiliar & Unifamiliar

dsf2$tipologia[which(dsf2$tipologia=="Apartamento")] <- "Plurifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Ático")] <- "Plurifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Casa")] <- "Unifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Casa adosada")] <- "Unifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Casa pareada")] <- "Unifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Unifamiliar pareada")] <- "Unifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Chalet")] <- "Unifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Dúplex")] <- "Plurifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Estudio")] <- "Plurifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Loft")] <- "Plurifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Masía")] <- "Unifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Piso")] <- "Plurifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Planta baja")] <- "Plurifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Torre")] <- "Plurifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Tríplex")] <- "Plurifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Casa-Chalet")] <- "Unifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Finca rústica")] <- "Unifamiliar"
dsf2$tipologia[which(dsf2$tipologia=="Duplex")] <- "Plurifamiliar"
dsf2 <- dsf2 %>% 
  filter(!grepl(c('Undefined'), c(property_subtype)))
count(dsf2, "transaction_type")
count(dsf2, "tipologia")
count(dsf2, "numero_mesos_anunci_apareix")
count(dsf2, "property_subtype")

dsf2 <- dsf2[, c(19, 2, 1, 3:18, 20:24)]
dsf2 <- dsf2[, c(1:19, 21, 20, 22:24)]
dsf2 <- dsf2[, c(1:12, 23, 13:22, 24)]
write.csv(dsf2, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Habitaclia_oferta/habitaclia_oferta_dades_tractades_mesos_2022.csv")

### Selecting the first row

dsh3 <-
  dsh3 %>% 
  group_by(property_id) %>% 
  filter(row_number()==1)

### Tipologias Plurifamiliar & Unifamiliar

dsh3$property_subtype[which(dsh3$property_subtype=="Apartamento")] <- "Plurifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Ático")] <- "Plurifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Casa")] <- "Unifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Casa adosada")] <- "Unifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Casa pareada")] <- "Unifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Chalet")] <- "Unifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Duplex")] <- "Plurifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Estudio")] <- "Plurifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Loft")] <- "Plurifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Masía")] <- "Unifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Piso")] <- "Plurifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Planta baja")] <- "Plurifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Torre")] <- "Plurifamiliar"
dsh3$property_subtype[which(dsh3$property_subtype=="Tríplex")] <- "Plurifamiliar"
dsh3 <- dsh3 %>% 
  filter(!grepl(c('Undefined'), c(property_subtype)))
count(dsh3, "property_subtype")


#############################################
##### Adding Mitjana de Lloguer per Municipi
#############################################

dsh4 <- data_series_habitaclia_2022 %>% group_by(NOMMUNI) %>% tally()
names(dsh4)[2] <- "frequency_Municipi"
dsh4 <- merge(dsh3, dsh4, by= "NOMMUNI")
dsh4 <- dsh4 %>% 
  filter(!grepl(c(99999998), c(price)))
dsh4 <- dsh4 %>% 
  filter(!grepl(c(309000), c(price)))
dsh4 <- dsh4 %>% 
  filter(!grepl(c(652300), c(price)))
dsh4 <- dsh4 %>% 
  filter(!grepl(c(340000), c(price)))

dsh4 <-dsh4 %>% 
  group_by(property_id) %>% 
  filter(price > 10 & price < 10000)
min(dsh4$price)
max(dsh4$price)
dsh4 <- dsh4 %>% 
  filter(!grepl(c(0), c(surface)))
count(dsh4, "surface")

### Checking the frequency of prices
price_dsh4 <- data.frame(count(dsh4, "price"))
###

dsh5 <- aggregate(dsh4$price, list(dsh4$NOMMUNI), FUN=mean)
names(dsh5)[1] <- "NOMMUNI"
names(dsh5)[2] <- "LLoguer_Mitjana_Municipi"
dsh5 <- dsh5 %>% mutate_if(is.numeric, round, digits = 1)
dsh5 <- merge(dsh4, dsh5, by= "NOMMUNI")


#############################################
##### Adding Mitjana de Preu M2 per Municipi
#############################################

dsh6 <- aggregate(dsh5$surface, list(dsh5$NOMMUNI), FUN=mean)
names(dsh6)[1] <- "NOMMUNI"
names(dsh6)[2] <- "Superficie_Mitjana_Municipi"
dsh6 <- dsh6 %>% mutate_if(is.numeric, round, digits = 1)
dsh6 <- merge(dsh5, dsh6, by= "NOMMUNI")

dsh6 <-dsh6 %>% 
  group_by(NOMMUNI) %>% 
  filter(Superficie_Mitjana_Municipi > 10 & Superficie_Mitjana_Municipi < 10000)
count(dsh6, "Superficie_Mitjana_Municipi")

dsh6 <- dsh6[ -c(3:6,9, 10, 15) ]
names(dsh6)[10] <- "ultima_data"
dsh6 <- dsh6[, c(12, 15, 16, 5, 4, 11, 10, 1:3, 6:9, 13:14)]
dsh6 <- dsh6[, c(1:9, 15, 10:14, 16)]
dsh6 <- dsh6[, c(1:8, 11, 9, 10, 12:16)]

library(lubridate)
#transform data to month names
linelist <- data.frame(dsh6$ultima_data <-month(ymd(dsh6$ultima_data), label = TRUE, abbr = FALSE))
names(linelist)[1] <- "mes"
dsh6 <- cbind(dsh6, linelist)

dsh6 <- dsh6 %>%
  mutate(
    mes = factor(mes, levels = month.name)
  ) %>%
  arrange(mes)

dsh6$mes <- as.character(dsh6$mes)

dsh6$mes[which(dsh6$mes=="January")] <- "gener-2022"
dsh6$mes[which(dsh6$mes=="February")] <- "febrer-2022"
dsh6$mes[which(dsh6$mes=="March")] <- "març-2022"
dsh6$mes[which(dsh6$mes=="April")] <- "abril-2022"
dsh6$mes[which(dsh6$mes=="May")] <- "maig-2022"
dsh6$mes[which(dsh6$mes=="June")] <- "juny-2022"
dsh6$mes[which(dsh6$mes=="July")] <- "juliol-2022"
dsh6$mes[which(dsh6$mes=="August")] <- "agost-2022"
dsh6$mes[which(dsh6$mes=="September")] <- "septembre-2022"
dsh6$mes[which(dsh6$mes=="October")] <- "octubre-2022"

dsh6 <- dsh6[, c(17, 1:16)]


##########################################

write.csv(dsh6, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Habitaclia_oferta/habitaclia_oferta_2022.csv")

##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
