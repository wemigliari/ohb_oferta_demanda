library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
#require(sf)
library(arrow)
library(plyr)
library(stringr)
library(dplyr)
library(purrr)
library(lubridate)

###################################
####### Oferta Fotocasa 2019
###################################

#To disable the scientific notation in R, pass the following argument: 
options(scipen=999)
options(digits=2)

catalunya_noms <- read.xlsx("/Volumes/OHB/04.Laboratoris/LAB201703-Lloguer/04.Dades/Joffre_Wellington/Catalunya_noms_oficials.xlsx",
                            sheetName = "Sheet1")
#foto_ofer_2019 <- read.csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/extraccions_foto_oferta_2019.csv")

##### Data més recent i més antigua
#foto_ofer_2019 <- foto_ofer_2019 %>% group_by(property_id, mes)%>%slice(c(1,n()))
#####


##########################################
##### Adding NOMMUNI, CODIMUNI & Cleaning
##########################################
foto_ofer_2019 <- foto_oferta_2019
foto_ofer_2019$NOMMUNI <- foto_ofer_2019$level5

foto_ofer_2019$NOMMUNI<-gsub(" Capital","",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub(" Girona","Girona",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub(" Lleida","Lleida",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub("L'Hospitalet de Llobregat","l'Hospitalet de Llobregat",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub("L'Ametlla de Mar","l'Ametlla de Mar",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub("L'Ametlla del Vallès","l'Ametlla del Vallès",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub("El Bruc","el Bruc",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub("El Brull","el Brull",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub("El Vendrell","el Vendrell",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub("L'","l'",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub("El","el",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI<-gsub("La","la",as.character(foto_ofer_2019$NOMMUNI))
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Torrent (Girona)")] <- "Torrent"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Alàs I Cerc")] <- "Alàs i Cerc"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Avellanes i Santa Linya")] <- "les Avellanes i Santa Linya"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Borges Blanques")] <- "les Borges Blanques"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Borges del Camp")] <- "les Borges del Camp"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Cabanyes")] <- "les Cabanyes"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Franqueses del Vallès")] <- "les Franqueses del Vallès"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Masies de Roda")] <- "les Masies de Roda"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Masies de Voltregà")] <- "les Masies de Voltregà"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Piles")] <- "les Piles"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Preses")] <- "les Preses"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Les Valls de Valira")] <- "les Valls de Valira"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="de Les")] <- "de les"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="l'Ametlla de Mar ")] <- "l'Ametlla de Mar"
foto_ofer_2019$NOMMUNI[which(foto_ofer_2019$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"

foto_ofer_2019$level7[which(foto_ofer_2019$level7=="Sants - Montjuïc")] <- "Sants-Montjuïc"
foto_ofer_2019$level7[which(foto_ofer_2019$level7=="Sarrià - Sant Gervasi")] <- "Sarrià-Sant Gervasi"
foto_ofer_2019$level7[which(foto_ofer_2019$level7=="Horta - Guinardò")] <- "Horta-Guinardò"


#foto_ofer_2019 <- foto_ofer_2019[,c(2, 16, 24, 26, 27, 7, 13, 5, 6, 18, 4, 3, 19, 17, 29)]
foto_ofer_2019 <- foto_ofer_2019 %>%
  mutate_if(is.character, str_trim)

foto_ofer_2019$property_id <- as.character(foto_ofer_2019$property_id)

############################################################
############ Adding month, date and trimester
############################################################

foto_ofer_2019$mes <- foto_ofer_2019$date
data.frame(foto_ofer_2019$mes <-month(ymd(foto_ofer_2019$mes)))
foto_ofer_2019$date <- as.Date(foto_ofer_2019$date)
foto_ofer_2019$any <- format(as.Date(foto_ofer_2019$date, format="%Y/%m/%d"),"%Y")

#Adding first date column
test11a <- foto_ofer_2019[order(foto_ofer_2019$date_posting),]

test11a <- test11a %>%
  group_by(property_id) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = date_posting)
test11a <- test11a[,c(13,18)]

#Adding last date column

test11b <- foto_ofer_2019 %>%
  group_by(property_id) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = date)
test11b <- test11b[,c(13,18)]

test111 <- join(test11a, test11b, by = "property_id")
test111 <- join(foto_ofer_2019, test111, by = "property_id")
test111 <- as.data.frame(test111)


#####################

test111 <- test111 %>% mutate(trimestre =
                                dplyr::case_when(mes <= 3 & any == "2019" ~ "2019/T1", 
                                                 mes >= 4 & mes <= 6 & any == "2019" ~ "2019/T2",
                                                 mes >= 7 & mes <= 9 & any == "2019" ~ "2019/T3",
                                                 mes >= 10 & mes <= 12 & any == "2019" ~ "2019/T4",
                                                 mes <= 3 & any == "2020" ~ "2020/T1",
                                                 mes >= 4 & mes <= 6 & any == "2020" ~ "2020/T2",
                                                 mes >= 7 & mes <= 9 & any == "2020" ~ "2020/T3",
                                                 mes >= 10 & mes <= 12 & any == "2020" ~ "2020/T4",
                                                 mes <= 3 & any == "2021" ~ "2021/T1",
                                                 mes >= 4 & mes <= 6 & any == "2021" ~ "2021/T2",
                                                 mes >= 7 & mes <= 9 & any == "2021" ~ "2021/T3",
                                                 mes >= 10 & mes <= 12 & any == "2021" ~ "2021/T4",
                                                 mes <= 3 & any == "2022" ~ "2022/T1",
                                                 mes >= 4 & mes <= 6 & any == "2022" ~ "2022/T2",
                                                 mes >= 7 & mes <= 9 & any == "2022" ~ "2022/T3",
                                                 mes >= 10 & mes <= 12 & any == "2022" ~ "2022/T4")
)


############################################################
############ Mitjanes Mensuals
############################################################
mitjana_preu_mes <- test111 %>% aggregate(price~property_id+mes, mean, digits=2)
mitjana_preu_mes$mes <- NULL
names(mitjana_preu_mes)[2] <- "mitjana_preu_mes"
mitjana_superficie_mes <- test111 %>% aggregate(surface~property_id+mes, mean)
mitjana_superficie_mes$mes <- NULL
names(mitjana_superficie_mes)[2] <- "mitjana_superficie_mes"
############################################################
############################################################

test111 <- test111 %>% group_by(property_id, mes, trimestre) %>%
  filter(row_number()==1)

############
test111 <- join(test111, mitjana_preu_mes, by = "property_id" )
test111 <- join(test111, mitjana_superficie_mes, by = "property_id")

############################################################
############ Tipologias Plurifamiliar & Unifamiliar
############################################################

test111$tipologia <- test111$property_subtype

test111$tipologia[which(test111$tipologia=="Apartamento")] <- "Plurifamiliar"
test111$tipologia[which(test111$tipologia=="Ático")] <- "Plurifamiliar"
test111$tipologia[which(test111$tipologia=="Casa")] <- "Unifamiliar"
test111$tipologia[which(test111$tipologia=="Casa adosada")] <- "Unifamiliar"
test111$tipologia[which(test111$tipologia=="Casa pareada")] <- "Unifamiliar"
test111$tipologia[which(test111$tipologia=="Unifamiliar pareada")] <- "Unifamiliar"
test111$tipologia[which(test111$tipologia=="Chalet")] <- "Unifamiliar"
test111$tipologia[which(test111$tipologia=="Dúplex")] <- "Plurifamiliar"
test111$tipologia[which(test111$tipologia=="Estudio")] <- "Plurifamiliar"
test111$tipologia[which(test111$tipologia=="Loft")] <- "Plurifamiliar"
test111$tipologia[which(test111$tipologia=="Masía")] <- "Unifamiliar"
test111$tipologia[which(test111$tipologia=="Piso")] <- "Plurifamiliar"
test111$tipologia[which(test111$tipologia=="Planta baja")] <- "Plurifamiliar"
test111$tipologia[which(test111$tipologia=="Torre")] <- "Plurifamiliar"
test111$tipologia[which(test111$tipologia=="Tríplex")] <- "Plurifamiliar"
test111$tipologia[which(test111$tipologia=="Casa-Chalet")] <- "Unifamiliar"
test111$tipologia[which(test111$tipologia=="Finca rústica")] <- "Unifamiliar"
test111$tipologia[which(test111$tipologia=="Duplex")] <- "Plurifamiliar"

test111$level7[which(test111$level7=="Sants Montjuïc")] <- "Sants-Montjuïc"
test111$level7[which(test111$level7=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
test111$level7[which(test111$level7=="Horta Guinardó")] <- "Horta-Guinardò"
test111$level7[which(test111$level7=="Horta - Guinardó")] <- "Horta-Guinardò"

test111 <- test111 %>% group_by(property_id, mes) %>%
  filter(row_number()==1)

test111$mes_any_primera_data <- test111$date

test111$preu_m2 <- test111$price/test111$surface

#test111 <- filter(test111, preu_m2 > 10 & preu_m2 < 10000)
#test111 <- filter(test111, surface > 19 & preu_m2 < 10000)

catalunya_noms <- data.frame(catalunya_noms$NOMMUNI, catalunya_noms$CODIMUNI)
names(catalunya_noms)[c(1,2)] <- c("NOMMUNI", "CODIMUNI")
test111 <- join(test111, catalunya_noms, by = "NOMMUNI")
foto_oferta_2019 <- test111
foto_oferta_2019$date_posting <- as.Date(foto_oferta_2019$date_posting)

count(test111, "mitjana_superficie_mes")
count(test111, "mitjana_preu_mes")


library(xlsx)
write_csv(test111, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/tractament_foto_oferta_2019.csv")

############################################################
############ Mitjanes Trimestrals
############################################################
mitjana_preu_trimestre <- test111 %>% aggregate(price~property_id+trimestre, mean, digits=2)
mitjana_preu_trimestre$property_id <- NULL
names(mitjana_preu_trimestre)[2] <- "mitjana_preu_trimestre"
mitjana_superficie_trimestre <- test111 %>% aggregate(surface~property_id+trimestre, mean)
mitjana_superficie_trimestre$property_id <- NULL
names(mitjana_superficie_trimestre)[2] <- "mitjana_superficie_trimestre"

############################################################
############################################################




