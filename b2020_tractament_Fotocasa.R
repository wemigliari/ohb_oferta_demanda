library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(arrow)
library(plyr)
library(dplyr)
library(purrr)
library(lubridate)

###################################
####### Oferta Fotocasa 2020
###################################
catalunya_noms <- read.xlsx("/Volumes/OHB/04.Laboratoris/LAB201703-Lloguer/04.Dades/Joffre_Wellington/Catalunya_noms_oficials.xlsx",
                            sheetName = "Sheet1")
#To disable the scientific notation in R, pass the following argument: 

options(scipen=999)
options(digits=2)

foto_ofer_b2020 <- f_20

foto_ofer_b2020$communitycosts_included <- NULL
foto_ofer_b2020$deposit_required <- NULL
foto_ofer_b2020$street <- NULL
foto_ofer_b2020$number <- NULL
foto_ofer_b2020$hide_address <- NULL
foto_ofer_b2020$rooms <- NULL
foto_ofer_b2020$baths <- NULL
foto_ofer_b2020$level1 <- NULL
foto_ofer_b2020$level2 <- NULL
foto_ofer_b2020$level3 <- NULL
foto_ofer_b2020$level4 <- NULL
foto_ofer_b2020$level6 <- NULL
foto_ofer_b2020$level8 <- NULL

names(foto_ofer_b2020)[12] <- "municipality"
names(foto_ofer_b2020)[13] <- "district"
foto_ofer_b2020$date_posting <- as.Date(as.character(foto_ofer_b2020$date_posting), format = "%Y%m%d")

foto_ofer_b2020$NOMMUNI <- foto_ofer_b2020$municipality


##########################################
##### Recovering NOMMUNI, CODIMUNI & Cleaning
##########################################

foto_ofer_b2020$NOMMUNI<-gsub(" Capital","",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" Girona","Girona",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" Lleida","Lleida",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(foto_ofer_b2020$NOMMUNI))


foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI==" Capital")] <- ""
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI==" Barcelona")] <- "Barcelona"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI==" Girona")] <- "Girona"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI==" Lleida")] <- "Lleida"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI==" Tarragona")] <- "Tarragona"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Hospitalet de Llobregat (L??)")] <- "l'Hospitalet de Llobregat"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Ametlla de Mar (L??)")] <- "l'Ametlla de Mar"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Escala (L??)")] <- "l'Escala"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Ametlla del Vall??s (L??)")] <- "l'Ametlla del Vall??s"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Ampolla (L??)")] <- "l'Ampolla"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Arbo?? (L??)")] <- "l'Arbo??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Aldea (L??)")] <- "l'Aldea"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Espluga de Francol?? (L??)")] <- "l'Espluga de Francol??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Armentera (L??)")] <- "l'Armentera"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Espluga Calba (L??)")] <- "l'Espluga Calba"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Estany (L??)")] <- "l'Estany"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Albiol (L??)")] <- "l'Albiol"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Bruc (El)")] <- "el Bruc"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Brull (El)")] <- "el Brull"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Masnou (El)")] <- "el Masnou"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Morell (El)")] <- "el Morell"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Montmell (El)")] <- "el Montmell"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Papiol (El)")] <- "el Papiol"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Poal (El)")] <- "el Poal"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Catllar (El)")] <- "el Catllar"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Rourell (El)")] <- "el Rourell"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pla del Pened??s (El)")] <- "el Pla del Pened??s"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Far d??Empord?? (El)")] <- "el Far d'Empord??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Perell?? (El)")] <- "el Perell??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Alam??s (Els)")] <- "els Alam??s"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Riera de Gai?? (La)")] <- "la Riera de Gai??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Seu d??Urgell (La)")] <- "la Seu d'Urgell"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Roca del Vall??s (La)")] <- "la Roca del Vall??s"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Palma de Cervell?? (La)")] <- "la Palma de Cervell??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Garriga (La)")] <- "la Garriga"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Molina (La)")] <- "la Molina"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Bisbal d??Empord?? (La)")] <- "la Bisbal d'Empord??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Tallada d??Empord?? (La)")] <- "la Tallada d'Empord??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Vall d??en Bas (La)")] <- "la Vall d'en Bas"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pobla de C??rvoles (La)")] <- "la Pobla de C??rvoles"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Vall de Bo?? (La)")] <- "la Vall de Bo??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Granada (La)")] <- "la Granada"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Nou de Gai?? (La)")] <- "la Nou de Gai??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Riba (La)")] <- "la Riba"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Secuita (La)")] <- "la Secuita"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Portella (La)")] <- "la Portella"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pobla de Montorn??s (La)")] <- "la Pobla de Montorn??s"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Galera (La)")] <- "la Galera"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Guingueta d????neu (La)")] <- "la Guingueta d'??neu"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Bisbal del Pened??s (La)")] <- "la Bisbal del Pened??s"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Torre de l??Espanyol (La)")] <- "la Torre de l'Espanyol"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="S??nia (La)")] <- "la S??nia"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Vajol (La)")] <- "la Vajol"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Pera (La)")] <- "la Pera"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Sentiu de Si?? (La)")] <- "la Sentiu"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Franqueses del Vall??s (Les)")] <- "les Franqueses del Vall??s"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Masies de Voltreg?? (Les)")] <- "les Masies de Voltreg??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Planes d??Hostoles (Les)")] <- "les Planes d'Hostoles"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Preses (Les)")] <- "les Preses"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Piles (Les)")] <- "les Piles"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Sant Carles de la R??pita")] <- "la R??pita"
foto_ofer_b2020$NOMMUNI<-gsub(" d??"," d'",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" n??"," n'",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" l??"," l'",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Coma-ruga")] <- "el Vendrell"


foto_ofer_b2020$NOMMUNI<-gsub(" Capital","",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" Girona","Girona",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" Lleida","Lleida",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub("L'Hospitalet de Llobregat","l'Hospitalet de Llobregat",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub("L'Ametlla de Mar","l'Ametlla de Mar",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub("L'Ametlla del Vall??s","l'Ametlla del Vall??s",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub("El Bruc","el Bruc",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub("El Brull","el Brull",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub("El Vendrell","el Vendrell",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub("L'","l'",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub("El","el",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI<-gsub("La","la",as.character(foto_ofer_b2020$NOMMUNI))
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Torrent (Girona)")] <- "Torrent"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Al??s I Cerc")] <- "Al??s i Cerc"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Avellanes i Santa Linya")] <- "les Avellanes i Santa Linya"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Borges Blanques")] <- "les Borges Blanques"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Borges del Camp")] <- "les Borges del Camp"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Cabanyes")] <- "les Cabanyes"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Franqueses del Vall??s")] <- "les Franqueses del Vall??s"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Masies de Roda")] <- "les Masies de Roda"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Masies de Voltreg??")] <- "les Masies de Voltreg??"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Piles")] <- "les Piles"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Preses")] <- "les Preses"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Les Valls de Valira")] <- "les Valls de Valira"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="de Les")] <- "de les"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="l'Ametlla de Mar ")] <- "l'Ametlla de Mar"
foto_ofer_b2020$NOMMUNI[which(foto_ofer_b2020$NOMMUNI=="Sant Carles de la R??pita")] <- "la R??pita"

foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Sants - Montju??c")] <- "Sants-Montju??c"
foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Sants Montju??c")] <- "Sants-Montju??c"
foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Sarri?? - Sant Gervasi")] <- "Sarri??-Sant Gervasi"
foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Sarri?? Sant Gervasi")] <- "Sarri??-Sant Gervasi"
foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Horta - Guinard??")] <- "Horta-Guinard??"
foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Horta Guinard??")] <- "Horta-Guinard??"
foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Horta - Guinard??")] <- "Horta-Guinard??"
foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Horta Guinard??")] <- "Horta-Guinard??"
foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Sants Montju??c")] <- "Sants-Montju??c"
foto_ofer_b2020$district[which(foto_ofer_b2020$district=="Sarri?? Sant Gervasi")] <- "Sarri??-Sant Gervasi"

########## Filtering Barcelona ########################
foto_ofer_b2020 <- foto_ofer_b2020 %>% filter(NOMMUNI=="Barcelona")
#####################################################

foto_ofer_b2020 <- foto_ofer_b2020 %>%
  mutate_if(is.character, str_trim)

foto_ofer_b2020$property_id <- as.character(foto_ofer_b2020$property_id)

############################################################
############ Adding month, date and trimester
############################################################

foto_ofer_b2020$mes <- foto_ofer_b2020$date
data.frame(foto_ofer_b2020$mes <-month(ymd(foto_ofer_b2020$mes)))
foto_ofer_b2020$date <- as.Date(foto_ofer_b2020$date)
foto_ofer_b2020$any <- format(as.Date(foto_ofer_b2020$date, format="%Y/%m/%d"),"%Y")
library(data.table)
foto_ofer_b2020 <- data.table(foto_ofer_b2020)
foto_ofer_b2020 <- foto_ofer_b2020%>%
  arrange(property_id, date, date_posting)

###########################################################
######## Mitjanes de superficie
###########################################################
foto_ofer_b2020 <- foto_ofer_b2020[foto_ofer_b2020$surface != -1, ] 

mitjana_surface_o_dist <- aggregate(x = foto_ofer_b2020$surface,    
                                    by = list(foto_ofer_b2020$property_id, 
                                              foto_ofer_b2020$district, 
                                              foto_ofer_b2020$mes),             
                                    FUN = mean,round(mean(foto_ofer_b2020$surface), digits=2))                            

names(mitjana_surface_o_dist)[1:4] <- c("property_id", "district", "mes", "mitjana_superf_o_mes")

test111_20 <- merge(x=foto_ofer_b2020, y=mitjana_surface_o_dist, 
                    by.x=c("property_id","district", "mes"), 
                    by.y=c("property_id","district", "mes"))

##################################################################
############## Mitjanes de preu
##################################################################

mitjana_price_o_dist <- aggregate(x = foto_ofer_b2020$price,    
                                  by = list(foto_ofer_b2020$property_id, 
                                            foto_ofer_b2020$district, 
                                            foto_ofer_b2020$mes),             
                                  FUN = mean, round(mean(foto_ofer_b2020$price), digits=2))                           

names(mitjana_price_o_dist)[1:4] <- c("property_id", "district", "mes", "mitjana_price_o_dist_mes")


test111_20 <- merge(x=test111_20, y=mitjana_price_o_dist, 
                    by.x = c("property_id","district", "mes"),
                    by.y = c("property_id","district", "mes"))


##################################################################
############## Preu M2
##################################################################

test111_20$preu_m2_mes <- test111_20$mitjana_price_o_dist_mes/test111_20$mitjana_superf_o_mes

test111_20 <- filter(test111_20, price >= 10 & price <= 10000)
test111_20 <- filter(test111_20, surface >= 10 & surface <= 10000)


##################################################
###### Primera data, ultima data and date posting
##################################################
##################################################
###### Spare this subsection for the next code
##################################################

#2000002500003786677
#2000050000003768984

#foto_ofer_b2020 <- cbind(foto_ofer_b2020, missing_date_posting2$date_posting2)
#names(foto_ofer_b2020)[19] <-"dia_public_nas"

#Adding first date column
test11a_20_fd <- test111_20[order(foto_ofer_b2020$date),]
test11a_20_fd <- test111_20[,c(1:4)]

test11a_20_fd <- test11a_20_fd %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = date)
test11a_20_fd <- test11a_20_fd[,c(1,2,3,5)]

#Adding last date column
test11b_20_ld <- test111_20[order(foto_ofer_b2020$date),]
test11b_20_ld <- test111_20[,c(1:4)]

test11b_20_ld <- test11b_20_ld %>%
  group_by(property_id, district) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = date)
test11b_20_ld <- test11b_20_ld[,c(1,2,3,5)]

test111_20_pm_ud <- merge(x=test11a_20_fd, y=test11b_20_ld, by.x=c("property_id","district"), 
                          by.y=c("property_id","district"))


#Adding date posting
test11b_20_dp <- test111_20[order(foto_ofer_b2020$date_posting),]
test11b_20_dp <- test111_20[,c(1:3, 12)]

test11b_20_dp <- test11b_20_dp %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
test11b_20_dp <- test11b_20_dp[,c(1,2,3,5)]

test111_20_pm_ud_dp <- merge(x=test111_20_pm_ud, y=test11b_20_dp, by.x=c("property_id","district"), 
                             by.y=c("property_id","district"))

test111_20_pm_ud_dp <- data.frame(test111_20_pm_ud_dp$property_id, 
                                  test111_20_pm_ud_dp$district, 
                                  test111_20_pm_ud_dp$primera_data,
                                  test111_20_pm_ud_dp$ultima_data,
                                  test111_20_pm_ud_dp$date_posting_calcul)

names(test111_20_pm_ud_dp)[1:5] <- c("property_id", "district", "primera_data", "ultlima_data", "date_posting_calcul")


test111_20_pm_ud_dp <- test111_20_pm_ud_dp[order(test111_20_pm_ud_dp$primera_data),]

test111_20 <- merge(x=test111_20, y=test111_20_pm_ud_dp, by.x=c("property_id","district"), 
                    by.y=c("property_id","district"))


##################################################
##################### Adding trimestre
##################################################

test111_20 <- test111_20 %>% mutate(trimestre =
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
############ Tipologias Plurifamiliar & Unifamiliar
############################################################

test111_20$tipologia <- test111_20$property_subtype

test111_20$tipologia[which(test111_20$tipologia=="Apartamento")] <- "Plurifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="??tico")] <- "Plurifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Casa")] <- "Unifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Casa adosada")] <- "Unifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Casa pareada")] <- "Unifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Unifamiliar pareada")] <- "Unifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Chalet")] <- "Unifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="D??plex")] <- "Plurifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Estudio")] <- "Plurifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Loft")] <- "Plurifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Mas??a")] <- "Unifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Piso")] <- "Plurifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Planta baja")] <- "Plurifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Torre")] <- "Plurifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Tr??plex")] <- "Plurifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Casa-Chalet")] <- "Unifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Finca r??stica")] <- "Unifamiliar"
test111_20$tipologia[which(test111_20$tipologia=="Duplex")] <- "Plurifamiliar"

count(test111_20, "tipologia")

##################################################################

catalunya_noms <- data.frame(catalunya_noms$NOMMUNI, catalunya_noms$CODIMUNI)
names(catalunya_noms)[c(1,2)] <- c("NOMMUNI", "CODIMUNI")
test111_20 <- join(test111_20, catalunya_noms, by = "NOMMUNI")

##################################################################
############## Quantiles 2021
##################################################################

test_b2020_f <- test111_20
counting_dist_2020 <- test_b2020_f %>% 
  count(c("district", "mes"))
names(counting_dist_2020)[3] <- "freq_dist_mes"

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_mitjana_mes_2020 <- test111_20 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2_mes), funs(!!!p_funs))

names(preu_mitjana_mes_2020)[c(4:6)] <- c("q1_dist", "q2_dist", "q3_dist")

test_b2020_f <- merge(test_b2020_f, preu_mitjana_mes_2020,
                      by.x = c("district", "any", "mes"),
                      by.y = c("district", "any", "mes"), .keep_all=TRUE)

test_b2020_f <- merge(test_b2020_f, counting_dist_2020,
                      by.x = c("district","mes"),
                      by.y = c("district","mes"), .keep_all = TRUE)

####################

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

test_b2020_f$data1 <- monthStart(test_b2020_f$date)
test_b2020_f$data_final <- (test_b2020_f$data1 %m+% months(1))

which(test_b2020_f$date_posting > test_b2020_f$primera_data)
which(is.na(test_b2020_f$date_posting))

test_b2020_f <- test_b2020_f%>%
  group_by(property_id, district)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

which(test_b2020_f$date_posting_calcul > test_b2020_f$primera_data)
which(is.na(test_b2020_f$date_posting_calcul))
149540709

write_csv(test_b2020_f, "/Users/wemigliari/Documents/Po??s-Doutorado & Doutorado/Po??s-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/tractament_foto_oferta_b2020.csv")
