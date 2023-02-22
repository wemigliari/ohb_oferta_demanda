library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(arrow)
library(plyr)
library(dplyr)
library(purrr)
library(lubridate)

##########################################
####### Demanda Fotocasa Serie Historica
##########################################

#To disable the scientific notation in R, pass the following argument: 
options(scipen=999)
options(digits=2)

catalunya_noms <- read.xlsx("/Volumes/OHB/04.Laboratoris/LAB201703-Lloguer/04.Dades/Joffre_Wellington/Catalunya_noms_oficials.xlsx",
                            sheetName = "Sheet1")

#Fer la càrrega de les extraccions i així tenir foto_demanda_series_tract

foto_demanda_series_tract <- rbind(foto_demanda_2019, foto_demanda_2020, foto_demanda_2021, foto_demanda_2022)

#Remember to run and load each year table before running this code


foto_demanda_series_tract$transaction_type <- NULL
foto_demanda_series_tract$transaction_subtype <- NULL
foto_demanda_series_tract$property_type <- NULL
foto_demanda_series_tract$level1 <- NULL
foto_demanda_series_tract$level2 <- NULL
foto_demanda_series_tract$level3 <- NULL
foto_demanda_series_tract$level4 <- NULL
foto_demanda_series_tract$level6 <- NULL
foto_demanda_series_tract$level8 <- NULL

names(foto_demanda_series_tract)[5] <- "property_id"

names(foto_demanda_series_tract)[6] <- "municipality"
names(foto_demanda_series_tract)[7] <- "district"

#adding month
foto_demanda_series_tract$mes <- foto_demanda_series_tract$date
data.frame(foto_demanda_series_tract$mes <-month(ymd(foto_demanda_series_tract$mes)))
foto_demanda_series_tract$date <- as.Date(foto_demanda_series_tract$date)
foto_demanda_series_tract$any <- format(as.Date(foto_demanda_series_tract$date, format="%Y/%m/%d"),"%Y")
foto_demanda_series_tract <- data.table(foto_demanda_series_tract)
foto_demanda_series_tract <- foto_demanda_series_tract[order(foto_demanda_series_tract$property_id),]
foto_demanda_series_tract$property_id <- as.character(foto_demanda_series_tract$property_id)

names(foto_demanda_series_tract)[3] <- "price_d"
names(foto_demanda_series_tract)[4] <- "surface_d"

#names(foto_demanda_series_tract)[7] <- "date"


foto_demanda_series_tract <- filter(foto_demanda_series_tract, surface_d >= 10 & surface_d <= 10000)
foto_demanda_series_tract <- filter(foto_demanda_series_tract, price_d >= 10 & price_d <= 10000)

foto_demanda_series_tract$NOMMUNI <- foto_demanda_series_tract$municipality

foto_demanda_series_tract <- foto_demanda_series_tract %>%
  mutate_if(is.character, str_trim)

foto_demanda_series_tract$property_id <- as.character(foto_demanda_series_tract$property_id)

foto_demanda_series_tract$NOMMUNI <- foto_demanda_series_tract$municipality

##########################################
##### Recovering NOMMUNI, CODIMUNI & Cleaning
##########################################

foto_demanda_series_tract$NOMMUNI<-gsub(" Capital","",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub(" Girona","Girona",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub(" Lleida","Lleida",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$##########
NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI==" Capital")] <- ""
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI==" Barcelona")] <- "Barcelona"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI==" Girona")] <- "Girona"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI==" Lleida")] <- "Lleida"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI==" Tarragona")] <- "Tarragona"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Hospitalet de Llobregat (L´)")] <- "l'Hospitalet de Llobregat"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Ametlla de Mar (L´)")] <- "l'Ametlla de Mar"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Escala (L´)")] <- "l'Escala"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Ametlla del Vallès (L´)")] <- "l'Ametlla del Vallès"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Ampolla (L´)")] <- "l'Ampolla"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Arboç (L´)")] <- "l'Arboç"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Aldea (L´)")] <- "l'Aldea"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Espluga de Francolí (L´)")] <- "l'Espluga de Francolí"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Armentera (L´)")] <- "l'Armentera"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Espluga Calba (L´)")] <- "l'Espluga Calba"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Estany (L´)")] <- "l'Estany"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Albiol (L´)")] <- "l'Albiol"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Bruc (El)")] <- "el Bruc"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Brull (El)")] <- "el Brull"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Masnou (El)")] <- "el Masnou"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Morell (El)")] <- "el Morell"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Montmell (El)")] <- "el Montmell"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Papiol (El)")] <- "el Papiol"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Poal (El)")] <- "el Poal"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Catllar (El)")] <- "el Catllar"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Rourell (El)")] <- "el Rourell"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pla del Penedès (El)")] <- "el Pla del Penedès"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Far d´Empordà (El)")] <- "el Far d'Empordà"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Perelló (El)")] <- "el Perelló"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Alamús (Els)")] <- "els Alamús"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Riera de Gaià (La)")] <- "la Riera de Gaià"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Seu d´Urgell (La)")] <- "la Seu d'Urgell"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Roca del Vallès (La)")] <- "la Roca del Vallès"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Palma de Cervelló (La)")] <- "la Palma de Cervelló"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Garriga (La)")] <- "la Garriga"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Molina (La)")] <- "la Molina"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Bisbal d´Empordà (La)")] <- "la Bisbal d'Empordà"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Tallada d´Empordà (La)")] <- "la Tallada d'Empordà"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Vall d´en Bas (La)")] <- "la Vall d'en Bas"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pobla de Cérvoles (La)")] <- "la Pobla de Cérvoles"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Vall de Boí (La)")] <- "la Vall de Boí"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Granada (La)")] <- "la Granada"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Nou de Gaià (La)")] <- "la Nou de Gaià"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Riba (La)")] <- "la Riba"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Secuita (La)")] <- "la Secuita"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Portella (La)")] <- "la Portella"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pobla de Montornès (La)")] <- "la Pobla de Montornès"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Galera (La)")] <- "la Galera"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Guingueta d´Àneu (La)")] <- "la Guingueta d'Àneu"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Bisbal del Penedès (La)")] <- "la Bisbal del Penedès"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Torre de l´Espanyol (La)")] <- "la Torre de l'Espanyol"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Sénia (La)")] <- "la Sénia"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Vajol (La)")] <- "la Vajol"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Pera (La)")] <- "la Pera"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Sentiu de Sió (La)")] <- "la Sentiu"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Franqueses del Vallès (Les)")] <- "les Franqueses del Vallès"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Masies de Voltregà (Les)")] <- "les Masies de Voltregà"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Planes d´Hostoles (Les)")] <- "les Planes d'Hostoles"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Preses (Les)")] <- "les Preses"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Piles (Les)")] <- "les Piles"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"
foto_demanda_series_tract$NOMMUNI<-gsub(" d´"," d'",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub(" n´"," n'",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub(" l´"," l'",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Coma-ruga")] <- "el Vendrell"


foto_demanda_series_tract$NOMMUNI<-gsub(" Capital","",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub(" Girona","Girona",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub(" Lleida","Lleida",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub("L'Hospitalet de Llobregat","l'Hospitalet de Llobregat",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub("L'Ametlla de Mar","l'Ametlla de Mar",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub("L'Ametlla del Vallès","l'Ametlla del Vallès",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub("El Bruc","el Bruc",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub("El Brull","el Brull",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub("El Vendrell","el Vendrell",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub("L'","l'",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub("El","el",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI<-gsub("La","la",as.character(foto_demanda_series_tract$NOMMUNI))
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Torrent (Girona)")] <- "Torrent"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Alàs I Cerc")] <- "Alàs i Cerc"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Avellanes i Santa Linya")] <- "les Avellanes i Santa Linya"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Borges Blanques")] <- "les Borges Blanques"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Borges del Camp")] <- "les Borges del Camp"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Cabanyes")] <- "les Cabanyes"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Franqueses del Vallès")] <- "les Franqueses del Vallès"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Masies de Roda")] <- "les Masies de Roda"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Masies de Voltregà")] <- "les Masies de Voltregà"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Piles")] <- "les Piles"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Preses")] <- "les Preses"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Valls de Valira")] <- "les Valls de Valira"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="de Les")] <- "de les"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="l'Ametlla de Mar ")] <- "l'Ametlla de Mar"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"

foto_demanda_series_tract$district[which(foto_demanda_series_tract$district=="Sants - Montjuïc")] <- "Sants-Montjuïc"
foto_demanda_series_tract$district[which(foto_demanda_series_tract$district=="Sarrià - Sant Gervasi")] <- "Sarrià-Sant Gervasi"
foto_demanda_series_tract$district[which(foto_demanda_series_tract$district=="Horta - Guinardò")] <- "Horta-Guinardò"
foto_demanda_series_tract$district[which(foto_demanda_series_tract$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
foto_demanda_series_tract$district[which(foto_demanda_series_tract$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
foto_demanda_series_tract$district[which(foto_demanda_series_tract$district=="Horta Guinardó")] <- "Horta-Guinardò"


foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Bellaterra")] <- "Cerdanyola del Vallès"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Bigues i Riells")] <- "Bigues i Riells del Fai"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Calella de Palafrugell")] <- "Calella"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Camallera")] <- "Saus, Camallera i Llampaies"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Sant Antoni de Calonge")] <- "Calonge i Sant Antoni"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Calonge")] <- "Calonge de Segarra"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Canonja (la)")] <- "la Canonja"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Castell d´Aro")] <- "Castell-Platja d'Aro"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Empuriabrava")] <- "Castelló d'Empúries"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="el Vendrel")] <- "el Vendrell"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Estartit")] <- "Torroella de Montgrí"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="la Hostalets de Pierola")] <- "els Hostalets de Pierola"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="la Molina")] <- "Alp"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Llfranc")] <- "Palafrugell"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Miami Platja")] <- "Mont-roig del Camp"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Platja d'Aro")] <- "Castell-Platja d'Aro"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Castell d'Aro")] <- "Castell-Platja d'Aro"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Roda de Barà")] <- "Roda de Berà"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="S´Agaró")] <- "Castell-Platja d'Aro"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Segur de Calafell")] <- "Calafell"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Tamariu")] <- "Palafrugell"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Santa Margarida I Els Monjos")] <- "Santa Margarida i els Monjos"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Santa Margarida I els Monjos")] <- "Santa Margarida i els Monjos"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Sant Vicenç Dels Horts")] <- "Sant Vicenç dels Horts"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Sant Joan Les Fonts")] <- "Sant Joan les Fonts"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Cruïlles, Monells I Sant Sadurní de L'Heura")] <- "Cruïlles, Monells i Sant Sadurní de l'Heura"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Cruïlles, Monells I Sant Sadurní de l'Heura")] <- "Cruïlles, Monells i Sant Sadurní de l'Heura"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Montoliu deLleida")] <- "Montoliu de Lleida"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Cànoves I Samalús")] <- "Cànoves i Samalús"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Llosses")] <- "les Llosses"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Cervià de Les Garrigues")] <- "Cervià de les Garrigues"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Brunyola")] <- "Brunyola i Sant Martí Sapresa"

foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Sarroca deLleida")] <- "Sarroca de Lleida"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Mieres (Girona)")] <- "Mieres"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Mieres (Girona)")] <- "Mieres"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Planes d'Hostoles")] <- "les Planes d'Hostoles"

foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Artesa deLleida")] <- "Artesa de Lleida"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Artesa deLleida")] <- "Artesa de Lleida"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Puigverd deLleida")] <- "Puigverd de Lleida"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Cabanes (Girona)")] <- "Cabanes"
foto_demanda_series_tract$NOMMUNI[which(foto_demanda_series_tract$NOMMUNI=="Les Oluges")] <- "les Oluges"



foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Segre - Ebre - Ter", NOMMUNI := "Castelló d'Empúries"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "L'Estartit Poble", NOMMUNI := "Torroella de Montgrí"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Muga - Gran Reserva - Badia", NOMMUNI := "Sant Llorenç de la Muga"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Salins - Cavall de Mar", NOMMUNI := "la Jonquera"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Puigmal - Mas Nou", NOMMUNI := "el Masnou"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Carlit - Montseny", NOMMUNI := "Montseny"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Port Grec - Port Moxó", NOMMUNI := "la Jonquera"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Requesens", NOMMUNI := "la Jonquera"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Torre Vella - Torre Gran - Les Dunes", NOMMUNI := "Torroella de Montgrí"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Tordera - Fluvià - Llobregat", NOMMUNI := "Tordera"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Moxó - Sant Mori", NOMMUNI := "Sant Mori"]
foto_demanda_series_tract <- setDT(foto_demanda_series_tract)[municipality == "Undefined" & district == "Alberes", NOMMUNI := "Castelló d'Empúries"]


##################################################################

catalunya_noms <- data.frame(catalunya_noms$NOMMUNI, catalunya_noms$CODIMUNI)
names(catalunya_noms)[c(1,2)] <- c("NOMMUNI", "CODIMUNI")

foto_demanda_series_tract <- join(foto_demanda_series_tract, catalunya_noms, by = "NOMMUNI")

foto_demanda_series_tract <- foto_demanda_series_tract[!is.na(foto_demanda_series_tract$NOMMUNI),]

na_foto_demanda_muni <- as.data.frame(is.na(foto_demanda_series_tract$CODIMUNI))
na_foto_demanda_muni <- cbind(na_foto_demanda_muni, foto_demanda_series_tract$NOMMUNI, foto_demanda_series_tract$municipality)
na_foto_demanda_muni <- na_foto_demanda_muni%>%group_by(`is.na(foto_demanda_series_tract$CODIMUNI)`,
                                                        `foto_demanda_series_tract$NOMMUNI`)%>%
                                                        filter(row_number()==1)


##################################################################
############## Leads mensuals
##################################################################
#demanda_dia seleccionar la ultima fila amb els totals de leads mensuals
leads_mensuals <- foto_demanda_series_tract %>%                            
  group_by(property_id, NOMMUNI, mes, any) %>%
  dplyr::mutate(leads_mensuals = cumsum(num_leads))

leads_mensuals <- leads_mensuals[,c(5,11,9,10,13)]

leads_mensuals <- arrange(leads_mensuals, leads_mensuals)
2000222200003804476
2000001100000514119

leads_mensuals_1 <- leads_mensuals

leads_mensuals_1 <- leads_mensuals_1%>%
  group_by(property_id, NOMMUNI, any, mes)%>%
  filter(row_number()==n())

foto_demanda_series_tract <- merge(x=foto_demanda_series_tract, y=leads_mensuals_1, 
                             by.x = c("property_id","NOMMUNI", "mes", "any"),
                             by.y = c("property_id","NOMMUNI", "mes", "any"))

##### Aggregating the data per month 
foto_demanda_series_tract <- foto_demanda_series_tract%>%
  group_by(property_id, NOMMUNI, mes, any)%>%
  filter(row_number()==1)

write.csv(foto_demanda_series_tract, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/tractament_data_series_demanda_fotocasa.csv")
