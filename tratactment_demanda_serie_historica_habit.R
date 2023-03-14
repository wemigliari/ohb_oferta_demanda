library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(arrow)
library(plyr)
library(dplyr)
library(purrr)
library(lubridate)
library(data.table)

##########################################
####### Demanda Habitaclia Serie Historica
##########################################

#To disable the scientific notation in R, pass the following argument: 
options(scipen=999)
options(digits=2)

catalunya_noms <- read.xlsx("/Volumes/OHB/04.Laboratoris/LAB201703-Lloguer/04.Dades/Joffre_Wellington/Catalunya_noms_oficials.xlsx",
                            sheetName = "Sheet1")

#Fer la càrrega de les extraccions i així tenir habit_demanda_series_tract

habit_demanda_series_tract <- rbind(habit_demanda_2019, habit_demanda_2020, habit_demanda_2021, habit_demanda_2022)

#Remember to run and load each year table before running this code

habit_demanda_series_tract$province <- NULL
habit_demanda_series_tract$region <- NULL
habit_demanda_series_tract$area <- NULL
habit_demanda_series_tract$zone <- NULL

#adding month
habit_demanda_series_tract$mes <- habit_demanda_series_tract$date
data.frame(habit_demanda_series_tract$mes <-month(ymd(habit_demanda_series_tract$mes)))
habit_demanda_series_tract$date <- as.Date(habit_demanda_series_tract$date)
habit_demanda_series_tract$any <- format(as.Date(habit_demanda_series_tract$date, format="%Y/%m/%d"),"%Y")
habit_demanda_series_tract <- data.table(habit_demanda_series_tract)
habit_demanda_series_tract <- habit_demanda_series_tract[order(habit_demanda_series_tract$property_id),]
habit_demanda_series_tract$property_id <- as.character(habit_demanda_series_tract$property_id)

names(habit_demanda_series_tract)[5] <- "surface_d"
names(habit_demanda_series_tract)[6] <- "price_d"
#names(habit_demanda_series_tract)[7] <- "date"


habit_demanda_series_tract <- filter(habit_demanda_series_tract, surface_d >= 10 & surface_d <= 10000)
habit_demanda_series_tract <- filter(habit_demanda_series_tract, price_d >= 10 & price_d <= 10000)


habit_demanda_series_tract$NOMMUNI <- habit_demanda_series_tract$municipality

habit_demanda_series_tract <- habit_demanda_series_tract %>%
  mutate_if(is.character, str_trim)

habit_demanda_series_tract$property_id <- as.character(habit_demanda_series_tract$property_id)

habit_demanda_series_tract$NOMMUNI <- habit_demanda_series_tract$municipality

##########################################
##### Recovering NOMMUNI, CODIMUNI & Cleaning
##########################################

habit_demanda_series_tract$NOMMUNI<-gsub(" Capital","",as.character(habit_demanda_series_tract$NOMMUNI))
habit_demanda_series_tract$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(habit_demanda_series_tract$NOMMUNI))
habit_demanda_series_tract$NOMMUNI<-gsub(" Girona","Girona",as.character(habit_demanda_series_tract$NOMMUNI))
habit_demanda_series_tract$NOMMUNI<-gsub(" Lleida","Lleida",as.character(habit_demanda_series_tract$NOMMUNI))
habit_demanda_series_tract$##########
NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(habit_demanda_series_tract$NOMMUNI))
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI==" Capital")] <- ""
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI==" Barcelona")] <- "Barcelona"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI==" Girona")] <- "Girona"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI==" Lleida")] <- "Lleida"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI==" Tarragona")] <- "Tarragona"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Hospitalet de Llobregat (L´)")] <- "l'Hospitalet de Llobregat"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Ametlla de Mar (L´)")] <- "l'Ametlla de Mar"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Escala (L´)")] <- "l'Escala"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Ametlla del Vallès (L´)")] <- "l'Ametlla del Vallès"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Ampolla (L´)")] <- "l'Ampolla"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Arboç (L´)")] <- "l'Arboç"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Aldea (L´)")] <- "l'Aldea"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Espluga de Francolí (L´)")] <- "l'Espluga de Francolí"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Armentera (L´)")] <- "l'Armentera"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Espluga Calba (L´)")] <- "l'Espluga Calba"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Estany (L´)")] <- "l'Estany"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Albiol (L´)")] <- "l'Albiol"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Bruc (El)")] <- "el Bruc"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Brull (El)")] <- "el Brull"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Masnou (El)")] <- "el Masnou"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Morell (El)")] <- "el Morell"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Montmell (El)")] <- "el Montmell"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Papiol (El)")] <- "el Papiol"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Poal (El)")] <- "el Poal"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Catllar (El)")] <- "el Catllar"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Rourell (El)")] <- "el Rourell"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pla del Penedès (El)")] <- "el Pla del Penedès"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Far d´Empordà (El)")] <- "el Far d'Empordà"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Perelló (El)")] <- "el Perelló"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Alamús (Els)")] <- "els Alamús"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Riera de Gaià (La)")] <- "la Riera de Gaià"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Seu d´Urgell (La)")] <- "la Seu d'Urgell"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Roca del Vallès (La)")] <- "la Roca del Vallès"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Palma de Cervelló (La)")] <- "la Palma de Cervelló"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Garriga (La)")] <- "la Garriga"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Molina (La)")] <- "la Molina"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Bisbal d´Empordà (La)")] <- "la Bisbal d'Empordà"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Tallada d´Empordà (La)")] <- "la Tallada d'Empordà"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Vall d´en Bas (La)")] <- "la Vall d'en Bas"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pobla de Cérvoles (La)")] <- "la Pobla de Cérvoles"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Vall de Boí (La)")] <- "la Vall de Boí"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Granada (La)")] <- "la Granada"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Nou de Gaià (La)")] <- "la Nou de Gaià"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Riba (La)")] <- "la Riba"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Secuita (La)")] <- "la Secuita"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Portella (La)")] <- "la Portella"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pobla de Montornès (La)")] <- "la Pobla de Montornès"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Galera (La)")] <- "la Galera"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Guingueta d´Àneu (La)")] <- "la Guingueta d'Àneu"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Bisbal del Penedès (La)")] <- "la Bisbal del Penedès"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Torre de l´Espanyol (La)")] <- "la Torre de l'Espanyol"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Sénia (La)")] <- "la Sénia"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Vajol (La)")] <- "la Vajol"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Pera (La)")] <- "la Pera"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Sentiu de Sió (La)")] <- "la Sentiu"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Franqueses del Vallès (Les)")] <- "les Franqueses del Vallès"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Masies de Voltregà (Les)")] <- "les Masies de Voltregà"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Planes d´Hostoles (Les)")] <- "les Planes d'Hostoles"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Preses (Les)")] <- "les Preses"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Piles (Les)")] <- "les Piles"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"
habit_demanda_series_tract$NOMMUNI<-gsub(" d´"," d'",as.character(habit_demanda_series_tract$NOMMUNI))
habit_demanda_series_tract$NOMMUNI<-gsub(" n´"," n'",as.character(habit_demanda_series_tract$NOMMUNI))
habit_demanda_series_tract$NOMMUNI<-gsub(" l´"," l'",as.character(habit_demanda_series_tract$NOMMUNI))
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Coma-ruga")] <- "el Vendrell"

habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Bellaterra")] <- "Cerdanyola del Vallès"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Bigues i Riells")] <- "Bigues i Riells del Fai"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Calella de Palafrugell")] <- "Calella"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Camallera")] <- "Saus, Camallera i Llampaies"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Sant Antoni de Calonge")] <- "Calonge i Sant Antoni"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Calonge")] <- "Calonge de Segarra"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Canonja (la)")] <- "la Canonja"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Castell d´Aro")] <- "Castell-Platja d'Aro"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Empuriabrava")] <- "Castelló d'Empúries"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="el Vendrel")] <- "el Vendrell"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Estartit")] <- "Torroella de Montgrí"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="la Hostalets de Pierola")] <- "els Hostalets de Pierola"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="la Molina")] <- "Alp"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Llfranc")] <- "Palafrugell"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Miami Platja")] <- "Mont-roig del Camp"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Platja d'Aro")] <- "Castell-Platja d'Aro"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Castell d'Aro")] <- "Castell-Platja d'Aro"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Roda de Barà")] <- "Roda de Berà"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="S´Agaró")] <- "Castell-Platja d'Aro"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Segur de Calafell")] <- "Calafell"
habit_demanda_series_tract$NOMMUNI[which(habit_demanda_series_tract$NOMMUNI=="Tamariu")] <- "Palafrugell"

habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Sants - Montjuïc")] <- "Sants-Montjuïc"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Sarrià - Sant Gervasi")] <- "Sarrià-Sant Gervasi"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Horta - Guinardò")] <- "Horta-Guinardó"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Horta Guinardó")] <- "Horta-Guinardó"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Horta - Guinardó")] <- "Horta-Guinardó"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Horta Guinardò")] <- "Horta-Guinardó"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
habit_demanda_series_tract$district[which(habit_demanda_series_tract$district=="Horta-Guinardò")] <- "Horta-Guinardó"

##################################################################

catalunya_noms <- data.frame(catalunya_noms$NOMMUNI, catalunya_noms$CODIMUNI)
names(catalunya_noms)[c(1,2)] <- c("NOMMUNI", "CODIMUNI")

habit_demanda_series_tract <- left_join(habit_demanda_series_tract, catalunya_noms, by = "NOMMUNI")
count(habit_demanda_series_tract, "NOMMUNI")

habit_demanda_series_tract <- habit_demanda_series_tract[!is.na(habit_demanda_series_tract$municipality),]


##################################################################
############## Leads mensuals
##################################################################
#demanda_dia seleccionar la ultima fila amb els totals de leads mensuals
leads_mensuals <- habit_demanda_series_tract %>%                            
  group_by(property_id, NOMMUNI, mes, any) %>%
  dplyr::mutate(leads_mensuals = cumsum(num_leads))

leads_mensuals <- leads_mensuals[,c(3,11,9,10,13)]

leads_mensuals <- arrange(leads_mensuals, leads_mensuals)
2000222200003804476
2000001100000514119

leads_mensuals_1 <- leads_mensuals

leads_mensuals_1 <- leads_mensuals_1%>%
  group_by(property_id, NOMMUNI, mes, any)%>%
  filter(row_number()==n())

leads_mensuals_1 <- leads_mensuals_1[order(leads_mensuals_1$leads_mensuals),]


habit_demanda_series_tract <- merge(x=habit_demanda_series_tract, y=leads_mensuals_1, 
                             by.x = c("property_id","NOMMUNI", "mes", "any"),
                             by.y = c("property_id","NOMMUNI", "mes", "any"))

##################################################################


##### Aggregating the data per month 
habit_demanda_series_tract <- habit_demanda_series_tract%>%
  group_by(property_id, NOMMUNI, mes, any)%>%
  filter(row_number()==1)

write.csv(habit_demanda_series_tract, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/tractament_data_series_demanda_habitaclia.csv")

