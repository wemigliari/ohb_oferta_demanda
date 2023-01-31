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

#To disable the scientific notation in R, pass the following argument: 
options(scipen=999)
options(digits=2)

catalunya_noms <- read.xlsx("/Volumes/OHB/04.Laboratoris/LAB201703-Lloguer/04.Dades/Joffre_Wellington/Catalunya_noms_oficials.xlsx",
                            sheetName = "Sheet1")

#habit_oferta_2020 <- read.csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_oferta/extraccions_habit_oferta_2020.csv")
h_20 <- habit_oferta_2020 
habit_oferta_2020$NOMMUNI <- habit_oferta_2020$municipality
habit_oferta_2020 <- habit_oferta_2020[order(as.Date(habit_oferta_2020$date)),]

#habit_oferta_2020 <- habit_oferta_2020[,c(2:19)]

habit_oferta_2020 <- habit_oferta_2020 %>%
  mutate_if(is.character, str_trim)

habit_oferta_2020$property_id <- as.character(habit_oferta_2020$property_id)

##########################################
##### Adding NOMMUNI, CODIMUNI & Cleaning
################################
habit_oferta_2020$NOMMUNI<-gsub(" Capital","",as.character(habit_oferta_2020$NOMMUNI))
habit_oferta_2020$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(habit_oferta_2020$NOMMUNI))
habit_oferta_2020$NOMMUNI<-gsub(" Girona","Girona",as.character(habit_oferta_2020$NOMMUNI))
habit_oferta_2020$NOMMUNI<-gsub(" Lleida","Lleida",as.character(habit_oferta_2020$NOMMUNI))
habit_oferta_2020$##########
NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(habit_oferta_2020$NOMMUNI))
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI==" Capital")] <- ""
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI==" Barcelona")] <- "Barcelona"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI==" Girona")] <- "Girona"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI==" Lleida")] <- "Lleida"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI==" Tarragona")] <- "Tarragona"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Hospitalet de Llobregat (L´)")] <- "l'Hospitalet de Llobregat"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Ametlla de Mar (L´)")] <- "l'Ametlla de Mar"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Escala (L´)")] <- "l'Escala"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Ametlla del Vallès (L´)")] <- "l'Ametlla del Vallès"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Ampolla (L´)")] <- "l'Ampolla"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Arboç (L´)")] <- "l'Arboç"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Aldea (L´)")] <- "l'Aldea"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Espluga de Francolí (L´)")] <- "l'Espluga de Francolí"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Armentera (L´)")] <- "l'Armentera"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Espluga Calba (L´)")] <- "l'Espluga Calba"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Estany (L´)")] <- "l'Estany"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Albiol (L´)")] <- "l'Albiol"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Bruc (El)")] <- "el Bruc"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Brull (El)")] <- "el Brull"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Masnou (El)")] <- "el Masnou"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Morell (El)")] <- "el Morell"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Montmell (El)")] <- "el Montmell"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Papiol (El)")] <- "el Papiol"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Poal (El)")] <- "el Poal"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Catllar (El)")] <- "el Catllar"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Rourell (El)")] <- "el Rourell"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pla del Penedès (El)")] <- "el Pla del Penedès"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Far d´Empordà (El)")] <- "el Far d'Empordà"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Perelló (El)")] <- "el Perelló"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Alamús (Els)")] <- "els Alamús"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Riera de Gaià (La)")] <- "la Riera de Gaià"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Seu d´Urgell (La)")] <- "la Seu d'Urgell"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Roca del Vallès (La)")] <- "la Roca del Vallès"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Palma de Cervelló (La)")] <- "la Palma de Cervelló"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Garriga (La)")] <- "la Garriga"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Molina (La)")] <- "la Molina"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Bisbal d´Empordà (La)")] <- "la Bisbal d'Empordà"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Tallada d´Empordà (La)")] <- "la Tallada d'Empordà"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Vall d´en Bas (La)")] <- "la Vall d'en Bas"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pobla de Cérvoles (La)")] <- "la Pobla de Cérvoles"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Vall de Boí (La)")] <- "la Vall de Boí"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Granada (La)")] <- "la Granada"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Nou de Gaià (La)")] <- "la Nou de Gaià"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Riba (La)")] <- "la Riba"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Secuita (La)")] <- "la Secuita"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Portella (La)")] <- "la Portella"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pobla de Montornès (La)")] <- "la Pobla de Montornès"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Galera (La)")] <- "la Galera"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Guingueta d´Àneu (La)")] <- "la Guingueta d'Àneu"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Bisbal del Penedès (La)")] <- "la Bisbal del Penedès"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Torre de l´Espanyol (La)")] <- "la Torre de l'Espanyol"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Sénia (La)")] <- "la Sénia"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Vajol (La)")] <- "la Vajol"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Pera (La)")] <- "la Pera"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Sentiu de Sió (La)")] <- "la Sentiu"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Franqueses del Vallès (Les)")] <- "les Franqueses del Vallès"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Masies de Voltregà (Les)")] <- "les Masies de Voltregà"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Planes d´Hostoles (Les)")] <- "les Planes d'Hostoles"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Preses (Les)")] <- "les Preses"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Piles (Les)")] <- "les Piles"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"
habit_oferta_2020$NOMMUNI<-gsub(" d´"," d'",as.character(habit_oferta_2020$NOMMUNI))
habit_oferta_2020$NOMMUNI<-gsub(" n´"," n'",as.character(habit_oferta_2020$NOMMUNI))
habit_oferta_2020$NOMMUNI<-gsub(" l´"," l'",as.character(habit_oferta_2020$NOMMUNI))
habit_oferta_2020$NOMMUNI[which(habit_oferta_2020$NOMMUNI=="Coma-ruga")] <- "el Vendrell"


############################################################
############ Adding month, date and trimester
############################################################


habit_oferta_2020$mes <- habit_oferta_2020$date
data.frame(habit_oferta_2020$mes <-month(ymd(habit_oferta_2020$mes)))
habit_oferta_2020$date <- as.Date(habit_oferta_2020$date)
habit_oferta_2020$any <- format(as.Date(habit_oferta_2020$date, format="%Y/%m/%d"),"%Y")
habit_oferta_2020 <- data.table(habit_oferta_2020)
habit_oferta_2020 <- habit_oferta_2020[order(habit_oferta_2020$property_id),]

missing_date_posting <- habit_oferta_2020 %>% group_by(property_id) %>%
  arrange(date) %>%
  filter(row_number()==1)
missing_date_posting <- data.frame(missing_date_posting)
names(missing_date_posting)[1] <- "date_posting2"
missing_date_posting <- missing_date_posting[,c(1,2)]
missing_date_posting2 <- data.frame(habit_oferta_2020$property_id, habit_oferta_2020$date_posting)
names(missing_date_posting2)[c(1,2)] <- c("property_id", "date_posting2")

library(data.table)
unique_key1 <- paste(missing_date_posting$property_id)
unique_key2 <- paste(missing_date_posting2$property_id)
inds <- is.na(missing_date_posting$date_posting2)
missing_date_posting2$date_posting2[inds] <- missing_date_posting$date_posting2[match(unique_key2[inds], unique_key1)]

setDT(habit_oferta_2020); setDT(missing_date_posting)
habit_oferta_2020[is.na(date_posting), date_posting := missing_date_posting[.SD, on=.(property_id), date_posting2]]
habit_oferta_2020 <- habit_oferta_2020[order(habit_oferta_2020$property_id),]

#2000002500003786677
#2000050000003768984

habit_oferta_2020 <- cbind(habit_oferta_2020, missing_date_posting2$date_posting2)
names(habit_oferta_2020)[20] <-"dia_public_nas"

#Adding first date column
test11a <- habit_oferta_2020[order(habit_oferta_2020$date_posting),]
test11a <- habit_oferta_2020[,c(2,6,14)]

test11a <- test11a %>%
  group_by(property_id, municipality, date_posting) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = date_posting)
test11a <- test11a[,c(1,2,4)]

#Adding last date column
test11b <- habit_oferta_2020[order(habit_oferta_2020$date_posting),]
test11b <- habit_oferta_2020[,c(1,2,6)]

test11b <- test11b %>%
  group_by(municipality, property_id) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = date)
test11b <- test11b[,c(2:4)]

test111 <- merge(x=test11a, y=test11b, by.x=c("property_id","municipality"), 
                                  by.y=c("property_id","municipality"))

test111 <- merge(x=habit_oferta_2020, y=test111, by.x=c("property_id","municipality"), 
                 by.y=c("property_id","municipality"), allow.cartesian=TRUE)


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
mitjana_superficie_mes <- test111 %>% aggregate(surface~property_id+mes, mean, digits=2)
mitjana_superficie_mes$mes <- NULL
names(mitjana_superficie_mes)[2] <- "mitjana_superficie_mes"
############################################################
############################################################

test111 <- test111 %>% group_by(property_id, municipality, mes, trimestre) %>%
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

test111$district[which(test111$district=="Sants - Montjuïc")] <- "Sants-Montjuïc"
test111$district[which(test111$district=="Sarrià - Sant Gervasi")] <- "Sarrià-Sant Gervasi"
test111$district[which(test111$district=="Horta - Guinardò")] <- "Horta-Guinardò"
test111$district[which(test111$district=="Horta - Guinardó")] <- "Horta-Guinardò"
test111$district[which(test111$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
test111$district[which(test111$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
test111$district[which(test111$district=="Horta Guinardó")] <- "Horta-Guinardò"


test1111 <- test111 %>% group_by(property_id, municipality, mes) %>%
  filter(row_number()==1)

test1111$mes_any_primera_data <- test1111$date

test1111$preu_m2 <- test1111$price/test1111$surface

test1111 <- filter(test1111, price >= 10 & price <= 10000)
test1111 <- filter(test1111, surface >= 10 & surface <= 10000)

catalunya_noms <- data.frame(catalunya_noms$NOMMUNI, catalunya_noms$CODIMUNI)
names(catalunya_noms)[c(1,2)] <- c("NOMMUNI", "CODIMUNI")
test1111 <- join(test1111, catalunya_noms, by = "NOMMUNI")
habit_oferta_2020 <- test1111

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

habit_oferta_2020$data1 <- monthStart(habit_oferta_2020$date)
habit_oferta_2020$data_final <- (habit_oferta_2020$data1 %m+% months(1))

habit_oferta_2020 <- habit_oferta_2020[,c(1:2, 14, 6:8, 27, 9:13, 15:19, 20:22, 30:31, 23:29)]

write.csv(habit_oferta_2020, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_oferta/tractament_habit_oferta_2020.csv")
