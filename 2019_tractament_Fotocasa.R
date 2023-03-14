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

###################################
####### Oferta Fotocasa 2019
###################################
catalunya_noms <- read.xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Catalunya_noms_oficials.xlsx",
                            sheetName = "Sheet1")
#To disable the scientific notation in R, pass the following argument: 

options(scipen=999)
options(digits=2)

foto_ofer_2019 <- f_19

foto_ofer_2019$communitycosts_included <- NULL
foto_ofer_2019$deposit_required <- NULL
foto_ofer_2019$street <- NULL
foto_ofer_2019$number <- NULL
foto_ofer_2019$hide_address <- NULL
foto_ofer_2019$rooms <- NULL
foto_ofer_2019$baths <- NULL
foto_ofer_2019$level1 <- NULL
foto_ofer_2019$level2 <- NULL
foto_ofer_2019$level3 <- NULL
foto_ofer_2019$level4 <- NULL
foto_ofer_2019$level6 <- NULL
foto_ofer_2019$level8 <- NULL

names(foto_ofer_2019)[12] <- "municipality"
names(foto_ofer_2019)[13] <- "district"
foto_ofer_2019$date_posting <- as.Date(as.character(foto_ofer_2019$date_posting), format = "%Y%m%d")


foto_ofer_2019$NOMMUNI <- foto_ofer_2019$municipality

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
foto_ofer_2019 <- data.table(foto_ofer_2019)
foto_ofer_2019 <- foto_ofer_2019%>%
  arrange(property_id, date, date_posting)

###########################################################
######## Mitjanes de superficie
###########################################################
foto_ofer_2019 <- foto_ofer_2019[foto_ofer_2019$surface != -1, ] 

mitjana_surface_o_m <- aggregate(x = foto_ofer_2019$surface,    
                                 by = list(foto_ofer_2019$property_id, 
                                           foto_ofer_2019$municipality, 
                                           foto_ofer_2019$mes),             
                                 FUN = mean,round(mean(foto_ofer_2019$surface), digits=2))                           

names(mitjana_surface_o_m)[1:4] <- c("property_id", "municipality", "mes", "mitjana_superf_o_mes")

test111_19 <- merge(x=foto_ofer_2019, y=mitjana_surface_o_m, by.x=c("property_id","municipality", "mes"), 
                    by.y=c("property_id","municipality", "mes"))

##################################################################
############## Mitjanes de preu
##################################################################

mitjana_price_o_m <- aggregate(x = foto_ofer_2019$price,    
                               by = list(foto_ofer_2019$property_id, 
                                         foto_ofer_2019$municipality, 
                                         foto_ofer_2019$mes),             
                               FUN = mean,round(mean(foto_ofer_2019$price), digits=2))                           

names(mitjana_price_o_m)[1:4] <- c("property_id", "municipality", "mes", "mitjana_price_o_mes")


test111_19 <- merge(x=test111_19, y=mitjana_price_o_m, 
                    by.x = c("property_id","municipality", "mes"),
                    by.y = c("property_id","municipality", "mes"))


##################################################################
############## Preu M2
##################################################################

test111_19$preu_m2_mes <- test111_19$mitjana_price_o_mes/test111_19$mitjana_superf_o_mes

test111_19 <- filter(test111_19, price >= 10 & price <= 10000)
test111_19 <- filter(test111_19, surface >= 10 & surface <= 10000)


##################################################
###### Primera data, ultima data and date posting
##################################################
##################################################
###### Spare this subsection for the next code
##################################################

#2000002500003786677
#2000050000003768984

#foto_ofer_2019 <- cbind(foto_ofer_2019, missing_date_posting2$date_posting2)
#names(foto_ofer_2019)[19] <-"dia_public_nas"

#Adding first date column
test11a_19_fd <- test111_19[order(foto_ofer_2019$date),]
test11a_19_fd <- test111_19[,c(1:4)]

test11a_19_fd <- test11a_19_fd %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = date)
test11a_19_fd <- test11a_19_fd[,c(1,2,3,5)]

#Adding last date column
test11b_19_ld <- test111_19[order(foto_ofer_2019$date),]
test11b_19_ld <- test111_19[,c(1:4)]

test11b_19_ld <- test11b_19_ld %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = date)
test11b_19_ld <- test11b_19_ld[,c(1,2,3,5)]

test111_19_pm_ud <- merge(x=test11a_19_fd, y=test11b_19_ld, by.x=c("property_id","municipality"), 
                          by.y=c("property_id","municipality"))


#Adding date posting
test11b_19_dp <- test111_19[order(foto_ofer_2019$date_posting),]
test11b_19_dp <- test111_19[,c(1:3, 12)]

test11b_19_dp <- test11b_19_dp %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
test11b_19_dp <- test11b_19_dp[,c(1,2,3,5)]

test111_19_pm_ud_dp <- merge(x=test111_19_pm_ud, y=test11b_19_dp, by.x=c("property_id","municipality"), 
                             by.y=c("property_id","municipality"))

test111_19_pm_ud_dp <- data.frame(test111_19_pm_ud_dp$property_id, 
                                  test111_19_pm_ud_dp$municipality, 
                                  test111_19_pm_ud_dp$primera_data,
                                  test111_19_pm_ud_dp$ultima_data,
                                  test111_19_pm_ud_dp$date_posting_calcul)

names(test111_19_pm_ud_dp)[1:5] <- c("property_id", "municipality", "primera_data", "ultlima_data", "date_posting_calcul")


test111_19_pm_ud_dp <- test111_19_pm_ud_dp[order(test111_19_pm_ud_dp$primera_data),]

test111_19 <- merge(x=test111_19, y=test111_19_pm_ud_dp, by.x=c("property_id","municipality"), 
                    by.y=c("property_id","municipality"))


##################################################
##################### Adding trimestre
##################################################

test111_19 <- test111_19 %>% mutate(trimestre =
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





##########################################
##### Recovering NOMMUNI, CODIMUNI & Cleaning
##########################################

test111_19$NOMMUNI<-gsub(" Capital","",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub(" Girona","Girona",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub(" Lleida","Lleida",as.character(test111_19$NOMMUNI))
test111_19$##########
NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI[which(test111_19$NOMMUNI==" Capital")] <- ""
test111_19$NOMMUNI[which(test111_19$NOMMUNI==" Barcelona")] <- "Barcelona"
test111_19$NOMMUNI[which(test111_19$NOMMUNI==" Girona")] <- "Girona"
test111_19$NOMMUNI[which(test111_19$NOMMUNI==" Lleida")] <- "Lleida"
test111_19$NOMMUNI[which(test111_19$NOMMUNI==" Tarragona")] <- "Tarragona"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Hospitalet de Llobregat (L´)")] <- "l'Hospitalet de Llobregat"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Ametlla de Mar (L´)")] <- "l'Ametlla de Mar"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Escala (L´)")] <- "l'Escala"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Ametlla del Vallès (L´)")] <- "l'Ametlla del Vallès"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Ampolla (L´)")] <- "l'Ampolla"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Arboç (L´)")] <- "l'Arboç"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Aldea (L´)")] <- "l'Aldea"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Espluga de Francolí (L´)")] <- "l'Espluga de Francolí"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Armentera (L´)")] <- "l'Armentera"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Espluga Calba (L´)")] <- "l'Espluga Calba"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Estany (L´)")] <- "l'Estany"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Albiol (L´)")] <- "l'Albiol"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Bruc (El)")] <- "el Bruc"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Brull (El)")] <- "el Brull"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Masnou (El)")] <- "el Masnou"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Morell (El)")] <- "el Morell"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Montmell (El)")] <- "el Montmell"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Papiol (El)")] <- "el Papiol"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Poal (El)")] <- "el Poal"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Catllar (El)")] <- "el Catllar"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Rourell (El)")] <- "el Rourell"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pla del Penedès (El)")] <- "el Pla del Penedès"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Far d´Empordà (El)")] <- "el Far d'Empordà"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Perelló (El)")] <- "el Perelló"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Alamús (Els)")] <- "els Alamús"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Riera de Gaià (La)")] <- "la Riera de Gaià"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Seu d´Urgell (La)")] <- "la Seu d'Urgell"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Roca del Vallès (La)")] <- "la Roca del Vallès"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Palma de Cervelló (La)")] <- "la Palma de Cervelló"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Garriga (La)")] <- "la Garriga"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Molina (La)")] <- "la Molina"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Bisbal d´Empordà (La)")] <- "la Bisbal d'Empordà"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Tallada d´Empordà (La)")] <- "la Tallada d'Empordà"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Vall d´en Bas (La)")] <- "la Vall d'en Bas"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pobla de Cérvoles (La)")] <- "la Pobla de Cérvoles"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Vall de Boí (La)")] <- "la Vall de Boí"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Granada (La)")] <- "la Granada"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Nou de Gaià (La)")] <- "la Nou de Gaià"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Riba (La)")] <- "la Riba"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Secuita (La)")] <- "la Secuita"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Portella (La)")] <- "la Portella"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pobla de Montornès (La)")] <- "la Pobla de Montornès"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Galera (La)")] <- "la Galera"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Guingueta d´Àneu (La)")] <- "la Guingueta d'Àneu"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Bisbal del Penedès (La)")] <- "la Bisbal del Penedès"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Torre de l´Espanyol (La)")] <- "la Torre de l'Espanyol"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Sénia (La)")] <- "la Sénia"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Vajol (La)")] <- "la Vajol"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Pera (La)")] <- "la Pera"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Sentiu de Sió (La)")] <- "la Sentiu"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Franqueses del Vallès (Les)")] <- "les Franqueses del Vallès"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Masies de Voltregà (Les)")] <- "les Masies de Voltregà"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Planes d´Hostoles (Les)")] <- "les Planes d'Hostoles"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Preses (Les)")] <- "les Preses"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Piles (Les)")] <- "les Piles"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"
test111_19$NOMMUNI<-gsub(" d´"," d'",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub(" n´"," n'",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub(" l´"," l'",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Coma-ruga")] <- "el Vendrell"


test111_19$NOMMUNI<-gsub(" Capital","",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub(" Girona","Girona",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub(" Lleida","Lleida",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub("L'Hospitalet de Llobregat","l'Hospitalet de Llobregat",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub("L'Ametlla de Mar","l'Ametlla de Mar",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub("L'Ametlla del Vallès","l'Ametlla del Vallès",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub("El Bruc","el Bruc",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub("El Brull","el Brull",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub("El Vendrell","el Vendrell",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub("L'","l'",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub("El","el",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI<-gsub("La","la",as.character(test111_19$NOMMUNI))
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Torrent (Girona)")] <- "Torrent"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Alàs I Cerc")] <- "Alàs i Cerc"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Avellanes i Santa Linya")] <- "les Avellanes i Santa Linya"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Borges Blanques")] <- "les Borges Blanques"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Borges del Camp")] <- "les Borges del Camp"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Cabanyes")] <- "les Cabanyes"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Franqueses del Vallès")] <- "les Franqueses del Vallès"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Masies de Roda")] <- "les Masies de Roda"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Masies de Voltregà")] <- "les Masies de Voltregà"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Piles")] <- "les Piles"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Preses")] <- "les Preses"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Les Valls de Valira")] <- "les Valls de Valira"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="de Les")] <- "de les"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="l'Ametlla de Mar ")] <- "l'Ametlla de Mar"
test111_19$NOMMUNI[which(test111_19$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"

test111_19$district[which(test111_19$district=="Sants - Montjuïc")] <- "Sants-Montjuïc"
test111_19$district[which(test111_19$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
test111_19$district[which(test111_19$district=="Sarrià - Sant Gervasi")] <- "Sarrià-Sant Gervasi"
test111_19$district[which(test111_19$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
test111_19$district[which(test111_19$district=="Horta - Guinardò")] <- "Horta-Guinardò"
test111_19$district[which(test111_19$district=="Horta Guinardó")] <- "Horta-Guinardò"
test111_19$district[which(test111_19$district=="Horta - Guinardó")] <- "Horta-Guinardò"
test111_19$district[which(test111_19$district=="Horta Guinardò")] <- "Horta-Guinardò"
test111_19$district[which(test111_19$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
test111_19$district[which(test111_19$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"


count(test111_19, "district")

############################################################
############ Tipologias Plurifamiliar & Unifamiliar
############################################################

test111_19$tipologia <- test111_19$property_subtype

test111_19$tipologia[which(test111_19$tipologia=="Apartamento")] <- "Plurifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Ático")] <- "Plurifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Casa")] <- "Unifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Casa adosada")] <- "Unifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Casa pareada")] <- "Unifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Unifamiliar pareada")] <- "Unifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Chalet")] <- "Unifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Dúplex")] <- "Plurifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Estudio")] <- "Plurifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Loft")] <- "Plurifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Masía")] <- "Unifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Piso")] <- "Plurifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Planta baja")] <- "Plurifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Torre")] <- "Plurifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Tríplex")] <- "Plurifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Casa-Chalet")] <- "Unifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Finca rústica")] <- "Unifamiliar"
test111_19$tipologia[which(test111_19$tipologia=="Duplex")] <- "Plurifamiliar"

count(test111_19, "tipologia")

##################################################################

catalunya_noms <- data.frame(catalunya_noms$NOMMUNI, catalunya_noms$CODIMUNI)
names(catalunya_noms)[c(1,2)] <- c("NOMMUNI", "CODIMUNI")
test111_19 <- join(test111_19, catalunya_noms, by = "NOMMUNI")

##################################################################
############## Quantiles 2021
##################################################################

test_2019_f <- test111_19
counting_muni_2019 <- test_2019_f %>% 
  count(c("NOMMUNI", "mes"))
names(counting_muni_2019)[3] <- "freq_muni_mes"

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_mitjana_mes_2019 <- test111_19 %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2_mes), funs(!!!p_funs))

names(preu_mitjana_mes_2019)[c(4:6)] <- c("q1_muni", "q2_muni", "q3_muni")

test_2019_f <- merge(test_2019_f, preu_mitjana_mes_2019,
                     by.x= c("NOMMUNI", "any", "mes"),
                     by.y= c("NOMMUNI", "any", "mes"), .keep_all = TRUE)

test_2019_f <- merge(test_2019_f, counting_muni_2019,
                     by.x = c("NOMMUNI","mes"),
                     by.y = c("NOMMUNI","mes"), .keep_all = TRUE)

####################

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

test_2019_f$data1 <- monthStart(test_2019_f$date)
test_2019_f$data_final <- (test_2019_f$data1 %m+% months(1))

which(test_2019_f$date_posting > test_2019_f$primera_data)
which(is.na(test_2019_f$date_posting))

test_2019_f <- test_2019_f%>%
  group_by(property_id, municipality)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

which(test_2019_f$date_posting_calcul > test_2019_f$primera_data)
which(is.na(test_2019_f$date_posting_calcul))
149540709

write_csv(test_2019_f, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/tractament_foto_oferta_2019.csv")





