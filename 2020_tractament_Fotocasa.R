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

foto_ofer_2020 <- f_20

foto_ofer_2020$communitycosts_included <- NULL
foto_ofer_2020$deposit_required <- NULL
foto_ofer_2020$street <- NULL
foto_ofer_2020$number <- NULL
foto_ofer_2020$hide_address <- NULL
foto_ofer_2020$rooms <- NULL
foto_ofer_2020$baths <- NULL
foto_ofer_2020$level1 <- NULL
foto_ofer_2020$level2 <- NULL
foto_ofer_2020$level3 <- NULL
foto_ofer_2020$level4 <- NULL
foto_ofer_2020$level6 <- NULL
foto_ofer_2020$level8 <- NULL

names(foto_ofer_2020)[12] <- "municipality"
names(foto_ofer_2020)[13] <- "district"
foto_ofer_2020$date_posting <- as.Date(as.character(foto_ofer_2020$date_posting), format = "%Y%m%d")


foto_ofer_2020$NOMMUNI <- foto_ofer_2020$municipality

foto_ofer_2020 <- foto_ofer_2020 %>%
  mutate_if(is.character, str_trim)

foto_ofer_2020$property_id <- as.character(foto_ofer_2020$property_id)

############################################################
############ Adding month, date and trimester
############################################################

foto_ofer_2020$mes <- foto_ofer_2020$date
data.frame(foto_ofer_2020$mes <-month(ymd(foto_ofer_2020$mes)))
foto_ofer_2020$date <- as.Date(foto_ofer_2020$date)
foto_ofer_2020$any <- format(as.Date(foto_ofer_2020$date, format="%Y/%m/%d"),"%Y")
library(data.table)
foto_ofer_2020 <- data.table(foto_ofer_2020)
foto_ofer_2020 <- foto_ofer_2020%>%
  arrange(property_id, date, date_posting)

###########################################################
######## Mitjanes de superficie
###########################################################
foto_ofer_2020 <- foto_ofer_2020[foto_ofer_2020$surface != -1, ] 

mitjana_surface_o_m <- aggregate(x = foto_ofer_2020$surface,    
                                 by = list(foto_ofer_2020$property_id, 
                                           foto_ofer_2020$municipality, 
                                           foto_ofer_2020$mes),             
                                 FUN = mean,round(mean(foto_ofer_2020$surface), digits=2))                           

names(mitjana_surface_o_m)[1:4] <- c("property_id", "municipality", "mes", "mitjana_superf_o_mes")

test111_20 <- merge(x=foto_ofer_2020, y=mitjana_surface_o_m, 
                    by.x=c("property_id","municipality", "mes"), 
                    by.y=c("property_id","municipality", "mes"))

##################################################################
############## Mitjanes de preu
##################################################################

mitjana_price_o_m <- aggregate(x = foto_ofer_2020$price,    
                               by = list(foto_ofer_2020$property_id, 
                                         foto_ofer_2020$municipality, 
                                         foto_ofer_2020$mes),             
                               FUN = mean,round(mean(foto_ofer_2020$price), digits=2))                           

names(mitjana_price_o_m)[1:4] <- c("property_id", "municipality", "mes", "mitjana_price_o_mes")


test111_20 <- merge(x=test111_20, y=mitjana_price_o_m, 
                    by.x = c("property_id","municipality", "mes"),
                    by.y = c("property_id","municipality", "mes"))


##################################################################
############## Preu M2
##################################################################

test111_20$preu_m2_mes <- test111_20$mitjana_price_o_mes/test111_20$mitjana_superf_o_mes

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

#foto_ofer_2020 <- cbind(foto_ofer_2020, missing_date_posting2$date_posting2)
#names(foto_ofer_2020)[19] <-"dia_public_nas"

#Adding first date column
test11a_20_fd <- test111_20[order(foto_ofer_2020$date),]
test11a_20_fd <- test111_20[,c(1:4)]

test11a_20_fd <- test11a_20_fd %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = date)
test11a_20_fd <- test11a_20_fd[,c(1,2,3,5)]

#Adding last date column
test11b_20_ld <- test111_20[order(foto_ofer_2020$date),]
test11b_20_ld <- test111_20[,c(1:4)]

test11b_20_ld <- test11b_20_ld %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = date)
test11b_20_ld <- test11b_20_ld[,c(1,2,3,5)]

test111_20_pm_ud <- merge(x=test11a_20_fd, y=test11b_20_ld, 
                          by.x=c("property_id","municipality"), 
                          by.y=c("property_id","municipality"))


#Adding date posting
test11b_20_dp <- test111_20[order(foto_ofer_2020$date_posting),]
test11b_20_dp <- test111_20[,c(1:3, 12)]

test11b_20_dp <- test11b_20_dp %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
test11b_20_dp <- test11b_20_dp[,c(1,2,3,5)]

test111_20_pm_ud_dp <- merge(x=test111_20_pm_ud, y=test11b_20_dp, 
                             by.x=c("property_id","municipality"), 
                             by.y=c("property_id","municipality"))

test111_20_pm_ud_dp <- data.frame(test111_20_pm_ud_dp$property_id, 
                                  test111_20_pm_ud_dp$municipality, 
                                  test111_20_pm_ud_dp$primera_data,
                                  test111_20_pm_ud_dp$ultima_data,
                                  test111_20_pm_ud_dp$date_posting_calcul)

names(test111_20_pm_ud_dp)[1:5] <- c("property_id", "municipality", "primera_data", "ultlima_data", "date_posting_calcul")


test111_20_pm_ud_dp <- test111_20_pm_ud_dp[order(test111_20_pm_ud_dp$primera_data),]

test111_20 <- merge(x=test111_20, y=test111_20_pm_ud_dp, 
                    by.x=c("property_id","municipality"), 
                    by.y=c("property_id","municipality"))


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





##########################################
##### Recovering NOMMUNI, CODIMUNI & Cleaning
##########################################

test111_20$NOMMUNI<-gsub(" Capital","",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub(" Girona","Girona",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub(" Lleida","Lleida",as.character(test111_20$NOMMUNI))
test111_20$##########
NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI[which(test111_20$NOMMUNI==" Capital")] <- ""
test111_20$NOMMUNI[which(test111_20$NOMMUNI==" Barcelona")] <- "Barcelona"
test111_20$NOMMUNI[which(test111_20$NOMMUNI==" Girona")] <- "Girona"
test111_20$NOMMUNI[which(test111_20$NOMMUNI==" Lleida")] <- "Lleida"
test111_20$NOMMUNI[which(test111_20$NOMMUNI==" Tarragona")] <- "Tarragona"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Hospitalet de Llobregat (L??)")] <- "l'Hospitalet de Llobregat"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Ametlla de Mar (L??)")] <- "l'Ametlla de Mar"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Escala (L??)")] <- "l'Escala"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Ametlla del Vall??s (L??)")] <- "l'Ametlla del Vall??s"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Ampolla (L??)")] <- "l'Ampolla"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Arbo?? (L??)")] <- "l'Arbo??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Aldea (L??)")] <- "l'Aldea"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Espluga de Francol?? (L??)")] <- "l'Espluga de Francol??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Armentera (L??)")] <- "l'Armentera"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Espluga Calba (L??)")] <- "l'Espluga Calba"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Estany (L??)")] <- "l'Estany"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Albiol (L??)")] <- "l'Albiol"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Bruc (El)")] <- "el Bruc"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Brull (El)")] <- "el Brull"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Masnou (El)")] <- "el Masnou"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Morell (El)")] <- "el Morell"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Montmell (El)")] <- "el Montmell"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Papiol (El)")] <- "el Papiol"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Poal (El)")] <- "el Poal"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Catllar (El)")] <- "el Catllar"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Rourell (El)")] <- "el Rourell"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pla del Pened??s (El)")] <- "el Pla del Pened??s"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Far d??Empord?? (El)")] <- "el Far d'Empord??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Perell?? (El)")] <- "el Perell??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Alam??s (Els)")] <- "els Alam??s"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Riera de Gai?? (La)")] <- "la Riera de Gai??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Seu d??Urgell (La)")] <- "la Seu d'Urgell"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Roca del Vall??s (La)")] <- "la Roca del Vall??s"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Palma de Cervell?? (La)")] <- "la Palma de Cervell??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Garriga (La)")] <- "la Garriga"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Molina (La)")] <- "la Molina"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Bisbal d??Empord?? (La)")] <- "la Bisbal d'Empord??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Tallada d??Empord?? (La)")] <- "la Tallada d'Empord??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Vall d??en Bas (La)")] <- "la Vall d'en Bas"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pobla de C??rvoles (La)")] <- "la Pobla de C??rvoles"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Vall de Bo?? (La)")] <- "la Vall de Bo??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Granada (La)")] <- "la Granada"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Nou de Gai?? (La)")] <- "la Nou de Gai??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Riba (La)")] <- "la Riba"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Secuita (La)")] <- "la Secuita"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Portella (La)")] <- "la Portella"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pobla de Montorn??s (La)")] <- "la Pobla de Montorn??s"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Galera (La)")] <- "la Galera"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Guingueta d????neu (La)")] <- "la Guingueta d'??neu"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Bisbal del Pened??s (La)")] <- "la Bisbal del Pened??s"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Torre de l??Espanyol (La)")] <- "la Torre de l'Espanyol"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="S??nia (La)")] <- "la S??nia"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Vajol (La)")] <- "la Vajol"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Pera (La)")] <- "la Pera"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Sentiu de Si?? (La)")] <- "la Sentiu"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Franqueses del Vall??s (Les)")] <- "les Franqueses del Vall??s"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Masies de Voltreg?? (Les)")] <- "les Masies de Voltreg??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Planes d??Hostoles (Les)")] <- "les Planes d'Hostoles"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Preses (Les)")] <- "les Preses"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Piles (Les)")] <- "les Piles"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Sant Carles de la R??pita")] <- "la R??pita"
test111_20$NOMMUNI<-gsub(" d??"," d'",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub(" n??"," n'",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub(" l??"," l'",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Coma-ruga")] <- "el Vendrell"


test111_20$NOMMUNI<-gsub(" Capital","",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub(" Girona","Girona",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub(" Lleida","Lleida",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub("L'Hospitalet de Llobregat","l'Hospitalet de Llobregat",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub("L'Ametlla de Mar","l'Ametlla de Mar",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub("L'Ametlla del Vall??s","l'Ametlla del Vall??s",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub("El Bruc","el Bruc",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub("El Brull","el Brull",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub("El Vendrell","el Vendrell",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub("L'","l'",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub("El","el",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI<-gsub("La","la",as.character(test111_20$NOMMUNI))
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Torrent (Girona)")] <- "Torrent"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Al??s I Cerc")] <- "Al??s i Cerc"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Avellanes i Santa Linya")] <- "les Avellanes i Santa Linya"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Borges Blanques")] <- "les Borges Blanques"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Borges del Camp")] <- "les Borges del Camp"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Cabanyes")] <- "les Cabanyes"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Franqueses del Vall??s")] <- "les Franqueses del Vall??s"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Masies de Roda")] <- "les Masies de Roda"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Masies de Voltreg??")] <- "les Masies de Voltreg??"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Piles")] <- "les Piles"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Preses")] <- "les Preses"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Les Valls de Valira")] <- "les Valls de Valira"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="de Les")] <- "de les"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="l'Ametlla de Mar ")] <- "l'Ametlla de Mar"
test111_20$NOMMUNI[which(test111_20$NOMMUNI=="Sant Carles de la R??pita")] <- "la R??pita"

test111_20$district[which(test111_20$district=="Sants - Montju??c")] <- "Sants-Montju??c"
test111_20$district[which(test111_20$district=="Sants Montju??c")] <- "Sants-Montju??c"
test111_20$district[which(test111_20$district=="Sarri?? - Sant Gervasi")] <- "Sarri??-Sant Gervasi"
test111_20$district[which(test111_20$district=="Sarri?? Sant Gervasi")] <- "Sarri??-Sant Gervasi"
test111_20$district[which(test111_20$district=="Horta - Guinard??")] <- "Horta-Guinard??"
test111_20$district[which(test111_20$district=="Horta Guinard??")] <- "Horta-Guinard??"
test111_20$district[which(test111_20$district=="Horta - Guinard??")] <- "Horta-Guinard??"
test111_20$district[which(test111_20$district=="Horta Guinard??")] <- "Horta-Guinard??"
test111_20$district[which(test111_20$district=="Sants Montju??c")] <- "Sants-Montju??c"
test111_20$district[which(test111_20$district=="Sarri?? Sant Gervasi")] <- "Sarri??-Sant Gervasi"


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
############## Quantiles 2020
##################################################################

test_2020_f <- test111_20
counting_muni_2020 <- test_2020_f %>% 
  count(c("NOMMUNI", "mes"))
names(counting_muni_2020)[3] <- "freq_muni_mes"

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_mitjana_mes_2020 <- test111_20 %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2_mes), funs(!!!p_funs))

names(preu_mitjana_mes_2020)[c(4:6)] <- c("q1_muni", "q2_muni", "q3_muni")

test_2020_f <- merge(test_2020_f, preu_mitjana_mes_2020,
                     by.x = c("NOMMUNI", "any", "mes"),
                     by.y = c("NOMMUNI", "any", "mes"), .keep_all=TRUE)

test_2020_f <- merge(test_2020_f, counting_muni_2020,
                     by.x = c("NOMMUNI","mes"),
                     by.y = c("NOMMUNI","mes"), .keep_all = TRUE)

####################

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

test_2020_f$data1 <- monthStart(test_2020_f$date)
test_2020_f$data_final <- (test_2020_f$data1 %m+% months(1))

which(test_2020_f$date_posting > test_2020_f$primera_data)
which(is.na(test_2020_f$date_posting))

test_2020_f <- test_2020_f%>%
  group_by(property_id, municipality)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

which(test_2020_f$date_posting_calcul > test_2020_f$primera_data)
which(is.na(test_2020_f$date_posting_calcul))

write_csv(test_2020_f, "/Users/wemigliari/Documents/Po??s-Doutorado & Doutorado/Po??s-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/tractament_foto_oferta_2020.csv")



