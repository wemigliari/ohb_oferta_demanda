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
####### Oferta Habitaclia BCN 2021
###################################
catalunya_noms <- read.xlsx("/Volumes/OHB/04.Laboratoris/LAB201703-Lloguer/04.Dades/Joffre_Wellington/Catalunya_noms_oficials.xlsx",
                            sheetName = "Sheet1")
#To disable the scientific notation in R, pass the following argument: 

options(scipen=999)
options(digits=2)

habit_ofer_b2021 <- h_21
habit_ofer_b2021 <- habit_ofer_b2021 %>% filter(municipality=="Barcelona")
habit_ofer_b2021$province <- NULL
habit_ofer_b2021$region <- NULL
habit_ofer_b2021$area <- NULL
habit_ofer_b2021$zone <- NULL

habit_ofer_b2021$NOMMUNI <- habit_ofer_b2021$municipality

habit_ofer_b2021 <- habit_ofer_b2021 %>%
  mutate_if(is.character, str_trim)

habit_ofer_b2021$property_id <- as.character(habit_ofer_b2021$property_id)

############################################################
############ Adding month, date and trimester
############################################################

habit_ofer_b2021$mes <- habit_ofer_b2021$date
data.frame(habit_ofer_b2021$mes <-month(ymd(habit_ofer_b2021$mes)))
habit_ofer_b2021$date <- as.Date(habit_ofer_b2021$date)
habit_ofer_b2021$any <- format(as.Date(habit_ofer_b2021$date, format="%Y/%m/%d"),"%Y")
library(data.table)
habit_ofer_b2021 <- data.table(habit_ofer_b2021)
habit_ofer_b2021 <- habit_ofer_b2021%>%
  arrange(property_id, date, date_posting)

###########################################################
######## Mitjanes de superficie
###########################################################
habit_ofer_b2021 <- habit_ofer_b2021[habit_ofer_b2021$surface != -1, ] 

mitjana_surface_o_m <- aggregate(x = habit_ofer_b2021$surface,    
                                 by = list(habit_ofer_b2021$property_id, 
                                           habit_ofer_b2021$district, 
                                           habit_ofer_b2021$mes),             
                                 FUN = mean, round(mean(habit_ofer_b2021$surface), digits=2))                            

names(mitjana_surface_o_m)[1:4] <- c("property_id", "district", "mes", "mitjana_superf_o_mes")

test111_b21 <- merge(x=habit_ofer_b2021, y=mitjana_surface_o_m, by.x=c("property_id","district", "mes"), 
                     by.y=c("property_id","district", "mes"))

##################################################################
############## Mitjanes de preu
##################################################################

mitjana_price_o_m <- aggregate(x = habit_ofer_b2021$price,    
                               by = list(habit_ofer_b2021$property_id, 
                                         habit_ofer_b2021$district, 
                                         habit_ofer_b2021$mes),             
                               FUN = mean, round(mean(habit_ofer_b2021$price), digits=2))                            

names(mitjana_price_o_m)[1:4] <- c("property_id", "district", "mes", "mitjana_price_o_mes")


test111_b21 <- merge(x=test111_b21, y=mitjana_price_o_m, 
                     by.x = c("property_id","district", "mes"),
                     by.y = c("property_id","district", "mes"))

2000001100002716449

##################################################################
############## Preu M2
##################################################################

test111_b21$preu_m2_mes <- test111_b21$mitjana_price_o_mes/test111_b21$mitjana_superf_o_mes

test111_b21 <- filter(test111_b21, price >= 10 & price <= 10000)
test111_b21 <- filter(test111_b21, surface >= 10 & surface <= 10000)


##################################################
###### Primera data, ultima data and date posting
##################################################
##################################################
###### Spare this subsection for the next code
##################################################

#2000002500003786677
#2000050000003768984

#habit_ofer_b2021 <- cbind(habit_ofer_b2021, missing_date_posting2$date_posting2)
#names(habit_ofer_b2021)[19] <-"dia_public_nas"

#Adding first date column
test11a_21_fd <- habit_ofer_b2021[order(habit_ofer_b2021$date),]
test11a_21_fd <- habit_ofer_b2021[,c(1:4)]

test11a_21_fd <- test11a_21_fd %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = date)
test11a_21_fd <- test11a_21_fd[,c(1,2,4,5)]

#Adding last date column
test11b_21_ld <- habit_ofer_b2021[order(habit_ofer_b2021$date),]
test11b_21_ld <- habit_ofer_b2021[,c(1:4)]

test11b_21_ld <- test11b_21_ld %>%
  group_by(property_id, district) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = date)
test11b_21_ld <- test11b_21_ld[,c(1,2,4,5)]

test111_b21_pm_ud <- merge(x=test11a_21_fd, y=test11b_21_ld, by.x=c("property_id","district"), 
                           by.y=c("property_id","district"))


#Adding date posting
test11b_21_dp <- habit_ofer_b2021[order(habit_ofer_b2021$date_posting),]
test11b_21_dp <- habit_ofer_b2021[,c(1:2,4, 10)]

test11b_21_dp <- test11b_21_dp %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
test11b_21_dp <- test11b_21_dp[,c(1,2,3,5)]

test111_b21_pm_ud_dp <- merge(x=test111_b21_pm_ud, y=test11b_21_dp, by.x=c("property_id","district"), 
                              by.y=c("property_id","district"))

test111_b21_pm_ud_dp <- data.frame(test111_b21_pm_ud_dp$property_id, 
                                   test111_b21_pm_ud_dp$district, 
                                   test111_b21_pm_ud_dp$primera_data,
                                   test111_b21_pm_ud_dp$ultima_data,
                                   test111_b21_pm_ud_dp$date_posting_calcul)

names(test111_b21_pm_ud_dp)[1:5] <- c("property_id", "district", "primera_data", "ultlima_data", "date_posting_calcul")


test111_b21_pm_ud_dp <- test111_b21_pm_ud_dp[order(test111_b21_pm_ud_dp$primera_data),]

test111_b21 <- merge(x=test111_b21, y=test111_b21_pm_ud_dp, by.x=c("property_id","district"), 
                     by.y=c("property_id","district"))

2002375100004157071
##################################################
##################### Adding trimestre
##################################################

test111_b21 <- test111_b21 %>% mutate(trimestre =
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

test111_b21$NOMMUNI<-gsub(" Capital","",as.character(test111_b21$NOMMUNI))
test111_b21$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(test111_b21$NOMMUNI))
test111_b21$NOMMUNI<-gsub(" Girona","Girona",as.character(test111_b21$NOMMUNI))
test111_b21$NOMMUNI<-gsub(" Lleida","Lleida",as.character(test111_b21$NOMMUNI))
test111_b21$##########
NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(test111_b21$NOMMUNI))
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI==" Capital")] <- ""
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI==" Barcelona")] <- "Barcelona"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI==" Girona")] <- "Girona"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI==" Lleida")] <- "Lleida"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI==" Tarragona")] <- "Tarragona"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Hospitalet de Llobregat (L´)")] <- "l'Hospitalet de Llobregat"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Ametlla de Mar (L´)")] <- "l'Ametlla de Mar"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Escala (L´)")] <- "l'Escala"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Ametlla del Vallès (L´)")] <- "l'Ametlla del Vallès"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Ampolla (L´)")] <- "l'Ampolla"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Arboç (L´)")] <- "l'Arboç"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Aldea (L´)")] <- "l'Aldea"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Espluga de Francolí (L´)")] <- "l'Espluga de Francolí"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Armentera (L´)")] <- "l'Armentera"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Espluga Calba (L´)")] <- "l'Espluga Calba"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Estany (L´)")] <- "l'Estany"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Albiol (L´)")] <- "l'Albiol"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Bruc (El)")] <- "el Bruc"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Brull (El)")] <- "el Brull"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Masnou (El)")] <- "el Masnou"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Morell (El)")] <- "el Morell"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Montmell (El)")] <- "el Montmell"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Papiol (El)")] <- "el Papiol"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Poal (El)")] <- "el Poal"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Catllar (El)")] <- "el Catllar"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Rourell (El)")] <- "el Rourell"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pla del Penedès (El)")] <- "el Pla del Penedès"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Far d´Empordà (El)")] <- "el Far d'Empordà"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Perelló (El)")] <- "el Perelló"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Alamús (Els)")] <- "els Alamús"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Riera de Gaià (La)")] <- "la Riera de Gaià"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Seu d´Urgell (La)")] <- "la Seu d'Urgell"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Roca del Vallès (La)")] <- "la Roca del Vallès"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Palma de Cervelló (La)")] <- "la Palma de Cervelló"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Garriga (La)")] <- "la Garriga"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Molina (La)")] <- "la Molina"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Bisbal d´Empordà (La)")] <- "la Bisbal d'Empordà"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Tallada d´Empordà (La)")] <- "la Tallada d'Empordà"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Vall d´en Bas (La)")] <- "la Vall d'en Bas"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pobla de Cérvoles (La)")] <- "la Pobla de Cérvoles"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Vall de Boí (La)")] <- "la Vall de Boí"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Granada (La)")] <- "la Granada"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Nou de Gaià (La)")] <- "la Nou de Gaià"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Riba (La)")] <- "la Riba"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Secuita (La)")] <- "la Secuita"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Portella (La)")] <- "la Portella"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pobla de Montornès (La)")] <- "la Pobla de Montornès"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Galera (La)")] <- "la Galera"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Guingueta d´Àneu (La)")] <- "la Guingueta d'Àneu"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Bisbal del Penedès (La)")] <- "la Bisbal del Penedès"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Torre de l´Espanyol (La)")] <- "la Torre de l'Espanyol"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Sénia (La)")] <- "la Sénia"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Vajol (La)")] <- "la Vajol"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Pera (La)")] <- "la Pera"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Sentiu de Sió (La)")] <- "la Sentiu"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Franqueses del Vallès (Les)")] <- "les Franqueses del Vallès"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Masies de Voltregà (Les)")] <- "les Masies de Voltregà"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Planes d´Hostoles (Les)")] <- "les Planes d'Hostoles"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Preses (Les)")] <- "les Preses"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Piles (Les)")] <- "les Piles"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"
test111_b21$NOMMUNI<-gsub(" d´"," d'",as.character(test111_b21$NOMMUNI))
test111_b21$NOMMUNI<-gsub(" n´"," n'",as.character(test111_b21$NOMMUNI))
test111_b21$NOMMUNI<-gsub(" l´"," l'",as.character(test111_b21$NOMMUNI))
test111_b21$NOMMUNI[which(test111_b21$NOMMUNI=="Coma-ruga")] <- "el Vendrell"

test111_b21$district[which(test111_b21$district=="Sants - Montjuïc")] <- "Sants-Montjuïc"
test111_b21$district[which(test111_b21$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
test111_b21$district[which(test111_b21$district=="Sarrià - Sant Gervasi")] <- "Sarrià-Sant Gervasi"
test111_b21$district[which(test111_b21$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
test111_b21$district[which(test111_b21$district=="Horta - Guinardò")] <- "Horta-Guinardò"
test111_b21$district[which(test111_b21$district=="Horta Guinardó")] <- "Horta-Guinardò"
test111_b21$district[which(test111_b21$district=="Horta - Guinardó")] <- "Horta-Guinardò"
test111_b21$district[which(test111_b21$district=="Horta Guinardò")] <- "Horta-Guinardò"
test111_b21$district[which(test111_b21$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
test111_b21$district[which(test111_b21$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"




############################################################
############ Tipologias Plurifamiliar & Unifamiliar
############################################################


test111_b21$tipologia <- test111_b21$property_subtype

test111_b21$tipologia[which(test111_b21$tipologia=="Apartamento")] <- "Plurifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Ático")] <- "Plurifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Casa")] <- "Unifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Casa adosada")] <- "Unifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Casa pareada")] <- "Unifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Unifamiliar pareada")] <- "Unifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Chalet")] <- "Unifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Dúplex")] <- "Plurifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Estudio")] <- "Plurifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Loft")] <- "Plurifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Masía")] <- "Unifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Piso")] <- "Plurifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Planta baja")] <- "Plurifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Torre")] <- "Plurifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Tríplex")] <- "Plurifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Casa-Chalet")] <- "Unifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Finca rústica")] <- "Unifamiliar"
test111_b21$tipologia[which(test111_b21$tipologia=="Duplex")] <- "Plurifamiliar"

2002384300000000219
##################################################################

catalunya_noms <- data.frame(catalunya_noms$NOMMUNI, catalunya_noms$CODIMUNI)
names(catalunya_noms)[c(1,2)] <- c("NOMMUNI", "CODIMUNI")
test111_b21 <- join(test111_b21, catalunya_noms, by = "NOMMUNI")

##################################################################
############## Quantiles 2021
##################################################################

test_b2021 <- test111_b21
counting_bcn_b2021 <- test_b2021 %>% 
  count(c("district", "mes"))
names(counting_bcn_b2021)[3] <- "freq_dist_mes"

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_mitjana_mes_b2021<- test111_b21 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2_mes), funs(!!!p_funs))

names(preu_mitjana_mes_b2021)[c(4:6)] <- c("q1_district", "q2_district", "q3_district")

test_b2021 <- merge(test_b2021, preu_mitjana_mes_b2021,
                    by.x = c("district", "any", "mes"),
                    by.y = c("district", "any", "mes"), .keep_all=TRUE)

test_b2021 <- merge(test_b2021, counting_bcn_b2021,
                    by.x = c("district","mes"),
                    by.y = c("district","mes"), .keep_all = TRUE)

####################

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

test_b2021$data1 <- monthStart(test_b2021$date)
test_b2021$data_final <- (test_b2021$data1 %m+% months(1))

which(test_b2021$date_posting > test_b2021$primera_data)
which(is.na(test_b2021$date_posting))

test_b2021 <- test_b2021%>%
  group_by(property_id, district)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
)

which(test_b2021$date_posting_calcul > test_b2021$primera_data)
which(is.na(test_b2021$date_posting_calcul))

write.csv(test_b2021, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Habitaclia_oferta/tractament_habit_oferta_b2021.csv")




