library(dplyr)
library(xlsx)
library(readr)
library(plyr)
library(lubridate)
library(tidyverse)
options(scipen = 999)
options(digits=3)

foto_oferta_2022 <- read_csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/tractament_foto_oferta_2022.csv")
foto_oferta_2021 <- read_csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/tractament_foto_oferta_2021.csv")
foto_oferta_2020 <-read_csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/tractament_foto_oferta_2020.csv")
foto_oferta_2019 <- read_csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/tractament_foto_oferta_2019.csv")
foto_oferta_series <- rbind(foto_oferta_2019, foto_oferta_2020, foto_oferta_2021, foto_oferta_2022)
foto_oferta_series <- foto_oferta_series[, -c(11)]
names(foto_oferta_series)[c(3:5)] <- c("municipality", "district", "zone")
foto_oferta_series$source <- "Fotocasa"
foto_oferta_series$district[which(foto_oferta_series$district=="Horta - Guinardó")] <- "Horta-Guinardò"


##################################################
################### Districtes BCN Plurifamiliar
##################################################
##################################################
################### Districtes BCN Plurifamiliar
##################################################
##################################################
################### Districtes BCN Plurifamiliar
##################################################
##################################################
################### Districtes BCN Plurifamiliar
##################################################

foto_oferta_series_pluri <- foto_oferta_series%>%filter(tipologia=="Plurifamiliar")


################################################################
################### Quantiles Districtes Barcelona Plurifamiliar
################################################################
################################################################
################### Quantiles Districtes Barcelona Plurifamiliar
################################################################
################################################################
################### Quantiles Districtes Barcelona Plurifamiliar
################################################################

quantiles_catalunya <- foto_oferta_series_pluri
quantiles_catalunya2 <- data.frame(quantiles_catalunya)

test_2019 <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2019")
counting_district_2019 <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI", "district","any", "mes"))
counting_district_2019 <- counting_district_2019 %>% filter(NOMMUNI=="Barcelona")
counting_district_2019 <- counting_district_2019[,c(2,5)]
names(counting_district_2019)[2] <- "freq_district"

preu_m2_2019 <- data.frame(test_2019$any, test_2019$district, test_2019$property_id,test_2019$mes, test_2019$preu_m2)
names(preu_m2_2019)[c(1:5)] <- c("any", "district","property_id", "mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2019<- preu_m2_2019 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2019)[c(4:6)] <- c("q1_district", "q2_district", "q3_district")

test_2019 <- left_join(test_2019, preu_m2_2019)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2019 <- left_join(test_2019, counting_district_2019)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

test_2019 <-test_2019[!(test_2019$NOMMUNI=="Barcelona" & test_2019$district=="Undefined"),]
test_2019 <-test_2019[!(test_2019$NOMMUNI=="Barcelona" & test_2019$district=="N/A"),]

##################

test_2020 <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2020")
counting_district_2020 <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI", "district","any", "mes"))
counting_district_2020 <- counting_district_2020 %>% filter(NOMMUNI=="Barcelona")
counting_district_2020 <- counting_district_2020[,c(2,5)]
names(counting_district_2020)[2] <- "freq_district"

preu_m2_2020 <- data.frame(test_2020$any, test_2020$district, test_2020$property_id,test_2020$mes, test_2020$preu_m2)
names(preu_m2_2020)[c(1:5)] <- c("any", "district","property_id", "mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2020<- preu_m2_2020 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2020)[c(4:6)] <- c("q1_district", "q2_district", "q3_district")

test_2020 <- left_join(test_2020, preu_m2_2020)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2020 <- left_join(test_2020, counting_district_2020)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

test_2020 <-test_2020[!(test_2020$NOMMUNI=="Barcelona" & test_2020$district=="Undefined"),]
test_2020 <-test_2020[!(test_2020$NOMMUNI=="Barcelona" & test_2020$district=="N/A"),]

##################

test_2021 <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2021")
counting_district_2021 <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI", "district","any", "mes"))
counting_district_2021 <- counting_district_2021 %>% filter(NOMMUNI=="Barcelona")
counting_district_2021 <- counting_district_2021[,c(2,5)]
names(counting_district_2021)[2] <- "freq_district"

preu_m2_2021 <- data.frame(test_2021$any, test_2021$district, test_2021$property_id,test_2021$mes, test_2021$preu_m2)
names(preu_m2_2021)[c(1:5)] <- c("any", "district","property_id", "mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2021<- preu_m2_2021 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2021)[c(4:6)] <- c("q1_district", "q2_district", "q3_district")

test_2021 <- left_join(test_2021, preu_m2_2021)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2021 <- left_join(test_2021, counting_district_2021)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

test_2021 <-test_2021[!(test_2021$NOMMUNI=="Barcelona" & test_2021$district=="Undefined"),]
test_2021 <-test_2021[!(test_2021$NOMMUNI=="Barcelona" & test_2021$district=="N/A"),]

##################

test_2022 <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2022")
counting_district_2022 <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI", "district","any", "mes"))
counting_district_2022 <- counting_district_2022 %>% filter(NOMMUNI=="Barcelona")
counting_district_2022 <- counting_district_2022[,c(2,5)]
names(counting_district_2022)[2] <- "freq_district"

preu_m2_2022 <- data.frame(test_2022$any, test_2022$district, test_2022$property_id,test_2022$mes, test_2022$preu_m2)
names(preu_m2_2022)[c(1:5)] <- c("any", "district","property_id", "mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2022<- preu_m2_2022 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2022)[c(4:6)] <- c("q1_district", "q2_district", "q3_district")

test_2022 <- left_join(test_2022, preu_m2_2022)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2022 <- left_join(test_2022, counting_district_2022)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

test_2022 <-test_2022[!(test_2022$NOMMUNI=="Barcelona" & test_2022$district=="Undefined"),]
test_2022 <-test_2022[!(test_2022$NOMMUNI=="Barcelona" & test_2022$district=="N/A"),]

####################

quantiles_districtes_bcn <- rbind(test_2019, test_2020, test_2021,test_2022)
quantiles_districtes_bcn_pluri <- quantiles_districtes_bcn[,c(1, 3:4, 10, 12:24, 26:30)]


################################################################
################### Quantiles Municipis Catalunya Plurifamiliar
#################################################################
#################################################################
################### Quantiles Municipis Catalunya Plurifamiliar
#################################################################
#################################################################
################### Quantiles Municipis Catalunya Plurifamiliar
#################################################################

test_2019_c <- quantiles_catalunya2 %>% group_by(any) %>% filter(any=="2019")
counting_municipi_2019_c <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI","any", "mes"))
names(counting_municipi_2019_c)[4] <- "freq_municipi"

preu_m2_2019_c <- data.frame(test_2019_c$any, test_2019_c$NOMMUNI, test_2019_c$property_id,test_2019_c$mes, test_2019_c$preu_m2)
names(preu_m2_2019_c)[c(1:5)] <- c("any", "NOMMUNI","property_id","mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2019_c<- preu_m2_2019_c %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2019_c)[c(4:6)] <- c("q1_municipi", "q2_municipi", "q3_municipi")

test_2019_c <- left_join(test_2019_c, preu_m2_2019_c)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2019_c <- left_join(test_2019_c, counting_municipi_2019_c)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

####################

test_2020_c <- quantiles_catalunya2 %>% group_by(any) %>% filter(any=="2020")
counting_municipi_2020_c <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI","any", "mes"))
names(counting_municipi_2020_c)[4] <- "freq_municipi"

preu_m2_2020_c <- data.frame(test_2020_c$any, test_2020_c$NOMMUNI, test_2020_c$property_id,test_2020_c$mes, test_2020_c$preu_m2)
names(preu_m2_2020_c)[c(1:5)] <- c("any", "NOMMUNI","property_id","mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2020_c<- preu_m2_2020_c %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2020_c)[c(4:6)] <- c("q1_municipi", "q2_municipi", "q3_municipi")

test_2020_c <- left_join(test_2020_c, preu_m2_2020_c)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2020_c <- left_join(test_2020_c, counting_municipi_2020_c)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

####################

test_2021_c <- quantiles_catalunya2 %>% group_by(any) %>% filter(any=="2021")
counting_municipi_2021_c <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI","any", "mes"))
names(counting_municipi_2021_c)[4] <- "freq_municipi"

preu_m2_2021_c <- data.frame(test_2021_c$any, test_2021_c$NOMMUNI, test_2021_c$property_id,test_2021_c$mes, test_2021_c$preu_m2)
names(preu_m2_2021_c)[c(1:5)] <- c("any", "NOMMUNI","property_id","mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2021_c<- preu_m2_2021_c %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2021_c)[c(4:6)] <- c("q1_municipi", "q2_municipi", "q3_municipi")

test_2021_c <- left_join(test_2021_c, preu_m2_2021_c)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2021_c <- left_join(test_2021_c, counting_municipi_2021_c)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

####################
test_2022_c <- quantiles_catalunya2 %>% group_by(any) %>% filter(any=="2022")
counting_municipi_2022_c <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI","any", "mes"))
names(counting_municipi_2022_c)[4] <- "freq_municipi"

preu_m2_2022_c <- data.frame(test_2022_c$any, test_2022_c$NOMMUNI, test_2022_c$property_id,test_2022_c$mes, test_2022_c$preu_m2)
names(preu_m2_2022_c)[c(1:5)] <- c("any", "NOMMUNI","property_id","mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2022_c<- preu_m2_2022_c %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2022_c)[c(4:6)] <- c("q1_municipi", "q2_municipi", "q3_municipi")

test_2022_c <- left_join(test_2022_c, preu_m2_2022_c)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2022_c <- left_join(test_2022_c, counting_municipi_2022_c)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

####################
quantiles_catalunya_muni_pluri <- rbind(test_2019_c, test_2020_c, test_2021_c,test_2022_c)
quantiles_catalunya_muni_pluri <- quantiles_catalunya_muni_pluri[,c(1, 3:4, 10, 12:24, 26:30)]

##################################################
################### Districtes BCN Plurifamiliar
##################################################
##################################################
################### Districtes BCN Plurifamiliar
##################################################
##################################################
################### Districtes BCN Plurifamiliar
##################################################

foto_oferta_series_uni <- foto_oferta_series%>%filter(tipologia=="Unifamiliar")

##############################################################
################### Quantiles Districtes Barcelona Unifamiliar
##############################################################
##############################################################
################### Quantiles Districtes Barcelona Unifamiliar
##############################################################
##############################################################
################### Quantiles Districtes Barcelona Unifamiliar
##############################################################

quantiles_catalunya <- foto_oferta_series_uni
quantiles_catalunya2 <- data.frame(quantiles_catalunya)

test_2019_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2019")
counting_district_2019_uni <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI", "district","any", "mes"))
counting_district_2019_uni <- counting_district_2019_uni %>% filter(NOMMUNI=="Barcelona")
counting_district_2019_uni <- counting_district_2019_uni[,c(2,5)]
names(counting_district_2019_uni)[2] <- "freq_district"

preu_m2_2019 <- data.frame(test_2019_uni$any, test_2019_uni$district, test_2019_uni$property_id,test_2019_uni$mes, test_2019_uni$preu_m2)
names(preu_m2_2019)[c(1:5)] <- c("any", "district","property_id", "mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2019<- preu_m2_2019 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2019)[c(4:6)] <- c("q1_district", "q2_district", "q3_district")

test_2019_uni <- left_join(test_2019_uni, preu_m2_2019)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2019_uni <- left_join(test_2019_uni, counting_district_2019_uni)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

test_2019_uni <-test_2019_uni[!(test_2019_uni$NOMMUNI=="Barcelona" & test_2019_uni$district=="Undefined"),]
test_2019_uni <-test_2019_uni[!(test_2019_uni$NOMMUNI=="Barcelona" & test_2019_uni$district=="N/A"),]

##################

test_2020_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2020")
counting_district_2020_uni <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI", "district","any", "mes"))
counting_district_2020_uni <- counting_district_2020_uni %>% filter(NOMMUNI=="Barcelona")
counting_district_2020_uni <- counting_district_2020_uni[,c(2,5)]
names(counting_district_2020_uni)[2] <- "freq_district"

preu_m2_2020 <- data.frame(test_2020_uni$any, test_2020_uni$district, test_2020_uni$property_id,test_2020_uni$mes, test_2020_uni$preu_m2)
names(preu_m2_2020)[c(1:5)] <- c("any", "district","property_id", "mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2020<- preu_m2_2020 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2020)[c(4:6)] <- c("q1_district", "q2_district", "q3_district")

test_2020_uni <- left_join(test_2020_uni, preu_m2_2020)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2020_uni <- left_join(test_2020_uni, counting_district_2020_uni)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

test_2020_uni <-test_2020_uni[!(test_2020_uni$NOMMUNI=="Barcelona" & test_2020_uni$district=="Undefined"),]
test_2020_uni <-test_2020_uni[!(test_2020_uni$NOMMUNI=="Barcelona" & test_2020_uni$district=="N/A"),]

##################

test_2021_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2021")
counting_district_2021_uni <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI", "district","any", "mes"))
counting_district_2021_uni <- counting_district_2021_uni %>% filter(NOMMUNI=="Barcelona")
counting_district_2021_uni <- counting_district_2021_uni[,c(2,5)]
names(counting_district_2021_uni)[2] <- "freq_district"

preu_m2_2021 <- data.frame(test_2021_uni$any, test_2021_uni$district, test_2021_uni$property_id,test_2021_uni$mes, test_2021_uni$preu_m2)
names(preu_m2_2021)[c(1:5)] <- c("any", "district","property_id", "mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2021<- preu_m2_2021 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2021)[c(4:6)] <- c("q1_district", "q2_district", "q3_district")

test_2021_uni <- left_join(test_2021_uni, preu_m2_2021)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2021_uni <- left_join(test_2021_uni, counting_district_2021_uni)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

test_2021_uni <-test_2021_uni[!(test_2021_uni$NOMMUNI=="Barcelona" & test_2021_uni$district=="Undefined"),]
test_2021_uni <-test_2021_uni[!(test_2021_uni$NOMMUNI=="Barcelona" & test_2021_uni$district=="N/A"),]

##################

test_2022_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2022")
counting_district_2022_uni <- foto_oferta_series_pluri %>% 
  count(c("NOMMUNI", "district","any", "mes"))
counting_district_2022_uni <- counting_district_2022_uni %>% filter(NOMMUNI=="Barcelona")
counting_district_2022_uni <- counting_district_2022_uni[,c(2,5)]
names(counting_district_2022_uni)[2] <- "freq_district"

preu_m2_2022 <- data.frame(test_2022_uni$any, test_2022_uni$district, test_2022_uni$property_id,test_2022_uni$mes, test_2022_uni$preu_m2)
names(preu_m2_2022)[c(1:5)] <- c("any", "district","property_id", "mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2022<- preu_m2_2022 %>% 
  group_by(district, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2022)[c(4:6)] <- c("q1_district", "q2_district", "q3_district")

test_2022_uni <- left_join(test_2022_uni, preu_m2_2022)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2022_uni <- left_join(test_2022_uni, counting_district_2022_uni)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

test_2022_uni <-test_2022_uni[!(test_2022_uni$NOMMUNI=="Barcelona" & test_2022_uni$district=="Undefined"),]
test_2022_uni <-test_2022_uni[!(test_2022_uni$NOMMUNI=="Barcelona" & test_2022_uni$district=="N/A"),]

####################

quantiles_districtes_bcn_uni <- rbind(test_2019_uni, test_2020_uni, test_2021_uni,test_2022_uni)
quantiles_districtes_bcn_uni <- quantiles_districtes_bcn_uni[,c(1, 3:4, 10, 12:24, 26:30)]

##############################################################
################### Quantiles Municipis Barcelona Unifamiliar
##############################################################
##############################################################
################### Quantiles Municipis Barcelona Unifamiliar
##############################################################
##############################################################
################### Quantiles Municipis Barcelona Unifamiliar
##############################################################


test_2019_c_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(any=="2019")
counting_municipi_2019_c_uni <- foto_oferta_series_uni %>% 
  count(c("NOMMUNI","any", "mes"))
names(counting_municipi_2019_c_uni)[4] <- "freq_municipi"

preu_m2_2019_c_uni <- data.frame(test_2019_c_uni$any, test_2019_c_uni$NOMMUNI, test_2019_c_uni$property_id,test_2019_c_uni$mes, test_2019_c_uni$preu_m2)
names(preu_m2_2019_c_uni)[c(1:5)] <- c("any", "NOMMUNI","property_id","mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2019_c_uni<- preu_m2_2019_c_uni %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2019_c_uni)[c(4:6)] <- c("q1_municipi", "q2_municipi", "q3_municipi")

test_2019_c_uni <- left_join(test_2019_c_uni, preu_m2_2019_c_uni)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2019_c_uni <- left_join(test_2019_c_uni, counting_municipi_2019_c_uni)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

####################

test_2020_c_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(any=="2020")
counting_municipi_2020_c_uni <- foto_oferta_series_uni %>% 
  count(c("NOMMUNI","any", "mes"))
names(counting_municipi_2020_c_uni)[4] <- "freq_municipi"

preu_m2_2020_c_uni <- data.frame(test_2020_c_uni$any, test_2020_c_uni$NOMMUNI, test_2020_c_uni$property_id,test_2020_c_uni$mes, test_2020_c_uni$preu_m2)
names(preu_m2_2020_c_uni)[c(1:5)] <- c("any", "NOMMUNI","property_id","mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2020_c_uni<- preu_m2_2020_c_uni %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2020_c_uni)[c(4:6)] <- c("q1_municipi", "q2_municipi", "q3_municipi")

test_2020_c_uni <- left_join(test_2020_c_uni, preu_m2_2020_c_uni)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2020_c_uni <- left_join(test_2020_c_uni, counting_municipi_2020_c_uni)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

####################

test_2021_c_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(any=="2021")
counting_municipi_2021_c_uni <- foto_oferta_series_uni %>% 
  count(c("NOMMUNI","any", "mes"))
names(counting_municipi_2021_c_uni)[4] <- "freq_municipi"

preu_m2_2021_c_uni <- data.frame(test_2021_c_uni$any, test_2021_c_uni$NOMMUNI, test_2021_c_uni$property_id,test_2021_c_uni$mes, test_2021_c_uni$preu_m2)
names(preu_m2_2021_c_uni)[c(1:5)] <- c("any", "NOMMUNI","property_id","mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2021_c_uni<- preu_m2_2021_c_uni %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2021_c_uni)[c(4:6)] <- c("q1_municipi", "q2_municipi", "q3_municipi")

test_2021_c_uni <- left_join(test_2021_c_uni, preu_m2_2021_c_uni)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2021_c_uni <- left_join(test_2021_c_uni, counting_municipi_2021_c_uni)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

####################
test_2022_c_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(any=="2022")
counting_municipi_2022_c_uni <- foto_oferta_series_uni %>% 
  count(c("NOMMUNI","any", "mes"))
names(counting_municipi_2022_c_uni)[4] <- "freq_municipi"

preu_m2_2022_c_uni <- data.frame(test_2022_c_uni$any, test_2022_c_uni$NOMMUNI, test_2022_c_uni$property_id,test_2022_c_uni$mes, test_2022_c_uni$preu_m2)
names(preu_m2_2022_c_uni)[c(1:5)] <- c("any", "NOMMUNI","property_id","mes", "preu_m2")

q = c(.25, .5, .75)
p_names <- map_chr(q, ~paste0(.x*100, "%"))
p_funs <- map(q, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
p_funs

preu_m2_2022_c_uni<- preu_m2_2022_c_uni %>% 
  group_by(NOMMUNI, any, mes) %>% 
  summarize_at(vars(preu_m2), funs(!!!p_funs))

names(preu_m2_2022_c_uni)[c(4:6)] <- c("q1_municipi", "q2_municipi", "q3_municipi")

test_2022_c_uni <- left_join(test_2022_c_uni, preu_m2_2022_c_uni)%>%
  distinct(property_id, mes, .keep_all =TRUE)
test_2022_c_uni <- left_join(test_2022_c_uni, counting_municipi_2022_c_uni)%>%
  distinct(property_id, mes, district, .keep_all =TRUE)

####################
quantiles_catalunya_muni_uni <- rbind(test_2019_c_uni, test_2020_c_uni, test_2021_c_uni,test_2022_c_uni)
quantiles_catalunya_muni_uni <- quantiles_catalunya_muni_uni[,c(1, 3:4, 10, 12:24, 26:30)]


########################################
########################################
########################################
###### Interquartils
########################################
########################################
########################################

interquartil_bcn_districtes <- rbind(quantiles_districtes_bcn_pluri, quantiles_districtes_bcn_uni)
interquartil_bcn_districtes$rang <-interquartil_bcn_districtes$q3_district - interquartil_bcn_districtes$q1_district
interquartil_bcn_districtes$valors_inferiors <- interquartil_bcn_districtes$q1_district-(1.5*interquartil_bcn_districtes$rang)
interquartil_bcn_districtes$valors_superiors <- interquartil_bcn_districtes$q3_district+(1.5*interquartil_bcn_districtes$rang)

interquartil_bcn_districtes <- interquartil_bcn_districtes%>%
  mutate(Preu = case_when(
  preu_m2 <= valors_inferiors ~ "No Vàlid",
  preu_m2 >= valors_superiors ~ "No Vàlid",
  preu_m2 >= valors_inferiors & preu_m2 <= valors_superiors ~ "Vàlid",
  )
)
interquartil_bcn_districtes_foto <- interquartil_bcn_districtes


##################################################################
###################### Interquartil BCN Districtes Dies Vàlids
##################################################################
posting_date_dist <- as.Date(interquartil_bcn_districtes_foto$date_posting)
first_date_dist <- as.Date(interquartil_bcn_districtes_foto$mes_any_primera_data)

ud_dist <- data.frame(interquartil_bcn_districtes_foto$property_id, interquartil_bcn_districtes_foto$ultima_data)
names(ud_dist)[c(1,2)] <- c("property_id", "ultima_data")

ud_dist <- ud_dist%>%
  group_by(property_id)%>%
  filter(row_number()==n())


diff_dates_dist = data.frame(difftime(first_date_dist,posting_date_dist, units="days"))
names(diff_dates_dist)[1] <- "sum"
diff_dates_dist$sum  <- as.numeric(diff_dates_dist$sum)

dies_dist <- cbind(interquartil_bcn_districtes_foto$property_id, 
                   interquartil_bcn_districtes_foto$mes_any_primera_data, 
                   interquartil_bcn_districtes_foto$ultima_data,
                   diff_dates_dist)
names(dies_dist)[c(1:4)] <- c("property_id", "mes_any_primera_data", "ultima_data", "acumulat")

dies_2_dist <- dies_dist %>%
  group_by(property_id)%>%
  filter(row_number()==n())

dies_22_dist <- dies_2_dist %>%
  group_by(property_id, mes_any_primera_data)%>%
  summarise(ultima_data - mes_any_primera_data)

names(dies_22_dist)[1] <- "Differenca"
dies_22_dist$Differenca <- as.numeric(dies_22_dist$Differenca)
dies22_dist <- cbind(dies_2_dist, dies_22_dist)
names(dies22_dist)[4] <- "acumulat"
dies22_dist$sum <- dies22_dist$acumulat + dies22_dist$Differenca
dies22_dist <- dies22_dist[,-c(4,5)]
names(dies_dist)[4] <- "sum"


dies_dist <- dies_dist %>% 
  group_by(property_id) %>%
  filter(n() == 1 | row_number() < n())

143909121
2000473700002942720
2000356600003257856


dies_valids_dist <- rbind(dies_dist, dies22_dist)
dies_valids_dist <- unique( dies_valids_dist[ , c('property_id','mes_any_primera_data','ultima_data','sum') ] )

dies_valids_dist <- left_join(interquartil_bcn_districtes_foto, dies_valids_dist) %>%
  distinct(property_id, mes_any_primera_data, ultima_data, .keep_all =TRUE)
names(dies_valids_dist)[27] <- "Dies_Acum"

interquartil_bcn_districtes_foto <- dies_valids_dist%>%
  mutate(Dia = case_when(
    Dies_Acum <= 180 ~ "Vàlid",
    Dies_Acum > 180 ~ "No Vàlid"
  )
  )

############################

interquartil_cat_muni <- rbind(quantiles_catalunya_muni_pluri, quantiles_catalunya_muni_uni)

interquartil_cat_muni$rang <-interquartil_cat_muni$q3_municipi - interquartil_cat_muni$q1_municipi
interquartil_cat_muni$valors_inferiors <- interquartil_cat_muni$q1_municipi-(1.5*interquartil_cat_muni$rang)
interquartil_cat_muni$valors_superiors <- interquartil_cat_muni$q3_municipi+(1.5*interquartil_cat_muni$rang)

interquartil_cat_muni <- interquartil_cat_muni%>%
  mutate(Preu = case_when(
  preu_m2 <= valors_inferiors ~ "No valid",
  preu_m2 >= valors_superiors ~ "No valid",
  preu_m2 >= valors_inferiors & preu_m2 <= valors_superiors ~ "Valid",
  )
)

interquartil_cat_muni_foto <- interquartil_cat_muni

########################################################################################
###################### Interquartil Municipis Catalunya Dies Vàlids
########################################################################################

posting_date_muni <- as.Date(interquartil_cat_muni_foto$date_posting)
first_date_muni <- as.Date(interquartil_cat_muni_foto$mes_any_primera_data)

ud_muni <- data.frame(interquartil_cat_muni_foto$property_id, interquartil_cat_muni_foto$ultima_data)
names(ud_muni)[c(1,2)] <- c("property_id", "ultima_data")

ud_muni <- ud_muni%>%
  group_by(property_id)%>%
  filter(row_number()==n())


diff_dates_muni = data.frame(difftime(first_date_muni,posting_date_muni, units="days"))
names(diff_dates_muni)[1] <- "sum"
diff_dates_muni$sum  <- as.numeric(diff_dates_muni$sum)

dies_muni <- cbind(interquartil_cat_muni_foto$property_id, 
                   interquartil_cat_muni_foto$mes_any_primera_data, 
                   interquartil_cat_muni_foto$ultima_data,
                   diff_dates_muni)
names(dies_muni)[c(1:4)] <- c("property_id", "mes_any_primera_data", "ultima_data", "acumulat")

dies_2_muni <- dies_muni %>%
  group_by(property_id)%>%
  filter(row_number()==n())

dies_22_muni <- dies_2_muni %>%
  group_by(property_id, mes_any_primera_data)%>%
  summarise(ultima_data - mes_any_primera_data)

names(dies_22_muni)[1] <- "Differenca"
dies_22_muni$Differenca <- as.numeric(dies_22_muni$Differenca)
dies22_muni <- cbind(dies_2_muni, dies_22_muni)
names(dies22_muni)[4] <- "acumulat"
dies22_muni$sum <- dies22_muni$acumulat + dies22_muni$Differenca
dies22_muni <- dies22_muni[,-c(4,5)]
names(dies_muni)[4] <- "sum"


dies_muni <- dies_muni %>% 
  group_by(property_id) %>%
  filter(n() == 1 | row_number() < n())

143909121
2000473700002942720
2000356600003257856


dies_valids_muni <- rbind(dies_muni, dies22_muni)
dies_valids_muni <- unique( dies_valids_muni[ , c('property_id','mes_any_primera_data','ultima_data','sum') ] )

dies_valids_muni <- left_join(interquartil_cat_muni_foto, dies_valids_muni) %>%
  distinct(property_id, mes_any_primera_data, ultima_data, .keep_all =TRUE)
names(dies_valids_muni)[27] <- "Dies_Acum"

interquartil_cat_muni_foto <- dies_valids_muni%>%
  mutate(Dia = case_when(
    Dies_Acum <= 180 ~ "Vàlid",
    Dies_Acum > 180 ~ "No Vàlid"
  )
  )


library(writexl)
write_xlsx(interquartil_bcn_districtes_foto, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Fotocasa_interquartil_bcn_districtes.xlsx")
write_xlsx(interquartil_cat_muni_foto, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Fotocasa_interquartil_cat_muni.xlsx")

############################################
###################### Boxplots
############################################

boxplot(interquartil_cat_muni_foto$preu_m2,
        main = "Fotocasa | Mitjanes de Preus M2 (€) Catalunya",
        sub = "Catalunya Municipis 2019-2022 octubre",
        at = c(1),
        names = c("CAT"),
        las = 2,
        col = c("orange"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
        outline=FALSE
)

boxplot(interquartil_bcn_districtes_foto$preu_m2,
        main = "Fotocasa | Mitjanaes de Preus M2 (€) Barcelona",
        sub = "Barcelona Districtes 2019-2022 octubre",
        at = c(1),
        names = c("BCN"),
        las = 2,
        col = c("steelblue"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
        outline=FALSE
)
