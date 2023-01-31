library(dplyr)
library(xlsx)
library(readr)
library(plyr)
library(lubridate)
library(tidyverse)
options(scipen = 999)
options(digits=3)

#habit_oferta_2022 <- read_csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Habitaclia_Oferta/tractament_habit_oferta_2022.csv")
#habit_oferta_2021 <- read_csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Habitaclia_Oferta/tractament_habit_oferta_2021.csv")
#habit_oferta_2020 <-read_csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_Oferta/tractament_habit_oferta_2020.csv")
#habit_oferta_2019 <- read_csv("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Habitaclia_Oferta/tractament_habit_oferta_2019.csv")

habit_oferta_series <- rbind(habit_oferta_2019, habit_oferta_2020, habit_oferta_2021, habit_oferta_2022)
habit_oferta_series$district[which(habit_oferta_series$district=="Horta - Guinardó")] <- "Horta-Guinardò"
habit_oferta_series$source <- "habitaclia"


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

habit_oferta_series_pluri <- habit_oferta_series%>%filter(tipologia=="Plurifamiliar")
habit_oferta_series_pluri$mes_any_primera_data...27 <- NULL 
names(habit_oferta_series_pluri)[7] <- "mes_any_primera_data"

################################################################
################### Quantiles Districtes Barcelona Plurifamiliar
################################################################
################################################################
################### Quantiles Districtes Barcelona Plurifamiliar
################################################################
################################################################
################### Quantiles Districtes Barcelona Plurifamiliar
################################################################

quantiles_catalunya <- habit_oferta_series_pluri
quantiles_catalunya2 <- data.frame(quantiles_catalunya)

test_2019 <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2019")
counting_district_2019 <- habit_oferta_series_pluri %>% 
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

#test_2019 <-test_2019[!(test_2019$NOMMUNI=="Barcelona" & test_2019$district=="Undefined"),]
#test_2019 <-test_2019[!(test_2019$NOMMUNI=="Barcelona" & test_2019$district=="N/A"),]

##################

test_2020 <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2020")
counting_district_2020 <- habit_oferta_series_pluri %>% 
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

#test_2020 <-test_2020[!(test_2020$NOMMUNI=="Barcelona" & test_2020$district=="Undefined"),]
#test_2020 <-test_2020[!(test_2020$NOMMUNI=="Barcelona" & test_2020$district=="N/A"),]

##################

test_2021 <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2021")
counting_district_2021 <- habit_oferta_series_pluri %>% 
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

#test_2021 <-test_2021[!(test_2021$NOMMUNI=="Barcelona" & test_2021$district=="Undefined"),]
#test_2021 <-test_2021[!(test_2021$NOMMUNI=="Barcelona" & test_2021$district=="N/A"),]

##################

test_2022 <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2022")
counting_district_2022 <- habit_oferta_series_pluri %>% 
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

#test_2022 <-test_2022[!(test_2022$NOMMUNI=="Barcelona" & test_2022$district=="Undefined"),]
#test_2022 <-test_2022[!(test_2022$NOMMUNI=="Barcelona" & test_2022$district=="N/A"),]

####################

quantiles_districtes_bcn_pluri <- rbind(test_2019, test_2020, test_2021,test_2022)


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
counting_municipi_2019_c <- habit_oferta_series_pluri %>% 
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
counting_municipi_2020_c <- habit_oferta_series_pluri %>% 
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
counting_municipi_2021_c <- habit_oferta_series_pluri %>% 
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
counting_municipi_2022_c <- habit_oferta_series_pluri %>% 
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

##################################################
################### Districtes BCN Plurifamiliar
##################################################
##################################################
################### Districtes BCN Plurifamiliar
##################################################
##################################################
################### Districtes BCN Plurifamiliar
##################################################

habit_oferta_series_uni <- habit_oferta_series%>%filter(tipologia=="Unifamiliar")
habit_oferta_series_uni$mes_any_primera_data...27 <- NULL 
names(habit_oferta_series_uni)[7] <- "mes_any_primera_data"

##############################################################
################### Quantiles Districtes Barcelona Unifamiliar
##############################################################
##############################################################
################### Quantiles Districtes Barcelona Unifamiliar
##############################################################
##############################################################
################### Quantiles Districtes Barcelona Unifamiliar
##############################################################

quantiles_catalunya <- habit_oferta_series_uni
quantiles_catalunya2 <- data.frame(quantiles_catalunya)

test_2019_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2019")
counting_district_2019_uni <- habit_oferta_series_uni %>% 
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

#test_2019_uni <-test_2019_uni[!(test_2019_uni$NOMMUNI=="Barcelona" & test_2019_uni$district=="Undefined"),]
#test_2019_uni <-test_2019_uni[!(test_2019_uni$NOMMUNI=="Barcelona" & test_2019_uni$district=="N/A"),]

##################

test_2020_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2020")
counting_district_2020_uni <- habit_oferta_series_uni %>% 
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

#test_2020_uni <-test_2020_uni[!(test_2020_uni$NOMMUNI=="Barcelona" & test_2020_uni$district=="Undefined"),]
#test_2020_uni <-test_2020_uni[!(test_2020_uni$NOMMUNI=="Barcelona" & test_2020_uni$district=="N/A"),]

##################

test_2021_uni <- quantiles_catalunya2 %>% group_by(any) %>% filter(NOMMUNI=="Barcelona", any=="2021")
counting_district_2021_uni <- habit_oferta_series_uni %>% 
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
counting_district_2022_uni <- habit_oferta_series_uni %>% 
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

#test_2022_uni <-test_2022_uni[!(test_2022_uni$NOMMUNI=="Barcelona" & test_2022_uni$district=="Undefined"),]
#test_2022_uni <-test_2022_uni[!(test_2022_uni$NOMMUNI=="Barcelona" & test_2022_uni$district=="N/A"),]

####################

quantiles_districtes_bcn_uni <- rbind(test_2019_uni, test_2020_uni, test_2021_uni,test_2022_uni)

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
counting_municipi_2019_c_uni <- habit_oferta_series_uni %>% 
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
counting_municipi_2020_c_uni <- habit_oferta_series_uni %>% 
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
counting_municipi_2021_c_uni <- habit_oferta_series_uni %>% 
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
counting_municipi_2022_c_uni <- habit_oferta_series_uni %>% 
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

########################################
########################################
########################################
###### Interquartils
########################################
########################################
########################################

####*Districtes*####

interquartil_bcn_districtes <- rbind(quantiles_districtes_bcn_pluri, quantiles_districtes_bcn_uni)


#Making date_posting uniform values
library(collapse)


#2000050000003623940
#2000050000003544380


ibddp1 <- data.frame(interquartil_bcn_districtes$property_id, 
                     interquartil_bcn_districtes$date_posting, 
                     interquartil_bcn_districtes$dia_public_nas,
                     interquartil_bcn_districtes$municipality)
ibddp1 <- ibddp1%>%
  group_by(interquartil_bcn_districtes.property_id, interquartil_bcn_districtes.municipality)%>%
  filter(row_number()==1)

names(ibddp1)[1:4] <- c("property_id","date_posting", "dia_public_nas", "municipality")
ibddp2 <- interquartil_bcn_districtes
ibddp2$date_posting <- NULL
ibddp2$dia_public_nas <- NULL

ibddp3 <- merge(x=ibddp2, y=ibddp1, by.x=c("property_id","municipality"), 
                by.y=c("property_id","municipality"))
ibddp3 <- ibddp3[order(ibddp3$mes_any_primera_data),]
ibddp3 <- ibddp3[,c(1:2, 32, 3:16, 33, 17:31)]

#2000050000003544380

#Reinserting the last date column because some property_ids are available from one year to another
test11b <- ibddp3[order(ibddp3$ultima_data),]
test11b <- test11b[,c(1,2,5,20)]

test11b <- test11b %>%
  group_by(municipality, district, property_id) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = ultima_data)

ibdud <- ibddp3
ibdud$ultima_data <- NULL

ibdud <- merge(x=ibdud, y=test11b, by.x=c("property_id","municipality"), 
                 by.y=c("property_id","municipality"))

ibdud$district.y <- NULL
names(ibdud)[5] <- "district"

ibdud <- ibdud[order(ibdud$mes_any_primera_data),]
ibdud$date <- ibdud$mes_any_primera_data
ibdud <- ibdud[,c(34, 1, 2, 5, 6, 3, 18, 15, 17, 22, 19, 33, 20:21, 7, 8:14, 16, 27, 23:26, 29:32, 28)]

############################
interquartil_bcn_districtes <- ibdud

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

interquartil_bcn_districtes_habit <- interquartil_bcn_districtes
interquartil_bcn_districtes_habit <- interquartil_bcn_districtes_habit[order(interquartil_bcn_districtes_habit$date),]

############################

####*Municipis*####

interquartil_cat_muni <- rbind(quantiles_catalunya_muni_pluri, quantiles_catalunya_muni_uni)

#Making date_posting uniform values
library(collapse)

imcdp1 <- data.frame(interquartil_cat_muni$property_id, 
                     interquartil_cat_muni$date_posting, 
                     interquartil_cat_muni$dia_public_nas,
                     interquartil_cat_muni$municipality)
imcdp1 <- imcdp1%>%
  group_by(interquartil_cat_muni.property_id, interquartil_cat_muni.municipality)%>%
  filter(row_number()==1)

names(imcdp1)[1:4] <- c("property_id","date_posting", "dia_public_nas", "municipality")
imcdp2 <- interquartil_cat_muni
imcdp2$date_posting <- NULL
imcdp2$dia_public_nas <- NULL

imcdp3 <- merge(x=imcdp2, y=imcdp1, by.x=c("property_id","municipality"), 
               by.y=c("property_id","municipality"))
imcdp3 <- imcdp3[order(imcdp3$mes_any_primera_data),]
imcdp3 <- imcdp3[,c(1:2, 32, 3:16, 33, 17:31)]


#Reinserting the last date column because some property_ids are available from one year to another
test11c <- imcdp3[order(imcdp3$ultima_data),]
test11c <- test11c[,c(1,2,20)]

test11c <- test11c %>%
  group_by(municipality, property_id) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = ultima_data)

icmud <- imcdp3
icmud$ultima_data <- NULL

icmud <- merge(x=icmud, y=test11c, by.x=c("property_id","municipality"), 
               by.y=c("property_id","municipality"))


icmud <- icmud[order(ibdud$mes_any_primera_data),]
icmud$date <- icmud$mes_any_primera_data
icmud <- icmud[,c(34, 1, 2, 5, 6, 3, 18, 15, 17, 22, 19, 33, 20:21, 7, 8:14, 16, 27, 23:26, 29:32, 28)]

############################

interquartil_cat_muni <- icmud

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

interquartil_cat_muni_habit <- interquartil_cat_muni
interquartil_cat_muni_habit <- interquartil_cat_muni_habit[order(interquartil_cat_muni_habit$date),]

##2000008600003757484

##################################################################
###################### Interquartil BCN Districtes Dies Vàlids
##################################################################
posting_date_dist <- as.Date(interquartil_bcn_districtes_habit$date_posting)
last_date_dist <- as.Date(interquartil_bcn_districtes_habit$data_final)

ud_dist <- data.frame(interquartil_bcn_districtes_habit$property_id, interquartil_bcn_districtes_habit$data_final)
names(ud_dist)[c(1,2)] <- c("property_id", "data_final")

diff_dates_dist = data.frame(difftime(last_date_dist,posting_date_dist, units="days"))
names(diff_dates_dist)[1] <- "sum"
diff_dates_dist$sum  <- as.numeric(diff_dates_dist$sum)

dies_dist <- cbind(interquartil_bcn_districtes_habit$property_id, 
                   interquartil_bcn_districtes_habit$date_posting, 
                   interquartil_bcn_districtes_habit$data_final,
                   diff_dates_dist)
names(dies_dist)[c(1:4)] <- c("property_id", "date_posting", "data_final", "dies_acumulats")

#property_ids Barcelona Districtes
#2000398100003116496
#2000473700002758196
#2000473700003025842
#2000007000002098084
#2004245800000000003
#2000006300003845054

dies_valids_dist <- merge(interquartil_bcn_districtes_habit, dies_dist) %>%
  distinct(property_id, date_posting, data_final, .keep_all =TRUE)

interquartil_bcn_districtes_habit <- dies_valids_dist%>%
  mutate(Dia = case_when(
    dies_acumulats <= 180 ~ "Vàlid",
    dies_acumulats > 180 ~ "No Vàlid"
  )
)

interquartil_bcn_districtes_habit <- interquartil_bcn_districtes_habit[,c(4, 1, 2, 5:14, 3, 15:39)]

####Adjusting the column "primera_data"

ibdpd <- interquartil_bcn_districtes_habit%>%
  group_by(property_id, municipality)%>%
  filter(row_number()==1)

ibdpd <- ibdpd[,c(2,11)]

ibdpd <- merge(ibdpd,interquartil_bcn_districtes_habit, by ="property_id")
ibdpd$primera_data.y <- NULL
names(ibdpd)[2] <- "primera_data" 
interquartil_bcn_districtes_habit <- ibdpd
names(interquartil_bcn_districtes_habit)[37:39] <- c("preu_valid","dies_duracio", "dia_valid")


#2000053800003233485
#2000053800003321353
#2000050000003970413
#2000007000002098084
#2004245800000000003
##2001307400000012288

########################################################################################
###################### Interquartil Municipis Catalunya Dies Vàlids
########################################################################################

posting_date_muni <- as.Date(interquartil_cat_muni_habit$date_posting)
last_date_muni <- as.Date(interquartil_cat_muni_habit$data_final)

ud_muni <- data.frame(interquartil_cat_muni_habit$property_id, interquartil_cat_muni_habit$data_final)
names(ud_muni)[c(1,2)] <- c("property_id", "data_final")

diff_dates_muni = data.frame(difftime(last_date_muni,posting_date_muni, units="days"))
names(diff_dates_muni)[1] <- "sum"
diff_dates_muni$sum  <- as.numeric(diff_dates_muni$sum)

dies_muni <- cbind(interquartil_cat_muni_habit$property_id, 
                   interquartil_cat_muni_habit$date_posting, 
                   interquartil_cat_muni_habit$data_final,
                   diff_dates_muni)
names(dies_muni)[c(1:4)] <- c("property_id", "date_posting", "data_final", "dies_acumulats")

#2000007000002098084
#2004245800000000003
##2001307400000012288

dies_valids_muni <- merge(interquartil_cat_muni_habit, dies_muni) %>%
  distinct(property_id, date_posting, data_final, .keep_all =TRUE)

interquartil_cat_muni_habit <- dies_valids_muni%>%
  mutate(Dia = case_when(
    dies_acumulats <= 180 ~ "Vàlid",
    dies_acumulats > 180 ~ "No Vàlid"
  )
)


interquartil_cat_muni_habit <- interquartil_cat_muni_habit[,c(4, 1, 2, 5:14, 3, 15:39)]

####Adjusting the column "primera_data"

icmpd <- interquartil_cat_muni_habit%>%
  group_by(property_id, municipality)%>%
  filter(row_number()==1)

icmpd <- icmpd[,c(2,11)]

icmpd <- merge(icmpd,interquartil_cat_muni_habit, by ="property_id")
icmpd$primera_data.y <- NULL
names(icmpd)[2] <- "primera_data" 
interquartil_cat_muni_habit <- icmpd
names(interquartil_cat_muni_habit)[37:39] <- c("preu_valid","dies_duracio", "dia_valid")

#property_ids Catalunya Municipis
#2000001100000514119
#2000053800003233485
#2000053800003321353
#2000058600000734280
#2000050000003970413

write_csv2(interquartil_bcn_districtes_habit, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/habitaclia_interquartil_bcn_districtes.csv")
write_csv2(interquartil_cat_muni_habit, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/habitaclia_interquartil_cat_muni.csv")


############################################
###################### Boxplots
############################################
boxplot(interquartil_cat_muni_habit$preu_m2,
        main = "habitaclia | Mitjanes de Preus M2 Euros Catalunya",
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

boxplot(interquartil_bcn_districtes_habit$preu_m2,
        main = "habitaclia | Mitjanes de Preus M2 Euros Barcelona",
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


######################

