library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(arrow)
library(plyr)
library(arrow)
library(lubridate)
library(data.table)
library(bit64)

options(scipen=999)
options(digits=2)
#######################################################################
###################### Interquartils i Preus Valids CAT 2019
#######################################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_cat_habit <- test_2019

interquartil_cat_habit$rang <-interquartil_cat_habit$q3_muni-interquartil_cat_habit$q1_muni
interquartil_cat_habit$valors_inferiors <- interquartil_cat_habit$q1_muni-(1.5*interquartil_cat_habit$rang)
interquartil_cat_habit$valors_superiors <- interquartil_cat_habit$q3_muni+(1.5*interquartil_cat_habit$rang)

interquartil_cat_habit <- interquartil_cat_habit%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

interquartil_cat_habit <- interquartil_cat_habit[order(interquartil_cat_habit$date),]

count(interquartil_cat_habit, "preu_valid")

colnames(interquartil_cat_habit)
#######################################################################
###################### Dates CAT 2019
#######################################################################

#Adding first date column
habit_oferta_series_fd <- interquartil_cat_habit[,c(1:4, 19)]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
habit_oferta_series_fd <- habit_oferta_series_fd[,c(1,4:5)]

interquartil_cat_habit$primera_data <- NULL


interquartil_cat_habit <- merge(x=interquartil_cat_habit, y=habit_oferta_series_fd, by.x=c("property_id","municipality"), 
                                by.y=c("property_id","municipality"))


#Adding last date column
colnames(interquartil_cat_habit)
habit_oferta_series_ld <- interquartil_cat_habit[,c(1:4,19)]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
habit_oferta_series_ld <- habit_oferta_series_ld[,c(1,2,6)]

interquartil_cat_habit$ultlima_data <- NULL

interquartil_cat_habit <- merge(x=interquartil_cat_habit, y=habit_oferta_series_ld, by.x=c("property_id","municipality"), 
                                by.y=c("property_id","municipality"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_cat_habit
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]

colnames(habit_oferta_series_dp)

habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,12)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1,2,6)]

interquartil_cat_habit$date_posting_calcul <- NULL

interquartil_cat_habit <- merge(x=interquartil_cat_habit, y=habit_oferta_series_dp, by.x=c("property_id","municipality"), 
                                by.y=c("property_id","municipality"))


interquartil_cat_habit <- interquartil_cat_habit%>%
  group_by(property_id, municipality)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_cat_habit <- interquartil_cat_habit%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_cat_habit$date_posting_calcul))

#2000001100000514119
#2000058600003824088
##2000008600003757484

#######################################################################
###################### Interquartils i Dies Valids CAT 2019
#######################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_cat_habit$data_final, interquartil_cat_habit$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_cat_habit, date_test_cm_calcul)  

names(date_test_cm_calcul)[35] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_cat_habit <- date_test_cm_calcul
min(interquartil_cat_habit$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_cat_habit <- interquartil_cat_habit%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_cat_habit <- interquartil_cat_habit%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")

interquartil_cat_habit$price <- NULL
interquartil_cat_habit$surface <- NULL

interquartil_cat_habit <- interquartil_cat_habit%>%
  group_by(property_id, municipality, mes)%>%
  filter(row_number()==1)

#######################################################################
###################### Interquartils i Preus Valids CAT 2020
#######################################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_cat_habit2 <- test_2020

interquartil_cat_habit2$rang <-interquartil_cat_habit2$q3_muni-interquartil_cat_habit2$q1_muni
interquartil_cat_habit2$valors_inferiors <- interquartil_cat_habit2$q1_muni-(1.5*interquartil_cat_habit2$rang)
interquartil_cat_habit2$valors_superiors <- interquartil_cat_habit2$q3_muni+(1.5*interquartil_cat_habit2$rang)

interquartil_cat_habit2 <- interquartil_cat_habit2%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

interquartil_cat_habit2 <- interquartil_cat_habit2[order(interquartil_cat_habit2$date),]

count(interquartil_cat_habit2, "preu_valid")

colnames(interquartil_cat_habit2)
#######################################################################
###################### Dates CAT 2020
#######################################################################


#Adding first date column
habit_oferta_series_fd <- interquartil_cat_habit2[,c(1:4, 19)]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
habit_oferta_series_fd <- habit_oferta_series_fd[,c(1,4:5)]

interquartil_cat_habit2$primera_data <- NULL


interquartil_cat_habit2 <- merge(x=interquartil_cat_habit2, y=habit_oferta_series_fd, by.x=c("property_id","municipality"), 
                                 by.y=c("property_id","municipality"))


#Adding last date column
colnames(interquartil_cat_habit2)
habit_oferta_series_ld <- interquartil_cat_habit2[,c(1:4,19)]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
habit_oferta_series_ld <- habit_oferta_series_ld[,c(1,2,6)]

interquartil_cat_habit2$ultlima_data <- NULL

interquartil_cat_habit2 <- merge(x=interquartil_cat_habit2, y=habit_oferta_series_ld, by.x=c("property_id","municipality"), 
                                 by.y=c("property_id","municipality"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_cat_habit2
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]

colnames(habit_oferta_series_dp)

habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,12)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1,2,6)]

interquartil_cat_habit2$date_posting_calcul <- NULL

interquartil_cat_habit2 <- merge(x=interquartil_cat_habit2, y=habit_oferta_series_dp, by.x=c("property_id","municipality"), 
                                 by.y=c("property_id","municipality"))


interquartil_cat_habit2 <- interquartil_cat_habit2%>%
  group_by(property_id, municipality)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_cat_habit2 <- interquartil_cat_habit2%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_cat_habit2$date_posting_calcul))

#2000001100000514119
#2000058600003824088
##2000008600003757484

#######################################################################
###################### Interquartils i Dies Valids CAT 2020
#######################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_cat_habit2$data_final, interquartil_cat_habit2$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_cat_habit2, date_test_cm_calcul)  

names(date_test_cm_calcul)[35] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_cat_habit2 <- date_test_cm_calcul
min(interquartil_cat_habit2$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_cat_habit2 <- interquartil_cat_habit2%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_cat_habit2 <- interquartil_cat_habit2%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")

interquartil_cat_habit2$price <- NULL
interquartil_cat_habit2$surface <- NULL

interquartil_cat_habit2 <- interquartil_cat_habit2%>%
  group_by(property_id, municipality, mes)%>%
  filter(row_number()==1)

#######################################################################
###################### Interquartils i Preus Valids CAT 2021
#######################################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_cat_habit3 <- test_2021
colnames(interquartil_cat_habit3)

interquartil_cat_habit3$rang <-interquartil_cat_habit3$q3_muni-interquartil_cat_habit3$q1_muni
interquartil_cat_habit3$valors_inferiors <- interquartil_cat_habit3$q1_muni-(1.5*interquartil_cat_habit3$rang)
interquartil_cat_habit3$valors_superiors <- interquartil_cat_habit3$q3_muni+(1.5*interquartil_cat_habit3$rang)

interquartil_cat_habit3 <- interquartil_cat_habit3%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

interquartil_cat_habit3 <- interquartil_cat_habit3[order(interquartil_cat_habit3$date),]

count(interquartil_cat_habit3, "preu_valid")

#######################################################################
###################### Dates CAT 2021
#######################################################################


#Adding first date column
colnames(interquartil_cat_habit3)
habit_oferta_series_fd <- interquartil_cat_habit3[,c(1:4, 19)]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
habit_oferta_series_fd <- habit_oferta_series_fd[,c(1,4:5)]

interquartil_cat_habit3$primera_data <- NULL


interquartil_cat_habit3 <- merge(x=interquartil_cat_habit3, y=habit_oferta_series_fd, by.x=c("property_id","municipality"), 
                                 by.y=c("property_id","municipality"))


#Adding last date column
colnames(interquartil_cat_habit3)
habit_oferta_series_ld <- interquartil_cat_habit3[,c(1:4,19)]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
habit_oferta_series_ld <- habit_oferta_series_ld[,c(1,2,6)]

interquartil_cat_habit3$ultlima_data <- NULL

interquartil_cat_habit3 <- merge(x=interquartil_cat_habit3, y=habit_oferta_series_ld, by.x=c("property_id","municipality"), 
                                 by.y=c("property_id","municipality"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_cat_habit3
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]

colnames(habit_oferta_series_dp)

habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,12)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1,2,6)]

interquartil_cat_habit3$date_posting_calcul <- NULL

interquartil_cat_habit3 <- merge(x=interquartil_cat_habit3, y=habit_oferta_series_dp, by.x=c("property_id","municipality"), 
                                 by.y=c("property_id","municipality"))


interquartil_cat_habit3 <- interquartil_cat_habit3%>%
  group_by(property_id, municipality)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_cat_habit3 <- interquartil_cat_habit3%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_cat_habit3$date_posting_calcul))

#2000001100000514119
#2000058600003824088
##2000008600003757484

#######################################################################
###################### Interquartils i Dies Valids CAT 2021
#######################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_cat_habit3$data_final, interquartil_cat_habit3$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_cat_habit3, date_test_cm_calcul)  

names(date_test_cm_calcul)[35] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_cat_habit3 <- date_test_cm_calcul
min(interquartil_cat_habit3$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_cat_habit3 <- interquartil_cat_habit3%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_cat_habit3 <- interquartil_cat_habit3%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")

interquartil_cat_habit3$price <- NULL
interquartil_cat_habit3$surface <- NULL

interquartil_cat_habit3 <- interquartil_cat_habit3%>%
  group_by(property_id, municipality, mes)%>%
  filter(row_number()==1)

#######################################################################
###################### Interquartils i Preus Valids CAT 2022
#######################################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_cat_habit4 <- test_2022

interquartil_cat_habit4$rang <-interquartil_cat_habit4$q3_muni-interquartil_cat_habit4$q1_muni
interquartil_cat_habit4$valors_inferiors <- interquartil_cat_habit4$q1_muni-(1.5*interquartil_cat_habit4$rang)
interquartil_cat_habit4$valors_superiors <- interquartil_cat_habit4$q3_muni+(1.5*interquartil_cat_habit4$rang)

interquartil_cat_habit4 <- interquartil_cat_habit4%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

interquartil_cat_habit4 <- interquartil_cat_habit4[order(interquartil_cat_habit4$date),]

count(interquartil_cat_habit4, "preu_valid")

colnames(interquartil_cat_habit4)
#######################################################################
###################### Dates CAT 2022
#######################################################################


#Adding first date column
habit_oferta_series_fd <- interquartil_cat_habit4[,c(1:4, 19)]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
habit_oferta_series_fd <- habit_oferta_series_fd[,c(1,4:5)]

interquartil_cat_habit4$primera_data <- NULL


interquartil_cat_habit4 <- merge(x=interquartil_cat_habit4, y=habit_oferta_series_fd, by.x=c("property_id","municipality"), 
                                 by.y=c("property_id","municipality"))


#Adding last date column
colnames(interquartil_cat_habit4)
habit_oferta_series_ld <- interquartil_cat_habit4[,c(1:4,19)]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
habit_oferta_series_ld <- habit_oferta_series_ld[,c(1,2,6)]

interquartil_cat_habit4$ultlima_data <- NULL

interquartil_cat_habit4 <- merge(x=interquartil_cat_habit4, y=habit_oferta_series_ld, by.x=c("property_id","municipality"), 
                                 by.y=c("property_id","municipality"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_cat_habit4
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]

colnames(habit_oferta_series_dp)

habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,12)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1,2,6)]

interquartil_cat_habit4$date_posting_calcul <- NULL

interquartil_cat_habit4 <- merge(x=interquartil_cat_habit4, y=habit_oferta_series_dp, by.x=c("property_id","municipality"), 
                                 by.y=c("property_id","municipality"))


interquartil_cat_habit4 <- interquartil_cat_habit4%>%
  group_by(property_id, municipality)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_cat_habit4 <- interquartil_cat_habit4%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_cat_habit4$date_posting_calcul))


#2000001100000514119
#2000058600003824088
##2000008600003757484

#######################################################################
###################### Interquartils i Dies Valids CAT 2022
#######################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_cat_habit4$data_final, interquartil_cat_habit4$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_cat_habit4, date_test_cm_calcul)  

names(date_test_cm_calcul)[35] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_cat_habit4 <- date_test_cm_calcul
min(interquartil_cat_habit4$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_cat_habit4 <- interquartil_cat_habit4%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_cat_habit4 <- interquartil_cat_habit4%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")

interquartil_cat_habit4$price <- NULL
interquartil_cat_habit4$surface <- NULL

interquartil_cat_habit4 <- interquartil_cat_habit4%>%
  group_by(property_id, municipality, mes)%>%
  filter(row_number()==1)

#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

#######################################################################
###################### Interquartils i Preus Valids BCN 2019
#######################################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_bcn_habit <- test_b2019
colnames(interquartil_bcn_habit)
interquartil_bcn_habit$rang <-interquartil_bcn_habit$q3_district-interquartil_bcn_habit$q1_district
interquartil_bcn_habit$valors_inferiors <- interquartil_bcn_habit$q1_district-(1.5*interquartil_bcn_habit$rang)
interquartil_bcn_habit$valors_superiors <- interquartil_bcn_habit$q3_district+(1.5*interquartil_bcn_habit$rang)

interquartil_bcn_habit <- interquartil_bcn_habit%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

interquartil_bcn_habit <- interquartil_bcn_habit[order(interquartil_bcn_habit$date),]

count(interquartil_bcn_habit, "preu_valid")

colnames(interquartil_bcn_habit)
#######################################################################
###################### Dates BCN 2019
#######################################################################

habit_oferta_series_fd <- interquartil_bcn_habit[,c(1:4, 19)]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
habit_oferta_series_fd <- habit_oferta_series_fd[,c(1,4:5)]

interquartil_bcn_habit$primera_data <- NULL


interquartil_bcn_habit <- merge(x=interquartil_bcn_habit, y=habit_oferta_series_fd, by.x=c("property_id","district"), 
                                by.y=c("property_id","district"))


#Adding last date column
colnames(interquartil_bcn_habit)
habit_oferta_series_ld <- interquartil_bcn_habit[,c(1:4,19)]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
habit_oferta_series_ld <- habit_oferta_series_ld[,c(1,2,6)]

interquartil_bcn_habit$ultlima_data <- NULL

interquartil_bcn_habit <- merge(x=interquartil_bcn_habit, y=habit_oferta_series_ld, by.x=c("property_id","district"), 
                                by.y=c("property_id","district"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_bcn_habit
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]
colnames(habit_oferta_series_dp)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,12)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1,2,6)]

interquartil_bcn_habit$date_posting_calcul <- NULL

interquartil_bcn_habit <- merge(x=interquartil_bcn_habit, y=habit_oferta_series_dp, by.x=c("property_id","district"), 
                                by.y=c("property_id","district"))


interquartil_bcn_habit <- interquartil_bcn_habit%>%
  group_by(property_id, district)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_bcn_habit <- interquartil_bcn_habit%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_bcn_habit$date_posting_calcul))

#2000001100000514119
#2000058600003824088
##2000008600003757484

#######################################################################
###################### Interquartils i Dies Valids BCN 2019
#######################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_bcn_habit$data_final, interquartil_bcn_habit$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_bcn_habit, date_test_cm_calcul)  

names(date_test_cm_calcul)[35] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_bcn_habit <- date_test_cm_calcul
min(interquartil_bcn_habit$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_bcn_habit <- interquartil_bcn_habit%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_bcn_habit <- interquartil_bcn_habit%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")

interquartil_bcn_habit$price <- NULL
interquartil_bcn_habit$surface <- NULL

interquartil_bcn_habit <- interquartil_bcn_habit%>%
  group_by(property_id, district, mes)%>%
  filter(row_number()==1)

#######################################################################
###################### Interquartils i Preus Valids BCN 2020
#######################################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_bcn_habit_2 <- test_b2020

interquartil_bcn_habit_2$rang <-interquartil_bcn_habit_2$q3_district-interquartil_bcn_habit_2$q1_district
interquartil_bcn_habit_2$valors_inferiors <- interquartil_bcn_habit_2$q1_district-(1.5*interquartil_bcn_habit_2$rang)
interquartil_bcn_habit_2$valors_superiors <- interquartil_bcn_habit_2$q3_district+(1.5*interquartil_bcn_habit_2$rang)

interquartil_bcn_habit_2 <- interquartil_bcn_habit_2%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

interquartil_bcn_habit_2 <- interquartil_bcn_habit_2[order(interquartil_bcn_habit_2$date),]

count(interquartil_bcn_habit_2, "preu_valid")

colnames(interquartil_bcn_habit_2)
#######################################################################
###################### Dates BCN 2020
#######################################################################


#Adding first date column

habit_oferta_series_fd <- interquartil_bcn_habit_2[,c(1:4, 19)]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
habit_oferta_series_fd <- habit_oferta_series_fd[,c(1,4:5)]

interquartil_bcn_habit_2$primera_data <- NULL


interquartil_bcn_habit_2 <- merge(x=interquartil_bcn_habit_2, y=habit_oferta_series_fd, by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


#Adding last date column
colnames(interquartil_bcn_habit_2)
habit_oferta_series_ld <- interquartil_bcn_habit_2[,c(1:4,19)]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
habit_oferta_series_ld <- habit_oferta_series_ld[,c(1,2,6)]

interquartil_bcn_habit_2$ultlima_data <- NULL

interquartil_bcn_habit_2 <- merge(x=interquartil_bcn_habit_2, y=habit_oferta_series_ld, by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_bcn_habit_2
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]
colnames(habit_oferta_series_dp)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,12)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1,2,6)]

interquartil_bcn_habit_2$date_posting_calcul <- NULL

interquartil_bcn_habit_2 <- merge(x=interquartil_bcn_habit_2, y=habit_oferta_series_dp, by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


interquartil_bcn_habit_2 <- interquartil_bcn_habit_2%>%
  group_by(property_id, district)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_bcn_habit_2 <- interquartil_bcn_habit_2%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_bcn_habit_2$date_posting_calcul))

#2000001100000514119
#2000058600003824088
##2000008600003757484

#######################################################################
###################### Interquartils i Dies Valids BCN 2020
#######################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_bcn_habit_2$data_final, interquartil_bcn_habit_2$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_bcn_habit_2, date_test_cm_calcul)  

names(date_test_cm_calcul)[35] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_bcn_habit_2 <- date_test_cm_calcul
min(interquartil_bcn_habit_2$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_bcn_habit_2 <- interquartil_bcn_habit_2%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_bcn_habit_2 <- interquartil_bcn_habit_2%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")

interquartil_bcn_habit_2$price <- NULL
interquartil_bcn_habit_2$surface <- NULL

interquartil_bcn_habit_2 <- interquartil_bcn_habit_2%>%
  group_by(property_id, district, mes)%>%
  filter(row_number()==1)

#######################################################################
###################### Interquartils i Preus Valids BCN 2021
#######################################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_bcn_habit_3 <- test_b2021

interquartil_bcn_habit_3$rang <-interquartil_bcn_habit_3$q3_district-interquartil_bcn_habit_3$q1_district
interquartil_bcn_habit_3$valors_inferiors <- interquartil_bcn_habit_3$q1_district-(1.5*interquartil_bcn_habit_3$rang)
interquartil_bcn_habit_3$valors_superiors <- interquartil_bcn_habit_3$q3_district+(1.5*interquartil_bcn_habit_3$rang)

interquartil_bcn_habit_3 <- interquartil_bcn_habit_3%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

interquartil_bcn_habit_3 <- interquartil_bcn_habit_3[order(interquartil_bcn_habit_3$date),]

count(interquartil_bcn_habit_3, "preu_valid")

colnames(interquartil_bcn_habit_3)
#######################################################################
###################### Dates BCN 2021
#######################################################################


#Adding first date column

habit_oferta_series_fd <- interquartil_bcn_habit_3[,c(1:4, 19)]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
habit_oferta_series_fd <- habit_oferta_series_fd[,c(1,4:5)]

interquartil_bcn_habit_3$primera_data <- NULL


interquartil_bcn_habit_3 <- merge(x=interquartil_bcn_habit_3, y=habit_oferta_series_fd, by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


#Adding last date column
colnames(interquartil_bcn_habit_3)
habit_oferta_series_ld <- interquartil_bcn_habit_3[,c(1:4,19)]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
habit_oferta_series_ld <- habit_oferta_series_ld[,c(1,2,6)]

interquartil_bcn_habit_3$ultlima_data <- NULL

interquartil_bcn_habit_3 <- merge(x=interquartil_bcn_habit_3, y=habit_oferta_series_ld, by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_bcn_habit_3
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]
colnames(habit_oferta_series_dp)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,12)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1,2,6)]

interquartil_bcn_habit_3$date_posting_calcul <- NULL

interquartil_bcn_habit_3 <- merge(x=interquartil_bcn_habit_3, y=habit_oferta_series_dp, by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


interquartil_bcn_habit_3 <- interquartil_bcn_habit_3%>%
  group_by(property_id, district)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_bcn_habit_3 <- interquartil_bcn_habit_3%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_bcn_habit_3$date_posting_calcul))

#2000001100000514119
#2000058600003824088
##2000008600003757484

#######################################################################
###################### Interquartils i Dies Valids BCN 2021
#######################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_bcn_habit_3$data_final, interquartil_bcn_habit_3$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_bcn_habit_3, date_test_cm_calcul)  

names(date_test_cm_calcul)[35] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_bcn_habit_3 <- date_test_cm_calcul
min(interquartil_bcn_habit_3$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_bcn_habit_3 <- interquartil_bcn_habit_3%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_bcn_habit_3 <- interquartil_bcn_habit_3%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")

interquartil_bcn_habit_3$price <- NULL
interquartil_bcn_habit_3$surface <- NULL

interquartil_bcn_habit_3 <- interquartil_bcn_habit_3%>%
  group_by(property_id, district, mes)%>%
  filter(row_number()==1)

#######################################################################
###################### Interquartils i Preus Valids BCN 2022
#######################################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_bcn_habit_4 <- test_b2022

interquartil_bcn_habit_4$rang <-interquartil_bcn_habit_4$q3_district-interquartil_bcn_habit_4$q1_district
interquartil_bcn_habit_4$valors_inferiors <- interquartil_bcn_habit_4$q1_district-(1.5*interquartil_bcn_habit_4$rang)
interquartil_bcn_habit_4$valors_superiors <- interquartil_bcn_habit_4$q3_district+(1.5*interquartil_bcn_habit_4$rang)

interquartil_bcn_habit_4 <- interquartil_bcn_habit_4%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

interquartil_bcn_habit_4 <- interquartil_bcn_habit_4[order(interquartil_bcn_habit_4$date),]

count(interquartil_bcn_habit_4, "preu_valid")

colnames(interquartil_bcn_habit_4)
#######################################################################
###################### Dates BCN 2022
#######################################################################


#Adding first date column

habit_oferta_series_fd <- interquartil_bcn_habit_4[,c(1:4, 19)]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
habit_oferta_series_fd <- habit_oferta_series_fd[,c(1,4:5)]

interquartil_bcn_habit_4$primera_data <- NULL


interquartil_bcn_habit_4 <- merge(x=interquartil_bcn_habit_4, y=habit_oferta_series_fd, by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


#Adding last date column
colnames(interquartil_bcn_habit_4)
habit_oferta_series_ld <- interquartil_bcn_habit_4[,c(1:4,19)]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
habit_oferta_series_ld <- habit_oferta_series_ld[,c(1,2,6)]

interquartil_bcn_habit_4$ultlima_data <- NULL

interquartil_bcn_habit_4 <- merge(x=interquartil_bcn_habit_4, y=habit_oferta_series_ld, by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_bcn_habit_4
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]
colnames(habit_oferta_series_dp)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,12)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1,2,6)]

interquartil_bcn_habit_4$date_posting_calcul <- NULL

interquartil_bcn_habit_4 <- merge(x=interquartil_bcn_habit_4, y=habit_oferta_series_dp, by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


interquartil_bcn_habit_4 <- interquartil_bcn_habit_4%>%
  group_by(property_id, district)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_bcn_habit_4 <- interquartil_bcn_habit_4%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_bcn_habit_4$date_posting_calcul))


#2000001100000514119
#2000058600003824088
##2000008600003757484

#######################################################################
###################### Interquartils i Dies Valids BCN 2022
#######################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_bcn_habit_4$data_final, interquartil_bcn_habit_4$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_bcn_habit_4, date_test_cm_calcul)  

names(date_test_cm_calcul)[35] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_bcn_habit_4 <- date_test_cm_calcul
min(interquartil_bcn_habit_4$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_bcn_habit_4 <- interquartil_bcn_habit_4%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_bcn_habit_4 <- interquartil_bcn_habit_4%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")

interquartil_bcn_habit_4$price <- NULL
interquartil_bcn_habit_4$surface <- NULL

interquartil_bcn_habit_4 <- interquartil_bcn_habit_4%>%
  group_by(property_id, district, mes)%>%
  filter(row_number()==1)

#2000001100000514119
#2000058600003824088
#2000008600003757484
#2000001100001446550
#2000058600003824088

##################################################################

#property_ids Barcelona Districtes
#2000398100003116496
#2000473700002758196
#2000473700003025842
#2000007000002098084
#2004245800000000003
#2000006300003845054

####################################################
##### Recovering NOMMUNI, CODIMUNI & Cleaning CAT
####################################################

interquartil_cat_habit_1 <- bind_rows(interquartil_cat_habit, interquartil_cat_habit2, interquartil_cat_habit3, interquartil_cat_habit4)

interquartil_cat_habit_1$NOMMUNI<-gsub(" Capital","",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" Girona","Girona",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" Lleida","Lleida",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$##########
NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Capital")] <- ""
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Barcelona")] <- "Barcelona"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Girona")] <- "Girona"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Lleida")] <- "Lleida"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Tarragona")] <- "Tarragona"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Hospitalet de Llobregat (L´)")] <- "l'Hospitalet de Llobregat"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Ametlla de Mar (L´)")] <- "l'Ametlla de Mar"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Escala (L´)")] <- "l'Escala"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Ametlla del Vallès (L´)")] <- "l'Ametlla del Vallès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Ampolla (L´)")] <- "l'Ampolla"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Arboç (L´)")] <- "l'Arboç"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Aldea (L´)")] <- "l'Aldea"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Espluga de Francolí (L´)")] <- "l'Espluga de Francolí"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Armentera (L´)")] <- "l'Armentera"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Espluga Calba (L´)")] <- "l'Espluga Calba"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Estany (L´)")] <- "l'Estany"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Albiol (L´)")] <- "l'Albiol"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bruc (El)")] <- "el Bruc"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Brull (El)")] <- "el Brull"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Masnou (El)")] <- "el Masnou"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Morell (El)")] <- "el Morell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Montmell (El)")] <- "el Montmell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Papiol (El)")] <- "el Papiol"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Poal (El)")] <- "el Poal"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Catllar (El)")] <- "el Catllar"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Rourell (El)")] <- "el Rourell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pla del Penedès (El)")] <- "el Pla del Penedès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Far d´Empordà (El)")] <- "el Far d'Empordà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Perelló (El)")] <- "el Perelló"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Alamús (Els)")] <- "els Alamús"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Riera de Gaià (La)")] <- "la Riera de Gaià"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Seu d´Urgell (La)")] <- "la Seu d'Urgell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Roca del Vallès (La)")] <- "la Roca del Vallès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Palma de Cervelló (La)")] <- "la Palma de Cervelló"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Garriga (La)")] <- "la Garriga"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Molina (La)")] <- "la Molina"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bisbal d´Empordà (La)")] <- "la Bisbal d'Empordà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Tallada d´Empordà (La)")] <- "la Tallada d'Empordà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vall d´en Bas (La)")] <- "la Vall d'en Bas"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Cérvoles (La)")] <- "la Pobla de Cérvoles"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vall de Boí (La)")] <- "la Vall de Boí"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Granada (La)")] <- "la Granada"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Nou de Gaià (La)")] <- "la Nou de Gaià"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Riba (La)")] <- "la Riba"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Secuita (La)")] <- "la Secuita"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Portella (La)")] <- "la Portella"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Montornès (La)")] <- "la Pobla de Montornès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Galera (La)")] <- "la Galera"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Guingueta d´Àneu (La)")] <- "la Guingueta d'Àneu"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bisbal del Penedès (La)")] <- "la Bisbal del Penedès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Torre de l´Espanyol (La)")] <- "la Torre de l'Espanyol"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Sénia (La)")] <- "la Sénia"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vajol (La)")] <- "la Vajol"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pera (La)")] <- "la Pera"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Sentiu de Sió (La)")] <- "la Sentiu"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Franqueses del Vallès (Les)")] <- "les Franqueses del Vallès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Masies de Voltregà (Les)")] <- "les Masies de Voltregà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Planes d´Hostoles (Les)")] <- "les Planes d'Hostoles"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Preses (Les)")] <- "les Preses"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Piles (Les)")] <- "les Piles"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"
interquartil_cat_habit_1$NOMMUNI<-gsub(" d´"," d'",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" n´"," n'",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" l´"," l'",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Coma-ruga")] <- "el Vendrell"

interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bellaterra")] <- "Cerdanyola del Vallès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bigues i Riells")] <- "Bigues i Riells del Fai"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Calella de Palafrugell")] <- "Calella"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Camallera")] <- "Saus, Camallera i Llampaies"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Sant Antoni de Calonge")] <- "Calonge i Sant Antoni"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Calonge")] <- "Calonge de Segarra"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Canonja (la)")] <- "la Canonja"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Castell d´Aro")] <- "Castell-Platja d'Aro"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Empuriabrava")] <- "Castelló d'Empúries"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="el Vendrel")] <- "el Vendrell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Estartit")] <- "Torroella de Montgrí"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="la Hostalets de Pierola")] <- "els Hostalets de Pierola"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="la Molina")] <- "Alp"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Llfranc")] <- "Palafrugell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Miami Platja")] <- "Mont-roig del Camp"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Platja d'Aro")] <- "Castell-Platja d'Aro"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Castell d'Aro")] <- "Castell-Platja d'Aro"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Roda de Barà")] <- "Roda de Berà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="S´Agaró")] <- "Castell-Platja d'Aro"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Segur de Calafell")] <- "Calafell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Tamariu")] <- "Palafrugell"

interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Sants - Montjuïc")] <- "Sants-Montjuïc"
interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Sarrià - Sant Gervasi")] <- "Sarrià-Sant Gervasi"
interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Horta - Guinardò")] <- "Horta-Guinardò"
interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Horta Guinardó")] <- "Horta-Guinardò"
interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Horta - Guinardó")] <- "Horta-Guinardò"
interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Horta Guinardò")] <- "Horta-Guinardò"
interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
interquartil_cat_habit_1$district[which(interquartil_cat_habit_1$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"


#################################################
##### Recovering NOMMUNI, CODIMUNI & Cleaning CAT
#################################################

interquartil_cat_habit_1$NOMMUNI<-gsub(" Capital","",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" Girona","Girona",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" Lleida","Lleida",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$##########
NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Capital")] <- ""
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Barcelona")] <- "Barcelona"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Girona")] <- "Girona"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Lleida")] <- "Lleida"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI==" Tarragona")] <- "Tarragona"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Hospitalet de Llobregat (L´)")] <- "l'Hospitalet de Llobregat"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Ametlla de Mar (L´)")] <- "l'Ametlla de Mar"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Escala (L´)")] <- "l'Escala"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Ametlla del Vallès (L´)")] <- "l'Ametlla del Vallès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Ampolla (L´)")] <- "l'Ampolla"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Arboç (L´)")] <- "l'Arboç"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Aldea (L´)")] <- "l'Aldea"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Espluga de Francolí (L´)")] <- "l'Espluga de Francolí"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Armentera (L´)")] <- "l'Armentera"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Espluga Calba (L´)")] <- "l'Espluga Calba"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Estany (L´)")] <- "l'Estany"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Albiol (L´)")] <- "l'Albiol"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bruc (El)")] <- "el Bruc"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Brull (El)")] <- "el Brull"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Masnou (El)")] <- "el Masnou"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Morell (El)")] <- "el Morell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Montmell (El)")] <- "el Montmell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Papiol (El)")] <- "el Papiol"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Poal (El)")] <- "el Poal"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Catllar (El)")] <- "el Catllar"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Rourell (El)")] <- "el Rourell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pla del Penedès (El)")] <- "el Pla del Penedès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Far d´Empordà (El)")] <- "el Far d'Empordà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Perelló (El)")] <- "el Perelló"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Alamús (Els)")] <- "els Alamús"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Riera de Gaià (La)")] <- "la Riera de Gaià"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Seu d´Urgell (La)")] <- "la Seu d'Urgell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Roca del Vallès (La)")] <- "la Roca del Vallès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Palma de Cervelló (La)")] <- "la Palma de Cervelló"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Garriga (La)")] <- "la Garriga"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Molina (La)")] <- "la Molina"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bisbal d´Empordà (La)")] <- "la Bisbal d'Empordà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Tallada d´Empordà (La)")] <- "la Tallada d'Empordà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vall d´en Bas (La)")] <- "la Vall d'en Bas"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Cérvoles (La)")] <- "la Pobla de Cérvoles"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vall de Boí (La)")] <- "la Vall de Boí"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Granada (La)")] <- "la Granada"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Nou de Gaià (La)")] <- "la Nou de Gaià"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Riba (La)")] <- "la Riba"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Secuita (La)")] <- "la Secuita"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Portella (La)")] <- "la Portella"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Montornès (La)")] <- "la Pobla de Montornès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Galera (La)")] <- "la Galera"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Guingueta d´Àneu (La)")] <- "la Guingueta d'Àneu"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bisbal del Penedès (La)")] <- "la Bisbal del Penedès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Torre de l´Espanyol (La)")] <- "la Torre de l'Espanyol"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Sénia (La)")] <- "la Sénia"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Vajol (La)")] <- "la Vajol"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Pera (La)")] <- "la Pera"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Sentiu de Sió (La)")] <- "la Sentiu"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Franqueses del Vallès (Les)")] <- "les Franqueses del Vallès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Masies de Voltregà (Les)")] <- "les Masies de Voltregà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Planes d´Hostoles (Les)")] <- "les Planes d'Hostoles"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Preses (Les)")] <- "les Preses"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Piles (Les)")] <- "les Piles"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"
interquartil_cat_habit_1$NOMMUNI<-gsub(" d´"," d'",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" n´"," n'",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI<-gsub(" l´"," l'",as.character(interquartil_cat_habit_1$NOMMUNI))
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Coma-ruga")] <- "el Vendrell"

interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bellaterra")] <- "Cerdanyola del Vallès"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Bigues i Riells")] <- "Bigues i Riells del Fai"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Calella de Palafrugell")] <- "Calella"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Camallera")] <- "Saus, Camallera i Llampaies"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Sant Antoni de Calonge")] <- "Calonge i Sant Antoni"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Calonge")] <- "Calonge de Segarra"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Canonja (la)")] <- "la Canonja"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Castell d´Aro")] <- "Castell-Platja d'Aro"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Empuriabrava")] <- "Castelló d'Empúries"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="el Vendrel")] <- "el Vendrell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Estartit")] <- "Torroella de Montgrí"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="la Hostalets de Pierola")] <- "els Hostalets de Pierola"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="la Molina")] <- "Alp"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Llfranc")] <- "Palafrugell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Miami Platja")] <- "Mont-roig del Camp"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Platja d'Aro")] <- "Castell-Platja d'Aro"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Castell d'Aro")] <- "Castell-Platja d'Aro"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Roda de Barà")] <- "Roda de Berà"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="S´Agaró")] <- "Castell-Platja d'Aro"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Segur de Calafell")] <- "Calafell"
interquartil_cat_habit_1$NOMMUNI[which(interquartil_cat_habit_1$NOMMUNI=="Tamariu")] <- "Palafrugell"


#######################################################################
###################### Dates CAT final
#######################################################################

#Adding first date column
colnames(interquartil_cat_habit_1)

habit_oferta_series_fd <- interquartil_cat_habit_1[,c(1:4, 30)]
habit_oferta_series_fd <- habit_oferta_series_fd[order(habit_oferta_series_fd$primera_data),]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
habit_oferta_series_fd <- habit_oferta_series_fd[,c(1:2,5)]
interquartil_cat_habit_1$primera_data <- NULL

interquartil_cat_habit_1 <- merge(x=interquartil_cat_habit_1, y=habit_oferta_series_fd, 
                                  by.x=c("property_id","municipality"), 
                                  by.y=c("property_id","municipality"))


#Adding last date column
colnames(interquartil_cat_habit_1)
habit_oferta_series_ld <- interquartil_cat_habit_1[,c(1:4,30)]
habit_oferta_series_ld <- habit_oferta_series_ld[order(habit_oferta_series_ld$ultima_data),]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = ultima_data)
habit_oferta_series_ld <- habit_oferta_series_ld[,c(1:2,5)]

interquartil_cat_habit_1$ultima_data <- NULL

interquartil_cat_habit_1 <- merge(x=interquartil_cat_habit_1, y=habit_oferta_series_ld, 
                                  by.x=c("property_id","municipality"), 
                                  by.y=c("property_id","municipality"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_cat_habit_1
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]

colnames(habit_oferta_series_dp)

habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,10)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, municipality) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:2,6)]

interquartil_cat_habit_1$date_posting_calcul <- NULL

interquartil_cat_habit_1 <- merge(x=interquartil_cat_habit_1, y=habit_oferta_series_dp, 
                                  by.x=c("property_id","municipality"), 
                                  by.y=c("property_id","municipality"))


interquartil_cat_habit_1 <- interquartil_cat_habit_1%>%
  group_by(property_id, municipality)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_cat_habit_1 <- interquartil_cat_habit_1%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_cat_habit_1$date_posting_calcul))

interquartil_cat_habit_1 <- interquartil_cat_habit_1%>%
  arrange(property_id, mes, any)


#######################################################################
###################### Interquartils i Dies Valids CAT
#######################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_cat_habit_1$data_final, interquartil_cat_habit_1$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_cat_habit_1, date_test_cm_calcul)  

date_test_cm_calcul$dies_duracio <- NULL
date_test_cm_calcul$dia_valid <- NULL

colnames(date_test_cm_calcul)
names(date_test_cm_calcul)[33] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_cat_habit_1 <- date_test_cm_calcul
min(interquartil_cat_habit_1$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_cat_habit_1 <- interquartil_cat_habit_1%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_cat_habit_1 <- interquartil_cat_habit_1%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")


######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################


###################################################
##### Recovering NOMMUNI, CODIMUNI & Cleaning BCN
###################################################

interquartil_bcn_habit_1 <- bind_rows(interquartil_bcn_habit, interquartil_bcn_habit_2 , interquartil_bcn_habit_3, interquartil_bcn_habit_4)

interquartil_bcn_habit_1$NOMMUNI<-gsub(" Capital","",as.character(interquartil_bcn_habit_1$NOMMUNI))
interquartil_bcn_habit_1$NOMMUNI<-gsub(" Barcelona","Barcelona",as.character(interquartil_bcn_habit_1$NOMMUNI))
interquartil_bcn_habit_1$NOMMUNI<-gsub(" Girona","Girona",as.character(interquartil_bcn_habit_1$NOMMUNI))
interquartil_bcn_habit_1$NOMMUNI<-gsub(" Lleida","Lleida",as.character(interquartil_bcn_habit_1$NOMMUNI))
interquartil_bcn_habit_1$##########
NOMMUNI<-gsub(" Tarragona","Tarragona",as.character(interquartil_bcn_habit_1$NOMMUNI))
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI==" Capital")] <- ""
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI==" Barcelona")] <- "Barcelona"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI==" Girona")] <- "Girona"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI==" Lleida")] <- "Lleida"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI==" Tarragona")] <- "Tarragona"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Hospitalet de Llobregat (L´)")] <- "l'Hospitalet de Llobregat"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Ametlla de Mar (L´)")] <- "l'Ametlla de Mar"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Escala (L´)")] <- "l'Escala"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Ametlla del Vallès (L´)")] <- "l'Ametlla del Vallès"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Ampolla (L´)")] <- "l'Ampolla"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Arboç (L´)")] <- "l'Arboç"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Aldea (L´)")] <- "l'Aldea"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Espluga de Francolí (L´)")] <- "l'Espluga de Francolí"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Armentera (L´)")] <- "l'Armentera"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Espluga Calba (L´)")] <- "l'Espluga Calba"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Estany (L´)")] <- "l'Estany"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Albiol (L´)")] <- "l'Albiol"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Bruc (El)")] <- "el Bruc"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Brull (El)")] <- "el Brull"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Vendrell (El)")] <- "el Vendrel"

interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Masnou (El)")] <- "el Masnou"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Morell (El)")] <- "el Morell"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pont de Suert (El)")] <- "el Pont de Suert"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Prat de Llobregat (El)")] <- "el Prat de Llobregat"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Montmell (El)")] <- "el Montmell"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pla de Santa Maria (El)")] <- "el Pla de Santa Maria"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Papiol (El)")] <- "el Papiol"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Poal (El)")] <- "el Poal"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Catllar (El)")] <- "el Catllar"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pont de Vilomara i Rocafort (El)")] <- "el Pont de Vilomara i Rocafort"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Port de la Selva (El)")] <- "el Port de la Selva"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Rourell (El)")] <- "el Rourell"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pla del Penedès (El)")] <- "el Pla del Penedès"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pont de Bar (El)")] <- "el Pont de Bar"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Far d´Empordà (El)")] <- "el Far d'Empordà"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Perelló (El)")] <- "el Perelló"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pallaresos (Els)")] <- "els Pallaresos"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Alamús (Els)")] <- "els Alamús"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Prats de Rei (Els)")] <- "els Prats de Rei"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Hostalets de Pierola (Els)")] <- "els Hostalets de Pierola"


interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Riera de Gaià (La)")] <- "la Riera de Gaià"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Seu d´Urgell (La)")] <- "la Seu d'Urgell"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Roca del Vallès (La)")] <- "la Roca del Vallès"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Palma de Cervelló (La)")] <- "la Palma de Cervelló"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Garriga (La)")] <- "la Garriga"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Molina (La)")] <- "la Molina"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Jonquera (La)")] <- "la Jonquera"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Llagosta (La)")] <- "la Llagosta"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pobla de Mafumet (La)")] <- "la Pobla de Mafumet"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Coma i la Pedra (La)")] <- "la Coma i la Pedra"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pobla de Lillet (La)")] <- "la Pobla de Lillet"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Bisbal d´Empordà (La)")] <- "la Bisbal d'Empordà"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Tallada d´Empordà (La)")] <- "la Tallada d'Empordà"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pobla de Claramunt (La)")] <- "la Pobla de Claramunt"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Morera de Montsant (La)")] <- "la Morera de Montsant"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Vall d´en Bas (La)")] <- "la Vall d'en Bas"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Llacuna (La)")] <- "la Hostalets de Pierola"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pobla de Cérvoles (La)")] <- "la Pobla de Cérvoles"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Selva del Camp (La)")] <- "la Selva del Camp"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Cellera de Ter (La)")] <- "la Cellera de Ter"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Vall de Boí (La)")] <- "la Vall de Boí"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Granada (La)")] <- "la Granada"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Nou de Gaià (La)")] <- "la Nou de Gaià"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Riba (La)")] <- "la Riba"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Secuita (La)")] <- "la Secuita"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Portella (La)")] <- "la Portella"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pobla de Montornès (La)")] <- "la Pobla de Montornès"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Galera (La)")] <- "la Galera"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Guingueta d´Àneu (La)")] <- "la Guingueta d'Àneu"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pobla de Segur (La)")] <- "la Pobla de Segur"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Canonja (la) ")] <- "la Canonja"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Bisbal del Penedès (La)")] <- "la Bisbal del Penedès"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Torre de l´Espanyol (La)")] <- "la Torre de l'Espanyol"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Vall de Bianya (La)")] <- "la Vall de Bianya"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Sénia (La)")] <- "la Sénia"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Vajol (La)")] <- "la Vajol"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Torre de Cabdella (La)")] <- "la Torre de Cabdella"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Pera (La)")] <- "la Pera"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Sentiu de Sió (La)")] <- "la Sentiu"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Torre de Claramunt (La)")] <- "la Torre de Claramunt"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Baronia de Rialb (La)")] <- "la Baronia de Rialb"

interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Franqueses del Vallès (Les)")] <- "les Franqueses del Vallès"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Borges del Camp (Les)")] <- "les Borges del Camp"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Borges Blanques (Les)")] <- "les Borges Blanques"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Masies de Voltregà (Les)")] <- "les Masies de Voltregà"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Planes d´Hostoles (Les)")] <- "les Planes d'Hostoles"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Valls de Valira (Les)")] <- "les Valls de Valira"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Preses (Les)")] <- "les Preses"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Piles (Les)")] <- "les Piles"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Masies de Roda (Les)")] <- "les Masies de Roda"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Cabanyes (Les)")] <- "les Cabanyes"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Sant Carles de la Ràpita")] <- "la Ràpita"
interquartil_bcn_habit_1$NOMMUNI<-gsub(" d´"," d'",as.character(interquartil_bcn_habit_1$NOMMUNI))
interquartil_bcn_habit_1$NOMMUNI<-gsub(" n´"," n'",as.character(interquartil_bcn_habit_1$NOMMUNI))
interquartil_bcn_habit_1$NOMMUNI<-gsub(" l´"," l'",as.character(interquartil_bcn_habit_1$NOMMUNI))
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Coma-ruga")] <- "el Vendrell"

interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Bellaterra")] <- "Cerdanyola del Vallès"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Bigues i Riells")] <- "Bigues i Riells del Fai"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Calella de Palafrugell")] <- "Calella"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Camallera")] <- "Saus, Camallera i Llampaies"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Sant Antoni de Calonge")] <- "Calonge i Sant Antoni"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Calonge")] <- "Calonge de Segarra"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Canonja (la)")] <- "la Canonja"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Castell d´Aro")] <- "Castell-Platja d'Aro"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Empuriabrava")] <- "Castelló d'Empúries"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="el Vendrel")] <- "el Vendrell"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Estartit")] <- "Torroella de Montgrí"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="la Hostalets de Pierola")] <- "els Hostalets de Pierola"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="la Molina")] <- "Alp"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Llfranc")] <- "Palafrugell"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Miami Platja")] <- "Mont-roig del Camp"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Platja d'Aro")] <- "Castell-Platja d'Aro"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Castell d'Aro")] <- "Castell-Platja d'Aro"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Roda de Barà")] <- "Roda de Berà"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="S´Agaró")] <- "Castell-Platja d'Aro"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Segur de Calafell")] <- "Calafell"
interquartil_bcn_habit_1$NOMMUNI[which(interquartil_bcn_habit_1$NOMMUNI=="Tamariu")] <- "Palafrugell"

interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Sants - Montjuïc")] <- "Sants-Montjuïc"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Sarrià - Sant Gervasi")] <- "Sarrià-Sant Gervasi"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Horta - Guinardò")] <- "Horta-Guinardó"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Horta Guinardó")] <- "Horta-Guinardó"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Horta - Guinardó")] <- "Horta-Guinardó"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Horta Guinardò")] <- "Horta-Guinardó"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Sants Montjuïc")] <- "Sants-Montjuïc"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Sarrià Sant Gervasi")] <- "Sarrià-Sant Gervasi"
interquartil_bcn_habit_1$district[which(interquartil_bcn_habit_1$district=="Horta-Guinardò")] <- "Horta-Guinardó"


#######################################################################
###################### Dates BCN final
#######################################################################

#Adding first date column
colnames(interquartil_bcn_habit_1)

habit_oferta_series_fd <- interquartil_bcn_habit_1[,c(1:4, 30)]
habit_oferta_series_fd <- habit_oferta_series_fd[order(habit_oferta_series_fd$primera_data),]

habit_oferta_series_fd <- habit_oferta_series_fd %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)

habit_oferta_series_fd <- habit_oferta_series_fd[,c(1:2,5)]
interquartil_bcn_habit_1$primera_data <- NULL


interquartil_bcn_habit_1 <- merge(x=interquartil_bcn_habit_1, y=habit_oferta_series_fd, 
                                  by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


#Adding last date column
colnames(interquartil_bcn_habit_1)
habit_oferta_series_ld <- interquartil_bcn_habit_1[,c(1:4,30)]
habit_oferta_series_ld <- habit_oferta_series_ld[order(habit_oferta_series_ld$ultima_data),]

habit_oferta_series_ld <- habit_oferta_series_ld %>%
  group_by(property_id, district) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = ultima_data)

habit_oferta_series_ld <- habit_oferta_series_ld[,c(1:2,5)]

interquartil_bcn_habit_1$ultima_data <- NULL

interquartil_bcn_habit_1 <- merge(x=interquartil_bcn_habit_1, y=habit_oferta_series_ld, 
                                  by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


#Adding date_posting_calcul
habit_oferta_series_dp <- interquartil_bcn_habit_1
habit_oferta_series_dp <- habit_oferta_series_dp[order(habit_oferta_series_dp$date_posting),]

colnames(habit_oferta_series_dp)

habit_oferta_series_dp <- habit_oferta_series_dp[,c(1:4,10)]

habit_oferta_series_dp <- habit_oferta_series_dp %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
habit_oferta_series_dp <- habit_oferta_series_dp[,c(1,2,6)]

interquartil_bcn_habit_1$date_posting_calcul <- NULL

interquartil_bcn_habit_1 <- merge(x=interquartil_bcn_habit_1, y=habit_oferta_series_dp, 
                                  by.x=c("property_id","district"), 
                                  by.y=c("property_id","district"))


interquartil_bcn_habit_1 <- interquartil_bcn_habit_1%>%
  group_by(property_id, district)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

interquartil_bcn_habit_1 <- interquartil_bcn_habit_1%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(interquartil_bcn_habit_1$date_posting_calcul))

interquartil_bcn_habit_1 <- interquartil_bcn_habit_1%>%
  arrange(property_id, mes, any)

#######################################################################
###################### Interquartils i Dies Valids BCN
#######################################################################

###################### Calculating the number of valid days #######

colnames(interquartil_bcn_habit_1)

date_test_cm_calcul <- difftime(interquartil_bcn_habit_1$data_final, interquartil_bcn_habit_1$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_bcn_habit_1, date_test_cm_calcul)  

date_test_cm_calcul$dies_duracio <- NULL
date_test_cm_calcul$dia_valid <- NULL

colnames(date_test_cm_calcul)
names(date_test_cm_calcul)[33] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_bcn_habit_1 <- date_test_cm_calcul
min(interquartil_bcn_habit_1$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_bcn_habit_1 <- interquartil_bcn_habit_1%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_bcn_habit_1 <- interquartil_bcn_habit_1%>%
  filter(preu_valid=="Vàlid" & dia_valid=="Vàlid")


###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

write_csv2(interquartil_bcn_habit_1, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/habitaclia_interquartil_bcn_districtes.csv")
write_csv2(interquartil_cat_habit_1, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/habitaclia_interquartil_cat_muni.csv")

############################################
###################### Boxplots
############################################
boxplot(interquartil_cat_habit_muni_habit$preu_m2,
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

boxplot(interquartil_bcn_habit_districtes_habit$preu_m2,
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