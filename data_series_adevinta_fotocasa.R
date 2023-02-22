library(dplyr)
library(xlsx)
library(readr)
library(plyr)
library(lubridate)
library(tidyverse)
options(scipen = 999)
options(digits=3)


foto_oferta_series <- bind_rows(test_2019_f, test_2020_f, test_2021_f, test_2022_f)
#foto_oferta_series$district[which(foto_oferta_series$district=="Horta - Guinardó")] <- "Horta-Guinardò"

#######################################################################
###################################### Correcting Dates CAT
#######################################################################
foto_oferta_series <- foto_oferta_series[order(foto_oferta_series$primera_data),]

foto_oferta_series_fd <- foto_oferta_series

#Adding first date column
foto_oferta_series_fd <- foto_oferta_series_fd[,c(1, 15, 3, 16, 20)]

foto_oferta_series_fd <- foto_oferta_series_fd %>%
  group_by(property_id, NOMMUNI) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
foto_oferta_series_fd <- foto_oferta_series_fd[,c(1,2,5)]

foto_oferta_series$primera_data <- NULL

foto_oferta_series <- merge(x=foto_oferta_series, y=foto_oferta_series_fd, by.x=c("property_id","NOMMUNI"), 
                            by.y=c("property_id","NOMMUNI"))


#Adding last date column
foto_oferta_series_ld <- foto_oferta_series

foto_oferta_series_ld <- foto_oferta_series_ld[,c(1,2,4,16,20)]

foto_oferta_series_ld <- foto_oferta_series_ld %>%
  group_by(property_id, NOMMUNI) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
foto_oferta_series_ld <- foto_oferta_series_ld[,c(1,2,6)]

foto_oferta_series$ultlima_data <- NULL

foto_oferta_series <- merge(x=foto_oferta_series, y=foto_oferta_series_ld, by.x=c("property_id","NOMMUNI"), 
                            by.y=c("property_id","NOMMUNI"))


#Adding date_posting_calcul
foto_oferta_series_dp <- foto_oferta_series
foto_oferta_series_dp <- foto_oferta_series_dp[order(foto_oferta_series_dp$date_posting),]

foto_oferta_series_dp <- foto_oferta_series_dp[,c(1,2,4,16,13)]

foto_oferta_series_dp <- foto_oferta_series_dp %>%
  group_by(property_id, NOMMUNI) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
foto_oferta_series_dp <- foto_oferta_series_dp[,c(1,2,6)]
foto_oferta_series$date_posting_calcul <- NULL

foto_oferta_series <- merge(x=foto_oferta_series, y=foto_oferta_series_dp, by.x=c("property_id","NOMMUNI"), 
                            by.y=c("property_id","NOMMUNI"))


foto_oferta_series <- foto_oferta_series%>%
  group_by(property_id, municipality)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

foto_oferta_series <- foto_oferta_series%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(foto_oferta_series$date_posting_calcul))

#######################################################################
#######################################################################

2000058600003124425

#######################################################################
###################### Interquartils i Preus Valids CAT
#######################################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_cat <- foto_oferta_series

interquartil_cat$rang <-interquartil_cat$q3_muni - interquartil_cat$q1_muni
interquartil_cat$valors_inferiors <- interquartil_cat$q1_muni-(1.5*interquartil_cat$rang)
interquartil_cat$valors_superiors <- interquartil_cat$q3_muni+(1.5*interquartil_cat$rang)

interquartil_cat <- interquartil_cat%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

interquartil_cat <- interquartil_cat[order(interquartil_cat$date),]

count(interquartil_cat, "preu_valid")

#2000001100000514119
#2000058600003824088
##2000008600003757484


########################################################################################
###################### Interquartils i Dies Valids CAT
########################################################################################

###################### Calculating the number of valid days #######

date_test_cm_calcul <- difftime(interquartil_cat$data_final, interquartil_cat$date_posting_calcul, units = "days")
date_test_cm_calcul <- as.numeric(date_test_cm_calcul)
date_test_cm_calcul <- data.frame(date_test_cm_calcul)
names(date_test_cm_calcul)[1] <- "diferencia"
min(date_test_cm_calcul$diferencia)
which(is.na(date_test_cm_calcul$diferencia))

date_test_cm_calcul <- bind_cols(interquartil_cat, date_test_cm_calcul)  

names(date_test_cm_calcul)[36] <- "dies_duracio"
min(date_test_cm_calcul$dies_duracio)

interquartil_cat <- date_test_cm_calcul
min(interquartil_cat$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_cat <- interquartil_cat%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_cat$price <- NULL
interquartil_cat$surface <- NULL


#######################################################################
###################### Correcting Dates BCN
#######################################################################

foto_oferta_series_bcn <- bind_rows(test_b2019_f , test_b2020_f, test_b2021_f, test_b2022_f)
#foto_oferta_series_bcn$district[which(foto_oferta_series_bcn$district=="Horta - Guinardó")] <- "Horta-Guinardò"

################################################### Correcting date_posting
foto_oferta_series_bcn <- foto_oferta_series_bcn[order(foto_oferta_series_bcn$primera_data),]

foto_oferta_series_fd <- foto_oferta_series_bcn

#Adding first date column
foto_oferta_series_fd <- foto_oferta_series_fd[,c(1:3, 16, 20)]

foto_oferta_series_fd <- foto_oferta_series_fd %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = primera_data)
foto_oferta_series_fd <- foto_oferta_series_fd[,c(1,2,5)]

foto_oferta_series_bcn$primera_data <- NULL

foto_oferta_series_bcn <- merge(x=foto_oferta_series_bcn, y=foto_oferta_series_fd, by.x=c("property_id","district"), 
                                by.y=c("property_id","district"))


#Adding last date column
foto_oferta_series_ld <- foto_oferta_series_bcn

foto_oferta_series_ld <- foto_oferta_series_ld[,c(1:3, 16, 20)]

foto_oferta_series_ld <- foto_oferta_series_ld %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(ultima_data = ultlima_data)
foto_oferta_series_ld <- foto_oferta_series_ld[,c(1,2,6)]

foto_oferta_series_bcn$ultlima_data <- NULL

foto_oferta_series_bcn <- merge(x=foto_oferta_series_bcn, y=foto_oferta_series_ld, by.x=c("property_id","district"), 
                                by.y=c("property_id","district"))


#Adding date_posting_calcul
foto_oferta_series_dp <- foto_oferta_series_bcn
foto_oferta_series_dp <- foto_oferta_series_dp[order(foto_oferta_series_dp$date_posting),]

foto_oferta_series_dp <- foto_oferta_series_dp[,c(1:3, 12, 15)]

foto_oferta_series_dp <- foto_oferta_series_dp %>%
  group_by(property_id, district) %>%
  filter(row_number()==1) %>%
  mutate(date_posting_calcul = date_posting)
foto_oferta_series_dp <- foto_oferta_series_dp[,c(1,2,6)]
foto_oferta_series_bcn$date_posting_calcul <- NULL

foto_oferta_series_bcn <- merge(x=foto_oferta_series_bcn, y=foto_oferta_series_dp, by.x=c("property_id","district"), 
                                by.y=c("property_id","district"))


foto_oferta_series_bcn <- foto_oferta_series_bcn%>%
  group_by(property_id, district)%>%
  dplyr::mutate(date_posting_calcul = case_when(
    date_posting_calcul < primera_data ~ date_posting_calcul,
    date_posting_calcul == primera_data ~ date_posting_calcul,
    date_posting_calcul > primera_data ~ primera_data
  )
  )

foto_oferta_series_bcn <- foto_oferta_series_bcn%>%
  dplyr::mutate(date_posting_calcul = case_when(!is.na(date_posting_calcul) ~ date_posting_calcul,
                                                TRUE ~ primera_data))

which(is.na(foto_oferta_series_bcn$date_posting_calcul))

###################################################

2000058600003124425

#######################################################
################### Interquartils i Preus Valids BCN
#######################################################

#2000050000003544380
#2000050000003623940 NA
#2000050000003544380
#2000050000003544380

interquartil_bcn <- foto_oferta_series_bcn

interquartil_bcn$rang <-interquartil_bcn$q3_dist - interquartil_bcn$q1_dist
interquartil_bcn$valors_inferiors <- interquartil_bcn$q1_dist-(1.5*interquartil_bcn$rang)
interquartil_bcn$valors_superiors <- interquartil_bcn$q3_dist+(1.5*interquartil_bcn$rang)

interquartil_bcn <- interquartil_bcn%>%
  mutate(preu_valid = case_when(
    preu_m2_mes <= valors_inferiors ~ "No Vàlid",
    preu_m2_mes >= valors_superiors ~ "No Vàlid",
    preu_m2_mes >= valors_inferiors & preu_m2_mes <= valors_superiors ~ "Vàlid",
  )
  )

count(interquartil_bcn, "preu_valid")


#2000001100000514119
#2000058600003824088
##2000008600003757484


#######################################################
###################### Interquartils i Dies Valies BCN
#######################################################

###################### Calculating the number of valid days #######

date_test_bcn_calcul <- difftime(interquartil_bcn$data_final, interquartil_bcn$date_posting_calcul, units = "days")
date_test_bcn_calcul <- as.numeric(date_test_bcn_calcul)
date_test_bcn_calcul <- data.frame(date_test_bcn_calcul)
names(date_test_bcn_calcul)[1] <- "diferencia"
min(date_test_bcn_calcul$diferencia)
which(is.na(date_test_bcn_calcul$diferencia))

date_test_bcn_calcul <- bind_cols(interquartil_bcn, date_test_bcn_calcul)  

names(date_test_bcn_calcul)[36] <- "dies_duracio"
min(date_test_bcn_calcul$dies_duracio)

interquartil_bcn <- date_test_bcn_calcul
min(interquartil_bcn$dies_duracio)

#2000001100001446550
#2000058600003824088

##################################################################

#2000007000002098084
#2004245800000000003
##2001307400000012288

interquartil_bcn <- interquartil_bcn%>%
  mutate(dia_valid = case_when(
    dies_duracio <= 180 ~ "Vàlid",
    dies_duracio > 180 ~ "No Vàlid"
  )
  )

interquartil_bcn$price <- NULL
interquartil_bcn$surface <- NULL

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

interquartil_bcn_foto <- interquartil_bcn
interquartil_cat_foto <- interquartil_cat

write_csv2(interquartil_bcn_foto, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Fotocasa_interquartil_bcn_districtes.csv")
write_csv2(interquartil_cat_foto, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Fotocasa_interquartil_cat_muni.csv")


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
