library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(plyr)
library(lubridate)

#######################
options(scipen=999)
options(digits=2)
#######################

tractament_model <- read.xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/tractament_model.xlsx",
                              sheetName = "Sheet1")
tractament_model$property_id <- as.character(tractament_model$property_id)


tractament_model$mes <- tractament_model$date
data.frame(tractament_model$mes <-month(ymd(tractament_model$mes)))
tractament_model$any <- format(as.Date(tractament_model$date, format="%Y/%m/%d"),"%Y")

#Adding first date column
test11a <- tractament_model %>%
  group_by(property_id) %>%
  filter(row_number()==1) %>%
  mutate(primera_data = date)
test11a <- test11a[,c(4,8)]

#Adding last date column
test11b <- tractament_model %>%
  group_by(property_id) %>%
  filter(row_number()==n()) %>%
  mutate(ultima_data = date)
test11b <- test11b[,c(4,8)]

test111 <- join(test11a, test11b, by = "property_id")
test111 <- join(tractament_model, test111, by = "property_id")
test111 <- as.data.frame(test111)

#####################

test111 <- test111 %>% mutate(trimestre =
                                dplyr::case_when(mes <= 3 & any == "2019" ~ "2019/T1", 
                                 mes >= 4 & mes <= 6 & any == "2019" ~ "2019/T2",
                                 mes <= 3 & any == "2020" ~ "2020/T1",
                                 mes >= 4 & mes <= 6 & any == "2020" ~ "2020/T2",
                                 mes <= 3 & any == "2021" ~ "2021/T1",
                                 mes >= 4 & mes <= 6 & any == "2021" ~ "2021/T2",
                                 mes <= 3 & any == "2022" ~ "2022/T1",
                                 mes >= 4 & mes <= 6 & any == "2022" ~ "2022/T2")
)



mitjana_preu_trimestre <- test111 %>% aggregate(price~property_id+trimestre, mean, digits=2)
mitjana_preu_trimestre$property_id <- NULL
names(mitjana_preu_trimestre)[2] <- "mitjana_preu_trimestre"
mitjana_superficie_trimestre <- test111 %>% aggregate(surface~property_id+trimestre, mean)
mitjana_superficie_trimestre$property_id <- NULL
names(mitjana_superficie_trimestre)[2] <- "mitjana_superficie_trimestre"

############

#Adding mes any column
test11c <- tractament_model %>%
  group_by(property_id, any, mes) %>%
  filter(row_number()==1)%>%
  mutate(mes_any_primera_data = date)

test11c <- test11c %>% mutate(trimestre =
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
test11c <- test11c[, c(4,7:8)]
names(test11c)[3] <- "trimest"

test111 <- join(test111, test11c, by = "property_id")
test111$trimest <- NULL

test111$price <- NULL
test111$surface <- NULL
#test111$mes <- NULL
test111$any <- NULL

############
test111 <- join(test111, mitjana_preu_trimestre, by = "trimestre" )
test111 <- join(test111, mitjana_superficie_trimestre, by = "trimestre")
############

test111 <- test111 %>% group_by(property_id, mes, trimestre) %>%
  filter(row_number()==1)

test111 <- test111[,c(1:2, 4:10, 3)]
test111 <- test111[,c(2:5, 1, 6:10)]
test111$any <- NULL
count(test111, "mitjana_superficie_trimestre")
count(test111, "mitjana_preu_trimestre")

names(test111)[5] <- "primera_data_mes_any"

library(xlsx)
write_xlsx(test111, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/tractament.xlsx")

dataset_names <- list('Dades Model' = tractament_model, 'Cálculs' = test111)

write_xlsx(dataset_names, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/tractament_model1.xlsx")


################

############################################################
############ Mitjanes Mensuals
############################################################
mitjanes_mensuals <- test111 %>%
  group_by(property_id, mes, any) %>% 
  summarise(mitjana_preu_mensual=sprintf("%0.1f",mean(price)),
            mitjana_superficie_mensual=sprintf("%0.1f",mean(surface)))
############################################################
############################################################
