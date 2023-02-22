library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
require(sf)
library(arrow)
library(plyr)
library(lubridate)
library(data.table)

options(scipen=999)

filenames <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210101", pattern="*.snappy.parquet", full.names=TRUE)
foto_gener_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210101/part-00000-tid-5741233288830307318-99abb73e-31f0-4e02-b242-8dc957c18608-116280-1.c000.snappy.parquet")
foto_gener_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210101/part-00001-tid-5741233288830307318-99abb73e-31f0-4e02-b242-8dc957c18608-116295-1.c000.snappy.parquet")
foto_gener_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210101/part-00002-tid-5741233288830307318-99abb73e-31f0-4e02-b242-8dc957c18608-116296-1.c000.snappy.parquet")
foto_gener_1 <- foto_gener_1[,-c(27:58)]
foto_gener_2 <- foto_gener_2[,-c(27:58)]
foto_gener_3 <- foto_gener_3[,-c(27:58)]


foto_ofer_gener <- bind_rows(foto_gener_1, foto_gener_2, foto_gener_3)
foto_ofer_gener <- filter(foto_ofer_gener, level1 %in%  c("Cataluña"))
count(foto_ofer_gener, "level1")

foto_ofer_gener <- foto_ofer_gener %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_gener <- foto_ofer_gener %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_gener <- foto_ofer_gener %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_gener$date <- as.Date(as.character(foto_ofer_gener$date), format = "%Y%m%d")
foto_ofer_gener <- foto_ofer_gener[order(foto_ofer_gener$date),]
foto_ofer_gener$mes <- foto_ofer_gener$date
rm(foto_gener_1, foto_gener_2, foto_gener_3)

#transform data to month names
month1 <- data.frame(foto_ofer_gener$mes <-month(ymd(foto_ofer_gener$mes)))
names(month1)[1] <- "mes"


#########################################################

filenames2 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210201/", pattern="*.snappy.parquet", full.names=TRUE)
foto_febrer_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210201/part-00000-tid-1408266466605245502-995c97d9-81d7-4625-9aa3-d578cfb9c729-116123-1.c000.snappy.parquet")
foto_febrer_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210201/part-00001-tid-1408266466605245502-995c97d9-81d7-4625-9aa3-d578cfb9c729-116134-1.c000.snappy.parquet")
foto_febrer_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210201/part-00002-tid-1408266466605245502-995c97d9-81d7-4625-9aa3-d578cfb9c729-116146-1.c000.snappy.parquet")

foto_febrer_1 <- foto_febrer_1[,-c(27:58)]
foto_febrer_2 <- foto_febrer_2[,-c(27:58)]
foto_febrer_3 <- foto_febrer_3[,-c(27:58)]

foto_ofer_febrer <- bind_rows(foto_febrer_1, foto_febrer_2, foto_febrer_3)
foto_ofer_febrer <- filter(foto_ofer_febrer, level1 %in%  c("Cataluña"))
count(foto_ofer_febrer, "level1")

foto_ofer_febrer <- foto_ofer_febrer %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_febrer <- foto_ofer_febrer %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_febrer <- foto_ofer_febrer %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_febrer$date <- as.Date(as.character(foto_ofer_febrer$date), format = "%Y%m%d")
foto_ofer_febrer <- foto_ofer_febrer[order(foto_ofer_febrer$date),]
foto_ofer_febrer$mes <- foto_ofer_febrer$date
rm(foto_febrer_1, foto_febrer_2, foto_febrer_3)

#transform data to month names
month2 <- data.frame(foto_ofer_febrer$mes <-month(ymd(foto_ofer_febrer$mes)))
names(month2)[1] <- "mes"

#########################################################

filenames3 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210301", pattern="*.snappy.parquet", full.names=TRUE)
foto_marc_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210301/part-00000-tid-6860812048028453996-db034ea5-e381-44ca-93a3-960c86026c50-116451-1.c000.snappy.parquet")
foto_marc_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210301/part-00001-tid-6860812048028453996-db034ea5-e381-44ca-93a3-960c86026c50-116466-1.c000.snappy.parquet")
foto_marc_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210301/part-00002-tid-6860812048028453996-db034ea5-e381-44ca-93a3-960c86026c50-116492-1.c000.snappy.parquet")
foto_marc_1 <- foto_marc_1[,-c(27:58)]
foto_marc_2 <- foto_marc_2[,-c(27:58)]
foto_marc_3 <- foto_marc_3[,-c(27:58)]

foto_ofer_marc <- bind_rows(foto_marc_1, foto_marc_2, foto_marc_3)
foto_ofer_marc <- filter(foto_ofer_marc, level1 %in%  c("Cataluña"))
count(foto_ofer_marc, "level1")

foto_ofer_marc <- foto_ofer_marc %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_marc <- foto_ofer_marc %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_marc <- foto_ofer_marc %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_marc$date <- as.Date(as.character(foto_ofer_marc$date), format = "%Y%m%d")
foto_ofer_marc <- foto_ofer_marc[order(foto_ofer_marc$date),]
foto_ofer_marc$mes <- foto_ofer_marc$date
rm(foto_marc_1, foto_marc_2, foto_marc_3)

#transform data to month names
month3 <- data.frame(foto_ofer_marc$mes <-month(ymd(foto_ofer_marc$mes)))
names(month3)[1] <- "mes"

#########################################################

filenames4 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210401", pattern="*.snappy.parquet", full.names=TRUE)
foto_abril_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210401/part-00000-tid-5679812884309962888-d32beb35-a409-4a57-a11d-756c18a7f3cf-116199-1.c000.snappy.parquet")
foto_abril_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210401/part-00001-tid-5679812884309962888-d32beb35-a409-4a57-a11d-756c18a7f3cf-116200-1.c000.snappy.parquet")
foto_abril_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210401/part-00002-tid-5679812884309962888-d32beb35-a409-4a57-a11d-756c18a7f3cf-116201-1.c000.snappy.parquet")
foto_abril_1 <- foto_abril_1[,-c(27:58)]
foto_abril_2 <- foto_abril_2[,-c(27:58)]
foto_abril_3 <- foto_abril_3[,-c(27:58)]

foto_ofer_abril <- bind_rows(foto_abril_1, foto_abril_2, foto_abril_3)
foto_ofer_abril <- filter(foto_ofer_abril, level1 %in%  c("Cataluña"))
count(foto_ofer_abril, "level1")

foto_ofer_abril <- foto_ofer_abril %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_abril <- foto_ofer_abril %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_abril <- foto_ofer_abril %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_abril$date <- as.Date(as.character(foto_ofer_abril$date), format = "%Y%m%d")
foto_ofer_abril <- foto_ofer_abril[order(foto_ofer_abril$date),]
foto_ofer_abril$mes <- foto_ofer_abril$date
rm(foto_abril_1, foto_abril_2, foto_abril_3)

#transform data to month names
month4 <- data.frame(foto_ofer_abril$mes <-month(ymd(foto_ofer_abril$mes)))
names(month4)[1] <- "mes"

#########################################################

filenames5 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210501", pattern="*.snappy.parquet", full.names=TRUE)
foto_maig_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210501/part-00000-tid-2676337070566620249-09139178-f771-4b33-be92-1618507a684f-116302-1.c000.snappy.parquet")
foto_maig_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210501/part-00001-tid-2676337070566620249-09139178-f771-4b33-be92-1618507a684f-116304-1.c000.snappy.parquet")
foto_maig_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210501/part-00002-tid-2676337070566620249-09139178-f771-4b33-be92-1618507a684f-116395-1.c000.snappy.parquet")
foto_maig_1 <- foto_maig_1[,-c(27:58)]
foto_maig_2 <- foto_maig_2[,-c(27:58)]
foto_maig_3 <- foto_maig_3[,-c(27:58)]

foto_ofer_maig <- bind_rows(foto_maig_1, foto_maig_2, foto_maig_3)
foto_ofer_maig <- filter(foto_ofer_maig, level1 %in%  c("Cataluña"))
count(foto_ofer_maig, "level1")

foto_ofer_maig <- foto_ofer_maig %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_maig <- foto_ofer_maig %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_maig <- foto_ofer_maig %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_maig$date <- as.Date(as.character(foto_ofer_maig$date), format = "%Y%m%d")
foto_ofer_maig <- foto_ofer_maig[order(foto_ofer_maig$date),]
foto_ofer_maig$mes <- foto_ofer_maig$date
rm(foto_maig_1, foto_maig_2, foto_maig_3)

#transform data to month names
month5 <- data.frame(foto_ofer_maig$mes <-month(ymd(foto_ofer_maig$mes)))
names(month5)[1] <- "mes"

#########################################################

filenames6 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210601", pattern="*.snappy.parquet", full.names=TRUE)
foto_juny_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210601/part-00000-tid-721656278757391811-18191781-963f-4d84-a8db-359057b5e69a-117384-1.c000.snappy.parquet")
foto_juny_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210601/part-00001-tid-721656278757391811-18191781-963f-4d84-a8db-359057b5e69a-117417-1.c000.snappy.parquet")
foto_juny_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210601/part-00002-tid-721656278757391811-18191781-963f-4d84-a8db-359057b5e69a-117419-1.c000.snappy.parquet")
foto_juny_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210601/part-00003-tid-721656278757391811-18191781-963f-4d84-a8db-359057b5e69a-117421-1.c000.snappy.parquet")
foto_juny_1 <- foto_juny_1[,-c(27:58)]
foto_juny_2 <- foto_juny_2[,-c(27:58)]
foto_juny_3 <- foto_juny_3[,-c(27:58)]
foto_juny_4 <- foto_juny_4[,-c(27:58)]

foto_ofer_juny <- bind_rows(foto_juny_1, foto_juny_2, foto_juny_3, foto_juny_4)
foto_ofer_juny <- filter(foto_ofer_juny, level1 %in%  c("Cataluña"))
count(foto_ofer_juny, "level1")

foto_ofer_juny <- foto_ofer_juny %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_juny <- foto_ofer_juny %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_juny <- foto_ofer_juny %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_juny$date <- as.Date(as.character(foto_ofer_juny$date), format = "%Y%m%d")
foto_ofer_juny <- foto_ofer_juny[order(foto_ofer_juny$date),]
foto_ofer_juny$mes <- foto_ofer_juny$date
rm(foto_juny_1, foto_juny_2, foto_juny_3, foto_juny_4)

#transform data to month names
month6 <- data.frame(foto_ofer_juny$mes <-month(ymd(foto_ofer_juny$mes)))
names(month6)[1] <- "mes"

#########################################################

filenames7 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210701", pattern="*.snappy.parquet", full.names=TRUE)
foto_juliol_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210701/part-00000-tid-841024417423034080-a981d012-e99d-4ceb-9278-4efac49422b1-117388-1.c000.snappy.parquet")
foto_juliol_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210701/part-00001-tid-841024417423034080-a981d012-e99d-4ceb-9278-4efac49422b1-117418-1.c000.snappy.parquet")
foto_juliol_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210701/part-00002-tid-841024417423034080-a981d012-e99d-4ceb-9278-4efac49422b1-117420-1.c000.snappy.parquet")
foto_juliol_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210701/part-00003-tid-841024417423034080-a981d012-e99d-4ceb-9278-4efac49422b1-117422-1.c000.snappy.parquet")
foto_juliol_1 <- foto_juliol_1[,-c(27:58)]
foto_juliol_2 <- foto_juliol_2[,-c(27:58)]
foto_juliol_3 <- foto_juliol_3[,-c(27:58)]
foto_juliol_4 <- foto_juliol_4[,-c(27:58)]

foto_ofer_juliol <- bind_rows(foto_juliol_1, foto_juliol_2, foto_juliol_3, foto_juliol_4)
foto_ofer_juliol <- filter(foto_ofer_juliol, level1 %in%  c("Cataluña"))
count(foto_ofer_juliol, "level1")

foto_ofer_juliol <- foto_ofer_juliol %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_juliol <- foto_ofer_juliol %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_juliol <- foto_ofer_juliol %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_juliol$date <- as.Date(as.character(foto_ofer_juliol$date), format = "%Y%m%d")
foto_ofer_juliol <- foto_ofer_juliol[order(foto_ofer_juliol$date),]
foto_ofer_juliol$mes <- foto_ofer_juliol$date
rm(foto_juliol_1, foto_juliol_2, foto_juliol_3, foto_juliol_4)

#transform data to month names
month7 <- data.frame(foto_ofer_juliol$mes <-month(ymd(foto_ofer_juliol$mes)))
names(month7)[1] <- "mes"

#########################################################

filenames8 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210801", pattern="*.snappy.parquet", full.names=TRUE)
foto_agost_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210801/part-00000-tid-8900290084594328374-22bde9f2-59a7-4743-beac-ebbfbbae91fe-116776-1.c000.snappy.parquet")
foto_agost_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210801/part-00001-tid-8900290084594328374-22bde9f2-59a7-4743-beac-ebbfbbae91fe-116777-1.c000.snappy.parquet")
foto_agost_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210801/part-00002-tid-8900290084594328374-22bde9f2-59a7-4743-beac-ebbfbbae91fe-116778-1.c000.snappy.parquet")
foto_agost_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210801/part-00003-tid-8900290084594328374-22bde9f2-59a7-4743-beac-ebbfbbae91fe-116779-1.c000.snappy.parquet")
foto_agost_1 <- foto_agost_1[,-c(27:58)]
foto_agost_2 <- foto_agost_2[,-c(27:58)]
foto_agost_3 <- foto_agost_3[,-c(27:58)]
foto_agost_4 <- foto_agost_4[,-c(27:58)]

foto_ofer_agost <- bind_rows(foto_agost_1, foto_agost_2, foto_agost_3, foto_agost_4)
foto_ofer_agost <- filter(foto_ofer_agost, level1 %in%  c("Cataluña"))
count(foto_ofer_agost, "level1")

foto_ofer_agost <- foto_ofer_agost %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_agost <- foto_ofer_agost %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_agost <- foto_ofer_agost %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_agost$date <- as.Date(as.character(foto_ofer_agost$date), format = "%Y%m%d")
foto_ofer_agost <- foto_ofer_agost[order(foto_ofer_agost$date),]
foto_ofer_agost$mes <- foto_ofer_agost$date
rm(foto_agost_1, foto_agost_2, foto_agost_3, foto_agost_4)

#transform data to month names
month8 <- data.frame(foto_ofer_agost$mes <-month(ymd(foto_ofer_agost$mes)))
names(month8)[1] <- "mes"

#########################################################

filenames9 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210901", pattern="*.snappy.parquet", full.names=TRUE)
foto_sept_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210901/part-00000-tid-2742574068752805123-0b4d3136-1290-4273-9e4e-75b801af5ee4-116978-1.c000.snappy.parquet")
foto_sept_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210901/part-00001-tid-2742574068752805123-0b4d3136-1290-4273-9e4e-75b801af5ee4-117014-1.c000.snappy.parquet")
foto_sept_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210901/part-00002-tid-2742574068752805123-0b4d3136-1290-4273-9e4e-75b801af5ee4-117015-1.c000.snappy.parquet")
foto_sept_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20210901/part-00003-tid-2742574068752805123-0b4d3136-1290-4273-9e4e-75b801af5ee4-117056-1.c000.snappy.parquet")
foto_sept_1 <- foto_sept_1[,-c(27:58)]
foto_sept_2 <- foto_sept_2[,-c(27:58)]
foto_sept_3 <- foto_sept_3[,-c(27:58)]
foto_sept_4 <- foto_sept_4[,-c(27:58)]

foto_ofer_sept <- bind_rows(foto_sept_1, foto_sept_2, foto_sept_3, foto_sept_4)
foto_ofer_sept <- filter(foto_ofer_sept, level1 %in%  c("Cataluña"))
count(foto_ofer_sept, "level1")

foto_ofer_sept <- foto_ofer_sept %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_sept <- foto_ofer_sept %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_sept <- foto_ofer_sept %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_sept$date <- as.Date(as.character(foto_ofer_sept$date), format = "%Y%m%d")
foto_ofer_sept <- foto_ofer_sept[order(foto_ofer_sept$date),]
foto_ofer_sept$mes <- foto_ofer_sept$date
rm(foto_sept_1, foto_sept_2, foto_sept_3, foto_sept_4)

#transform data to month names
month9 <- data.frame(foto_ofer_sept$mes <-month(ymd(foto_ofer_sept$mes)))
names(month9)[1] <- "mes"

#########################################################

filenames10 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211001", pattern="*.snappy.parquet", full.names=TRUE)
foto_oct_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211001/part-00000-tid-6937269423192972623-4dae4196-c91a-4d47-aaaf-2ec412e54cd1-117147-1.c000.snappy.parquet")
foto_oct_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211001/part-00001-tid-6937269423192972623-4dae4196-c91a-4d47-aaaf-2ec412e54cd1-117148-1.c000.snappy.parquet")
foto_oct_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211001/part-00002-tid-6937269423192972623-4dae4196-c91a-4d47-aaaf-2ec412e54cd1-117149-1.c000.snappy.parquet")
foto_oct_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211001/part-00003-tid-6937269423192972623-4dae4196-c91a-4d47-aaaf-2ec412e54cd1-117150-1.c000.snappy.parquet")
foto_oct_1 <- foto_oct_1[,-c(27:58)]
foto_oct_2 <- foto_oct_2[,-c(27:58)]
foto_oct_3 <- foto_oct_3[,-c(27:58)]
foto_oct_4 <- foto_oct_4[,-c(27:58)]

foto_ofer_oct <- bind_rows(foto_oct_1, foto_oct_2, foto_oct_3, foto_oct_4)
foto_ofer_oct <- filter(foto_ofer_oct, level1 %in%  c("Cataluña"))
count(foto_ofer_oct, "level1")

foto_ofer_oct <- foto_ofer_oct %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_oct <- foto_ofer_oct %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_oct <- foto_ofer_oct %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_oct$date <- as.Date(as.character(foto_ofer_oct$date), format = "%Y%m%d")
foto_ofer_oct <- foto_ofer_oct[order(foto_ofer_oct$date),]
foto_ofer_oct$mes <- foto_ofer_oct$date
rm(foto_oct_1, foto_oct_2, foto_oct_3, foto_oct_4)

#transform data to month names
month10 <- data.frame(foto_ofer_oct$mes <-month(ymd(foto_ofer_oct$mes)))
names(month10)[1] <- "mes"

#########################################################

filenames11 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211101", pattern="*.snappy.parquet", full.names=TRUE)
foto_nov_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211101/part-00000-tid-4605514374877821928-6627ab47-271b-4a6e-9814-1a00698af867-117292-1.c000.snappy.parquet")
foto_nov_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211101/part-00001-tid-4605514374877821928-6627ab47-271b-4a6e-9814-1a00698af867-117293-1.c000.snappy.parquet")
foto_nov_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211101/part-00002-tid-4605514374877821928-6627ab47-271b-4a6e-9814-1a00698af867-117316-1.c000.snappy.parquet")
foto_nov_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211101/part-00003-tid-4605514374877821928-6627ab47-271b-4a6e-9814-1a00698af867-117326-1.c000.snappy.parquet")
foto_nov_1 <- foto_nov_1[,-c(27:58)]
foto_nov_2 <- foto_nov_2[,-c(27:58)]
foto_nov_3 <- foto_nov_3[,-c(27:58)]
foto_nov_4 <- foto_nov_4[,-c(27:58)]

foto_ofer_nov <- bind_rows(foto_nov_1, foto_nov_2, foto_nov_3, foto_nov_4)
foto_ofer_nov <- filter(foto_ofer_nov, level1 %in%  c("Cataluña"))
count(foto_ofer_nov, "level1")

foto_ofer_nov <- foto_ofer_nov %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_nov <- foto_ofer_nov %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_nov <- foto_ofer_nov %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_nov$date <- as.Date(as.character(foto_ofer_nov$date), format = "%Y%m%d")
foto_ofer_nov <- foto_ofer_nov[order(foto_ofer_nov$date),]
foto_ofer_nov$mes <- foto_ofer_nov$date
rm(foto_nov_1, foto_nov_2, foto_nov_3, foto_nov_4)

#transform data to month names
month11 <- data.frame(foto_ofer_nov$mes <-month(ymd(foto_ofer_nov$mes)))
names(month11)[1] <- "mes"

#########################################################

filenames12 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211201", pattern="*.snappy.parquet", full.names=TRUE)
foto_des_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211201/part-00000-tid-1595501539359649052-98090da7-b170-4911-8392-4ce30811cb10-118220-1.c000.snappy.parquet")
foto_des_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211201/part-00001-tid-1595501539359649052-98090da7-b170-4911-8392-4ce30811cb10-118354-1.c000.snappy.parquet")
foto_des_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211201/part-00002-tid-1595501539359649052-98090da7-b170-4911-8392-4ce30811cb10-118446-1.c000.snappy.parquet")
foto_des_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/dt=20211201/part-00003-tid-1595501539359649052-98090da7-b170-4911-8392-4ce30811cb10-118541-1.c000.snappy.parquet")
foto_des_1 <- foto_des_1[,-c(27:58)]
foto_des_2 <- foto_des_2[,-c(27:58)]
foto_des_3 <- foto_des_3[,-c(27:58)]
foto_des_4 <- foto_des_4[,-c(27:58)]

foto_ofer_des <- bind_rows(foto_des_1, foto_des_2, foto_des_3, foto_des_4)
foto_ofer_des <- filter(foto_ofer_des, level1 %in%  c("Cataluña"))
count(foto_ofer_des, "level1")

foto_ofer_des <- foto_ofer_des %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_ofer_des <- foto_ofer_des %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_ofer_des <- foto_ofer_des %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

foto_ofer_des$date <- as.Date(as.character(foto_ofer_des$date), format = "%Y%m%d")
foto_ofer_des <- foto_ofer_des[order(foto_ofer_des$date),]
foto_ofer_des$mes <- foto_ofer_des$date
rm(foto_des_1, foto_des_2, foto_des_3, foto_des_4)

#transform data to month names
month12 <- data.frame(foto_ofer_des$mes <-month(ymd(foto_ofer_des$mes)))
names(month12)[1] <- "mes"

###############################

foto_oferta_2021 <- bind_rows(foto_ofer_gener,
                              foto_ofer_febrer,
                              foto_ofer_marc,
                              foto_ofer_abril,
                              foto_ofer_maig,
                              foto_ofer_juny,
                              foto_ofer_juliol,
                              foto_ofer_agost,
                              foto_ofer_sept,
                              foto_ofer_oct,
                              foto_ofer_nov,
                              foto_ofer_des
)

class(foto_oferta_2021)
count(foto_oferta_2021, "transaction_type")

foto_oferta_2021$date_posting <- as.Date(as.character(foto_oferta_2021$date_posting), format = "%Y%m%d")


write.csv(foto_oferta_2021, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2021_Fotocasa_Oferta/extraccions_foto_oferta_2021.csv")


