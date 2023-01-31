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

filenames <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220101", pattern="*.snappy.parquet", full.names=TRUE)
foto_gener_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220101/part-00000-tid-7812022928398819517-7a836ef0-163e-42ce-a063-d1e4de14c105-125504-1.c000.snappy.parquet")
foto_gener_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220101/part-00001-tid-7812022928398819517-7a836ef0-163e-42ce-a063-d1e4de14c105-125601-1.c000.snappy.parquet")
foto_gener_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220101/part-00002-tid-7812022928398819517-7a836ef0-163e-42ce-a063-d1e4de14c105-125670-1.c000.snappy.parquet")
foto_gener_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220101/part-00003-tid-7812022928398819517-7a836ef0-163e-42ce-a063-d1e4de14c105-125909-1.c000.snappy.parquet")
foto_gener_1 <- foto_gener_1[,-c(27:58)]
foto_gener_2 <- foto_gener_2[,-c(27:58)]
foto_gener_3 <- foto_gener_3[,-c(27:58)]
foto_gener_4 <- foto_gener_4[,-c(27:58)]

foto_ofer_gener <- bind_rows(foto_gener_1, foto_gener_2, foto_gener_3, foto_gener_4)
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
rm(foto_gener_1, foto_gener_2, foto_gener_3, foto_gener_4)

#transform data to month names
month1 <- data.frame(foto_ofer_gener$mes <-month(ymd(foto_ofer_gener$mes)))
names(month1)[1] <- "mes"

#########################################################

filenames2 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220201", pattern="*.snappy.parquet", full.names=TRUE)
foto_febrer_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220201/part-00000-tid-1157573289571180571-e96b5fd0-7a19-473b-9fa0-b6613343fa9d-129592-1.c000.snappy.parquet")
foto_febrer_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220201/part-00001-tid-1157573289571180571-e96b5fd0-7a19-473b-9fa0-b6613343fa9d-129593-1.c000.snappy.parquet")
foto_febrer_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220201/part-00002-tid-1157573289571180571-e96b5fd0-7a19-473b-9fa0-b6613343fa9d-129594-1.c000.snappy.parquet")
foto_febrer_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220201/part-00003-tid-1157573289571180571-e96b5fd0-7a19-473b-9fa0-b6613343fa9d-129595-1.c000.snappy.parquet")
foto_febrer_1 <- foto_febrer_1[,-c(27:58)]
foto_febrer_2 <- foto_febrer_2[,-c(27:58)]
foto_febrer_3 <- foto_febrer_3[,-c(27:58)]
foto_febrer_4 <- foto_febrer_4[,-c(27:58)]

foto_ofer_febrer <- bind_rows(foto_febrer_1, foto_febrer_2, foto_febrer_3, foto_febrer_4)
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
rm(foto_febrer_1, foto_febrer_2, foto_febrer_3, foto_febrer_4)

#transform data to month names
month2 <- data.frame(foto_ofer_febrer$mes <-month(ymd(foto_ofer_febrer$mes)))
names(month2)[1] <- "mes"

#########################################################

filenames3 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220301", pattern="*.snappy.parquet", full.names=TRUE)
foto_marc_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220301/part-00000-tid-5344431917765658807-c96f9f53-0ac0-4ae0-9ccb-330f43a305eb-129602-1.c000.snappy.parquet")
foto_marc_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220301/part-00001-tid-5344431917765658807-c96f9f53-0ac0-4ae0-9ccb-330f43a305eb-129603-1.c000.snappy.parquet")
foto_marc_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220301/part-00002-tid-5344431917765658807-c96f9f53-0ac0-4ae0-9ccb-330f43a305eb-129604-1.c000.snappy.parquet")
foto_marc_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220301/part-00003-tid-5344431917765658807-c96f9f53-0ac0-4ae0-9ccb-330f43a305eb-129605-1.c000.snappy.parquet")
foto_marc_1 <- foto_marc_1[,-c(27:58)]
foto_marc_2 <- foto_marc_2[,-c(27:58)]
foto_marc_3 <- foto_marc_3[,-c(27:58)]
foto_marc_4 <- foto_marc_4[,-c(27:58)]

foto_ofer_marc <- bind_rows(foto_marc_1, foto_marc_2, foto_marc_3, foto_marc_4)
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
rm(foto_marc_1, foto_marc_2, foto_marc_3, foto_marc_4)

#transform data to month names
month3 <- data.frame(foto_ofer_marc$mes <-month(ymd(foto_ofer_marc$mes)))
names(month3)[1] <- "mes"

#########################################################

filenames4 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220401", pattern="*.snappy.parquet", full.names=TRUE)
foto_abril_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220401/part-00000-tid-4954320206141668838-cf71b11e-d468-480f-9dae-186a098f76a8-129771-1.c000.snappy.parquet")
foto_abril_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220401/part-00001-tid-4954320206141668838-cf71b11e-d468-480f-9dae-186a098f76a8-129772-1.c000.snappy.parquet")
foto_abril_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220401/part-00002-tid-4954320206141668838-cf71b11e-d468-480f-9dae-186a098f76a8-129773-1.c000.snappy.parquet")
foto_abril_4 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220401/part-00003-tid-4954320206141668838-cf71b11e-d468-480f-9dae-186a098f76a8-129774-1.c000.snappy.parquet")
foto_abril_1 <- foto_abril_1[,-c(27:58)]
foto_abril_2 <- foto_abril_2[,-c(27:58)]
foto_abril_3 <- foto_abril_3[,-c(27:58)]
foto_abril_4 <- foto_abril_4[,-c(27:58)]

foto_ofer_abril <- bind_rows(foto_abril_1, foto_abril_2, foto_abril_3, foto_abril_4)
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
rm(foto_abril_1, foto_abril_2, foto_abril_3, foto_abril_4)

#transform data to month names
month4 <- data.frame(foto_ofer_abril$mes <-month(ymd(foto_ofer_abril$mes)))
names(month4)[1] <- "mes"

#########################################################

filenames5 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220501", pattern="*.snappy.parquet", full.names=TRUE)
foto_maig_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220501/part-00000-tid-1220695848846489815-35a68c8f-26e4-4e46-8f16-e5a96107b614-129781-1.c000.snappy.parquet")
foto_maig_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220501/part-00001-tid-1220695848846489815-35a68c8f-26e4-4e46-8f16-e5a96107b614-129782-1.c000.snappy.parquet")
foto_maig_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220501/part-00002-tid-1220695848846489815-35a68c8f-26e4-4e46-8f16-e5a96107b614-129783-1.c000.snappy.parquet")
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

filenames6 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220601", pattern="*.snappy.parquet", full.names=TRUE)
foto_juny_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220601/part-00000-tid-671171860145196002-4837f406-50fb-473f-8f64-645ab6fa8a04-129784-1.c000.snappy.parquet")
foto_juny_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220601/part-00001-tid-671171860145196002-4837f406-50fb-473f-8f64-645ab6fa8a04-129785-1.c000.snappy.parquet")
foto_juny_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220601/part-00002-tid-671171860145196002-4837f406-50fb-473f-8f64-645ab6fa8a04-129786-1.c000.snappy.parquet")
foto_juny_1 <- foto_juny_1[,-c(27:58)]
foto_juny_2 <- foto_juny_2[,-c(27:58)]
foto_juny_3 <- foto_juny_3[,-c(27:58)]

foto_ofer_juny <- bind_rows(foto_juny_1, foto_juny_2, foto_juny_3)
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
rm(foto_juny_1, foto_juny_2, foto_juny_3)

#transform data to month names
month6 <- data.frame(foto_ofer_juny$mes <-month(ymd(foto_ofer_juny$mes)))
names(month6)[1] <- "mes"

#########################################################

filenames7 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220701", pattern="*.snappy.parquet", full.names=TRUE)
foto_juliol_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220701/part-00000-tid-2271633444302105428-240887d0-113f-4c7c-96a2-9bb022522792-129778-1.c000.snappy.parquet")
foto_juliol_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220701/part-00001-tid-2271633444302105428-240887d0-113f-4c7c-96a2-9bb022522792-129779-1.c000.snappy.parquet")
foto_juliol_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220701/part-00002-tid-2271633444302105428-240887d0-113f-4c7c-96a2-9bb022522792-129780-1.c000.snappy.parquet")
foto_juliol_1 <- foto_juliol_1[,-c(27:58)]
foto_juliol_2 <- foto_juliol_2[,-c(27:58)]
foto_juliol_3 <- foto_juliol_3[,-c(27:58)]

foto_ofer_juliol <- bind_rows(foto_juliol_1, foto_juliol_2, foto_juliol_3)
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
rm(foto_juliol_1, foto_juliol_2, foto_juliol_3)

#transform data to month names
month7 <- data.frame(foto_ofer_juliol$mes <-month(ymd(foto_ofer_juliol$mes)))
names(month7)[1] <- "mes"

#########################################################

filenames8 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220801", pattern="*.snappy.parquet", full.names=TRUE)
foto_agost_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220801/part-00000-tid-446181061098741977-3d812136-f179-4c73-b1a6-633d478b47b5-129775-1.c000.snappy.parquet")
foto_agost_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220801/part-00001-tid-446181061098741977-3d812136-f179-4c73-b1a6-633d478b47b5-129776-1.c000.snappy.parquet")
foto_agost_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220801/part-00002-tid-446181061098741977-3d812136-f179-4c73-b1a6-633d478b47b5-129777-1.c000.snappy.parquet")
foto_agost_1 <- foto_agost_1[,-c(27:58)]
foto_agost_2 <- foto_agost_2[,-c(27:58)]
foto_agost_3 <- foto_agost_3[,-c(27:58)]

foto_ofer_agost <- bind_rows(foto_agost_1, foto_agost_2, foto_agost_3)
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
rm(foto_agost_1, foto_agost_2, foto_agost_3)

#transform data to month names
month8 <- data.frame(foto_ofer_agost$mes <-month(ymd(foto_ofer_agost$mes)))
names(month8)[1] <- "mes"

#########################################################

filenames9 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220901", pattern="*.snappy.parquet", full.names=TRUE)
foto_sept_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220901/part-00000-tid-3922703553589852347-863b232d-d999-4c96-95b9-fcfb19ba0306-129787-1.c000.snappy.parquet")
foto_sept_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220901/part-00001-tid-3922703553589852347-863b232d-d999-4c96-95b9-fcfb19ba0306-129788-1.c000.snappy.parquet")
foto_sept_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20220901/part-00002-tid-3922703553589852347-863b232d-d999-4c96-95b9-fcfb19ba0306-129789-1.c000.snappy.parquet")
foto_sept_1 <- foto_sept_1[,-c(27:58)]
foto_sept_2 <- foto_sept_2[,-c(27:58)]
foto_sept_3 <- foto_sept_3[,-c(27:58)]

foto_ofer_sept <- bind_rows(foto_sept_1, foto_sept_2, foto_sept_3)
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
rm(foto_sept_1, foto_sept_2, foto_sept_3)

#transform data to month names
month9 <- data.frame(foto_ofer_sept$mes <-month(ymd(foto_ofer_sept$mes)))
names(month9)[1] <- "mes"

#########################################################

filenames10 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20221001", pattern="*.snappy.parquet", full.names=TRUE)
foto_oct_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20221001/part-00000-tid-4428605285436053369-321b5625-296e-4b0c-aa5e-57e0c3f0c2fc-17606-1.c000.snappy.parquet")
foto_oct_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20221001/part-00001-tid-4428605285436053369-321b5625-296e-4b0c-aa5e-57e0c3f0c2fc-17607-1.c000.snappy.parquet")
foto_oct_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/dt=20221001/part-00002-tid-4428605285436053369-321b5625-296e-4b0c-aa5e-57e0c3f0c2fc-17608-1.c000.snappy.parquet")
foto_oct_1 <- foto_oct_1[,-c(27:58)]
foto_oct_2 <- foto_oct_2[,-c(27:58)]
foto_oct_3 <- foto_oct_3[,-c(27:58)]

foto_ofer_oct <- bind_rows(foto_oct_1, foto_oct_2, foto_oct_3)
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
rm(foto_oct_1, foto_oct_2, foto_oct_3)

#transform data to month names
month10 <- data.frame(foto_ofer_oct$mes <-month(ymd(foto_ofer_oct$mes)))
names(month10)[1] <- "mes"

#########################################################

foto_oferta_2022 <- bind_rows(foto_ofer_gener,
                              foto_ofer_febrer,
                              foto_ofer_marc,
                              foto_ofer_abril,
                              foto_ofer_maig,
                              foto_ofer_juny,
                              foto_ofer_juliol,
                              foto_ofer_agost,
                              foto_ofer_sept,
                              foto_ofer_oct
)
class(foto_oferta_2022)
count(foto_oferta_2022, "transaction_type")

foto_oferta_2022$date_posting <- as.Date(as.character(foto_oferta_2022$date_posting), format = "%Y%m%d")

write.csv(foto_oferta_2022, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/extraccions_foto_oferta_2022.csv")


######### Hi havia property_ids que surtien com "Sell", però, en realitat, eran "Rent". El Joffre m'ha enviat
######### els "ids" i els he copiat en una taula d'Excel. Vaig importar la taula d'Excel i seguir els passos 
######### de les següents línies de codi abaix:
#ids_fotocasa <- read.xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2022_Fotocasa_Oferta/ids_fotocasa.xlsx",
                          #sheetName = "Sheet1")
#ids_fotocasa <- merge(test10, ids_fotocasa, by="property_id")
#names(ids_fotocasa)[29] <- "transaction_type"
#ids_fotocasa$transaction_type.x <- NULL
#ids_fotocasa <- ids_fotocasa[,c(1, 2, 28, 3:27)]
#test10 <- bind_rows(test10, ids_fotocasa)

