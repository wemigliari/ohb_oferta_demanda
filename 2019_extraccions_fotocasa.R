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

filenames <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190101", pattern="*.snappy.parquet", full.names=TRUE)
foto_gener_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190101/part-00000-tid-7064393009942742525-b3ca052c-4986-426c-bd9b-04476739fa8f-53220-1.c000.snappy.parquet")
foto_gener_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190101/part-00001-tid-7064393009942742525-b3ca052c-4986-426c-bd9b-04476739fa8f-53221-1.c000.snappy.parquet")
foto_gener_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190101/part-00002-tid-7064393009942742525-b3ca052c-4986-426c-bd9b-04476739fa8f-53222-1.c000.snappy.parquet")
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

filenames2 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190201/", pattern="*.snappy.parquet", full.names=TRUE)
foto_febrer_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190201/part-00000-tid-1778403609945268586-c060b45e-d2c4-42a9-8848-d7f9befc5f94-47240-1.c000.snappy.parquet")
foto_febrer_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190201/part-00001-tid-1778403609945268586-c060b45e-d2c4-42a9-8848-d7f9befc5f94-47241-1.c000.snappy.parquet")
foto_febrer_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190201/part-00002-tid-1778403609945268586-c060b45e-d2c4-42a9-8848-d7f9befc5f94-47311-1.c000.snappy.parquet")

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

filenames3 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190301", pattern="*.snappy.parquet", full.names=TRUE)
foto_marc_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190301/part-00000-tid-2480850211961936005-f6073d00-8256-47f9-b973-2a04a18af99a-54243-1.c000.snappy.parquet")
foto_marc_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190301/part-00001-tid-2480850211961936005-f6073d00-8256-47f9-b973-2a04a18af99a-54244-1.c000.snappy.parquet")
foto_marc_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190301/part-00002-tid-2480850211961936005-f6073d00-8256-47f9-b973-2a04a18af99a-54245-1.c000.snappy.parquet")
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

filenames4 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190401", pattern="*.snappy.parquet", full.names=TRUE)
foto_abril_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190401/part-00000-tid-1235848797128248644-b1093a54-b843-42d9-bc13-12f4adfb828c-60015-1.c000.snappy.parquet")
foto_abril_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190401/part-00001-tid-1235848797128248644-b1093a54-b843-42d9-bc13-12f4adfb828c-60016-1.c000.snappy.parquet")
foto_abril_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190401/part-00002-tid-1235848797128248644-b1093a54-b843-42d9-bc13-12f4adfb828c-60022-1.c000.snappy.parquet")
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

filenames5 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190501", pattern="*.snappy.parquet", full.names=TRUE)
foto_maig_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190501/part-00000-tid-3516006447967474240-902aff71-16f3-435d-8dd0-f7dceb169ced-90248-1.c000.snappy.parquet")
foto_maig_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190501/part-00001-tid-3516006447967474240-902aff71-16f3-435d-8dd0-f7dceb169ced-90254-1.c000.snappy.parquet")
foto_maig_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190501/part-00002-tid-3516006447967474240-902aff71-16f3-435d-8dd0-f7dceb169ced-90505-1.c000.snappy.parquet")
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

filenames6 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190601", pattern="*.snappy.parquet", full.names=TRUE)
foto_juny_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190601/part-00000-tid-6782968942084306005-9ffed132-40b1-4e8c-a271-ab7861a3a1cd-90280-1.c000.snappy.parquet")
foto_juny_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190601/part-00001-tid-6782968942084306005-9ffed132-40b1-4e8c-a271-ab7861a3a1cd-90514-1.c000.snappy.parquet")
foto_juny_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190601/part-00002-tid-6782968942084306005-9ffed132-40b1-4e8c-a271-ab7861a3a1cd-90591-1.c000.snappy.parquet")
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

filenames7 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190701", pattern="*.snappy.parquet", full.names=TRUE)
foto_juliol_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190701/part-00000-tid-6334085174399660118-d65e528b-b07f-4402-8428-df0b1f26a7e9-90442-1.c000.snappy.parquet")
foto_juliol_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190701/part-00001-tid-6334085174399660118-d65e528b-b07f-4402-8428-df0b1f26a7e9-90643-1.c000.snappy.parquet")
foto_juliol_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190701/part-00002-tid-6334085174399660118-d65e528b-b07f-4402-8428-df0b1f26a7e9-90644-1.c000.snappy.parquet")
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

filenames8 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190801", pattern="*.snappy.parquet", full.names=TRUE)
foto_agost_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190801/part-00000-tid-8734289817813160449-faee4e66-7be2-4606-816f-24cb15cb29d7-91418-1.c000.snappy.parquet")
foto_agost_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190801/part-00001-tid-8734289817813160449-faee4e66-7be2-4606-816f-24cb15cb29d7-91425-1.c000.snappy.parquet")
foto_agost_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190801/part-00002-tid-8734289817813160449-faee4e66-7be2-4606-816f-24cb15cb29d7-91510-1.c000.snappy.parquet")
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

filenames9 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190901", pattern="*.snappy.parquet", full.names=TRUE)
foto_sept_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190901/part-00000-tid-5699313218380010258-c116bf8c-982e-4764-865e-48616a6fc89d-91414-1.c000.snappy.parquet")
foto_sept_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190901/part-00001-tid-5699313218380010258-c116bf8c-982e-4764-865e-48616a6fc89d-91415-1.c000.snappy.parquet")
foto_sept_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20190901/part-00002-tid-5699313218380010258-c116bf8c-982e-4764-865e-48616a6fc89d-91417-1.c000.snappy.parquet")
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

filenames10 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191001", pattern="*.snappy.parquet", full.names=TRUE)
foto_oct_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191001/part-00000-tid-7259195211203301093-8d3771ff-9c29-4e11-91fd-066152ac5872-91405-1.c000.snappy.parquet")
foto_oct_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191001/part-00001-tid-7259195211203301093-8d3771ff-9c29-4e11-91fd-066152ac5872-91408-1.c000.snappy.parquet")
foto_oct_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191001/part-00002-tid-7259195211203301093-8d3771ff-9c29-4e11-91fd-066152ac5872-91413-1.c000.snappy.parquet")
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

filenames11 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191101", pattern="*.snappy.parquet", full.names=TRUE)
foto_nov_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191101/part-00000-tid-410064148731072445-5a999412-27ce-4824-a7e4-febc584288a2-91445-1.c000.snappy.parquet")
foto_nov_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191101/part-00001-tid-410064148731072445-5a999412-27ce-4824-a7e4-febc584288a2-91448-1.c000.snappy.parquet")
foto_nov_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191101/part-00002-tid-410064148731072445-5a999412-27ce-4824-a7e4-febc584288a2-91467-1.c000.snappy.parquet")
foto_nov_1 <- foto_nov_1[,-c(27:58)]
foto_nov_2 <- foto_nov_2[,-c(27:58)]
foto_nov_3 <- foto_nov_3[,-c(27:58)]

foto_ofer_nov <- bind_rows(foto_nov_1, foto_nov_2, foto_nov_3)
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
rm(foto_nov_1, foto_nov_2, foto_nov_3)

#transform data to month names
month11 <- data.frame(foto_ofer_nov$mes <-month(ymd(foto_ofer_nov$mes)))
names(month11)[1] <- "mes"

#########################################################

filenames12 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191201", pattern="*.snappy.parquet", full.names=TRUE)
foto_des_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191201/part-00000-tid-8689238237912483958-11516e74-14e1-4cd1-9e41-24dbbc6de3d0-91860-1.c000.snappy.parquet")
foto_des_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191201/part-00001-tid-8689238237912483958-11516e74-14e1-4cd1-9e41-24dbbc6de3d0-91861-1.c000.snappy.parquet")
foto_des_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/dt=20191201/part-00002-tid-8689238237912483958-11516e74-14e1-4cd1-9e41-24dbbc6de3d0-91862-1.c000.snappy.parquet")
foto_des_1 <- foto_des_1[,-c(27:58)]
foto_des_2 <- foto_des_2[,-c(27:58)]
foto_des_3 <- foto_des_3[,-c(27:58)]

foto_ofer_des <- bind_rows(foto_des_1, foto_des_2, foto_des_3)
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
rm(foto_des_1, foto_des_2, foto_des_3)

#transform data to month names
month12 <- data.frame(foto_ofer_des$mes <-month(ymd(foto_ofer_des$mes)))
names(month12)[1] <- "mes"

#########################################################

foto_oferta_2019 <- bind_rows(foto_ofer_gener,
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

class(foto_oferta_2019)
count(foto_oferta_2019, "transaction_type")
foto_oferta_2019 <- foto_oferta_2019 %>% 
  filter(!grepl(c('Transfer'), c(transaction_type)))

foto_oferta_2019$date_posting <- as.Date(as.character(foto_oferta_2019$date_posting), format = "%Y%m%d")

write.csv(foto_oferta_2019, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2019_Fotocasa_Oferta/extraccions_foto_oferta_2019.csv")

