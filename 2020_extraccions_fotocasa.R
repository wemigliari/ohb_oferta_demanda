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

filenames <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200101", pattern="*.snappy.parquet", full.names=TRUE)
foto_gener_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200101/part-00000-tid-4949556690695195037-691d2fb6-a353-4c18-893a-7f55438bf1c4-92212-1.c000.snappy.parquet")
foto_gener_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200101/part-00001-tid-4949556690695195037-691d2fb6-a353-4c18-893a-7f55438bf1c4-92213-1.c000.snappy.parquet")
foto_gener_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200101/part-00002-tid-4949556690695195037-691d2fb6-a353-4c18-893a-7f55438bf1c4-92214-1.c000.snappy.parquet")
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

filenames2 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200201/", pattern="*.snappy.parquet", full.names=TRUE)
foto_febrer_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200201/part-00000-tid-8729183780189181905-c7a50e65-7d83-46f1-a9ea-24ba6b67111b-91855-1.c000.snappy.parquet/")
foto_febrer_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200201/part-00001-tid-8729183780189181905-c7a50e65-7d83-46f1-a9ea-24ba6b67111b-91856-1.c000.snappy.parquet")
foto_febrer_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200201/part-00002-tid-8729183780189181905-c7a50e65-7d83-46f1-a9ea-24ba6b67111b-91857-1.c000.snappy.parquet")

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

filenames3 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200301", pattern="*.snappy.parquet", full.names=TRUE)
foto_marc_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200301/part-00000-tid-217363103198177543-c67daa07-78a7-4bc8-bf0a-4cb0bca7291a-92045-1.c000.snappy.parquet")
foto_marc_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200301/part-00001-tid-217363103198177543-c67daa07-78a7-4bc8-bf0a-4cb0bca7291a-92046-1.c000.snappy.parquet")
foto_marc_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200301/part-00002-tid-217363103198177543-c67daa07-78a7-4bc8-bf0a-4cb0bca7291a-92047-1.c000.snappy.parquet")
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

filenames4 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200401", pattern="*.snappy.parquet", full.names=TRUE)
foto_abril_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200401/part-00000-tid-1855683143874282054-5b8739b8-c32d-4b61-8c3b-687948611706-91868-1.c000.snappy.parquet")
foto_abril_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200401/part-00001-tid-1855683143874282054-5b8739b8-c32d-4b61-8c3b-687948611706-91869-1.c000.snappy.parquet")
foto_abril_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200401/part-00002-tid-1855683143874282054-5b8739b8-c32d-4b61-8c3b-687948611706-91873-1.c000.snappy.parquet")
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

filenames5 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200501", pattern="*.snappy.parquet", full.names=TRUE)
foto_maig_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200501/part-00000-tid-5909712806089975865-785b791a-ede8-4b3b-a019-1787eb2c9231-91295-1.c000.snappy.parquet")
foto_maig_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200501/part-00001-tid-5909712806089975865-785b791a-ede8-4b3b-a019-1787eb2c9231-91296-1.c000.snappy.parquet")
foto_maig_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200501/part-00002-tid-5909712806089975865-785b791a-ede8-4b3b-a019-1787eb2c9231-91335-1.c000.snappy.parquet")
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

filenames6 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200601", pattern="*.snappy.parquet", full.names=TRUE)
foto_juny_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200601/part-00000-tid-8848088813452715126-ffbc079f-92bc-4ae0-babc-ac097cb099e7-92279-1.c000.snappy.parquet")
foto_juny_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200601/part-00001-tid-8848088813452715126-ffbc079f-92bc-4ae0-babc-ac097cb099e7-92280-1.c000.snappy.parquet")
foto_juny_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200601/part-00002-tid-8848088813452715126-ffbc079f-92bc-4ae0-babc-ac097cb099e7-92281-1.c000.snappy.parquet")
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

filenames7 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200701", pattern="*.snappy.parquet", full.names=TRUE)
foto_juliol_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200701/part-00000-tid-3117660631261391964-290ce9d5-7acd-4172-988d-c2bea5b1a1d4-92811-1.c000.snappy.parquet")
foto_juliol_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200701/part-00001-tid-3117660631261391964-290ce9d5-7acd-4172-988d-c2bea5b1a1d4-92848-1.c000.snappy.parquet")
foto_juliol_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200701/part-00002-tid-3117660631261391964-290ce9d5-7acd-4172-988d-c2bea5b1a1d4-92855-1.c000.snappy.parquet")
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

filenames8 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200801", pattern="*.snappy.parquet", full.names=TRUE)
foto_agost_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200801/part-00000-tid-1230203057719988349-1f2e7390-58a7-44e4-b729-e1c2c04cfa32-95256-1.c000.snappy.parquet")
foto_agost_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200801/part-00001-tid-1230203057719988349-1f2e7390-58a7-44e4-b729-e1c2c04cfa32-95282-1.c000.snappy.parquet")
foto_agost_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200801/part-00002-tid-1230203057719988349-1f2e7390-58a7-44e4-b729-e1c2c04cfa32-95305-1.c000.snappy.parquet")
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

filenames9 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200901", pattern="*.snappy.parquet", full.names=TRUE)
foto_sept_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200901/part-00000-tid-6711676009867602318-b6d57951-8250-49f8-9087-b3aab39d29a1-111551-1.c000.snappy.parquet")
foto_sept_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200901/part-00001-tid-6711676009867602318-b6d57951-8250-49f8-9087-b3aab39d29a1-111552-1.c000.snappy.parquet")
foto_sept_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20200901/part-00002-tid-6711676009867602318-b6d57951-8250-49f8-9087-b3aab39d29a1-111553-1.c000.snappy.parquet")
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

filenames10 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201001", pattern="*.snappy.parquet", full.names=TRUE)
foto_oct_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201001/part-00000-tid-2631791846070731194-b85b6aeb-9b55-4250-a983-4d5074adbf4d-115284-1.c000.snappy.parquet")
foto_oct_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201001/part-00001-tid-2631791846070731194-b85b6aeb-9b55-4250-a983-4d5074adbf4d-115285-1.c000.snappy.parquet")
foto_oct_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201001/part-00002-tid-2631791846070731194-b85b6aeb-9b55-4250-a983-4d5074adbf4d-115286-1.c000.snappy.parquet")
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

filenames11 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201101", pattern="*.snappy.parquet", full.names=TRUE)
foto_nov_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201101/part-00000-tid-5307212355551049995-bd13ea6d-2f37-4852-a16c-37031f455ed2-114948-1.c000.snappy.parquet")
foto_nov_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201101/part-00001-tid-5307212355551049995-bd13ea6d-2f37-4852-a16c-37031f455ed2-114949-1.c000.snappy.parquet")
foto_nov_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201101/part-00002-tid-5307212355551049995-bd13ea6d-2f37-4852-a16c-37031f455ed2-114950-1.c000.snappy.parquet")
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

filenames12 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201201", pattern="*.snappy.parquet", full.names=TRUE)
foto_des_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201201/part-00000-tid-6935233197586234688-aa402ab4-14a2-4a98-96ce-1f556b297dc7-116363-1.c000.snappy.parquet")
foto_des_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201201/part-00001-tid-6935233197586234688-aa402ab4-14a2-4a98-96ce-1f556b297dc7-116368-1.c000.snappy.parquet")
foto_des_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/dt=20201201/part-00002-tid-6935233197586234688-aa402ab4-14a2-4a98-96ce-1f556b297dc7-116396-1.c000.snappy.parquet")
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

###############################

foto_oferta_2020 <- bind_rows(foto_ofer_gener,
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

class(foto_oferta_2020)
count(foto_oferta_2020, "transaction_type")
foto_oferta_2020 <- foto_oferta_2020 %>% 
  filter(!grepl(c('Transfer'), c(transaction_type)))

foto_oferta_2020$date_posting <- as.Date(as.character(foto_oferta_2020$date_posting), format = "%Y%m%d")

write.csv(foto_oferta_2020, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Oferta/extraccions_foto_oferta_2020.csv")


