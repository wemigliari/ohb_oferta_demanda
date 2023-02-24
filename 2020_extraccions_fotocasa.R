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

filenames <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20200101", pattern="*.snappy.parquet", full.names=TRUE)
ldf <- lapply(filenames, read_parquet)
class(ldf)
foto_demanda_gener <-  as.data.frame(do.call(rbind, ldf))

foto_demanda_gener <- filter(foto_demanda_gener, level1 %in%  c("Cataluña"))
count(foto_demanda_gener, "level1")

foto_demanda_gener$date <- as.Date(as.character(foto_demanda_gener$date), format = "%Y%m%d")
foto_demanda_gener <- foto_demanda_gener[order(foto_demanda_gener$date),]

foto_demanda_gener <- foto_demanda_gener %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_gener <- foto_demanda_gener %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames2 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20200201", pattern="*.snappy.parquet", full.names=TRUE)
ldf2 <- lapply(filenames2, read_parquet)
class(ldf2)
foto_demanda_febrer <-  as.data.frame(do.call(rbind, ldf2))

foto_demanda_febrer <- filter(foto_demanda_febrer, level1 %in%  c("Cataluña"))
count(foto_demanda_febrer, "level1")

foto_demanda_febrer$date <- as.Date(as.character(foto_demanda_febrer$date), format = "%Y%m%d")
foto_demanda_febrer <- foto_demanda_febrer[order(foto_demanda_febrer$date),]

foto_demanda_febrer <- foto_demanda_febrer %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_febrer <- foto_demanda_febrer %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames3 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20200301", pattern="*.snappy.parquet", full.names=TRUE)
ldf3 <- lapply(filenames3, read_parquet)
class(ldf3)
foto_demanda_marc <-  as.data.frame(do.call(rbind, ldf3))

foto_demanda_marc <- filter(foto_demanda_marc, level1 %in%  c("Cataluña"))
count(foto_demanda_marc, "level1")

foto_demanda_marc$date <- as.Date(as.character(foto_demanda_marc$date), format = "%Y%m%d")
foto_demanda_marc <- foto_demanda_marc[order(foto_demanda_marc$date),]

foto_demanda_marc <- foto_demanda_marc %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_marc <- foto_demanda_marc %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames4 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20200401", pattern="*.snappy.parquet", full.names=TRUE)
ldf4 <- lapply(filenames4, read_parquet)
class(ldf4)
foto_demanda_abril <-  as.data.frame(do.call(rbind, ldf4))

foto_demanda_abril <- filter(foto_demanda_abril, level1 %in%  c("Cataluña"))
count(foto_demanda_abril, "level1")

foto_demanda_abril$date <- as.Date(as.character(foto_demanda_abril$date), format = "%Y%m%d")
foto_demanda_abril <- foto_demanda_abril[order(foto_demanda_abril$date),]

foto_demanda_abril <- foto_demanda_abril %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_abril <- foto_demanda_abril %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames5 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20200501", pattern="*.snappy.parquet", full.names=TRUE)
ldf5 <- lapply(filenames5, read_parquet)
class(ldf5)
foto_demanda_maig <-  as.data.frame(do.call(rbind, ldf5))

foto_demanda_maig <- filter(foto_demanda_maig, level1 %in%  c("Cataluña"))
count(foto_demanda_maig, "level1")

foto_demanda_maig$date <- as.Date(as.character(foto_demanda_maig$date), format = "%Y%m%d")
foto_demanda_maig <- foto_demanda_maig[order(foto_demanda_maig$date),]

foto_demanda_maig <- foto_demanda_maig %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_maig <- foto_demanda_maig %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames6 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20200601", pattern="*.snappy.parquet", full.names=TRUE)
ldf6 <- lapply(filenames6, read_parquet)
class(ldf6)
foto_demanda_juny <-  as.data.frame(do.call(rbind, ldf6))

foto_demanda_juny <- filter(foto_demanda_juny, level1 %in%  c("Cataluña"))
count(foto_demanda_juny, "level1")

foto_demanda_juny$date <- as.Date(as.character(foto_demanda_juny$date), format = "%Y%m%d")
foto_demanda_juny <- foto_demanda_juny[order(foto_demanda_juny$date),]

foto_demanda_juny <- foto_demanda_juny %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_juny <- foto_demanda_juny %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames7 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20200701", pattern="*.snappy.parquet", full.names=TRUE)
ldf7 <- lapply(filenames7, read_parquet)
class(ldf7)
foto_demanda_juliol <- as.data.frame(do.call(rbind, ldf7))

foto_demanda_juliol <- filter(foto_demanda_juliol, level1 %in%  c("Cataluña"))
count(foto_demanda_juliol, "level1")

foto_demanda_juliol$date <- as.Date(as.character(foto_demanda_juliol$date), format = "%Y%m%d")
foto_demanda_juliol <- foto_demanda_juliol[order(foto_demanda_juliol$date),]

foto_demanda_juliol <- foto_demanda_juliol %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_juliol <- foto_demanda_juliol %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames8 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20200801", pattern="*.snappy.parquet", full.names=TRUE)
ldf8 <- lapply(filenames8, read_parquet)
class(ldf8)
foto_demanda_agost <- as.data.frame(do.call(rbind, ldf8))

foto_demanda_agost <- filter(foto_demanda_agost, level1 %in%  c("Cataluña"))
count(foto_demanda_agost, "level1")

foto_demanda_agost$date <- as.Date(as.character(foto_demanda_agost$date), format = "%Y%m%d")
foto_demanda_agost <- foto_demanda_agost[order(foto_demanda_agost$date),]

foto_demanda_agost <- foto_demanda_agost %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_agost <- foto_demanda_agost %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames9 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20200901", pattern="*.snappy.parquet", full.names=TRUE)
ldf9 <- lapply(filenames9, read_parquet)
class(ldf9)
foto_demanda_sept <- as.data.frame(do.call(rbind, ldf9))

foto_demanda_sept <- filter(foto_demanda_sept, level1 %in%  c("Cataluña"))
count(foto_demanda_sept, "level1")

foto_demanda_sept$date <- as.Date(as.character(foto_demanda_sept$date), format = "%Y%m%d")
foto_demanda_sept <- foto_demanda_sept[order(foto_demanda_sept$date),]

foto_demanda_sept <- foto_demanda_sept %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_sept <- foto_demanda_sept %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames10 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20201001", pattern="*.snappy.parquet", full.names=TRUE)
ldf10 <- lapply(filenames10, read_parquet)
class(ldf10)
foto_demanda_oct <- as.data.frame(do.call(rbind, ldf10))

foto_demanda_oct <- filter(foto_demanda_oct, level1 %in%  c("Cataluña"))
count(foto_demanda_oct, "level1")

foto_demanda_oct$date <- as.Date(as.character(foto_demanda_oct$date), format = "%Y%m%d")
foto_demanda_oct <- foto_demanda_oct[order(foto_demanda_oct$date),]

foto_demanda_oct <- foto_demanda_oct %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_oct <- foto_demanda_oct %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames11 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20201101", pattern="*.snappy.parquet", full.names=TRUE)
ldf11 <- lapply(filenames11, read_parquet)
class(ldf11)
foto_demanda_nov <- as.data.frame(do.call(rbind, ldf11))

foto_demanda_nov <- filter(foto_demanda_nov, level1 %in%  c("Cataluña"))
count(foto_demanda_nov, "level1")

foto_demanda_nov$date <- as.Date(as.character(foto_demanda_nov$date), format = "%Y%m%d")
foto_demanda_nov <- foto_demanda_nov[order(foto_demanda_nov$date),]

foto_demanda_nov <- foto_demanda_nov %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_nov <- foto_demanda_nov %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

filenames12 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/dt=20201201", pattern="*.snappy.parquet", full.names=TRUE)
ldf12 <- lapply(filenames12, read_parquet)
class(ldf12)
foto_demanda_des <- as.data.frame(do.call(rbind, ldf12))

foto_demanda_des <- filter(foto_demanda_des, level1 %in%  c("Cataluña"))
count(foto_demanda_des, "level1")

foto_demanda_des$date <- as.Date(as.character(foto_demanda_des$date), format = "%Y%m%d")
foto_demanda_des <- foto_demanda_des[order(foto_demanda_des$date),]

foto_demanda_des <- foto_demanda_des %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_demanda_des <- foto_demanda_des %>% 
  filter(!grepl(c('Share'), c(transaction_type)))

#########################################################

foto_demanda_2020 <- bind_rows(foto_demanda_gener,
                              foto_demanda_febrer,
                              foto_demanda_marc,
                              foto_demanda_maig,
                              foto_demanda_juny,
                              foto_demanda_juliol,
                              foto_demanda_agost,
                              foto_demanda_sept,
                              foto_demanda_oct,
                              foto_demanda_nov,
                              foto_demanda_des
                              )
class(foto_demanda_2020)
count(foto_demanda_2020, "level1")

write.csv(foto_demanda_2020, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Fotocasa_Demanda/extraccions_foto_demanda_2020.csv")


