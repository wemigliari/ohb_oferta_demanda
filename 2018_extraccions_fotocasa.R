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

filenames <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180101", pattern="*.snappy.parquet", full.names=TRUE)
foto_gener_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180101/part-00000-tid-7573259910137785982-aed9840a-59f1-45d0-8c8f-135123a6ac18-46466-1.c000.snappy.parquet")
foto_gener_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180101/part-00001-tid-7573259910137785982-aed9840a-59f1-45d0-8c8f-135123a6ac18-46478-1.c000.snappy.parquet")
foto_gener_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180101/part-00002-tid-7573259910137785982-aed9840a-59f1-45d0-8c8f-135123a6ac18-47713-1.c000.snappy.parquet")
foto_gener_1 <- foto_gener_1[,-c(27:58)]
foto_gener_2 <- foto_gener_2[,-c(27:58)]
foto_gener_3 <- foto_gener_3[,-c(27:58)]


foto_ofer_gener <- bind_rows(foto_gener_1, foto_gener_2, foto_gener_3)
foto_ofer_gener <- filter(foto_ofer_gener, level1 %in%  c("Cataluña"))
count(foto_ofer_gener, "level1")

foto_ofer_gener$date <- as.Date(as.character(foto_ofer_gener$date), format = "%Y%m%d")
foto_ofer_gener <- foto_ofer_gener[order(foto_ofer_gener$date),]
foto_ofer_gener$mes <- foto_ofer_gener$date

#transform data to month names
month1 <- data.frame(foto_ofer_gener$mes <-month(ymd(foto_ofer_gener$mes)))
names(month1)[1] <- "mes"

test1 <-  unique(setDT(foto_ofer_gener), by = c("property_id", "mes"))
test1$property_id <- test1$property_id
count(test1, "mes")
counting <- test1 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test1 <- merge(test1, counting, by = "property_id")
test1 <- test1[order(test1$property_id),]
count(test1, "numero_mesos_anunci_apareix")

rm(foto_gener_1, foto_gener_2, foto_gener_3)
#########################################################

filenames2 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180201/", pattern="*.snappy.parquet", full.names=TRUE)
foto_febrer_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180201/part-00000-tid-1252829383050113523-7866ff05-507e-489e-9140-28c67f0b307f-48050-1.c000.snappy.parquet")
foto_febrer_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180201/part-00001-tid-1252829383050113523-7866ff05-507e-489e-9140-28c67f0b307f-48174-1.c000.snappy.parquet")
foto_febrer_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180201/part-00002-tid-1252829383050113523-7866ff05-507e-489e-9140-28c67f0b307f-48441-1.c000.snappy.parquet")

foto_febrer_1 <- foto_febrer_1[,-c(27:58)]
foto_febrer_2 <- foto_febrer_2[,-c(27:58)]
foto_febrer_3 <- foto_febrer_3[,-c(27:58)]

foto_ofer_febrer <- bind_rows(foto_febrer_1, foto_febrer_2, foto_febrer_3)
foto_ofer_febrer <- filter(foto_ofer_febrer, level1 %in%  c("Cataluña"))
count(foto_ofer_febrer, "level1")

foto_ofer_febrer$date <- as.Date(as.character(foto_ofer_febrer$date), format = "%Y%m%d")
foto_ofer_febrer <- foto_ofer_febrer[order(foto_ofer_febrer$date),]
foto_ofer_febrer$mes <- foto_ofer_febrer$date

#transform data to month names
month2 <- data.frame(foto_ofer_febrer$mes <-month(ymd(foto_ofer_febrer$mes)))
names(month2)[1] <- "mes"

test2 <-  unique(setDT(foto_ofer_febrer), by = c("property_id", "mes"))
test2$property_id <- test2$property_id
count(test2, "mes")
counting <- test2 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test2 <- merge(test2, counting, by = "property_id")
test2 <- test2[order(test2$property_id),]
count(test2, "numero_mesos_anunci_apareix")

rm(foto_febrer_1, foto_febrer_2, foto_febrer_3)
#########################################################

filenames3 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180301", pattern="*.snappy.parquet", full.names=TRUE)
foto_marc_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180301/part-00000-tid-8617254518527058058-afd79989-f67a-4371-8bb1-5d3a7f4060f1-37883-1.c000.snappy.parquet")
foto_marc_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180301/part-00001-tid-8617254518527058058-afd79989-f67a-4371-8bb1-5d3a7f4060f1-37884-1.c000.snappy.parquet")
foto_marc_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180301/part-00002-tid-8617254518527058058-afd79989-f67a-4371-8bb1-5d3a7f4060f1-37885-1.c000.snappy.parquet")
foto_marc_1 <- foto_marc_1[,-c(27:58)]
foto_marc_2 <- foto_marc_2[,-c(27:58)]
foto_marc_3 <- foto_marc_3[,-c(27:58)]

foto_ofer_marc <- bind_rows(foto_marc_1, foto_marc_2, foto_marc_3)
foto_ofer_marc <- filter(foto_ofer_marc, level1 %in%  c("Cataluña"))
count(foto_ofer_marc, "level1")

foto_ofer_marc$date <- as.Date(as.character(foto_ofer_marc$date), format = "%Y%m%d")
foto_ofer_marc <- foto_ofer_marc[order(foto_ofer_marc$date),]
foto_ofer_marc$mes <- foto_ofer_marc$date

#transform data to month names
month3 <- data.frame(foto_ofer_marc$mes <-month(ymd(foto_ofer_marc$mes)))
names(month3)[1] <- "mes"

test3 <-  unique(setDT(foto_ofer_marc), by = c("property_id", "mes"))
test3$property_id <- test3$property_id
count(test3, "mes")
counting <- test3 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test3 <- merge(test3, counting, by = "property_id")
test3 <- test3[order(test3$property_id),]
count(test3, "numero_mesos_anunci_apareix")

rm(foto_marc_1, foto_marc_2, foto_marc_3)
#########################################################

filenames4 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180401", pattern="*.snappy.parquet", full.names=TRUE)
foto_abril_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180401/part-00000-tid-8726740395297501555-65527b35-addd-41fb-8875-e87a89d36ef2-42956-1.c000.snappy.parquet")
foto_abril_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180401/part-00001-tid-8726740395297501555-65527b35-addd-41fb-8875-e87a89d36ef2-45357-1.c000.snappy.parquet")
foto_abril_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180401/part-00002-tid-8726740395297501555-65527b35-addd-41fb-8875-e87a89d36ef2-45514-1.c000.snappy.parquet")
foto_abril_1 <- foto_abril_1[,-c(27:58)]
foto_abril_2 <- foto_abril_2[,-c(27:58)]
foto_abril_3 <- foto_abril_3[,-c(27:58)]

foto_ofer_abril <- bind_rows(foto_abril_1, foto_abril_2, foto_abril_3)
foto_ofer_abril <- filter(foto_ofer_abril, level1 %in%  c("Cataluña"))
count(foto_ofer_abril, "level1")

foto_ofer_abril$date <- as.Date(as.character(foto_ofer_abril$date), format = "%Y%m%d")
foto_ofer_abril <- foto_ofer_abril[order(foto_ofer_abril$date),]
foto_ofer_abril$mes <- foto_ofer_abril$date

#transform data to month names
month4 <- data.frame(foto_ofer_abril$mes <-month(ymd(foto_ofer_abril$mes)))
names(month4)[1] <- "mes"

test4 <-  unique(setDT(foto_ofer_abril), by = c("property_id", "mes"))
test4$property_id <- test4$property_id
count(test4, "mes")
counting <- test4 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test4 <- merge(test4, counting, by = "property_id")
test4 <- test4[order(test4$property_id),]
count(test4, "numero_mesos_anunci_apareix")

rm(foto_abril_1, foto_abril_2, foto_abril_3)
#########################################################

filenames5 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180501", pattern="*.snappy.parquet", full.names=TRUE)
foto_maig_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180501/part-00000-tid-5155470636857705873-7fa7ce3f-d7d1-4a62-a78c-0f3e57a15f9a-46320-1.c000.snappy.parquet")
foto_maig_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180501/part-00001-tid-5155470636857705873-7fa7ce3f-d7d1-4a62-a78c-0f3e57a15f9a-46322-1.c000.snappy.parquet")
foto_maig_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180501/part-00002-tid-5155470636857705873-7fa7ce3f-d7d1-4a62-a78c-0f3e57a15f9a-47318-1.c000.snappy.parquet")
foto_maig_1 <- foto_maig_1[,-c(27:58)]
foto_maig_2 <- foto_maig_2[,-c(27:58)]
foto_maig_3 <- foto_maig_3[,-c(27:58)]

foto_ofer_maig <- bind_rows(foto_maig_1, foto_maig_2, foto_maig_3)
foto_ofer_maig <- filter(foto_ofer_maig, level1 %in%  c("Cataluña"))
count(foto_ofer_maig, "level1")

foto_ofer_maig$date <- as.Date(as.character(foto_ofer_maig$date), format = "%Y%m%d")
foto_ofer_maig <- foto_ofer_maig[order(foto_ofer_maig$date),]
foto_ofer_maig$mes <- foto_ofer_maig$date

#transform data to month names
month5 <- data.frame(foto_ofer_maig$mes <-month(ymd(foto_ofer_maig$mes)))
names(month5)[1] <- "mes"

test5 <-  unique(setDT(foto_ofer_maig), by = c("property_id", "mes"))
test5$property_id <- test5$property_id
count(test5, "mes")
counting <- test5 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test5 <- merge(test5, counting, by = "property_id")
test5 <- test5[order(test5$property_id),]
count(test5, "numero_mesos_anunci_apareix")

rm(foto_maig_1, foto_maig_2, foto_maig_3)
#########################################################

filenames6 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180601", pattern="*.snappy.parquet", full.names=TRUE)
foto_juny_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180601/part-00000-tid-705677293423006374-42fdb2c4-135e-44d2-94a6-fcb356a30be1-37131-1.c000.snappy.parquet")
foto_juny_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180601/part-00001-tid-705677293423006374-42fdb2c4-135e-44d2-94a6-fcb356a30be1-37132-1.c000.snappy.parquet")
foto_juny_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180601/part-00002-tid-705677293423006374-42fdb2c4-135e-44d2-94a6-fcb356a30be1-37135-1.c000.snappy.parquet")
foto_juny_1 <- foto_juny_1[,-c(27:58)]
foto_juny_2 <- foto_juny_2[,-c(27:58)]
foto_juny_3 <- foto_juny_3[,-c(27:58)]

foto_ofer_juny <- bind_rows(foto_juny_1, foto_juny_2, foto_juny_3)
foto_ofer_juny <- filter(foto_ofer_juny, level1 %in%  c("Cataluña"))
count(foto_ofer_juny, "level1")

foto_ofer_juny$date <- as.Date(as.character(foto_ofer_juny$date), format = "%Y%m%d")
foto_ofer_juny <- foto_ofer_juny[order(foto_ofer_juny$date),]
foto_ofer_juny$mes <- foto_ofer_juny$date

#transform data to month names
month6 <- data.frame(foto_ofer_juny$mes <-month(ymd(foto_ofer_juny$mes)))
names(month6)[1] <- "mes"

test6 <-  unique(setDT(foto_ofer_juny), by = c("property_id", "mes"))
test6$property_id <- test6$property_id
count(test6, "mes")
counting <- test6 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test6 <- merge(test6, counting, by = "property_id")
test6 <- test6[order(test6$property_id),]
count(test6, "numero_mesos_anunci_apareix")

rm(foto_juny_1, foto_juny_2, foto_juny_3)

#########################################################

filenames7 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180701", pattern="*.snappy.parquet", full.names=TRUE)
foto_juliol_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180701/part-00000-tid-1601729267681508504-4c07765b-060c-41a8-9b4f-d032644aa9ba-47496-1.c000.snappy.parquet")
foto_juliol_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180701/part-00001-tid-1601729267681508504-4c07765b-060c-41a8-9b4f-d032644aa9ba-47499-1.c000.snappy.parquet")
foto_juliol_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180701/part-00002-tid-1601729267681508504-4c07765b-060c-41a8-9b4f-d032644aa9ba-47705-1.c000.snappy.parquet")
foto_juliol_1 <- foto_juliol_1[,-c(27:58)]
foto_juliol_2 <- foto_juliol_2[,-c(27:58)]
foto_juliol_3 <- foto_juliol_3[,-c(27:58)]

foto_ofer_juliol <- bind_rows(foto_juliol_1, foto_juliol_2, foto_juliol_3)
foto_ofer_juliol <- filter(foto_ofer_juliol, level1 %in%  c("Cataluña"))
count(foto_ofer_juliol, "level1")

foto_ofer_juliol$date <- as.Date(as.character(foto_ofer_juliol$date), format = "%Y%m%d")
foto_ofer_juliol <- foto_ofer_juliol[order(foto_ofer_juliol$date),]
foto_ofer_juliol$mes <- foto_ofer_juliol$date

#transform data to month names
month7 <- data.frame(foto_ofer_juliol$mes <-month(ymd(foto_ofer_juliol$mes)))
names(month7)[1] <- "mes"

test7 <-  unique(setDT(foto_ofer_juliol), by = c("property_id", "mes"))
test7$property_id <- test7$property_id
count(test7, "mes")
counting <- test7 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test7 <- merge(test7, counting, by = "property_id")
test7 <- test7[order(test7$property_id),]
count(test7, "numero_mesos_anunci_apareix")

rm(foto_juliol_1, foto_juliol_2, foto_juliol_3)
#########################################################

filenames8 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180801", pattern="*.snappy.parquet", full.names=TRUE)
foto_agost_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180801/part-00000-tid-7177249347814000014-fd30e8dd-36d1-44ab-8615-925f1f84c560-39658-1.c000.snappy.parquet")
foto_agost_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180801/part-00001-tid-7177249347814000014-fd30e8dd-36d1-44ab-8615-925f1f84c560-40582-1.c000.snappy.parquet")
foto_agost_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180801/part-00002-tid-7177249347814000014-fd30e8dd-36d1-44ab-8615-925f1f84c560-42713-1.c000.snappy.parquet")
foto_agost_1 <- foto_agost_1[,-c(27:58)]
foto_agost_2 <- foto_agost_2[,-c(27:58)]
foto_agost_3 <- foto_agost_3[,-c(27:58)]

foto_ofer_agost <- bind_rows(foto_agost_1, foto_agost_2, foto_agost_3)
foto_ofer_agost <- filter(foto_ofer_agost, level1 %in%  c("Cataluña"))
count(foto_ofer_agost, "level1")

foto_ofer_agost$date <- as.Date(as.character(foto_ofer_agost$date), format = "%Y%m%d")
foto_ofer_agost <- foto_ofer_agost[order(foto_ofer_agost$date),]
foto_ofer_agost$mes <- foto_ofer_agost$date

#transform data to month names
month8 <- data.frame(foto_ofer_agost$mes <-month(ymd(foto_ofer_agost$mes)))
names(month8)[1] <- "mes"

test8 <-  unique(setDT(foto_ofer_agost), by = c("property_id", "mes"))
test8$property_id <- test8$property_id
count(test8, "mes")
counting <- test8 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test8 <- merge(test8, counting, by = "property_id")
test8 <- test8[order(test8$property_id),]
count(test8, "numero_mesos_anunci_apareix")

rm(foto_agost_1, foto_agost_2, foto_agost_3)
#########################################################

filenames9 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180901", pattern="*.snappy.parquet", full.names=TRUE)
foto_sept_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180901/part-00000-tid-8935002828778772575-44cff5e3-9048-46a9-ac4e-dbd5558750af-48456-1.c000.snappy.parquet")
foto_sept_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180901/part-00001-tid-8935002828778772575-44cff5e3-9048-46a9-ac4e-dbd5558750af-48457-1.c000.snappy.parquet")
foto_sept_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20180901/part-00002-tid-8935002828778772575-44cff5e3-9048-46a9-ac4e-dbd5558750af-49251-1.c000.snappy.parquet")
foto_sept_1 <- foto_sept_1[,-c(27:58)]
foto_sept_2 <- foto_sept_2[,-c(27:58)]
foto_sept_3 <- foto_sept_3[,-c(27:58)]

foto_ofer_sept <- bind_rows(foto_sept_1, foto_sept_2, foto_sept_3)
foto_ofer_sept <- filter(foto_ofer_sept, level1 %in%  c("Cataluña"))
count(foto_ofer_sept, "level1")

foto_ofer_sept$date <- as.Date(as.character(foto_ofer_sept$date), format = "%Y%m%d")
foto_ofer_sept <- foto_ofer_sept[order(foto_ofer_sept$date),]
foto_ofer_sept$mes <- foto_ofer_sept$date

#transform data to month names
month9 <- data.frame(foto_ofer_sept$mes <-month(ymd(foto_ofer_sept$mes)))
names(month9)[1] <- "mes"

test9 <-  unique(setDT(foto_ofer_sept), by = c("property_id", "mes"))
test9$property_id <- test9$property_id
count(test9, "mes")
counting <- test9 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test9 <- merge(test9, counting, by = "property_id")
test9 <- test9[order(test9$property_id),]
count(test9, "numero_mesos_anunci_apareix")

rm(foto_sept_1, foto_sept_2, foto_sept_3)

#########################################################

filenames10 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181001", pattern="*.snappy.parquet", full.names=TRUE)
foto_oct_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181001/part-00000-tid-577688581439583593-6c388f87-26a3-4bc3-a98a-a76642dafe5c-47052-1.c000.snappy.parquet")
foto_oct_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181001/part-00001-tid-577688581439583593-6c388f87-26a3-4bc3-a98a-a76642dafe5c-47053-1.c000.snappy.parquet")
foto_oct_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181001/part-00002-tid-577688581439583593-6c388f87-26a3-4bc3-a98a-a76642dafe5c-47315-1.c000.snappy.parquet")
foto_oct_1 <- foto_oct_1[,-c(27:58)]
foto_oct_2 <- foto_oct_2[,-c(27:58)]
foto_oct_3 <- foto_oct_3[,-c(27:58)]

foto_ofer_oct <- bind_rows(foto_oct_1, foto_oct_2, foto_oct_3)
foto_ofer_oct <- filter(foto_ofer_oct, level1 %in%  c("Cataluña"))
count(foto_ofer_oct, "level1")

foto_ofer_oct$date <- as.Date(as.character(foto_ofer_oct$date), format = "%Y%m%d")
foto_ofer_oct <- foto_ofer_oct[order(foto_ofer_oct$date),]
foto_ofer_oct$mes <- foto_ofer_oct$date

#transform data to month names
month10 <- data.frame(foto_ofer_oct$mes <-month(ymd(foto_ofer_oct$mes)))
names(month10)[1] <- "mes"

test10 <-  unique(setDT(foto_ofer_oct), by = c("property_id", "mes"))
test10$property_id <- test10$property_id
count(test10, "mes")
counting <- test10 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test10 <- merge(test10, counting, by = "property_id")
test10 <- test10[order(test10$property_id),]
count(test10, "numero_mesos_anunci_apareix")

rm(foto_oct_1, foto_oct_2, foto_oct_3)
#########################################################

filenames11 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181101", pattern="*.snappy.parquet", full.names=TRUE)
foto_nov_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181101/part-00000-tid-9111804253177671752-591cd65a-77e8-4725-9086-ff711324f91b-45888-1.c000.snappy.parquet")
foto_nov_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181101/part-00001-tid-9111804253177671752-591cd65a-77e8-4725-9086-ff711324f91b-45889-1.c000.snappy.parquet")
foto_nov_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181101/part-00002-tid-9111804253177671752-591cd65a-77e8-4725-9086-ff711324f91b-45937-1.c000.snappy.parquet")
foto_nov_1 <- foto_nov_1[,-c(27:58)]
foto_nov_2 <- foto_nov_2[,-c(27:58)]
foto_nov_3 <- foto_nov_3[,-c(27:58)]

foto_ofer_nov <- bind_rows(foto_nov_1, foto_nov_2, foto_nov_3)
foto_ofer_nov <- filter(foto_ofer_nov, level1 %in%  c("Cataluña"))
count(foto_ofer_nov, "level1")

foto_ofer_nov$date <- as.Date(as.character(foto_ofer_nov$date), format = "%Y%m%d")
foto_ofer_nov <- foto_ofer_nov[order(foto_ofer_nov$date),]
foto_ofer_nov$mes <- foto_ofer_nov$date

#transform data to month names
month11 <- data.frame(foto_ofer_nov$mes <-month(ymd(foto_ofer_nov$mes)))
names(month11)[1] <- "mes"

test11 <-  unique(setDT(foto_ofer_nov), by = c("property_id", "mes"))
test11$property_id <- test11$property_id
count(test11, "mes")
counting <- test11 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test11 <- merge(test11, counting, by = "property_id")
test11 <- test11[order(test11$property_id),]
count(test11, "numero_mesos_anunci_apareix")

rm(foto_nov_1, foto_nov_2, foto_nov_3)
#########################################################

filenames12 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181201", pattern="*.snappy.parquet", full.names=TRUE)
foto_des_1 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181201/part-00000-tid-4764460651110147593-f22eadc8-69ba-4e6c-b73f-cc819ff4b506-50429-1.c000.snappy.parquet")
foto_des_2 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181201/part-00001-tid-4764460651110147593-f22eadc8-69ba-4e6c-b73f-cc819ff4b506-50430-1.c000.snappy.parquet")
foto_des_3 <- read_parquet("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/dt=20181201/part-00002-tid-4764460651110147593-f22eadc8-69ba-4e6c-b73f-cc819ff4b506-51180-1.c000.snappy.parquet")
foto_des_1 <- foto_des_1[,-c(27:58)]
foto_des_2 <- foto_des_2[,-c(27:58)]
foto_des_3 <- foto_des_3[,-c(27:58)]

foto_ofer_des <- bind_rows(foto_des_1, foto_des_2, foto_des_3)
foto_ofer_des <- filter(foto_ofer_des, level1 %in%  c("Cataluña"))
count(foto_ofer_des, "level1")

foto_ofer_des$date <- as.Date(as.character(foto_ofer_des$date), format = "%Y%m%d")
foto_ofer_des <- foto_ofer_des[order(foto_ofer_des$date),]
foto_ofer_des$mes <- foto_ofer_des$date

#transform data to month names
month12 <- data.frame(foto_ofer_des$mes <-month(ymd(foto_ofer_des$mes)))
names(month12)[1] <- "mes"

test12 <-  unique(setDT(foto_ofer_des), by = c("property_id", "mes"))
test12$property_id <- test12$property_id
count(test12, "mes")
counting <- test12 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test12 <- merge(test12, counting, by = "property_id")
test12 <- test12[order(test12$property_id),]
count(test12, "numero_mesos_anunci_apareix")

rm(foto_des_1, foto_des_2, foto_des_3)
#########################################################

foto_oferta_2018 <- bind_rows(test1,
                              test2,
                              test3,
                              test4,
                              test5,
                              test6,
                              test7,
                              test8,
                              test9,
                              test10,
                              test11,
                              test12
                              )
class(foto_oferta_2018)
count(foto_oferta_2018, "transaction_type")

foto_oferta_2018 <- foto_oferta_2018 %>% 
  filter(!grepl(c('Sell'), c(transaction_type)))
foto_oferta_2018 <- foto_oferta_2018 %>% 
  filter(!grepl(c('Share'), c(transaction_type)))
foto_oferta_2018 <- foto_oferta_2018 %>% 
  filter(!grepl(c('Transfer'), c(transaction_type)))
foto_oferta_2018 <- foto_oferta_2018 %>% 
  filter(!grepl(c('Undefined'), c(transaction_type)))

write.csv(foto_oferta_2018, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Fotocasa_Oferta/extraccions_foto_oferta_2018.csv")


