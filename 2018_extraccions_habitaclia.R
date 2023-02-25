library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
#require(sf)
library(arrow)
library(plyr)
library(arrow)
library(lubridate)
library(data.table)

options(scipen=999)

filenames <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20180101", pattern="*.snappy.parquet", full.names=TRUE)
ldf <- lapply(filenames, read_parquet)
class(ldf)
habit_ofer_gener <-  as.data.frame(do.call(rbind, ldf))

habit_ofer_gener <- habit_ofer_gener[,-c(14:34)]
habit_ofer_gener$date <- as.Date(as.character(habit_ofer_gener$date), format = "%Y%m%d")
habit_ofer_gener <- habit_ofer_gener[order(habit_ofer_gener$date),]
habit_ofer_gener$mes <- habit_ofer_gener$date

#transform data to month names
month1 <- data.frame(habit_ofer_gener$mes <-month(ymd(habit_ofer_gener$mes)))
names(month1)[1] <- "mes"

test1 <-  unique(setDT(habit_ofer_gener), by = c("property_id", "mes"))
test1$property_id <- test1$property_id
count(test1, "mes")
counting <- test1 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test1 <- merge(test1, counting, by = "property_id")
test1 <- test1[order(test1$property_id),]
count(test1, "numero_mesos_anunci_apareix")

rm(habit_ofer_gener)
#########################################################

filenames2 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20180201", pattern="*.snappy.parquet", full.names=TRUE)
ldf2 <- lapply(filenames2, read_parquet)
class(ldf2)
habit_ofer_febrer <-  as.data.frame(do.call(rbind, ldf2))

habit_ofer_febrer <- habit_ofer_febrer[,-c(14:34)]
habit_ofer_febrer$date <- as.Date(as.character(habit_ofer_febrer$date), format = "%Y%m%d")
habit_ofer_febrer <- habit_ofer_febrer[order(habit_ofer_febrer$date),]
habit_ofer_febrer$mes <- habit_ofer_febrer$date

#transform data to month names
month2 <- data.frame(habit_ofer_febrer$mes <-month(ymd(habit_ofer_febrer$mes)))
names(month2)[1] <- "mes"

test2 <-  unique(setDT(habit_ofer_febrer), by = c("property_id", "mes"))
test2$property_id <- test2$property_id
count(test2, "mes")
counting <- test2 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test2 <- merge(test2, counting, by = "property_id")
test2 <- test2[order(test2$property_id),]
count(test2, "numero_mesos_anunci_apareix")

rm(habit_ofer_febrer)
#########################################################

filenames3 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20180301", 
                         pattern="*.snappy.parquet", full.names=TRUE)
ldf3 <- lapply(filenames3, read_parquet)
class(ldf3)
habit_ofer_marc <-  as.data.frame(do.call(rbind, ldf3))

habit_ofer_marc <- habit_ofer_marc[,-c(14:34)]
habit_ofer_marc$date <- as.Date(as.character(habit_ofer_marc$date), format = "%Y%m%d")
habit_ofer_marc <- habit_ofer_marc[order(habit_ofer_marc$date),]
habit_ofer_marc$mes <- habit_ofer_marc$date

#transform data to month names
month3 <- data.frame(habit_ofer_marc$mes <-month(ymd(habit_ofer_marc$mes)))
names(month3)[1] <- "mes"

test3 <-  unique(setDT(habit_ofer_marc), by = c("property_id", "mes"))
test3$property_id <- test3$property_id
count(test3, "mes")
counting <- test3 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test3 <- merge(test3, counting, by = "property_id")
test3 <- test3[order(test3$property_id),]
count(test3, "numero_mesos_anunci_apareix")

rm(habit_ofer_marc)
#########################################################

filenames4 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20180401", pattern="*.snappy.parquet", full.names=TRUE)
ldf4 <- lapply(filenames4, read_parquet)
class(ldf4)
habit_ofer_abril <-  as.data.frame(do.call(rbind, ldf4))

habit_ofer_abril <- habit_ofer_abril[,-c(14:34)]
habit_ofer_abril$date <- as.Date(as.character(habit_ofer_abril$date), format = "%Y%m%d")
habit_ofer_abril <- habit_ofer_abril[order(habit_ofer_abril$date),]
habit_ofer_abril$mes <- habit_ofer_abril$date

#transform data to month names
month4 <- data.frame(habit_ofer_abril$mes <-month(ymd(habit_ofer_abril$mes)))
names(month4)[1] <- "mes"

test4 <-  unique(setDT(habit_ofer_abril), by = c("property_id", "mes"))
test4$property_id <- test4$property_id
count(test4, "mes")
counting <- test4 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test4 <- merge(test4, counting, by = "property_id")
test4 <- test4[order(test4$property_id),]
count(test4, "numero_mesos_anunci_apareix")

rm(habit_ofer_abril)
#########################################################

filenames5 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20180501", pattern="*.snappy.parquet", full.names=TRUE)
ldf5 <- lapply(filenames5, read_parquet)
class(ldf5)
habit_ofer_maig <-  as.data.frame(do.call(rbind, ldf5))

habit_ofer_maig <- habit_ofer_maig[,-c(14:34)]
habit_ofer_maig$date <- as.Date(as.character(habit_ofer_maig$date), format = "%Y%m%d")
habit_ofer_maig <- habit_ofer_maig[order(habit_ofer_maig$date),]
habit_ofer_maig$mes <- habit_ofer_maig$date

#transform data to month names
month5 <- data.frame(habit_ofer_maig$mes <-month(ymd(habit_ofer_maig$mes)))
names(month5)[1] <- "mes"

test5 <-  unique(setDT(habit_ofer_maig), by = c("property_id", "mes"))
test5$property_id <- test5$property_id
count(test5, "mes")
counting <- test5 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test5 <- merge(test5, counting, by = "property_id")
test5 <- test5[order(test5$property_id),]
count(test5, "numero_mesos_anunci_apareix")

rm(habit_ofer_maig)
#########################################################

filenames6 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20180601", pattern="*.snappy.parquet", full.names=TRUE)
ldf6 <- lapply(filenames6, read_parquet)
class(ldf6)
habit_ofer_juny <-  as.data.frame(do.call(rbind, ldf6))

habit_ofer_juny <- habit_ofer_juny[,-c(14:34)]
habit_ofer_juny$date <- as.Date(as.character(habit_ofer_juny$date), format = "%Y%m%d")
habit_ofer_juny <- habit_ofer_juny[order(habit_ofer_juny$date),]
habit_ofer_juny$mes <- habit_ofer_juny$date

#transform data to month names
month6 <- data.frame(habit_ofer_juny$mes <-month(ymd(habit_ofer_juny$mes)))
names(month6)[1] <- "mes"

test6 <-  unique(setDT(habit_ofer_juny), by = c("property_id", "mes"))
test6$property_id <- test6$property_id
count(test6, "mes")
counting <- test6 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test6 <- merge(test6, counting, by = "property_id")
test6 <- test6[order(test6$property_id),]
count(test6, "numero_mesos_anunci_apareix")

rm(habit_ofer_juny)
#########################################################

filenames7 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20180701", pattern="*.snappy.parquet", full.names=TRUE)
ldf7 <- lapply(filenames7, read_parquet)
class(ldf7)
habit_ofer_juliol <- as.data.frame(do.call(rbind, ldf7))

habit_ofer_juliol <- habit_ofer_juliol[,-c(14:34)]
habit_ofer_juliol$date <- as.Date(as.character(habit_ofer_juliol$date), format = "%Y%m%d")
habit_ofer_juliol <- habit_ofer_juliol[order(habit_ofer_juliol$date),]
habit_ofer_juliol$mes <- habit_ofer_juliol$date

#transform data to month names
month7 <- data.frame(habit_ofer_juliol$mes <-month(ymd(habit_ofer_juliol$mes)))
names(month7)[1] <- "mes"

test7 <-  unique(setDT(habit_ofer_juliol), by = c("property_id", "mes"))
test7$property_id <- test7$property_id
count(test7, "mes")
counting <- test7 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test7 <- merge(test7, counting, by = "property_id")
test7 <- test7[order(test7$property_id),]
count(test7, "numero_mesos_anunci_apareix")

rm(habit_ofer_juliol)
#########################################################

filenames8 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20180801", pattern="*.snappy.parquet", full.names=TRUE)
ldf8 <- lapply(filenames8, read_parquet)
class(ldf8)
habit_ofer_agost <- as.data.frame(do.call(rbind, ldf8))

habit_ofer_agost <- habit_ofer_agost[,-c(14:34)]
habit_ofer_agost$date <- as.Date(as.character(habit_ofer_agost$date), format = "%Y%m%d")
habit_ofer_agost <- habit_ofer_agost[order(habit_ofer_agost$date),]
habit_ofer_agost$mes <- habit_ofer_agost$date

#transform data to month names
month8 <- data.frame(habit_ofer_agost$mes <-month(ymd(habit_ofer_agost$mes)))
names(month8)[1] <- "mes"

test8 <-  unique(setDT(habit_ofer_agost), by = c("property_id", "mes"))
test8$property_id <- test8$property_id
count(test8, "mes")
counting <- test8 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test8 <- merge(test8, counting, by = "property_id")
test8 <- test8[order(test8$property_id),]
count(test8, "numero_mesos_anunci_apareix")

rm(habit_ofer_agost)
#########################################################

filenames9 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20180901", pattern="*.snappy.parquet", full.names=TRUE)
ldf9 <- lapply(filenames9, read_parquet)
class(ldf9)
habit_ofer_sept <- as.data.frame(do.call(rbind, ldf9))

habit_ofer_sept <- habit_ofer_sept[,-c(14:34)]
habit_ofer_sept$date <- as.Date(as.character(habit_ofer_sept$date), format = "%Y%m%d")
habit_ofer_sept <- habit_ofer_sept[order(habit_ofer_sept$date),]
habit_ofer_sept$mes <- habit_ofer_sept$date

#transform data to month names
month9 <- data.frame(habit_ofer_sept$mes <-month(ymd(habit_ofer_sept$mes)))
names(month9)[1] <- "mes"

test9 <-  unique(setDT(habit_ofer_sept), by = c("property_id", "mes"))
test9$property_id <- test9$property_id
count(test9, "mes")
counting <- test9 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test9 <- merge(test9, counting, by = "property_id")
test9 <- test9[order(test9$property_id),]
count(test9, "numero_mesos_anunci_apareix")

rm(habit_ofer_sept)
#########################################################

filenames10 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20181001", pattern="*.snappy.parquet", full.names=TRUE)
ldf10 <- lapply(filenames10, read_parquet)
class(ldf10)
habit_ofer_oct <- as.data.frame(do.call(rbind, ldf10))
#habit_ofer_oct[which(habit_ofer_oct$property_id == "2000002300003758420"), ]

habit_ofer_oct <- habit_ofer_oct[,-c(14:34)]
habit_ofer_oct$date <- as.Date(as.character(habit_ofer_oct$date), format = "%Y%m%d")
habit_ofer_oct <- habit_ofer_oct[order(habit_ofer_oct$date),]
habit_ofer_oct$mes <- habit_ofer_oct$date

#transform data to month names
month10 <- data.frame(habit_ofer_oct$mes <-month(ymd(habit_ofer_oct$mes)))
names(month10)[1] <- "mes"

test10 <-  unique(setDT(habit_ofer_oct), by = c("property_id", "mes"))
test10$property_id <- test10$property_id
count(test10, "mes")
counting <- test10 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test10 <- merge(test10, counting, by = "property_id")
test10 <- test10[order(test10$property_id),]
count(test10, "numero_mesos_anunci_apareix")

rm(habit_ofer_oct)
#########################################################

filenames11 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20181101", pattern="*.snappy.parquet", full.names=TRUE)
ldf11 <- lapply(filenames11, read_parquet)
class(ldf11)
habit_ofer_nov <- as.data.frame(do.call(rbind, ldf11))

habit_ofer_nov <- habit_ofer_nov[,-c(14:34)]
habit_ofer_nov$date <- as.Date(as.character(habit_ofer_nov$date), format = "%Y%m%d")
habit_ofer_nov <- habit_ofer_nov[order(habit_ofer_nov$date),]
habit_ofer_nov$mes <- habit_ofer_nov$date

#transform data to month names
month11 <- data.frame(habit_ofer_nov$mes <-month(ymd(habit_ofer_nov$mes)))
names(month11)[1] <- "mes"

test11 <-  unique(setDT(habit_ofer_nov), by = c("property_id", "mes"))
test11$property_id <- test11$property_id
count(test11, "mes")
counting <- test11 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test11 <- merge(test11, counting, by = "property_id")
test11 <- test11[order(test11$property_id),]
count(test11, "numero_mesos_anunci_apareix")

rm(habit_ofer_nov)
#########################################################

filenames12 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/dt=20181201", pattern="*.snappy.parquet", full.names=TRUE)
ldf12 <- lapply(filenames12, read_parquet)
class(ldf12)
habit_ofer_des <- as.data.frame(do.call(rbind, ldf12))

habit_ofer_des <- habit_ofer_des[,-c(14:34)]
habit_ofer_des$date <- as.Date(as.character(habit_ofer_des$date), format = "%Y%m%d")
habit_ofer_des <- habit_ofer_des[order(habit_ofer_des$date),]
habit_ofer_des$mes <- habit_ofer_des$date

#transform data to month names
month12 <- data.frame(habit_ofer_des$mes <-month(ymd(habit_ofer_des$mes)))
names(month12)[1] <- "mes"

test12 <-  unique(setDT(habit_ofer_des), by = c("property_id", "mes"))
test12$property_id <- test12$property_id
count(test12, "mes")
counting <- test12 %>% group_by(property_id) %>% tally()
names(counting)[1] <- "property_id"
names(counting)[2] <- "numero_mesos_anunci_apareix"
test12 <- merge(test12, counting, by = "property_id")
test12 <- test12[order(test12$property_id),]
count(test12, "numero_mesos_anunci_apareix")

rm(habit_ofer_des)
#########################################################

habit_oferta_2018 <- rbind(test1,
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

count(habit_oferta_2018, "transaction_type")

write.csv(habit_oferta_2018, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2018_Habitaclia_oferta/extraccions_habit_oferta_2018.csv")
