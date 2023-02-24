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
options(digits=2)

filenames <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20200101", pattern="*.snappy.parquet", full.names=TRUE)
ldf <- lapply(filenames, read_parquet)
class(ldf)
habit_demanda_gener <-  as.data.frame(do.call(rbind, ldf))

habit_demanda_gener$date <- as.Date(as.character(habit_demanda_gener$date), format = "%Y%m%d")
habit_demanda_gener <- habit_demanda_gener[order(habit_demanda_gener$date),]

#########################################################

filenames2 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20200201", pattern="*.snappy.parquet", full.names=TRUE)
ldf2 <- lapply(filenames2, read_parquet)
class(ldf2)
habit_demanda_febrer <-  as.data.frame(do.call(rbind, ldf2))

habit_demanda_febrer$date <- as.Date(as.character(habit_demanda_febrer$date), format = "%Y%m%d")
habit_demanda_febrer <- habit_demanda_febrer[order(habit_demanda_febrer$date),]

#########################################################

filenames3 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20200301", pattern="*.snappy.parquet", full.names=TRUE)
ldf3 <- lapply(filenames3, read_parquet)
class(ldf3)
habit_demanda_marc <-  as.data.frame(do.call(rbind, ldf3))

habit_demanda_marc$date <- as.Date(as.character(habit_demanda_marc$date), format = "%Y%m%d")
habit_demanda_marc <- habit_demanda_marc[order(habit_demanda_marc$date),]

#########################################################

filenames4 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20200401", pattern="*.snappy.parquet", full.names=TRUE)
ldf4 <- lapply(filenames4, read_parquet)
class(ldf4)
habit_demanda_abril <-  as.data.frame(do.call(rbind, ldf4))

habit_demanda_abril$date <- as.Date(as.character(habit_demanda_abril$date), format = "%Y%m%d")
habit_demanda_abril <- habit_demanda_abril[order(habit_demanda_abril$date),]

#########################################################

filenames5 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20200501", pattern="*.snappy.parquet", full.names=TRUE)
ldf5 <- lapply(filenames5, read_parquet)
class(ldf5)
habit_demanda_maig <-  as.data.frame(do.call(rbind, ldf5))

habit_demanda_maig$date <- as.Date(as.character(habit_demanda_maig$date), format = "%Y%m%d")
habit_demanda_maig <- habit_demanda_maig[order(habit_demanda_maig$date),]

#########################################################

filenames6 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20200601", pattern="*.snappy.parquet", full.names=TRUE)
ldf6 <- lapply(filenames6, read_parquet)
class(ldf6)
habit_demanda_juny <-  as.data.frame(do.call(rbind, ldf6))

habit_demanda_juny$date <- as.Date(as.character(habit_demanda_juny$date), format = "%Y%m%d")
habit_demanda_juny <- habit_demanda_juny[order(habit_demanda_juny$date),]

#########################################################

filenames7 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20200701", pattern="*.snappy.parquet", full.names=TRUE)
ldf7 <- lapply(filenames7, read_parquet)
class(ldf7)
habit_demanda_juliol <- as.data.frame(do.call(rbind, ldf7))

habit_demanda_juliol$date <- as.Date(as.character(habit_demanda_juliol$date), format = "%Y%m%d")
habit_demanda_juliol <- habit_demanda_juliol[order(habit_demanda_juliol$date),]

#########################################################

filenames8 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20200801", pattern="*.snappy.parquet", full.names=TRUE)
ldf8 <- lapply(filenames8, read_parquet)
class(ldf8)
habit_demanda_agost <- as.data.frame(do.call(rbind, ldf8))

habit_demanda_agost$date <- as.Date(as.character(habit_demanda_agost$date), format = "%Y%m%d")
habit_demanda_agost <- habit_demanda_agost[order(habit_demanda_agost$date),]

#########################################################

filenames9 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20200901", pattern="*.snappy.parquet", full.names=TRUE)
ldf9 <- lapply(filenames9, read_parquet)
class(ldf9)
habit_demanda_sept <- as.data.frame(do.call(rbind, ldf9))

habit_demanda_sept$date <- as.Date(as.character(habit_demanda_sept$date), format = "%Y%m%d")
habit_demanda_sept <- habit_demanda_sept[order(habit_demanda_sept$date),]

#########################################################

filenames10 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20201001", pattern="*.snappy.parquet", full.names=TRUE)
ldf10 <- lapply(filenames10, read_parquet)
class(ldf10)
habit_demanda_oct <- as.data.frame(do.call(rbind, ldf10))
#habit_demanda_oct[which(habit_demanda_oct$property_id == "2000002300003758420"), ]

habit_demanda_oct$date <- as.Date(as.character(habit_demanda_oct$date), format = "%Y%m%d")
habit_demanda_oct <- habit_demanda_oct[order(habit_demanda_oct$date),]

#########################################################

filenames11 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20201001", pattern="*.snappy.parquet", full.names=TRUE)
ldf11 <- lapply(filenames11, read_parquet)
class(ldf11)
habit_demanda_nov <- as.data.frame(do.call(rbind, ldf11))

habit_demanda_nov$date <- as.Date(as.character(habit_demanda_nov$date), format = "%Y%m%d")
habit_demanda_nov <- habit_demanda_nov[order(habit_demanda_nov$date),]

#########################################################

filenames12 <- list.files("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/dt=20201001", pattern="*.snappy.parquet", full.names=TRUE)
ldf12 <- lapply(filenames12, read_parquet)
class(ldf12)
habit_demanda_des <- as.data.frame(do.call(rbind, ldf12))

habit_demanda_des$date <- as.Date(as.character(habit_demanda_des$date), format = "%Y%m%d")
habit_demanda_des <- habit_demanda_des[order(habit_demanda_des$date),]

#########################################################

habit_demanda_2020 <- bind_rows(habit_demanda_gener,
                               habit_demanda_febrer,
                               habit_demanda_marc,
                               habit_demanda_maig,
                               habit_demanda_juny,
                               habit_demanda_juliol,
                               habit_demanda_agost,
                               habit_demanda_sept,
                               habit_demanda_oct,
                               habit_demanda_nov,
                               habit_demanda_des
)
class(habit_demanda_2020)
count(habit_demanda_2020, "province")

write.csv(habit_demanda_2020, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/2020_Habitaclia_demanda/extraccions_habit_demanda_2020.csv")
