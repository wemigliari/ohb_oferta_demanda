library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(arrow)
library(plyr)
library(arrow)
library(lubridate)
library(data.table)

options(scipen=999)
options(digits=2)

##################################################################
############## CAT Trimestrals
##################################################################

cs_demanda_fotocasa <- foto_demanda_series_tract

2000001100000514119
#adding trimestre
cs_demanda_fotocasa <- cs_demanda_fotocasa %>% mutate(trimestre =
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



###########################################################
######## Mitjanes de superficie
###########################################################
cs_demanda_fotocasa <- cs_demanda_fotocasa[cs_demanda_fotocasa$surface_d != -1, ] 

mitjana_surface_d_m <- aggregate(x = cs_demanda_fotocasa$surface_d,    
                                 by = list(cs_demanda_fotocasa$property_id, 
                                           cs_demanda_fotocasa$NOMMUNI, 
                                           cs_demanda_fotocasa$mes),             
                                 FUN = mean, na.action=NULL)                           

names(mitjana_surface_d_m)[1:4] <- c("property_id", "NOMMUNI", "mes", "mitjana_superf_d_mes")

cs_demanda_fotocasa <- merge(x=cs_demanda_fotocasa, y=mitjana_surface_d_m, 
                               by.x=c("property_id","NOMMUNI", "mes"), 
                               by.y=c("property_id","NOMMUNI", "mes"))

##################################################################
############## Mitjanes de preu
##################################################################

mitjana_price_d_m <- aggregate(x = cs_demanda_fotocasa$price_d,    
                               by = list(cs_demanda_fotocasa$property_id, 
                                         cs_demanda_fotocasa$NOMMUNI, 
                                         cs_demanda_fotocasa$mes),             
                               FUN = mean, na.action=NULL)                           

names(mitjana_price_d_m)[1:4] <- c("property_id", "NOMMUNI", "mes", "mitjana_price_d_mes")


cs_demanda_fotocasa <- merge(x=cs_demanda_fotocasa, y=mitjana_price_d_m, 
                               by.x = c("property_id","NOMMUNI", "mes"),
                               by.y = c("property_id","NOMMUNI", "mes"))

2000001100002716449

##################################################################
############## Preu M2
##################################################################

cs_demanda_fotocasa$preu_m2_mes_d <- cs_demanda_fotocasa$mitjana_price_d_mes/cs_demanda_fotocasa$mitjana_superf_d_mes


##################################################################
############## Joining oferta i demanda abans de juntar les taules
##################################################################

cs_demanda_fotocasa$date <- NULL
cs_demanda_fotocasa$trimestre <- NULL
cs_demanda_fotocasa$district <- NULL
cs_demanda_fotocasa$municipality <- NULL
cs_demanda_fotocasa$CODIMUNI <- NULL
cs_demanda_fotocasa$property_subtype <- NULL


df1 <- cs_demanda_fotocasa
df2 <- interquartil_cat_1

df3 <- merge(df2, df1, by.x = c("property_id", "NOMMUNI", "mes", "any"), 
             by.y =  c("property_id","NOMMUNI", "mes","any"),
             all.x = TRUE, all.y = TRUE)

df3["mitjana_superf_o_mes"][is.na(df3["mitjana_superf_o_mes"])] <- 0
df3["mitjana_price_o_mes"][is.na(df3["mitjana_price_o_mes"])] <- 0
df3["preu_m2_mes"][is.na(df3["preu_m2_mes"])] <- 0

cs_demanda_fotocasa <- df3
cs_demanda_fotocasa$preu_ponderat <- cs_demanda_fotocasa$mitjana_price_o_mes*cs_demanda_fotocasa$leads_mensuals

cs_demanda_fotocasa["price_d"][is.na(cs_demanda_fotocasa["price_d"])] <- 0
cs_demanda_fotocasa["surface_d"][is.na(cs_demanda_fotocasa["surface_d"])] <- 0
cs_demanda_fotocasa["num_leads"][is.na(cs_demanda_fotocasa["num_leads"])] <- 0
cs_demanda_fotocasa["leads_mensuals"][is.na(cs_demanda_fotocasa["leads_mensuals"])] <- 0
cs_demanda_fotocasa["mitjana_superf_d_mes"][is.na(cs_demanda_fotocasa["mitjana_superf_d_mes"])] <- 0
cs_demanda_fotocasa["mitjana_price_d_mes"][is.na(cs_demanda_fotocasa["mitjana_price_d_mes"])] <- 0
cs_demanda_fotocasa["preu_m2_mes_d"][is.na(cs_demanda_fotocasa["preu_m2_mes_d"])] <- 0
cs_demanda_fotocasa["preu_ponderat"][is.na(cs_demanda_fotocasa["preu_ponderat"])] <- 0

###############################################################################
############## Valids anunciantes despres de juntar les taules oferta i demanda
##############################################################################

cs_demanda_fotocasa <- cs_demanda_fotocasa%>%
  mutate(anunciant_valid_d = case_when(
    publisher_type == "Profesionales" ~ "Vàlid",
    publisher_type == "Particulares" & leads_mensuals < 2 ~ "No Vàlid",
    publisher_type == "Particulares" & leads_mensuals > 1 ~ "Vàlid",
    publisher_type == "Particulares" & leads_mensuals == NA ~ "No Vàlid",
    publisher_type == "Undefined" ~ "No Vàlid"
  )
  )


cs_demanda_fotocasa <- cs_demanda_fotocasa%>%
  arrange(property_id, trimestre, mes)

count(cs_demanda_fotocasa, "anunciant_valid_d")

#cs_demanda_fotocasa <- cs_demanda_fotocasa%>%
#mutate(preu_valid = case_when(
#price_d >= 10 & price_d <= 10000 ~ "Vàlid",
#surface_d >= 10 & surface_d <= 10000 ~ "Vàlid"
#)
#)

#cs_demanda_fotocasa <- cs_demanda_fotocasa%>%
#mutate(dia_valid = case_when(
#price_d >= 10 & price_d <= 10000 ~ "Vàlid",
#surface_d >= 10 & surface_d <= 10000 ~ "Vàlid"
#)
#)

#cs_demanda_fotocasa <- cs_demanda_fotocasa%>%
#mutate(anunciant_valid_d = case_when(
#price_d >= 10 & price_d <= 10000 ~ "Vàlid",
#surface_d >= 10 & surface_d <= 10000 ~ "Vàlid"
#)
#)

write.csv(cs_demanda_fotocasa, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/serie_of_dem_fotocasa_mensuals.csv")


##################################################################
############## Agregacions Trimestrals CAT
##################################################################
cs_demanda_fotocasa <- cs_demanda_fotocasa%>%
  filter(anunciant_valid_d=="Vàlid" & preu_valid=="Vàlid" & dia_valid=="Vàlid")

count(cs_demanda_fotocasa, "anunciant_valid_d")

##################################################################
############## Preu Oferta Trimestral
##################################################################

preu_o_trim <- aggregate(x = cs_demanda_fotocasa$mitjana_price_o_mes,    
                         by = list(cs_demanda_fotocasa$property_id, 
                                   cs_demanda_fotocasa$trimestre),             
                         FUN = mean, na.action=NULL)                           

names(preu_o_trim)[1:3] <- c("property_id", "trimestre", "preu_oferta_trimestral")


2000001100002716449

##################################################################
############## Superficie Oferta Trimestral
##################################################################

superf_o_trim <- aggregate(x = cs_demanda_fotocasa$mitjana_superf_o_mes,    
                           by = list(cs_demanda_fotocasa$property_id, 
                                     cs_demanda_fotocasa$trimestre),             
                           FUN = mean, na.action=NULL)       

names(superf_o_trim)[1:3] <- c("property_id", "trimestre", "superficie_oferta_trimestral")


##################################################################
############## Numero de leads per trimestre
##################################################################

num_leads_trim <- cs_demanda_fotocasa %>%                            
  group_by(property_id, municipality, trimestre) %>%
  dplyr::mutate(leads_trimestrals = cumsum(leads_mensuals))   

num_leads_trim <- arrange(num_leads_trim, leads_trimestrals)

153732723
153741692
153776847
153752751

num_leads_trim_1 <- num_leads_trim

num_leads_trim_1 <- num_leads_trim_1%>%
  group_by(property_id, NOMMUNI, trimestre)%>%
  filter(row_number()==n())

num_leads_trim_1 <- num_leads_trim_1[,c(1,2,18,45)]

names(num_leads_trim_1)[1:4] <- c("property_id", "NOMMUNI", "trimestre", "leads_trimestrals")

153732723
153741692
153776847
153752751

##################################################################
############## Preu Demanda Trimestral
##################################################################

preu_d_trim <- aggregate(x = cs_demanda_fotocasa$mitjana_price_d_mes,    
                         by = list(cs_demanda_fotocasa$property_id, 
                                   cs_demanda_fotocasa$trimestre),             
                         FUN = mean, na.action=NULL)                           

names(preu_d_trim)[1:3] <- c("property_id", "trimestre", "preu_demanda_trimestral")


##################################################################
############## Superficie Demanda Trimestral
##################################################################

superf_d_trim <- aggregate(x = cs_demanda_fotocasa$mitjana_superf_d_mes,    
                           by = list(cs_demanda_fotocasa$property_id, 
                                     cs_demanda_fotocasa$trimestre),             
                           FUN = mean, na.action=NULL)                           

names(superf_d_trim)[1:3] <- c("property_id", "trimestre", "superficie_demanda_trimestral")

##################################################################

trimestral_cat <- merge(x=preu_o_trim, y=superf_o_trim, 
                        by.x = c("property_id","trimestre"),
                        by.y = c("property_id","trimestre"))

trimestral_cat_2 <- merge(x=trimestral_cat, y=preu_d_trim, 
                          by.x = c("property_id","trimestre"),
                          by.y = c("property_id","trimestre"))

trimestral_cat_3 <- merge(x=trimestral_cat_2, y=superf_d_trim, 
                          by.x = c("property_id","trimestre"),
                          by.y = c("property_id","trimestre"))

trimestral_cat_4 <- merge(x=trimestral_cat_3, y=num_leads_trim_1, 
                          by.x = c("property_id","trimestre"),
                          by.y = c("property_id","trimestre"))

###########sumatori leads trimestrals per property_id, municipi i ponderacio trimestral
sumatori_leads_trimestrals <- trimestral_cat_4 %>%                            
  group_by(NOMMUNI, trimestre) %>%
  dplyr::mutate(leads_muni_trimestrals = cumsum(leads_trimestrals))

sumatori_leads_trimestrals <- sumatori_leads_trimestrals[order(sumatori_leads_trimestrals$leads_muni_trimestrals),]

sumatori_leads_trimestrals <- sumatori_leads_trimestrals%>%
  group_by(NOMMUNI, trimestre)%>%
  filter(row_number()==n())

sumatori_leads_trimestrals <- sumatori_leads_trimestrals[,c(7,2,9)]

trimestral_cat_4 <- merge(x=trimestral_cat_4, y=sumatori_leads_trimestrals, 
                          by.x = c("NOMMUNI","trimestre"),
                          by.y = c("NOMMUNI","trimestre"))

trimestral_cat_4$ponderacio_d <- trimestral_cat_4$leads_trimestrals*trimestral_cat_4$preu_demanda_trimestral

trimestral_cat_4["ponderacio_d"][is.na(trimestral_cat_4["ponderacio_d"])] <- 0

#################### Sumatori ponderacio trimestre municipis


sumatori_pond_muni_tri <- aggregate(trimestral_cat_4$ponderacio_d, 
                                    by=list(ponderacio_muni_trim=trimestral_cat_4$NOMMUNI, 
                                            ponderacio_muni_trim=trimestral_cat_4$trimestre), 
                                    FUN=sum)

names(sumatori_pond_muni_tri)[1:3] <- c("NOMMUNI", "trimestre", "ponderacio_muni_trim")

trimestral_cat_4 <- merge(x=trimestral_cat_4, y=sumatori_pond_muni_tri, 
                          by.x = c("NOMMUNI","trimestre"),
                          by.y = c("NOMMUNI","trimestre"))

#####################
#####################

#mitjana municipi

mitjana_municipi <- aggregate(x = trimestral_cat_4$preu_oferta_trimestral,    
                              by = list(trimestral_cat_4$NOMMUNI, 
                                        trimestral_cat_4$trimestre),             
                              FUN = mean, na.action=NULL)                           

names(mitjana_municipi)[1:3] <- c("NOMMUNI", "trimestre", "preu_mitja_o_muni_trim")

trimestral_cat_4 <- merge(x=trimestral_cat_4, y=mitjana_municipi, 
                          by.x = c("NOMMUNI","trimestre"),
                          by.y = c("NOMMUNI","trimestre"))

#mitjana demanda

trimestral_cat_4$preu_mitja_d_muni_trim <- trimestral_cat_4$ponderacio_muni_trim/trimestral_cat_4$leads_muni_trimestrals


################################ N o property_ids per trimestre

trimestral_cat_4$n <- 1

trimestral_cat_foto <- trimestral_cat_4

catalunya_noms <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Catalunya_noms_oficials.xlsx")
catalunya_noms <- catalunya_noms[,c(2,3,6,12,17,18)]

trimestral_cat_foto <- merge(trimestral_cat_foto, catalunya_noms,
                             by.x=c("NOMMUNI"),
                             by.y=c("NOMMUNI"))

trimestral_cat_foto$district <- "Undefined"
trimestral_cat_foto["preu_mitja_o_muni_trim"][is.na(trimestral_cat_foto["preu_mitja_o_muni_trim"])] <- 0
trimestral_cat_foto["preu_mitja_d_muni_trim"][is.na(trimestral_cat_foto["preu_mitja_d_muni_trim"])] <- 0

trimestral_cat_foto <-trimestral_cat_foto[,c(1, 20, 2:19)]

write.csv(trimestral_cat_foto, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/serie_of_dem_fotocasa_trimestrals.csv")


##################################################################
############## BCN Trimestrals
##################################################################

cs_demanda_fotocasa_bc <- foto_demanda_series_tract%>%
  filter(NOMMUNI=="Barcelona")


#adding trimestre
cs_demanda_fotocasa_bc <- cs_demanda_fotocasa_bc %>% mutate(trimestre =
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



###########################################################
######## Mitjanes de superficie
###########################################################
cs_demanda_fotocasa_bc <- cs_demanda_fotocasa_bc[cs_demanda_fotocasa_bc$surface_d != -1, ] 

mitjana_surface_d_bc <- aggregate(x = cs_demanda_fotocasa_bc$surface_d,    
                                  by = list(cs_demanda_fotocasa_bc$property_id, 
                                            cs_demanda_fotocasa_bc$district, 
                                            cs_demanda_fotocasa_bc$mes),             
                                  FUN = mean, na.action=NULL)                           

names(mitjana_surface_d_bc)[1:4] <- c("property_id", "district", "mes", "mitjana_superf_d_mes")

cs_demanda_fotocasa_bc <- merge(x=cs_demanda_fotocasa_bc, y=mitjana_surface_d_bc, 
                                  by.x=c("property_id","district", "mes"), 
                                  by.y=c("property_id","district", "mes"))


153732723
153741692
##################################################################
############## Mitjanes de preu
##################################################################

mitjana_price_d_bc <- aggregate(x = cs_demanda_fotocasa_bc$price_d,    
                                by = list(cs_demanda_fotocasa_bc$property_id, 
                                          cs_demanda_fotocasa_bc$district, 
                                          cs_demanda_fotocasa_bc$mes),             
                                FUN = mean, na.action=NULL)                           

names(mitjana_price_d_bc)[1:4] <- c("property_id", "district", "mes", "mitjana_price_d_mes")


cs_demanda_fotocasa_bc <- merge(x=cs_demanda_fotocasa_bc, y=mitjana_price_d_bc, 
                                  by.x = c("property_id","district", "mes"),
                                  by.y = c("property_id","district", "mes"))

153732723
153741692

##################################################################
############## Preu M2
##################################################################

cs_demanda_fotocasa_bc$preu_m2_mes_d <- cs_demanda_fotocasa_bc$mitjana_price_d_mes/cs_demanda_fotocasa_bc$mitjana_superf_d_mes

##################################################################
############## Joining oferta i demanda abans de juntar les taules
##################################################################

cs_demanda_fotocasa_bc$date <- NULL
cs_demanda_fotocasa_bc$trimestre <- NULL
cs_demanda_fotocasa_bc$municipality <- NULL
cs_demanda_fotocasa_bc$CODIMUNI <- NULL
cs_demanda_fotocasa_bc$NOMMUNI <- NULL
cs_demanda_fotocasa_bc$property_subtype <- NULL


df4 <- cs_demanda_fotocasa_bc
df5 <- interquartil_bcn_1

df6 <- merge(df5, df4, by.x = c("property_id", "district","mes", "any"), 
             by.y =  c("property_id", "district","mes", "any"),
             all.x = TRUE, all.y = TRUE)

df6["mitjana_superf_o_mes"][is.na(df6["mitjana_superf_o_mes"])] <- 0
df6["mitjana_price_o_dist_mes"][is.na(df6["mitjana_price_o_dist_mes"])] <- 0
df6["preu_m2_mes"][is.na(df6["preu_m2_mes"])] <- 0

cs_demanda_fotocasa_bc <- df6

cs_demanda_fotocasa_bc$preu_ponderat <- cs_demanda_fotocasa_bc$mitjana_price_o_dist_mes*cs_demanda_fotocasa_bc$leads_mensuals

cs_demanda_fotocasa_bc["price_d"][is.na(cs_demanda_fotocasa_bc["price_d"])] <- 0
cs_demanda_fotocasa_bc["surface_d"][is.na(cs_demanda_fotocasa_bc["surface_d"])] <- 0
cs_demanda_fotocasa_bc["num_leads"][is.na(cs_demanda_fotocasa_bc["num_leads"])] <- 0
cs_demanda_fotocasa_bc["leads_mensuals"][is.na(cs_demanda_fotocasa_bc["leads_mensuals"])] <- 0
cs_demanda_fotocasa_bc["mitjana_superf_d_mes"][is.na(cs_demanda_fotocasa_bc["mitjana_superf_d_mes"])] <- 0
cs_demanda_fotocasa_bc["mitjana_price_d_mes"][is.na(cs_demanda_fotocasa_bc["mitjana_price_d_mes"])] <- 0
cs_demanda_fotocasa_bc["preu_m2_mes_d"][is.na(cs_demanda_fotocasa_bc["preu_m2_mes_d"])] <- 0
cs_demanda_fotocasa_bc["preu_ponderat"][is.na(cs_demanda_fotocasa_bc["preu_ponderat"])] <- 0

###############################################################################
############## Valids anunciantes despres de juntar les taules oferta i demanda
##############################################################################

cs_demanda_fotocasa_bc <- cs_demanda_fotocasa_bc%>%
  mutate(anunciant_valid_d = case_when(
    publisher_type == "Profesionales" ~ "Vàlid",
    publisher_type == "Particulares" & leads_mensuals < 2 ~ "No Vàlid",
    publisher_type == "Particulares" & leads_mensuals > 1 ~ "Vàlid",
    publisher_type == "Particulares" & leads_mensuals == NA ~ "No Vàlid",
    publisher_type == "Undefined" ~ "No Vàlid"
  )
  )


#cs_demanda_fotocasa_bc <- cs_demanda_fotocasa_bc%>%
  #mutate(preu_valid = case_when(
    #price_d >= 10 & price_d <= 10000 ~ "Vàlid",
    #surface_d >= 10 & surface_d <= 10000 ~ "Vàlid"
  #)
  #)

#cs_demanda_fotocasa_bc <- cs_demanda_fotocasa_bc%>%
  #mutate(dia_valid = case_when(
    #price_d >= 10 & price_d <= 10000 ~ "Vàlid",
    #surface_d >= 10 & surface_d <= 10000 ~ "Vàlid"
  #)
  #)

#cs_demanda_fotocasa_bc <- cs_demanda_fotocasa_bc%>%
  #mutate(anunciant_valid_d = case_when(
    #price_d >= 10 & price_d <= 10000 ~ "Vàlid",
    #surface_d >= 10 & surface_d <= 10000 ~ "Vàlid"
  #)
  #)

cs_demanda_fotocasa_bc <- cs_demanda_fotocasa_bc%>%
  arrange(property_id, trimestre, mes)

write.csv(cs_demanda_fotocasa_bc, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/serie_of_dem_fotocasa_mensuals_bcn.csv")


##################################################################
############## Agregacions Trimestrals BCN
##################################################################
count(cs_demanda_fotocasa_bc, "anunciant_valid_d")
cs_demanda_fotocasa_bc <- cs_demanda_fotocasa_bc %>%
  filter(anunciant_valid_d=="Vàlid" & preu_valid=="Vàlid" & dia_valid=="Vàlid")

##################################################################
############## Preu Oferta Trimestral
##################################################################

preu_o_trim <- aggregate(x = cs_demanda_fotocasa_bc$mitjana_price_o_dist_mes,    
                         by = list(cs_demanda_fotocasa_bc$property_id, 
                                   cs_demanda_fotocasa_bc$trimestre),             
                         FUN = mean, na.action=NULL)                           

names(preu_o_trim)[1:3] <- c("property_id", "trimestre", "preu_oferta_trimestral")


153732723

##################################################################
############## Superficie Oferta Trimestral
##################################################################

superf_o_trim <- aggregate(x = cs_demanda_fotocasa_bc$mitjana_superf_o_mes,    
                           by = list(cs_demanda_fotocasa_bc$property_id, 
                                     cs_demanda_fotocasa_bc$trimestre),             
                           FUN = mean, na.action=NULL)       

names(superf_o_trim)[1:3] <- c("property_id", "trimestre", "superficie_oferta_trimestral")


153732723


##################################################################
############## Numero de leads per trimestre
##################################################################

num_leads_trim <- cs_demanda_fotocasa_bc %>%                            
  group_by(property_id, district, trimestre) %>%
  dplyr::mutate(leads_trimestrals = cumsum(leads_mensuals))   

num_leads_trim <- arrange(num_leads_trim, leads_trimestrals)

num_leads_trim_1 <- num_leads_trim

num_leads_trim_1 <- num_leads_trim_1%>%
  group_by(property_id, district, trimestre)%>%
  filter(row_number()==n())

num_leads_trim_1 <- num_leads_trim_1[,c(1, 18, 14, 2, 45)]

names(num_leads_trim_1)[1:5] <- c("property_id", "trimestre",  "NOMMUNI", "district", "leads_trimestrals")



##################################################################
############## Preu Demanda Trimestral
##################################################################

preu_d_trim <- aggregate(x = cs_demanda_fotocasa_bc$mitjana_price_d_mes,    
                         by = list(cs_demanda_fotocasa_bc$property_id, 
                                   cs_demanda_fotocasa_bc$trimestre),             
                         FUN = mean, na.action=NULL)                           

names(preu_d_trim)[1:3] <- c("property_id", "trimestre", "preu_demanda_trimestral")


##################################################################
############## Superficie Demanda Trimestral
##################################################################

superf_d_trim <- aggregate(x = cs_demanda_fotocasa_bc$mitjana_superf_d_mes,    
                           by = list(cs_demanda_fotocasa_bc$property_id, 
                                     cs_demanda_fotocasa_bc$trimestre),             
                           FUN = mean, na.action=NULL)                           

names(superf_d_trim)[1:3] <- c("property_id", "trimestre", "superficie_demanda_trimestral")

##################################################################

trimestral_bcn <- merge(x=preu_o_trim, y=superf_o_trim, 
                        by.x = c("property_id","trimestre"),
                        by.y = c("property_id","trimestre"))


trimestral_bcn_2 <- merge(x=trimestral_bcn, y=preu_d_trim, 
                          by.x = c("property_id","trimestre"),
                          by.y = c("property_id","trimestre"))

trimestral_bcn_3 <- merge(x=trimestral_bcn_2, y=superf_d_trim, 
                          by.x = c("property_id","trimestre"),
                          by.y = c("property_id","trimestre"))

trimestral_bcn_4 <- merge(x=trimestral_bcn_3, y=num_leads_trim_1, 
                          by.x = c("property_id","trimestre"),
                          by.y = c("property_id","trimestre"))

trimestral_bcn_4["preu_demanda_trimestral"][is.na(trimestral_bcn_4["preu_demanda_trimestral"])] <- 0
trimestral_bcn_4["superficie_demanda_trimestral"][is.na(trimestral_bcn_4["superficie_demanda_trimestral"])] <- 0
trimestral_bcn_4["leads_trimestrals"][is.na(trimestral_bcn_4["leads_trimestrals"])] <- 0

###########sumatori leads trimestrals per property_id, district i ponderacio trimestral
sumatori_leads_trimestrals <- trimestral_bcn_4 %>%                            
  group_by(district, trimestre) %>%
  dplyr::mutate(leads_dist_trimestrals = cumsum(leads_trimestrals))

sumatori_leads_trimestrals <- sumatori_leads_trimestrals[order(sumatori_leads_trimestrals$leads_dist_trimestrals),]

sumatori_leads_trimestrals <- sumatori_leads_trimestrals%>%
  group_by(district, trimestre)%>%
  filter(row_number()==n())
sumatori_leads_trimestrals<-sumatori_leads_trimestrals[!(sumatori_leads_trimestrals$district=="Undefined"),]

sumatori_leads_trimestrals <- sumatori_leads_trimestrals[,c(8,2,10)]

trimestral_bcn_4 <- merge(x=trimestral_bcn_4, y=sumatori_leads_trimestrals, 
                          by.x = c("district","trimestre"),
                          by.y = c("district","trimestre"))

trimestral_bcn_4$ponderacio_d <- trimestral_bcn_4$leads_trimestrals*trimestral_bcn_4$preu_demanda_trimestral

trimestral_bcn_4["ponderacio_d"][is.na(trimestral_bcn_4["ponderacio_d"])] <- 0

#################### Sumatori ponderacio trimestre district


sumatori_pond_dist_tri <- aggregate(trimestral_bcn_4$ponderacio_d, 
                                    by=list(ponderacio_dist_trim=trimestral_bcn_4$district, 
                                            ponderacio_dist_trim=trimestral_bcn_4$trimestre), 
                                    FUN=sum)

names(sumatori_pond_dist_tri)[1:3] <- c("district", "trimestre", "ponderacio_dist_trim")

trimestral_bcn_4 <- merge(x=trimestral_bcn_4, y=sumatori_pond_dist_tri, 
                          by.x = c("district","trimestre"),
                          by.y = c("district","trimestre"))

#####################
#####################

#mitjana district

mitjana_district <- aggregate(x = trimestral_bcn_4$preu_oferta_trimestral,    
                              by = list(trimestral_bcn_4$district, 
                                        trimestral_bcn_4$trimestre),             
                              FUN = mean, na.action=NULL)                           

names(mitjana_district)[1:3] <- c("district", "trimestre", "preu_mitja_o_dist_trim")

trimestral_bcn_4 <- merge(x=trimestral_bcn_4, y=mitjana_district, 
                          by.x = c("district","trimestre"),
                          by.y = c("district","trimestre"))

#mitjana demanda

trimestral_bcn_4$preu_mitja_d_dist_trim <- trimestral_bcn_4$ponderacio_dist_trim/trimestral_bcn_4$leads_dist_trimestrals

trimestral_bcn_4["preu_mitja_d_dist_trim"][is.na(trimestral_bcn_4["preu_mitja_d_dist_trim"])] <- 0

################################ N o property_ids per trimestre

trimestral_bcn_4$n <- 1

trimestral_bcn_4 <- trimestral_bcn_4[!is.na(trimestral_bcn_4$district),]
trimestral_bcn_4 <- trimestral_bcn_4[!(trimestral_bcn_4$district =="N/A"),]

trimestral_bcn_foto <- trimestral_bcn_4
153732723

trimestral_bcn_foto <- merge(trimestral_bcn_foto, catalunya_noms,
                             by.x=c("NOMMUNI"),
                             by.y=c("NOMMUNI"))

write.csv(trimestral_bcn_foto, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/serie_of_dem_fotocasa_trimestrals_bcn.csv")




