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

cs_demanda_habitaclia <- habit_demanda_series_tract

2000001100000514119
#adding trimestre
cs_demanda_habitaclia <- cs_demanda_habitaclia %>% mutate(trimestre =
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
cs_demanda_habitaclia <- cs_demanda_habitaclia[cs_demanda_habitaclia$surface_d != -1, ] 

mitjana_surface_d_m <- aggregate(x = cs_demanda_habitaclia$surface_d,    
                                 by = list(cs_demanda_habitaclia$property_id, 
                                           cs_demanda_habitaclia$NOMMUNI, 
                                           cs_demanda_habitaclia$mes),             
                                 FUN = mean)                           

names(mitjana_surface_d_m)[1:4] <- c("property_id", "NOMMUNI", "mes", "mitjana_superf_d_mes")


cs_demanda_habitaclia <- merge(x=cs_demanda_habitaclia, y=mitjana_surface_d_m, by.x=c("property_id","NOMMUNI", "mes"), 
                    by.y=c("property_id","NOMMUNI", "mes"))

##################################################################
############## Mitjanes de preu
##################################################################

mitjana_price_d_m <- aggregate(x = cs_demanda_habitaclia$price_d,    
                               by = list(cs_demanda_habitaclia$property_id, 
                                         cs_demanda_habitaclia$NOMMUNI, 
                                         cs_demanda_habitaclia$mes),             
                               FUN = mean)                           

names(mitjana_price_d_m)[1:4] <- c("property_id", "NOMMUNI", "mes", "mitjana_price_d_mes")


cs_demanda_habitaclia <- merge(x=cs_demanda_habitaclia, y=mitjana_price_d_m, 
                    by.x = c("property_id","NOMMUNI", "mes"),
                    by.y = c("property_id","NOMMUNI", "mes"))

2000001100002716449

##################################################################
############## Preu M2
##################################################################

cs_demanda_habitaclia$preu_m2_mes_d <- cs_demanda_habitaclia$mitjana_price_d_mes/cs_demanda_habitaclia$mitjana_superf_d_mes


##################################################################
############## Joining oferta i demanda abans de juntar les taules
##################################################################

interquartil_cat_1 <- interquartil_cat_habit

cs_demanda_habitaclia$date <- NULL
cs_demanda_habitaclia$municipality <- NULL
cs_demanda_habitaclia$district <- NULL
cs_demanda_habitaclia$CODIMUNI <- NULL
cs_demanda_habitaclia$property_type <- NULL

cs_demanda_habitaclia <- merge(x=cs_demanda_habitaclia, y=interquartil_cat_1, 
                               by.x = c("property_id","NOMMUNI", "mes", "any", "trimestre"),
                               by.y = c("property_id","NOMMUNI", "mes", "any", "trimestre"))

cs_demanda_habitaclia$preu_ponderat <- cs_demanda_habitaclia$mitjana_price_o_mes*cs_demanda_habitaclia$leads_mensuals


###############################################################################
############## Valids anunciantes despres de juntar les taules oferta i demanda
##############################################################################

cs_demanda_habitaclia <- cs_demanda_habitaclia%>%
  mutate(anunciant_valid_d = case_when(
    publisher_type == "Professional" ~ "Vàlid",
    publisher_type == "Private" & leads_mensuals < 2 ~ "No Vàlid",
    publisher_type == "Private" & leads_mensuals > 1 ~ "Vàlid",
    publisher_type == "Private" & leads_mensuals == NA ~ "No Vàlid",
    publisher_type == "Undefined" ~ "No Vàlid"
  )
)

cs_demanda_habitaclia <- cs_demanda_habitaclia%>%
  arrange(property_id, trimestre, mes)

write.csv(cs_demanda_habitaclia, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/serie_of_dem_habitaclia_mensuals.csv")


##################################################################
############## Agregacions Trimestrals CAT
##################################################################
cs_demanda_habitaclia <- cs_demanda_habitaclia%>%
  filter(anunciant_valid_d=="Vàlid" & preu_valid=="Vàlid" & dia_valid=="Vàlid")

count(cs_demanda_habitaclia, "anunciant_valid_d")

##################################################################
############## Preu Oferta Trimestral
##################################################################

preu_o_trim <- aggregate(x = cs_demanda_habitaclia$mitjana_price_o_mes,    
                               by = list(cs_demanda_habitaclia$property_id, 
                                         cs_demanda_habitaclia$trimestre),             
                               FUN = mean)                           

names(preu_o_trim)[1:3] <- c("property_id", "trimestre", "preu_oferta_trimestral")


2000001100002716449

##################################################################
############## Superficie Oferta Trimestral
##################################################################

superf_o_trim <- aggregate(x = cs_demanda_habitaclia$mitjana_superf_o_mes,    
                               by = list(cs_demanda_habitaclia$property_id, 
                                         cs_demanda_habitaclia$trimestre),             
                               FUN = mean)       

names(superf_o_trim)[1:3] <- c("property_id", "trimestre", "superficie_oferta_trimestral")


2000001100002716449


##################################################################
############## Numero de leads per trimestre
##################################################################

num_leads_trim <- cs_demanda_habitaclia %>%                            
  group_by(property_id, NOMMUNI, trimestre) %>%
  dplyr::mutate(leads_trimestrals = cumsum(leads_mensuals))   

num_leads_trim <- arrange(num_leads_trim, leads_trimestrals)
2000222200003804476
2000001100000514119

num_leads_trim_1 <- num_leads_trim

num_leads_trim_1 <- num_leads_trim_1%>%
  group_by(property_id, NOMMUNI, trimestre)%>%
  filter(row_number()==n())

num_leads_trim_1 <- num_leads_trim_1[,c(1,2,5,44)]

names(num_leads_trim_1)[1:4] <- c("property_id", "NOMMUNI", "trimestre", "leads_trimestrals")


2000001100002716449

##################################################################
############## Preu Demanda Trimestral
##################################################################

preu_d_trim <- aggregate(x = cs_demanda_habitaclia$mitjana_price_d_mes,    
                         by = list(cs_demanda_habitaclia$property_id, 
                                   cs_demanda_habitaclia$trimestre),             
                         FUN = mean)                           

names(preu_d_trim)[1:3] <- c("property_id", "trimestre", "preu_demanda_trimestral")


##################################################################
############## Superficie Demanda Trimestral
##################################################################

superf_d_trim <- aggregate(x = cs_demanda_habitaclia$mitjana_superf_d_mes,    
                         by = list(cs_demanda_habitaclia$property_id, 
                                   cs_demanda_habitaclia$trimestre),             
                         FUN = mean)                           

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


#################### Sumatori ponderacio trimestre municipis


sumatori_pond_muni_tri <- aggregate(trimestral_cat_4$ponderacio_d, by=list(ponderacio_muni_trim=trimestral_cat_4$NOMMUNI, ponderacio_muni_trim=trimestral_cat_4$trimestre), FUN=sum)

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
                         FUN = mean)                           

names(mitjana_municipi)[1:3] <- c("NOMMUNI", "trimestre", "preu_mitja_o_muni_trim")

trimestral_cat_4 <- merge(x=trimestral_cat_4, y=mitjana_municipi, 
                         by.x = c("NOMMUNI","trimestre"),
                         by.y = c("NOMMUNI","trimestre"))

#mitjana demanda

trimestral_cat_4$preu_mitja_d_muni_trim <- trimestral_cat_4$ponderacio_muni_trim/trimestral_cat_4$leads_muni_trimestrals
  

################################ N o property_ids per trimestre

trimestral_cat_4$n <- 1

trimestral_cat_habit <- trimestral_cat_4

catalunya_noms <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Catalunya_noms_oficials.xlsx")
catalunya_noms <- catalunya_noms[,c(2,3,6,12,17,18)]

trimestral_cat_habit <- merge(trimestral_cat_habit,catalunya_noms,
                              by.x = c("NOMMUNI"),
                              by.y = c("NOMMUNI"))

write.csv(trimestral_cat_habit, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/serie_of_dem_habitaclia_trimestrals.csv")


##################################################################
############## BCN Trimestrals
##################################################################

cs_demanda_habitaclia_bc <- habit_demanda_series_tract%>%
  filter(NOMMUNI=="Barcelona")

2000001100000514119
#adding trimestre
cs_demanda_habitaclia_bc <- cs_demanda_habitaclia_bc %>% mutate(trimestre =
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
cs_demanda_habitaclia_bc <- cs_demanda_habitaclia_bc[cs_demanda_habitaclia_bc$surface_d != -1, ] 

mitjana_surface_d_bc <- aggregate(x = cs_demanda_habitaclia_bc$surface_d,    
                                  by = list(cs_demanda_habitaclia_bc$property_id, 
                                            cs_demanda_habitaclia_bc$district, 
                                            cs_demanda_habitaclia_bc$mes),             
                                  FUN = mean)                           

names(mitjana_surface_d_bc)[1:4] <- c("property_id", "district", "mes", "mitjana_superf_d_mes")

cs_demanda_habitaclia_bc <- merge(x=cs_demanda_habitaclia_bc, y=mitjana_surface_d_bc, by.x=c("property_id","district", "mes"), 
                                  by.y=c("property_id","district", "mes"))

##################################################################
############## Mitjanes de preu
##################################################################

mitjana_price_d_bc <- aggregate(x = cs_demanda_habitaclia_bc$price_d,    
                                by = list(cs_demanda_habitaclia_bc$property_id, 
                                          cs_demanda_habitaclia_bc$district, 
                                          cs_demanda_habitaclia_bc$mes),             
                                FUN = mean)                           

names(mitjana_price_d_bc)[1:4] <- c("property_id", "district", "mes", "mitjana_price_d_mes")


cs_demanda_habitaclia_bc <- merge(x=cs_demanda_habitaclia_bc, y=mitjana_price_d_bc, 
                                  by.x = c("property_id","district", "mes"),
                                  by.y = c("property_id","district", "mes"))

2000001100002716449

##################################################################
############## Preu M2
##################################################################

cs_demanda_habitaclia_bc$preu_m2_mes_d <- cs_demanda_habitaclia_bc$mitjana_price_d_mes/cs_demanda_habitaclia_bc$mitjana_superf_d_mes

##################################################################
############## Joining oferta i demanda abans de juntar les taules
##################################################################

interquartil_bc_1 <- interquartil_bcn
cs_demanda_habitaclia_bc$date <- NULL
cs_demanda_habitaclia_bc$trimestre <- NULL
cs_demanda_habitaclia_bc$municipality <- NULL
cs_demanda_habitaclia_bc$CODIMUNI <- NULL
cs_demanda_habitaclia_bc$NOMMUNI <- NULL
cs_demanda_habitaclia_bc$property_type <- NULL

cs_demanda_habitaclia_bc <- merge(x=cs_demanda_habitaclia_bc, y=interquartil_bc_1, 
                                  by.x = c("property_id", "district", "mes", "any"),
                                  by.y = c("property_id", "district", "mes", "any"))

cs_demanda_habitaclia_bc$preu_ponderat <- cs_demanda_habitaclia_bc$mitjana_price_o_mes*cs_demanda_habitaclia_bc$leads_mensuals


###############################################################################
############## Valids anunciantes despres de juntar les taules oferta i demanda
##############################################################################

cs_demanda_habitaclia_bc <- cs_demanda_habitaclia_bc%>%
  mutate(anunciant_valid_d = case_when(
    publisher_type == "Professional" ~ "Vàlid",
    publisher_type == "Private" & leads_mensuals < 2 ~ "No Vàlid",
    publisher_type == "Private" & leads_mensuals > 1 ~ "Vàlid",
    publisher_type == "Private" & leads_mensuals == NA ~ "No Vàlid",
    publisher_type == "Undefined" ~ "No Vàlid"
  )
  )

cs_demanda_habitaclia_bc <- cs_demanda_habitaclia_bc%>%
  arrange(property_id, trimestre, mes)

write.csv(cs_demanda_habitaclia_bc, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/serie_of_dem_habitaclia_mensuals_bcn.csv")


##################################################################
############## Agregacions Trimestrals BCN
##################################################################
count(cs_demanda_habitaclia_bc, "anunciant_valid_d")
cs_demanda_habitaclia_bc <- cs_demanda_habitaclia_bc %>%
  filter(anunciant_valid_d=="Vàlid" & preu_valid=="Vàlid" & dia_valid=="Vàlid")

count(cs_demanda_habitaclia_bc, "anunciant_valid_d")
##################################################################
############## Preu Oferta Trimestral
##################################################################

preu_o_trim <- aggregate(x = cs_demanda_habitaclia_bc$mitjana_price_o_mes,    
                         by = list(cs_demanda_habitaclia_bc$property_id, 
                                   cs_demanda_habitaclia_bc$trimestre),             
                         FUN = mean)                           

names(preu_o_trim)[1:3] <- c("property_id", "trimestre", "preu_oferta_trimestral")


2000001100002716449

##################################################################
############## Superficie Oferta Trimestral
##################################################################

superf_o_trim <- aggregate(x = cs_demanda_habitaclia_bc$mitjana_superf_o_mes,    
                           by = list(cs_demanda_habitaclia_bc$property_id, 
                                     cs_demanda_habitaclia_bc$trimestre),             
                           FUN = mean)       

names(superf_o_trim)[1:3] <- c("property_id", "trimestre", "superficie_oferta_trimestral")


2000001100002716449


##################################################################
############## Numero de leads per trimestre
##################################################################

num_leads_trim <- cs_demanda_habitaclia_bc %>%                            
  group_by(property_id, district, trimestre) %>%
  dplyr::mutate(leads_trimestrals = cumsum(leads_mensuals))   

num_leads_trim <- arrange(num_leads_trim, leads_trimestrals)
2000222200003804476
2000001100000514119

num_leads_trim_1 <- num_leads_trim

num_leads_trim_1 <- num_leads_trim_1%>%
  group_by(property_id, district, trimestre)%>%
  filter(row_number()==n())

num_leads_trim_1 <- num_leads_trim_1[,c(1, 24, 20, 2, 44)]

names(num_leads_trim_1)[1:5] <- c("property_id", "trimestre",  "NOMMUNI", "district", "leads_trimestrals")


2000001100002716449

##################################################################
############## Preu Demanda Trimestral
##################################################################

preu_d_trim <- aggregate(x = cs_demanda_habitaclia_bc$mitjana_price_d_mes,    
                         by = list(cs_demanda_habitaclia_bc$property_id, 
                                   cs_demanda_habitaclia_bc$trimestre),             
                         FUN = mean)                           

names(preu_d_trim)[1:3] <- c("property_id", "trimestre", "preu_demanda_trimestral")


##################################################################
############## Superficie Demanda Trimestral
##################################################################

superf_d_trim <- aggregate(x = cs_demanda_habitaclia_bc$mitjana_superf_d_mes,    
                           by = list(cs_demanda_habitaclia_bc$property_id, 
                                     cs_demanda_habitaclia_bc$trimestre),             
                           FUN = mean)                           

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


#################### Sumatori ponderacio trimestre district


sumatori_pond_dist_tri <- aggregate(trimestral_bcn_4$ponderacio_d, by=list(ponderacio_dist_trim=trimestral_bcn_4$district, ponderacio_dist_trim=trimestral_bcn_4$trimestre), FUN=sum)

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
                              FUN = mean)                           

names(mitjana_district)[1:3] <- c("district", "trimestre", "preu_mitja_o_dist_trim")

trimestral_bcn_4 <- merge(x=trimestral_bcn_4, y=mitjana_district, 
                          by.x = c("district","trimestre"),
                          by.y = c("district","trimestre"))

#mitjana demanda

trimestral_bcn_4$preu_mitja_d_dist_trim <- trimestral_bcn_4$ponderacio_dist_trim/trimestral_bcn_4$leads_dist_trimestrals


################################ N o property_ids per trimestre

trimestral_bcn_4$n <- 1

trimestral_bcn_4 <- trimestral_bcn_4[!is.na(trimestral_bcn_4$district),]

trimestral_bcn_habit <- trimestral_bcn_4

trimestral_bcn_habit <- merge(trimestral_bcn_habit,catalunya_noms,
                              by.x = c("NOMMUNI"),
                              by.y = c("NOMMUNI"))

write.csv(trimestral_bcn_habit, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/serie_of_dem_habitaclia_trimestrals_bcn.csv")






