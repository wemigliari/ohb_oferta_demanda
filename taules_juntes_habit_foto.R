
trimestral_cat_habit_amb <- trimestral_cat_habit
trimestral_cat_foto_amb <- trimestral_cat_foto

########### BCN fotocasa
####oferta
ap_bcn_dist_f <- aggregate(x = trimestral_bcn_foto$preu_oferta_trimestral,    
                               by = list(trimestral_bcn_foto$NOMMUNI,
                                         trimestral_bcn_foto$trimestre),             
                               FUN = mean,round(mean(trimestral_bcn_foto$preu_oferta_trimestral), digits=2))

names(ap_bcn_dist_f)[1:3] <- c("NOMMUNI", "trimestre", "preu_o_f_bcn_dist")


########### AMB fotocasa
####oferta
apamb_f2 <- aggregate(x = trimestral_cat_foto_amb$preu_oferta_trimestral,    
                    by = list(trimestral_cat_foto_amb$Ambit_2022,
                              trimestral_cat_foto_amb$trimestre),             
                    FUN = mean,round(mean(trimestral_cat_foto_amb$preu_oferta_trimestral), digits=2)) 
names(apamb_f2)[1:3] <- c("ambit_rest_cat_f", "trimestre", "preu_oferta_f_amb")
apamb_f2 <- apamb_f2%>% filter(ambit_rest_cat_f=="Metropolità")

preus_o_bcn_amb_f <- merge(ap_bcn_dist_f,apamb_f2,
                           by="trimestre")

########################################################################################
########################################################################################


########### BCN habitaclia
####oferta
ap_bcn_dist_h <- aggregate(x = trimestral_bcn_habit$preu_oferta_trimestral,    
                           by = list(trimestral_bcn_habit$NOMMUNI,
                                     trimestral_bcn_habit$trimestre),             
                           FUN = mean,round(mean(trimestral_bcn_habit$preu_oferta_trimestral), digits=2))

names(ap_bcn_dist_h)[1:3] <- c("NOMMUNI","trimestre", "preu_o_h_bcn_dist")


########### AMB habitaclia
####oferta

ap_cat_f <- aggregate(x = trimestral_cat_habit_amb$preu_oferta_trimestral,    
                      by = list(trimestral_cat_habit_amb$Ambit_2022,
                                trimestral_cat_habit_amb$trimestre),             
                      FUN = mean,round(mean(trimestral_cat_habit_amb$preu_mitja_o_muni_trim), digits=2)) 
names(ap_cat_f)[1:3] <- c("ambit_rest_cat_h", "trimestre", "preu_oferta_h_amb")
ap_cat_f <- ap_cat_f%>% filter(ambit_rest_cat_h=="Metropolità")

preus_o_bcn_amb_h <- merge(ap_bcn_dist_h,ap_cat_f,
                           by="trimestre")


####################################################################
#### Demanda
####################################################################

#### Fotocasa BCN

dem_f_tri_bcn <- aggregate(x = trimestral_cat_foto_amb$preu_demanda_trimestral,    
                      by = list(trimestral_cat_foto_amb$NOMMUNI,
                                trimestral_cat_foto_amb$trimestre),             
                      FUN = mean,round(mean(trimestral_cat_foto_amb$preu_demanda_trimestral), digits=2))

names(dem_f_tri_bcn)[1:3] <- c("dist_d_f_bcn", "trimestre", "preu_demanda_f_bcn")
dem_f_tri_bcn <- dem_f_tri_bcn%>% filter(dist_d_f_bcn=="Barcelona")

#### Fotocasa CAT

dem_f_tri_cat <- aggregate(x = trimestral_cat_foto_amb$preu_demanda_trimestral,    
                     by = list(trimestral_cat_foto_amb$Ambit_2022,
                               trimestral_cat_foto_amb$trimestre),             
                     FUN = mean,round(mean(trimestral_cat_foto_amb$preu_demanda_trimestral), digits=2))

names(dem_f_tri_cat)[1:3] <- c("ambit_d_f_cat", "trimestre", "preu_demanda_f_cat")
dem_f_tri_cat <- dem_f_tri_cat%>% filter(ambit_d_f_cat=="Metropolità")

##################################
#### Habitaclia BCN

dem_h_tri_bcn <- aggregate(x = trimestral_cat_habit_amb$preu_demanda_trimestral,    
                               by = list(trimestral_cat_habit_amb$NOMMUNI,
                                         trimestral_cat_habit_amb$trimestre),             
                               FUN = mean,round(mean(trimestral_cat_habit_amb$preu_demanda_trimestral), digits=2))

names(dem_h_tri_bcn)[1:3] <- c("ambit_d_h_bcn", "trimestre", "preu_demanda_h_bcn")
dem_h_tri_bcn <- dem_h_tri_bcn%>% filter(ambit_d_h_bcn=="Barcelona")


#### Habitaclia CAT

dem_h_tri_cat <- aggregate(x = trimestral_cat_habit_amb$preu_demanda_trimestral,    
                       by = list(trimestral_cat_habit_amb$Ambit_2022,
                                 trimestral_cat_habit_amb$trimestre),             
                       FUN = mean,round(mean(trimestral_cat_habit_amb$preu_demanda_trimestral), digits=2))

names(dem_h_tri_cat)[1:3] <- c("ambit_d_h_cat", "trimestre", "preu_demanda_h_cat")
dem_h_tri_cat <- dem_h_tri_cat%>% filter(ambit_d_h_cat=="Metropolità")
####################################################################

final <- merge(preus_o_bcn_amb_f,preus_o_bcn_amb_h,
               by.x= c("trimestre", "NOMMUNI"),
               by.y = c("trimestre", "NOMMUNI"))

final <- merge(final, dem_f_tri_bcn,
               by="trimestre")

final <- merge(final,dem_f_tri_cat,
               by="trimestre")

final <- merge(final,dem_h_tri_bcn,
               by="trimestre")

final <- merge(final,dem_h_tri_cat,
               by="trimestre")

final <- final%>%
  group()

write.csv(final, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Trimestrals/final.csv")



#################################juntant taules foto i habit
#################################
### habit
library(lubridate)
library(dplyr)

trimestral_junts_habit1 <- trimestral_bcn_habit
names(trimestral_junts_habit1)[c(10,12,13,14)] <- c("leads_muni_trimestrals", "ponderacio_muni_trim", "preu_mitja_o_muni_trim", "preu_mitja_d_muni_trim")

trimestral_junts_habit2 <- trimestral_cat_habit
trimestral_junts_habit2 <- trimestral_junts_habit2%>%
  filter(NOMMUNI != c("Barcelona"))

trimestral_junts_habit2 <- bind_rows(trimestral_junts_habit2, trimestral_junts_habit1)

write.csv(trimestral_junts_habit2, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Trimestrals/trimestrals_bcn_cat_final_habit.csv")


### Foto
trimestral_junts_foto3 <- trimestral_bcn_foto
names(trimestral_junts_foto3)[c(10,12,13,14)] <- c("leads_muni_trimestrals", "ponderacio_muni_trim", "preu_mitja_o_muni_trim", "preu_mitja_d_muni_trim")

trimestral_junts_foto4 <- trimestral_cat_foto
trimestral_junts_foto4 <- trimestral_junts_foto4%>%
  filter(NOMMUNI != c("Barcelona"))

trimestral_junts_fotot4 <- bind_rows(trimestral_junts_foto4, trimestral_junts_foto3)

write.csv(trimestral_junts_fotot4, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Trimestrals/trimestrals_bcn_cat_final_foto.csv")


