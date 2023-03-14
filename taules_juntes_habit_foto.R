
options(scipen=999)
options(digits=3)

########### BCN fotocasa
####oferta
ap_bcn_dist_f <- aggregate(x = trimestral_bcn_foto$preu_mitja_o_dist_trim,    
                               by = list(trimestral_bcn_foto$NOMMUNI,
                                         trimestral_bcn_foto$trimestre),             
                           FUN = mean,round(mean(trimestral_bcn_foto$preu_mitja_o_dist_trim), digits=2))

names(ap_bcn_dist_f)[1:3] <- c("NOMMUNI", "trimestre", "preu_o_f_bcn_dist")


########### AMB fotocasa

####oferta
apamb_f2 <- aggregate(x = trimestral_cat_foto$preu_oferta_trimestral,    
                    by = list(trimestral_cat_foto$Ambit_2022,
                              trimestral_cat_foto$trimestre),             
                    FUN = mean,round(mean(trimestral_cat_foto$preu_oferta_trimestral), digits=2))

names(apamb_f2)[1:3] <- c("Àmbit_Metropolità", "trimestre", "preu_oferta_f_amb")
apamb_f2 <- apamb_f2%>% filter(Àmbit_Metropolità=="Metropolità")

preus_o_bcn_amb_f <- merge(ap_bcn_dist_f,apamb_f2,
                           by="trimestre")

########################################################################################
########################################################################################


########### BCN habitaclia
####oferta
ap_bcn_dist_h <- aggregate(x = trimestral_bcn_habit$preu_mitja_o_dist_trim,    
                           by = list(trimestral_bcn_habit$NOMMUNI,
                                     trimestral_bcn_habit$trimestre),             
                           FUN = mean,round(mean(trimestral_bcn_habit$preu_mitja_o_dist_trim), digits=2))

names(ap_bcn_dist_h)[1:3] <- c("NOMMUNI","trimestre", "preu_o_h_bcn_dist")


########### AMB habitaclia

####oferta

ap_cat_h <- aggregate(x = trimestral_cat_habit$preu_oferta_trimestral,    
                      by = list(trimestral_cat_habit$Ambit_2022,
                                trimestral_cat_habit$trimestre),             
                      FUN = mean,round(mean(trimestral_cat_habit$preu_oferta_trimestral), digits=2))

names(ap_cat_h)[1:3] <- c("Àmbit_Metropolità", "trimestre", "preu_oferta_h_amb")
ap_cat_h <- ap_cat_h%>% filter(Àmbit_Metropolità=="Metropolità")

preus_o_bcn_amb_h <- merge(ap_bcn_dist_h,ap_cat_h,
                           by="trimestre")

####################################################################
#### Demanda
####################################################################

#### Fotocasa BCN

dem_f_tri_bcn <- aggregate(x = trimestral_bcn_foto$preu_mitja_d_dist_trim,    
                      by = list(trimestral_bcn_foto$NOMMUNI,
                                trimestral_bcn_foto$trimestre),             
                      FUN = mean)

names(dem_f_tri_bcn)[1:3] <- c("NOMMUNI", "trimestre", "preu_demanda_f_bcn")


#### Fotocasa CAT

dem_f_tri_cat <- aggregate(x = trimestral_cat_foto$preu_mitja_d_muni_trim,    
                     by = list(trimestral_cat_foto$Ambit_2022,
                               trimestral_cat_foto$trimestre),             
                     FUN = mean)

names(dem_f_tri_cat)[1:3] <- c("Àmbit_Metropolità", "trimestre", "preu_demanda_f_cat")
dem_f_tri_cat <- dem_f_tri_cat%>% filter(Àmbit_Metropolità=="Metropolità")

##################################
#### Habitaclia BCN

dem_h_tri_bcn <- aggregate(x = trimestral_bcn_habit$preu_mitja_d_dist_trim,    
                               by = list(trimestral_bcn_habit$NOMMUNI,
                                         trimestral_bcn_habit$trimestre),             
                           FUN = mean)

names(dem_h_tri_bcn)[1:3] <- c("NOMMUNI", "trimestre", "preu_demanda_h_bcn")


#### Habitaclia CAT

dem_h_tri_cat <- aggregate(x = trimestral_cat_habit$preu_mitja_d_muni_trim,    
                       by = list(trimestral_cat_habit$Ambit_2022,
                                 trimestral_cat_habit$trimestre),             
                       FUN = mean)

names(dem_h_tri_cat)[1:3] <- c("Àmbit_Metropolità", "trimestre", "preu_demanda_h_cat")
dem_h_tri_cat <- dem_h_tri_cat%>% filter(Àmbit_Metropolità=="Metropolità")
####################################################################

final <- merge(preus_o_bcn_amb_f,preus_o_bcn_amb_h,
               by.x= c("trimestre", "NOMMUNI", "Àmbit_Metropolità"),
               by.y = c("trimestre", "NOMMUNI", "Àmbit_Metropolità"))

final <- merge(final, dem_f_tri_bcn,
               by.x= c("trimestre", "NOMMUNI"),
               by.y = c("trimestre", "NOMMUNI"))

final <- merge(final,dem_h_tri_bcn,
               by.x= c("trimestre", "NOMMUNI"),
               by.y = c("trimestre", "NOMMUNI"))

final <- merge(final,dem_f_tri_cat,
               by.x= c("trimestre", "Àmbit_Metropolità"),
               by.y = c("trimestre", "Àmbit_Metropolità"))

final <- merge(final,dem_h_tri_cat,
               by.x= c("trimestre", "Àmbit_Metropolità"),
               by.y = c("trimestre", "Àmbit_Metropolità"))



write.csv(final, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Trimestrals/final.csv",fileEncoding = "UTF-8")



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

write.csv(trimestral_junts_habit2, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Trimestrals/trimestrals_bcn_cat_final_habit.csv",fileEncoding = "UTF-8")


### Foto
trimestral_junts_foto3 <- trimestral_bcn_foto
names(trimestral_junts_foto3)[c(10,12,13,14)] <- c("leads_muni_trimestrals", "ponderacio_muni_trim", "preu_mitja_o_muni_trim", "preu_mitja_d_muni_trim")

trimestral_junts_foto4 <- trimestral_cat_foto
trimestral_junts_foto4 <- trimestral_junts_foto4%>%
  filter(NOMMUNI != c("Barcelona"))

trimestral_junts_foto4 <- bind_rows(trimestral_junts_foto4, trimestral_junts_foto3)

write.csv(trimestral_junts_foto4, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Trimestrals/trimestrals_bcn_cat_final_foto.csv",fileEncoding = "UTF-8")

#################### Plotting

library(ggplot2)
library(tidyverse)

df <- final %>%
  select(trimestre, preu_o_h_bcn_dist, preu_o_f_bcn_dist)
head(df)

df <- df%>%
  group_by(trimestre)%>%
  filter(row_number()==1)

df <- df %>%
  select(trimestre, preu_o_h_bcn_dist, preu_o_f_bcn_dist) %>%
  gather(key = "variable", value = "value", -trimestre)
head(df)


ggplot(df, aes(x = trimestre, y = value, group=variable)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue"))

######

dff <- final %>%
  select(trimestre, preu_demanda_f_bcn, preu_demanda_h_bcn)
head(df)

dff <- dff%>%
  group_by(trimestre)%>%
  filter(row_number()==1)

dff <- dff %>%
  select(trimestre, preu_demanda_f_bcn, preu_demanda_h_bcn) %>%
  gather(key = "variable", value = "value", -trimestre)
head(df2)

ggplot(dff, aes(x = trimestre, y = value, group=variable)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue"))


################ Nombre d'oferta
########### BCN fotocasa
####oferta
nombre_oferta_bcn_dist_f <- aggregate(x = trimestral_junts_foto3$n,    
                           by = list(trimestral_junts_foto3$district,
                                     trimestral_junts_foto3$trimestre),             
                           FUN = sum)

names(nombre_oferta_bcn_dist_f)[1:3] <- c("district", "trimestre", "nombre_oferta_bcn_f")

base_oferta <- nombre_oferta_bcn_dist_f %>%
  group_by(district, trimestre) %>%
  mutate(base = (nombre_oferta_bcn_f/nombre_oferta_bcn_f[trimestre=="2019/T1"])*100)

base_oferta <- aggregate(x = base_oferta$base,    
                                      by = list(base_oferta$trimestre),             
                                      FUN = mean)
names(base_oferta)[1:2] <- c("trimestre", "nombre_oferta_bcn_f")

########### CAT fotocasa
####oferta

trimestral_junts_foto44 <- trimestral_junts_foto4%>%filter(NOMMUNI != "Barcelona")

nombre_oferta_cat_muni_f <- aggregate(x = trimestral_junts_foto44$n,    
                                      by = list(trimestral_junts_foto44$Ambit_2022,
                                                trimestral_junts_foto44$trimestre),             
                                      FUN = sum)

names(nombre_oferta_cat_muni_f)[1:3] <- c("Àmbit_Metropolità", "trimestre", "nombre_oferta_cat_f")
nombre_oferta_cat_muni_f <- nombre_oferta_cat_muni_f%>% filter(Àmbit_Metropolità=="Metropolità")

base_oferta_2 <- nombre_oferta_cat_muni_f %>%
  group_by(Àmbit_Metropolità, trimestre) %>%
  mutate(nombre_oferta_cat_f = (nombre_oferta_cat_f/nombre_oferta_cat_f[trimestre=="2019/T1"])*100)

base_oferta_bcn_cat_f <- merge(base_oferta,base_oferta_2,
                           by="trimestre")

################ Nombre d'oferta
########### BCN habitaclia
####oferta
nombre_oferta_bcn_dist_h <- aggregate(x = trimestral_junts_habit1$n,    
                                      by = list(trimestral_junts_habit1$district,
                                                trimestral_junts_habit1$trimestre),             
                                      FUN = sum)

names(nombre_oferta_bcn_dist_h)[1:3] <- c("district", "trimestre", "nombre_oferta_bcn_h")

base_oferta_3 <- nombre_oferta_bcn_dist_h %>%
  group_by(district, trimestre) %>%
  mutate(nombre_oferta_bcn_h = (nombre_oferta_bcn_h/nombre_oferta_bcn_h[trimestre=="2019/T1"])*100)

base_oferta_3 <- aggregate(x = base_oferta_3$nombre_oferta_bcn_h,    
                         by = list(base_oferta_3$trimestre),             
                         FUN = mean)
names(base_oferta_3)[1:2] <- c("trimestre", "nombre_oferta_bcn_h")

########### CAT habitaclia
####oferta
trimestral_junts_habit22 <- trimestral_junts_habit2%>%filter(NOMMUNI != "Barcelona")

nombre_oferta_cat_muni_h <- aggregate(x = trimestral_junts_habit22$n,    
                                      by = list(trimestral_junts_habit22$Ambit_2022,
                                                trimestral_junts_habit22$trimestre),             
                                      FUN = sum)

names(nombre_oferta_cat_muni_h)[1:3] <- c("Àmbit_Metropolità", "trimestre", "nombre_oferta_cat_h")
nombre_oferta_cat_muni_h <- nombre_oferta_cat_muni_h%>% filter(Àmbit_Metropolità=="Metropolità")

base_oferta_4 <- nombre_oferta_cat_muni_h %>%
  group_by(Àmbit_Metropolità, trimestre) %>%
  mutate(nombre_oferta_cat_h = (nombre_oferta_cat_h/nombre_oferta_cat_h[trimestre=="2019/T1"])*100)

base_oferta_bcn_cat_h <- merge(base_oferta_3,base_oferta_4,
                             by="trimestre")

##### Joining the two tables

nombre_oferta <- merge(base_oferta_bcn_cat_f, base_oferta_bcn_cat_h,
                       by.x = c("trimestre", "Àmbit_Metropolità"),
                       by.y = c("trimestre", "Àmbit_Metropolità"))
nombre_oferta$Àmbit_Metropolità <- NULL

write.csv(nombre_oferta, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Trimestrals/final_nombre_oferta.csv",fileEncoding = "UTF-8")


#####

dff_n <- nombre_oferta %>%
  select(trimestre, nombre_oferta_bcn_f, nombre_oferta_cat_f, nombre_oferta_bcn_h, nombre_oferta_cat_h)
head(dff_n)

dff_n2 <- dff_n %>%
  select(trimestre, nombre_oferta_bcn_f, nombre_oferta_cat_f, nombre_oferta_bcn_h, nombre_oferta_cat_h) %>%
  gather(key = "variable", value = "value", -trimestre)
head(dff_n2)

ggplot(dff_n2, aes(x = trimestre, y = value, group=variable)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue", "blue", "black"))

############################################################

################ Intensitat de demanda
########### BCN fotocasa
####demanda
nombre_demanda_bcn_dist_f <- trimestral_junts_foto3%>%filter(leads_trimestrals > 0)

nombre_demanda_bcn_dist_f <- aggregate(x = nombre_demanda_bcn_dist_f$n,    
                                       by = list(nombre_demanda_bcn_dist_f$district,
                                                 nombre_demanda_bcn_dist_f$trimestre),             
                                       FUN = sum)

names(nombre_demanda_bcn_dist_f)[1:3] <- c("district", "trimestre", "nombre_demanda_bcn_f")

base_demanda <- nombre_demanda_bcn_dist_f %>%
  group_by(district, trimestre) %>%
  mutate(base = (nombre_demanda_bcn_f/nombre_demanda_bcn_f[trimestre=="2019/T1"])*100)

base_demanda <- aggregate(x = base_demanda$base,    
                          by = list(base_demanda$trimestre),             
                          FUN = mean)
names(base_demanda)[1:2] <- c("trimestre", "nombre_demanda_bcn_f")

########### CAT fotocasa
####demanda

nombre_demanda_bcn_dist_f <- trimestral_junts_foto4%>%filter(leads_trimestrals > 0)

nombre_demanda_cat_muni_f <- nombre_demanda_bcn_dist_f%>%filter(NOMMUNI != "Barcelona")

nombre_demanda_cat_muni_f <- aggregate(x = nombre_demanda_cat_muni_f$n,    
                                       by = list(nombre_demanda_cat_muni_f$Ambit_2022,
                                                 nombre_demanda_cat_muni_f$trimestre),             
                                       FUN = sum)

names(nombre_demanda_cat_muni_f)[1:3] <- c("Àmbit_Metropolità", "trimestre", "nombre_demanda_cat_f")
nombre_demanda_cat_muni_f <- nombre_demanda_cat_muni_f%>% filter(Àmbit_Metropolità=="Metropolità")

base_demanda_2 <- nombre_demanda_cat_muni_f %>%
  group_by(Àmbit_Metropolità, trimestre) %>%
  mutate(nombre_demanda_cat_f = (nombre_demanda_cat_f/nombre_demanda_cat_f[trimestre=="2019/T1"])*100)

base_demanda_bcn_cat_f <- merge(base_demanda,base_demanda_2,
                                by="trimestre")

################ Intensitat de demanda
########### BCN habitaclia
####demanda

nombre_demanda_bcn_dist_h <- trimestral_junts_habit1%>%filter(leads_trimestrals > 0)

nombre_demanda_bcn_dist_h <- aggregate(x = nombre_demanda_bcn_dist_h$n,    
                                       by = list(nombre_demanda_bcn_dist_h$district,
                                                 nombre_demanda_bcn_dist_h$trimestre),             
                                       FUN = sum)

names(nombre_demanda_bcn_dist_h)[1:3] <- c("district", "trimestre", "nombre_demanda_bcn_h")

base_demanda_3 <- nombre_demanda_bcn_dist_h %>%
  group_by(district, trimestre) %>%
  mutate(nombre_demanda_bcn_h = (nombre_demanda_bcn_h/nombre_demanda_bcn_h[trimestre=="2019/T1"])*100)

base_demanda_3 <- aggregate(x = base_demanda_3$nombre_demanda_bcn_h,    
                            by = list(base_demanda_3$trimestre),             
                            FUN = mean)
names(base_demanda_3)[1:2] <- c("trimestre", "nombre_demanda_bcn_h")

########### CAT habitaclia
####demanda
nombre_demanda_cat_muni_h <- trimestral_junts_habit2%>%filter(leads_trimestrals > 0)

nombre_demanda_cat_muni_h <- nombre_demanda_cat_muni_h%>%filter(NOMMUNI != "Barcelona")

nombre_demanda_cat_muni_h <- aggregate(x = nombre_demanda_cat_muni_h$n,    
                                       by = list(nombre_demanda_cat_muni_h$Ambit_2022,
                                                 nombre_demanda_cat_muni_h$trimestre),             
                                       FUN = sum)

names(nombre_demanda_cat_muni_h)[1:3] <- c("Àmbit_Metropolità", "trimestre", "nombre_demanda_cat_h")
nombre_demanda_cat_muni_h <- nombre_demanda_cat_muni_h%>% filter(Àmbit_Metropolità=="Metropolità")

base_demanda_4 <- nombre_demanda_cat_muni_h %>%
  group_by(Àmbit_Metropolità, trimestre) %>%
  mutate(nombre_demanda_cat_h = (nombre_demanda_cat_h/nombre_demanda_cat_h[trimestre=="2019/T1"])*100)

base_demanda_bcn_cat_h <- merge(base_demanda_3,base_demanda_4,
                                by="trimestre")

##### Joining the two tables

nombre_demanda <- merge(base_demanda_bcn_cat_f, base_demanda_bcn_cat_h,
                        by.x = c("trimestre", "Àmbit_Metropolità"),
                        by.y = c("trimestre", "Àmbit_Metropolità"))
nombre_demanda$Àmbit_Metropolità <- NULL

write.csv(nombre_demanda, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/Observatori_Metropolita/Dades/Trimestrals/final_nombre_demanda.csv",fileEncoding = "UTF-8")


#####

dff_n_demanda <- nombre_demanda %>%
  select(trimestre, nombre_demanda_bcn_f, nombre_demanda_cat_f, nombre_demanda_bcn_h, nombre_demanda_cat_h)
head(dff_n_demanda)

dff_n_demanda2 <- dff_n_demanda %>%
  select(trimestre, nombre_demanda_bcn_f, nombre_demanda_cat_f, nombre_demanda_bcn_h, nombre_demanda_cat_h) %>%
  gather(key = "variable", value = "value", -trimestre)
head(dff_n_demanda2)

ggplot(dff_n_demanda2, aes(x = trimestre, y = value, group=variable)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue", "blue", "black"))

############################################################

