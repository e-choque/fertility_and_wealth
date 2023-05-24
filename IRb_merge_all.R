#Carga de librer?as previamente instaladas
library(readstata13)
library(survey)
library(srvyr)
library(dplyr)
library(tidyverse)
library(sem) 
#Colocar directorio entre las comillas ""
setwd("J:/Mi unidad/Consultorias/Gus_Canavire/ProyectoDHS/Data/phase 5")
dir()
#create de new database ####
base_ir2 <- filter(base_ir1, total_children>=1 & age_group>0 & years_education<25 & years_education_h<25) # DONE make sure that base_ir1 is the first one, before v312=0

base_ir2 <- filter(base_ir2, urban==1) # para ejercicio se selecciona el area urbana 

#################### Generation of variables ####

b1 <- c(1,2,3,4,5)
base_ir2 <- base_ir2 %>% mutate(twin= b0_01 %in% b1 | b0_02 %in% b1 | b0_03 %in% b1 
                                | b0_04 %in% b1 | b0_05 %in% b1 | b0_06 %in% b1 | b0_07 %in% b1
                                | b0_08 %in% b1 | b0_09 %in% b1 | b0_10 %in% b1 | b0_11 %in% b1
                                | b0_12 %in% b1 | b0_13 %in% b1 | b0_14 %in% b1| b0_15 %in% b1
                                | b0_16 %in% b1 | b0_17 %in% b1 | b0_18 %in% b1 | b0_19 %in% b1
                                | b0_20 %in% b1)
base_ir2$twin <- as.numeric(base_ir2$twin)
table(base_ir2$twin)


############## Save data 
#save.dta13(base_ir2, file = "base_ir2.DTA") # DONE

