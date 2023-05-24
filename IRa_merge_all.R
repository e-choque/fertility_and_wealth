#Carga de librer?as previamente instaladas
library(readstata13)
library(survey)
library(srvyr)
library(dplyr)
library(tidyverse)
library(sem)

library(sjPlot) #html table results
library(sjmisc)
library(sjlabelled)

library(AER) # Ivreg

library(knitr) #to create frecuency tables
library(mlogit)
#Colocar directorio entre las comillas ""
setwd("H:/Mi unidad/Consultorias/Gus_Canavire/R_dhs_V/Data")
dir()

# Bolivia
#boir <- read.dta13("BOIR51FL.DTA", convert.factors = F)
base_ir1 <-  select(boir, caseid,v000, v001, v005,v022,v023,v002,v003, v005, v012, v013, v025, v106,v133, v135, v137,
                    v139, v150, v151, v152,v201, v202, v203, v212,v213, v218, v219, v312, v364, v376,v3a08e, v605, 
                    v602, v616, v191, v190, v715,v731, b0_01, b0_02, b0_03, b0_04, b0_05, b0_06, b0_07,
                    b0_08,b0_09, b0_10, b0_11, b0_12,b0_13, b0_14,b0_15,b0_16,b0_17,b0_18,b0_19,
                    b0_20,b8_01, b8_02, b8_03, b8_04, b8_05, b8_06,b8_07,b8_08,b8_09,b8_10,b8_11,
                    b8_12,b8_13,b8_14,b8_15,b8_16,b8_17,b8_18,b8_19,b8_20) 
#v364 -never had sex not apply because there is no information 
#v312 - Current contraceptive method, yes we apply this restriction 
base_ir1 <- filter(base_ir1, v150==2 | v150==1)
base_ir1$score_wi <- (base_ir1$v191-min(base_ir1$v191, na.rm = T))*100/(max(base_ir1$v191, na.rm = T)-min(base_ir1$v191, na.rm = T))

# Colombia
#coir <- read.dta13("COIR53FL.DTA", convert.factors = F)
co_ir1 <-  select(coir, caseid,v000, v001, v005,v022,v023,v002,v003, v005, v012, v013, v025, v106,v133, v135, v137,
                  v139, v150, v151, v152,v201, v202, v203, v212,v213, v218, v219, v312, v364, v376,v3a08e, v605, 
                  v602, v616, v191, v190, v715,v731, b0_01, b0_02, b0_03, b0_04, b0_05, b0_06, b0_07,
                  b0_08,b0_09, b0_10, b0_11, b0_12,b0_13, b0_14,b0_15,b0_16,b0_17,b0_18,b0_19,
                  b0_20,b8_01, b8_02, b8_03, b8_04, b8_05, b8_06,b8_07,b8_08,b8_09,b8_10,b8_11,
                  b8_12,b8_13,b8_14,b8_15,b8_16,b8_17,b8_18,b8_19,b8_20)  
co_ir1 <- filter(co_ir1, v150==1 | v150==2)
co_ir1$score_wi <- (co_ir1$v191-min(co_ir1$v191, na.rm = T))*100/(max(co_ir1$v191, na.rm = T)-min(co_ir1$v191, na.rm = T))

# Republica Dominicana
#drir <- read.dta13("DRIR52FL.DTA", convert.factors = F)
dr_ir1 <-  select(drir, caseid,v000, v001, v005,v022,v023,v002,v003, v005, v012, v013, v025, v106,v133, v135, v137,
                  v139, v150, v151, v152,v201, v202, v203, v212,v213, v218, v219, v312, v364, v376,v3a08e, v605, 
                  v602, v616, v191, v190, v715,v731, b0_01, b0_02, b0_03, b0_04, b0_05, b0_06, b0_07,
                  b0_08,b0_09, b0_10, b0_11, b0_12,b0_13, b0_14,b0_15,b0_16,b0_17,b0_18,b0_19,
                  b0_20,b8_01, b8_02, b8_03, b8_04, b8_05, b8_06,b8_07,b8_08,b8_09,b8_10,b8_11,
                  b8_12,b8_13,b8_14,b8_15,b8_16,b8_17,b8_18,b8_19,b8_20) 
dr_ir1 <- filter(dr_ir1, v150==2 | v150==1)
dr_ir1$score_wi <- (dr_ir1$v191-min(dr_ir1$v191, na.rm = T))*100/(max(dr_ir1$v191, na.rm = T)-min(dr_ir1$v191, na.rm = T))

# Guyana
#gyir <- read.dta13("GYIR5IFL.DTA", convert.factors = F)
gy_ir1 <-  select(gyir, caseid,v000, v001, v005,v022,v023,v002,v003, v005, v012, v013, v025, v106,v133, v135, v137,
                  v139, v150, v151, v152,v201, v202, v203, v212,v213, v218, v219, v312, v364, v376,v3a08e, v605, 
                  v602, v616, v191, v190, v715,v731, b0_01, b0_02, b0_03, b0_04, b0_05, b0_06, b0_07,
                  b0_08,b0_09, b0_10, b0_11, b0_12,b0_13, b0_14,b0_15,b0_16,b0_17,b0_18,b0_19,
                  b0_20,b8_01, b8_02, b8_03, b8_04, b8_05, b8_06,b8_07,b8_08,b8_09,b8_10,b8_11,
                  b8_12,b8_13,b8_14,b8_15,b8_16,b8_17,b8_18,b8_19,b8_20)  
gy_ir1 <- filter(gy_ir1, v150==1 | v150==2)
gy_ir1$score_wi <- (gy_ir1$v191-min(gy_ir1$v191, na.rm = T))*100/(max(gy_ir1$v191, na.rm = T)-min(gy_ir1$v191, na.rm = T))

# Honduras
#hnir <- read.dta13("HNIR52FL.DTA", convert.factors = F)
hn_ir1 <-  select(hnir, caseid,v000, v001, v005,v022,v023,v002,v003, v005, v012, v013, v025, v106,v133, v135, v137,
                  v139, v150, v151, v152,v201, v202, v203, v212,v213, v218, v219, v312, v364, v376,v3a08e, v605, 
                  v602, v616, v191, v190, v715,v731, b0_01, b0_02, b0_03, b0_04, b0_05, b0_06, b0_07,
                  b0_08,b0_09, b0_10, b0_11, b0_12,b0_13, b0_14,b0_15,b0_16,b0_17,b0_18,b0_19,
                  b0_20,b8_01, b8_02, b8_03, b8_04, b8_05, b8_06,b8_07,b8_08,b8_09,b8_10,b8_11,
                  b8_12,b8_13,b8_14,b8_15,b8_16,b8_17,b8_18,b8_19,b8_20)  
hn_ir1 <- filter(hn_ir1, v150==2 | v150==1)
hn_ir1$score_wi <- (hn_ir1$v191-min(hn_ir1$v191, na.rm = T))*100/(max(hn_ir1$v191, na.rm = T)-min(hn_ir1$v191, na.rm = T))

# Haiti
#htir <- read.dta13("HTIR52FL.DTA", convert.factors = F)
ht_ir1 <-  select(htir, caseid,v000, v001, v005,v022,v023,v002,v003, v005, v012, v013, v025, v106,v133, v135, v137,
                  v139, v150, v151, v152,v201, v202, v203, v212,v213, v218, v219, v312, v364, v376,v3a08e, v605, 
                  v602, v616, v191, v190, v715,v731, b0_01, b0_02, b0_03, b0_04, b0_05, b0_06, b0_07,
                  b0_08,b0_09, b0_10, b0_11, b0_12,b0_13, b0_14,b0_15,b0_16,b0_17,b0_18,b0_19,
                  b0_20,b8_01, b8_02, b8_03, b8_04, b8_05, b8_06,b8_07,b8_08,b8_09,b8_10,b8_11,
                  b8_12,b8_13,b8_14,b8_15,b8_16,b8_17,b8_18,b8_19,b8_20)  
ht_ir1 <- filter(ht_ir1, v150==1 | v150==2)
ht_ir1$score_wi <- (ht_ir1$v191-min(ht_ir1$v191, na.rm = T))*100/(max(ht_ir1$v191, na.rm = T)-min(ht_ir1$v191, na.rm = T))

# Delete bases to release memory
# rm(boir, coir, drir, gyir, hnir, htir)

##### databases ####

# Append # see if it works
base_ir1 <- rbind(base_ir1, co_ir1)
base_ir1 <- rbind(base_ir1, dr_ir1)
base_ir1 <- rbind(base_ir1, gy_ir1)
base_ir1 <- rbind(base_ir1, hn_ir1)
base_ir1 <- rbind(base_ir1, ht_ir1) 

#base_ir3 <- mlogit.data(base_ir2, choice = "wealth_index", shape = "wide",
#                       drop.index = T) #done

#base_ir4 <- filter(base_ir1, max_agechildren<=13) #done

  
################ generation of variables ####
table(co_ir1$v376)
table(dr_ir1$v376)
table(gy_ir1$v376)
table(hn_ir1$v376)
table(ht_ir1$v376)
table(base_ir1$v376)

base_ir1<- base_ir1 %>% mutate(infertility1 = v376 %in% 24)
base_ir1$infertility1=as.numeric(base_ir1$infertility1)
table(base_ir1$infertility1)

base_ir1<- base_ir1 %>% mutate(infertility2 = v602 %in% 5)
base_ir1$infertility2=as.numeric(base_ir1$infertility2)

base_ir1 <-  base_ir1 %>% mutate(children_home= v202+ v203)
#base_ir1 <-  base_ir1 %>% mutate(check= v218 %in% 0)
#base_ir1$check=as.numeric(base_ir1$check)

table(base_ir1$children_home)

table(base_ir1$v218)
a1 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18) # check, the last number should be the highest
base_ir1 <- base_ir1 %>% mutate(children_dummy= v218 %in% a1)
base_ir1$children_dummy=as.numeric(base_ir1$children_dummy)
table(base_ir1$children_dummy)

table(base_ir1$v731)
base_ir1 <- base_ir1 %>% mutate(working = v731 %in% 2)
base_ir1$worked <- as.numeric(base_ir1$working)

base_ir1 <-  base_ir1 %>% mutate(urban=v025 %in% 1)
base_ir1$urban=as.numeric(base_ir1$urban)
table(base_ir1$urban)

base_ir1 <-  base_ir1 %>% mutate(instrument= infertility1 %in% 1 | infertility2 %in% 1)
base_ir1$instrument=as.numeric(base_ir1$instrument)

# after that, you can avoid the generation of max_agechildren for the base_ir2
base_ir1$b8_01[which(is.na(base_ir1$b8_01))] <- 0
base_ir1$b8_02[which(is.na(base_ir1$b8_02))] <- 0
base_ir1$b8_03[which(is.na(base_ir1$b8_03))] <- 0
base_ir1$b8_04[which(is.na(base_ir1$b8_04))] <- 0
base_ir1$b8_05[which(is.na(base_ir1$b8_05))] <- 0
base_ir1$b8_06[which(is.na(base_ir1$b8_06))] <- 0
base_ir1$b8_07[which(is.na(base_ir1$b8_07))] <- 0
base_ir1$b8_08[which(is.na(base_ir1$b8_08))] <- 0
base_ir1$b8_09[which(is.na(base_ir1$b8_09))] <- 0
base_ir1$b8_10[which(is.na(base_ir1$b8_10))] <- 0
base_ir1$b8_11[which(is.na(base_ir1$b8_11))] <- 0
base_ir1$b8_12[which(is.na(base_ir1$b8_12))] <- 0
base_ir1$b8_13[which(is.na(base_ir1$b8_13))] <- 0
base_ir1$b8_14[which(is.na(base_ir1$b8_14))] <- 0
base_ir1$b8_15[which(is.na(base_ir1$b8_15))] <- 0
base_ir1$b8_16[which(is.na(base_ir1$b8_16))] <- 0
base_ir1$b8_17[which(is.na(base_ir1$b8_17))] <- 0
base_ir1$b8_18[which(is.na(base_ir1$b8_18))] <- 0
base_ir1$b8_19[which(is.na(base_ir1$b8_19))] <- 0
base_ir1$b8_20[which(is.na(base_ir1$b8_20))] <- 0

which(colnames(base_ir1) == "b8_01") 
base_ir1$max_agechildren <- apply(base_ir1[, 58:77], 1, max) # the best option, but modify the number of column

# couple 
base_ir1 <-  base_ir1 %>% mutate(couple=v150 %in% 2)
base_ir1$couple=as.numeric(base_ir1$couple)
table(base_ir1$couple)


############# rename variables and labels ####
base_ir1 <- base_ir1 %>% rename(living_children=v218)
base_ir1 <- base_ir1 %>% rename(age=v012)
base_ir1 <- base_ir1 %>% rename(years_education=v133)
base_ir1 <- base_ir1 %>% rename(age_firstbirth=v212)
base_ir1 <- base_ir1 %>% rename(total_children=v201)
base_ir1 <- base_ir1 %>% rename(wealth_index=v190)
base_ir1 <- base_ir1 %>% rename(age_group=v013)
base_ir1 <- base_ir1 %>% rename(years_education_h=v715)
base_ir1 <- base_ir1 %>% rename(pregnant=v213)


base_ir1$wealth_index[base_ir1$wealth_index==1]="Poorest"
base_ir1$wealth_index[base_ir1$wealth_index==2]="Poorer"
base_ir1$wealth_index[base_ir1$wealth_index==3]="Middle"
base_ir1$wealth_index[base_ir1$wealth_index==4]="Richer"
base_ir1$wealth_index[base_ir1$wealth_index==5]="Richest"

base_ir1$age_group[base_ir1$age_group==1]="15-19"
base_ir1$age_group[base_ir1$age_group==2]="20-24"
base_ir1$age_group[base_ir1$age_group==3]="25-29"
base_ir1$age_group[base_ir1$age_group==4]="30-34"
base_ir1$age_group[base_ir1$age_group==5]="35-39"
base_ir1$age_group[base_ir1$age_group==6]="40-44"
base_ir1$age_group[base_ir1$age_group==7]="45-49"

#### Sample restriction ####
# restriction from Aguero & Marks(2011)   
table(base_ir1$v602) #Fertility preference option 6 not available
table(base_ir1$v312) #Current contraceptive method

base_ir1 <- filter(base_ir1, total_children>=1 & v312==0 & age_group>0 & years_education<25 & years_education_h<25) # done - before that, make sure to create the base_ir2 sample


#detele outlires: years_educ, group_age


#save.dta13(base_ir1, file = "base_ir1.DTA") #nay



######################## Econometric Model 1 ####
#possible independent var: 
# v012 - current age respondent 
# v202 v203 - Son and Doughters at home --> children_home
# v212 Age of respondent at 1st birth
# v133 - education in single years
#v137 -Number of children 5 and under
# living_children - Number of living children
#v201 -   Total children ever born
# v219 -Living children + current preg
# v220 - Living children + curr preg 6+
# 310 - Living children at first use
# v613 - Ideal number of children
# mm14 - Number of sibling's children


########################Survey design ####
#Update subsets for each country 
#bo_ir1 <- base_ir1 %>% filter(v000=="BO5")
#sd1 <- svydesign(ids = ~v001, strata = ~v022, weights = ~v005, data = bo_ir1)
#mean(bo_ir1$v012)
#svymean(~v012,design = sd1,deff=T)
#m_lm1sd1 <- svyglm(score_wi ~ living_children + v012 + v133 + v212 + urban,
               #data = bo_ir1, design = sd1, deff=T )
#summary(m_lm1sd1)
