#### Author: M Samuel
#### Date: March 2023
#### This script calculates the proportion of the population eligible for the DWMP

## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(purrr)
library(broom)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)
library(skimr)






##################################
# READ IN FILES
##################################

dt <- read_csv(here::here("output/data", "BMI_complete_median_eth_corrected.csv"))

age_group <- read_feather(here::here ("output/data", "complete_meds_2021.feather"))





### create a joining file for age
age_group <- age_group %>% 
  dplyr::select("patient_id",
                "age_group_2")







dt <- dt %>% 
  dplyr::select(
    "patient_id", 
    "year",   
    "median_bmi", 
    "sex", 
    "region", 
    "imd", 
    "eth_16_corrected", 
    starts_with("comorbid")             
  )

dt <- dt %>% 
  left_join(age_group, by= 'patient_id')



dt %>% 
  tabyl(age_group_2)

# filter out those with problems with join
dt <- dt  %>% 
  drop_na(age_group_2)


## Filter out years with  missing BMI
dt <- dt %>% 
  drop_na(median_bmi)


dt <- dt %>% 
  dplyr::mutate(minority = case_when(
    (eth_16_corrected == "White_British" | eth_16_corrected == "White_Irish" | eth_16_corrected == "Other_White"|     eth_16_corrected == "None" ) ~ "white", 

  )) 

 dt <- dt %>%  mutate(
  minority  = as.character(minority),
  minority = ifelse(is.na(minority), "not_white", minority),
  minority = as.factor(minority)) 

 dt %>%
   tabyl(minority)
 





dt_2021 <- dt %>% 
  dplyr::filter(year >= 2017) 


dt_2021 <- dt_2021 %>% 
  dplyr::group_by(patient_id) %>% 
  arrange(desc(year), .by_group=TRUE)  %>% 
  slice_head %>% 
  ungroup()



######################################################
## Categorise weight_universal:  BMI < 18.5 underweight; BMI 18 < 25 healthy, BMI 25 < 30 overweight , BMI >= 30 obese
## Categorise weight_DWMP (ethnicity specific cut offs): obese:  BMI >= 30 in white, BMI >= 27.5 in minority ethnic groups
## minority variable flags individuals as white/not white:  None, not_white, white
######################################################

dt_2021 <- dt_2021 %>% 
  dplyr::mutate(weight_universal = case_when(
    median_bmi <18.5 ~ "underweight",
    median_bmi >= 18.5 & median_bmi < 25 ~ "healthy",
    median_bmi >= 25 & median_bmi < 30 ~ "overweight",
    median_bmi >= 30 ~ "obese",
  ))



 dt_2021 <- dt_2021 %>% 
  dplyr::mutate(weight_DWMP = case_when(
    ((median_bmi >= 30 & minority == "white") | (median_bmi >= 27.5 & minority == "not_white"))    ~  "DWMP_obese",
    ((median_bmi < 30 & minority == "white") | (median_bmi < 27.5 & minority == "not_white"))    ~  "DWMP_not_obese"
  )) 


 
 
 dt_2021 <- dt_2021 %>% 
   dplyr::mutate(dwmp_eligible = case_when(
     (weight_DWMP == "DWMP_obese" & (comorbid_hypertension == "TRUE")) |(weight_DWMP == "DWMP_obese" & (comorbid_diabetes_t1 == "TRUE"))|(weight_DWMP == "DWMP_obese" & (comorbid_diabetes_t2 == "TRUE")) ~ "DWMP_eligible", 
      (weight_DWMP == "DWMP_not_obese") ~ "not_dwmp", 
     (weight_DWMP == "DWMP_obese" & ((comorbid_hypertension != "TRUE") & (comorbid_diabetes_t1 != "TRUE") & (comorbid_diabetes_t2 != "TRUE")))~ "not_dwmp"
   ))
 
 
dt_2021 %>% 
  tabyl(dwmp_eligible)
 
dt_2021 %>% 
  tabyl(eth_16_corrected)

 ### WRITE FUNCTIONS

universal_weight_f <- function (data, var){
  data %>% 
  tabyl({{ var }}, weight_universal) %>%
  dplyr::rename(group = 1) %>% 
  dplyr::mutate(group = as.factor(group))
}



DWMP_weight_f <- function (data, var){
  data %>% 
    tabyl({{ var }}, weight_DWMP) %>%
    dplyr::rename(group = 1) %>% 
    dplyr::mutate(group = as.factor(group))
}


DWMP_eligible_f <- function (data, var){
  data %>% 
    tabyl({{ var }}, dwmp_eligible) %>%
    dplyr::rename(group = 1) %>% 
    dplyr::mutate(group = as.factor(group))
}


#####


sex <-  universal_weight_f(dt_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(dt_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(dt_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(dt_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(dt_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)

universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)

####





sex <-  DWMP_weight_f(dt_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(dt_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(dt_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(dt_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(dt_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)




### 

sex <-  DWMP_eligible_f(dt_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_eligible_f(dt_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_eligible_f(dt_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_eligible_f(dt_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_eligible_f(dt_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_eligible <- sex %>% 
  bind_rows(age, eth, imd, region)



summary_2021 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))%>% 
  left_join(DWMP_eligible, by = c("variable", "group"))


### T2D 2021
#### 
T2D_2021 <- dt_2021 %>% 
  dplyr::filter(comorbid_diabetes_t2 == TRUE)


sex <-  universal_weight_f(T2D_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(T2D_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(T2D_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(T2D_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(T2D_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


###
sex <-  DWMP_weight_f(T2D_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(T2D_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(T2D_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(T2D_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(T2D_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


T2D_2021 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))



### Hypertension_2021

BP_2021 <- dt_2021 %>% 
  dplyr::filter(comorbid_hypertension == TRUE)



sex <-  universal_weight_f(BP_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(BP_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(BP_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(BP_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(BP_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)









sex <-  DWMP_weight_f(BP_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(BP_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(BP_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(BP_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(BP_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


BP_2021 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))



########### Join into single table

summary_2021 <- summary_2021 %>% 
  dplyr::mutate (category = "all", .before=1)





BP_2021 <- BP_2021 %>% 
  dplyr::mutate (category = "BP", .before=1)


T2D_2021 <- T2D_2021 %>% 
  dplyr::mutate (category = "T2D", .before=1)


summary_2021 <- summary_2021 %>% 
  bind_rows(BP_2021, 
            T2D_2021)


summary_2021 <- summary_2021  %>% 
  dplyr::mutate(healthy = plyr::round_any(summary_2021$healthy, 5)) %>% 
  dplyr::mutate(obese = plyr::round_any(summary_2021$obese, 5))%>% 
  dplyr::mutate(overweight = plyr::round_any(summary_2021$overweight, 5))%>% 
  dplyr::mutate(underweight = plyr::round_any(summary_2021$underweight, 5))%>% 
  dplyr::mutate(DWMP_not_obese = plyr::round_any(summary_2021$DWMP_not_obese, 5))%>% 
  dplyr::mutate(DWMP_obese = plyr::round_any(summary_2021$DWMP_obese, 5))%>% 
  dplyr::mutate(DWMP_eligible = plyr::round_any(summary_2021$DWMP_eligible, 5))%>% 
  dplyr::mutate(not_dwmp = plyr::round_any(summary_2021$not_dwmp, 5))


write_csv (summary_2021, here::here ("output/data","Ethnicity_corrected_DWMP_2021.csv"))