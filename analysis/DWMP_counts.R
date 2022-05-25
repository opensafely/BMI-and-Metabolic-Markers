## This script looks at proportion eligible for DWMP 
## Author: M Samuel
## Date: 24th May 2022



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
library(lubridate)
library(skimr)
library(ggplot2)


DWMP <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

DWMP2 <- DWMP %>% 
  dplyr::filter(year==2017| year==2018| year==2019| year==2020| year==2021)


DWMP2 <- DWMP2 %>%
  rename_all(~stringr::str_replace(.,"^comorbid_",""))

DWMP2 <- DWMP2 %>% 
  dplyr::select(
    c("patient_id",
     "median_bmi",
      "sex", 
     "age_group_2", 
     "ethnic_no_miss", 
     "eth_group_16",
     "imd", 
     "region", 
     "hypertension",  
     "diabetes_t1",           
     "diabetes_t2",
     "chronic_cardiac", 
     "learning_disability",     
     "depression",            
     "dementia",               
     "psychosis_schiz_bipolar",
     "asthma",                
     "COPD",                   
     "stroke_and_TIA",       
     "all_cancer", 
     "smoking_status", 
     "year", 
     "DWMP")
  )

## drop NA in BMI
DWMP2 <- DWMP2 %>% 
  drop_na(median_bmi)


DWMP2 <- DWMP2 %>% 
  dplyr::mutate(DWMP = case_when(
    DWMP == "eligible" ~ "DWMP_eligible", 
    DWMP == "not_eligible" ~ "not_eligible"
  )) 


DWMP2 %>% 
  tabyl(sex, DWMP) %>% 
  adorn_totals()

DWMP_eligible_function <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data %>%
    tabyl({{var}}, DWMP) %>% 
    adorn_totals() %>%
    dplyr::mutate(N_total = DWMP_eligible + not_eligible) %>% 
    dplyr::mutate(proportion = DWMP_eligible/N_total) %>% 
    dplyr::select(-(not_eligible)) %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
    ungroup()%>%
    dplyr::mutate(DWMP_eligible = plyr::round_any(DWMP_eligible, 5)) %>% 
    dplyr::mutate(N_total = plyr::round_any(N_total, 5))  %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(variable = (v1))
}

DWMP_eligible_function(DWMP2, sex)

age_group_2 <- DWMP_eligible_function (DWMP2, age_group_2)
sex <- DWMP_eligible_function(DWMP2, sex)
ethnic_no_miss <- DWMP_eligible_function(DWMP2, ethnic_no_miss)
eth_group_16 <- DWMP_eligible_function(DWMP2, eth_group_16)
imd <- DWMP_eligible_function(DWMP2, imd)
region <- DWMP_eligible_function(DWMP2, region)
hypertension <- DWMP_eligible_function(DWMP2, hypertension)
diabetes_t1 <- DWMP_eligible_function(DWMP2, diabetes_t1)
diabetes_t2 <- DWMP_eligible_function(DWMP2, diabetes_t2)
chronic_cardiac <- DWMP_eligible_function(DWMP2, chronic_cardiac)
learning_disability <- DWMP_eligible_function(DWMP2, learning_disability)
depression <- DWMP_eligible_function(DWMP2, depression)
dementia <- DWMP_eligible_function(DWMP2, dementia)
psychosis_schiz_bipolar <- DWMP_eligible_function(DWMP2, psychosis_schiz_bipolar)
asthma <- DWMP_eligible_function(DWMP2, asthma)
COPD <- DWMP_eligible_function(DWMP2, COPD)
stroke_and_TIA <- DWMP_eligible_function(DWMP2, stroke_and_TIA)
all_cancer <- DWMP_eligible_function(DWMP2, all_cancer)
smoking_status <- DWMP_eligible_function(DWMP2, smoking_status)
year <- DWMP_eligible_function(DWMP2, year)



counts_DWMP_total_pop <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  bind_rows(year)  %>%
  dplyr::select(variable, group, DWMP_eligible, N_total, proportion) 






## Counts in those belonging to any eligible group
DWMP_all <- DWMP2 %>% 
  dplyr::filter(hypertension==TRUE | diabetes_t1 == TRUE | diabetes_t2 == TRUE)

age_group_2 <- DWMP_eligible_function (DWMP_all, age_group_2)
sex <- DWMP_eligible_function(DWMP_all, sex)
ethnic_no_miss <- DWMP_eligible_function(DWMP_all, ethnic_no_miss)
eth_group_16 <- DWMP_eligible_function(DWMP_all, eth_group_16)
imd <- DWMP_eligible_function(DWMP_all, imd)
region <- DWMP_eligible_function(DWMP_all, region)
hypertension <- DWMP_eligible_function(DWMP_all, hypertension)
diabetes_t1 <- DWMP_eligible_function(DWMP_all, diabetes_t1)
diabetes_t2 <- DWMP_eligible_function(DWMP_all, diabetes_t2)
chronic_cardiac <- DWMP_eligible_function(DWMP_all, chronic_cardiac)
learning_disability <- DWMP_eligible_function(DWMP_all, learning_disability)
depression <- DWMP_eligible_function(DWMP_all, depression)
dementia <- DWMP_eligible_function(DWMP_all, dementia)
psychosis_schiz_bipolar <- DWMP_eligible_function(DWMP_all, psychosis_schiz_bipolar)
asthma <- DWMP_eligible_function(DWMP_all, asthma)
COPD <- DWMP_eligible_function(DWMP_all, COPD)
stroke_and_TIA <- DWMP_eligible_function(DWMP_all, stroke_and_TIA)
all_cancer <- DWMP_eligible_function(DWMP_all, all_cancer)
smoking_status <- DWMP_eligible_function(DWMP_all, smoking_status)
year <- DWMP_eligible_function(DWMP_all, year)



counts_DWMP_eligible <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  bind_rows(year)  %>%
  dplyr::select(variable, group, DWMP_eligible, N_total, proportion) 


### Just amongst those with hypertension
DWMP_hypertension <- DWMP2 %>% 
  dplyr::filter(hypertension==TRUE)

age_group_2 <- DWMP_eligible_function (DWMP_hypertension, age_group_2)
sex <- DWMP_eligible_function(DWMP_hypertension, sex)
ethnic_no_miss <- DWMP_eligible_function(DWMP_hypertension, ethnic_no_miss)
eth_group_16 <- DWMP_eligible_function(DWMP_hypertension, eth_group_16)
imd <- DWMP_eligible_function(DWMP_hypertension, imd)
region <- DWMP_eligible_function(DWMP_hypertension, region)
diabetes_t1 <- DWMP_eligible_function(DWMP_hypertension, diabetes_t1)
diabetes_t2 <- DWMP_eligible_function(DWMP_hypertension, diabetes_t2)
chronic_cardiac <- DWMP_eligible_function(DWMP_hypertension, chronic_cardiac)
learning_disability <- DWMP_eligible_function(DWMP_hypertension, learning_disability)
depression <- DWMP_eligible_function(DWMP_hypertension, depression)
dementia <- DWMP_eligible_function(DWMP_hypertension, dementia)
psychosis_schiz_bipolar <- DWMP_eligible_function(DWMP_hypertension, psychosis_schiz_bipolar)
asthma <- DWMP_eligible_function(DWMP_hypertension, asthma)
COPD <- DWMP_eligible_function(DWMP_hypertension, COPD)
stroke_and_TIA <- DWMP_eligible_function(DWMP_hypertension, stroke_and_TIA)
all_cancer <- DWMP_eligible_function(DWMP_hypertension, all_cancer)
smoking_status <- DWMP_eligible_function(DWMP_hypertension, smoking_status)
year <- DWMP_eligible_function(DWMP_hypertension, year)


counts_DWMP_hypertension <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  bind_rows(year)  %>%
  dplyr::select(variable, group, DWMP_eligible, N_total, proportion) 

## Just amongst those with T2DM


## Just amognst those with T1DM
DWMP_T1DM <- DWMP2 %>% 
  dplyr::filter(diabetes_t1 == TRUE)

age_group_2 <- DWMP_eligible_function (DWMP_T1DM, age_group_2)
sex <- DWMP_eligible_function(DWMP_T1DM, sex)
ethnic_no_miss <- DWMP_eligible_function(DWMP_T1DM, ethnic_no_miss)
eth_group_16 <- DWMP_eligible_function(DWMP_T1DM, eth_group_16)
imd <- DWMP_eligible_function(DWMP_T1DM, imd)
region <- DWMP_eligible_function(DWMP_T1DM, region)
hypertension <- DWMP_eligible_function(DWMP_T1DM, hypertension)
chronic_cardiac <- DWMP_eligible_function(DWMP_T1DM, chronic_cardiac)
learning_disability <- DWMP_eligible_function(DWMP_T1DM, learning_disability)
depression <- DWMP_eligible_function(DWMP_T1DM, depression)
dementia <- DWMP_eligible_function(DWMP_T1DM, dementia)
psychosis_schiz_bipolar <- DWMP_eligible_function(DWMP_T1DM, psychosis_schiz_bipolar)
asthma <- DWMP_eligible_function(DWMP_T1DM, asthma)
COPD <- DWMP_eligible_function(DWMP_T1DM, COPD)
stroke_and_TIA <- DWMP_eligible_function(DWMP_T1DM, stroke_and_TIA)
all_cancer <- DWMP_eligible_function(DWMP_T1DM, all_cancer)
smoking_status <- DWMP_eligible_function(DWMP_T1DM, smoking_status)
year <- DWMP_eligible_function(DWMP_T1DM, year)


counts_DWMP_T1DM <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  bind_rows(year)  %>%
  dplyr::select(variable, group, DWMP_eligible, N_total, proportion)  


 ## Just those with T2DM

DWMP_T2DM <- DWMP2 %>% 
  dplyr::filter(diabetes_t2 == TRUE)

age_group_2 <- DWMP_eligible_function (DWMP_T2DM, age_group_2)
sex <- DWMP_eligible_function(DWMP_T2DM, sex)
ethnic_no_miss <- DWMP_eligible_function(DWMP_T2DM, ethnic_no_miss)
eth_group_16 <- DWMP_eligible_function(DWMP_T2DM, eth_group_16)
imd <- DWMP_eligible_function(DWMP_T2DM, imd)
region <- DWMP_eligible_function(DWMP_T2DM, region)
hypertension <- DWMP_eligible_function(DWMP_T2DM, hypertension)
chronic_cardiac <- DWMP_eligible_function(DWMP_T2DM, chronic_cardiac)
learning_disability <- DWMP_eligible_function(DWMP_T2DM, learning_disability)
depression <- DWMP_eligible_function(DWMP_T2DM, depression)
dementia <- DWMP_eligible_function(DWMP_T2DM, dementia)
psychosis_schiz_bipolar <- DWMP_eligible_function(DWMP_T2DM, psychosis_schiz_bipolar)
asthma <- DWMP_eligible_function(DWMP_T2DM, asthma)
COPD <- DWMP_eligible_function(DWMP_T2DM, COPD)
stroke_and_TIA <- DWMP_eligible_function(DWMP_T2DM, stroke_and_TIA)
all_cancer <- DWMP_eligible_function(DWMP_T2DM, all_cancer)
smoking_status <- DWMP_eligible_function(DWMP_T2DM, smoking_status)
year <- DWMP_eligible_function(DWMP_T2DM, year)


counts_DWMP_T2DM <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  bind_rows(year)  %>%
  dplyr::select(variable, group, DWMP_eligible, N_total, proportion)  



write.csv (counts_DWMP_total_pop, here::here ("output/data","DWMP_counts_total_pop.csv"))
write.csv (counts_DWMP_eligible, here::here ("output/data","DWMP_counts_all_eligible.csv"))
write.csv (counts_DWMP_hypertension, here::here ("output/data","DWMP_counts_hypertension.csv"))
write.csv (counts_DWMP_T2DM, here::here ("output/data","DWMP_counts_T2DM.csv"))
write.csv (counts_DWMP_T1DM, here::here ("output/data","DWMP_counts_T1DM.csv"))
