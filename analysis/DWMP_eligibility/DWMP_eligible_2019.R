#### Author: M Samuel
#### Date: 15th June
####  This script looks at No. of people eligible for DWMP using 2015 - 2019 data

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

BMI_data <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))


## dplyr eligible using data from last 5 years
BMI_data <- BMI_data  %>% 
  dplyr::filter(year != "2021") %>% 
  dplyr::filter(year != "2020") 






# create a data set with all patients to join later
BMI_data_all <- BMI_data  %>% 
  group_by(patient_id) %>% 
  dplyr::arrange (desc(year)) %>% 
  dplyr::slice_head() %>% 
  dplyr::select(-c(DWMP, year))



# create data set with most recent BMI/DWMP eligibility
BMI_DWMP <- BMI_data %>%
  drop_na(median_bmi)

BMI_DWMP <- BMI_DWMP %>% 
  dplyr::select("patient_id", "DWMP", "year") %>% 
  group_by(patient_id) %>% 
  dplyr::arrange(patient_id, desc(year))  %>% 
  slice_head


## Join the two 
BMI_all <- BMI_data_all %>% 
  dplyr::left_join(BMI_DWMP)



## Can present data: proportion of population eligible for DWMP
#1. All patients
#2. those who have had a BMI

## proportions all patients: 
BMI_all_patients <- BMI_all 

BMI_all_patients <- BMI_all_patients %>%  
  dplyr::mutate(DWMP = replace_na(DWMP, "no_bmi")) 

all <- BMI_all_patients %>% 
  tabyl(DWMP) %>% 
  dplyr::rename(group = DWMP) %>% 
  dplyr::mutate(variable = "DWMP")

year <- BMI_all_patients %>% 
  tabyl(year) %>% 
  dplyr::rename(group = year) %>% 
  dplyr::mutate(variable = "year")

year <- year %>%
  dplyr::mutate(group = replace_na(group, "no_bmi")) 

all <- all %>% 
  dplyr::bind_rows(year)






## calculate DWMP eligibility by exposure group

BMI_all_patients %>% 
  tabyl(comorbid_diabetes_t1, DWMP)
  


DWMP_function <- function(my_var) {
  v1 <- deparse(substitute(my_var))
  
  BMI_all_patients %>%
    tabyl({{my_var}}, DWMP)%>% 
    dplyr::mutate(total = (eligible + no_bmi + not_eligible)) %>%
    dplyr::mutate(prop_eligible = eligible/total) %>%
    dplyr::mutate(total_bmi = eligible + not_eligible) %>%
    dplyr::mutate(prop_not_eligible = not_eligible/total) %>%
    dplyr::mutate(prop_no_bmi = no_bmi/total) %>%
    dplyr::mutate(prop_eligible_bmi = eligible/total_bmi) %>% 
    dplyr::mutate(prop_not_eligible_bmi = not_eligible/total_bmi) %>%
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(group={{my_var}}) %>% 
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
} 


## Calculate DWMP for each
## Calculate DWMP for each
sex <- DWMP_function( sex)
age_group_2 <- DWMP_function( age_group_2)
eth_group_16 <- DWMP_function( eth_group_16)
imd <- DWMP_function( imd)
region <- DWMP_function( region)
hypertension <- DWMP_function( comorbid_hypertension)
diabetes_t1 <- DWMP_function( comorbid_diabetes_t1)
diabetes_t2 <- DWMP_function( comorbid_diabetes_t2)
chronic_cardiac <- DWMP_function( comorbid_chronic_cardiac)
learning_disability <- DWMP_function( comorbid_learning_disability)
depression <- DWMP_function( comorbid_depression)
dementia <- DWMP_function( comorbid_dementia)
psychosis_schiz_bipolar <- DWMP_function( comorbid_psychosis_schiz_bipolar)
asthma <- DWMP_function( comorbid_asthma)
COPD <- DWMP_function( comorbid_COPD)
stroke_and_TIA <- DWMP_function( comorbid_stroke_and_TIA)
smoking_status <- DWMP_function( smoking_status)


complete_percentages <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status)


## CREATE DATA SETS FILTERED BY DIABETESS AND HYPERTENSION

BMI_hypertension <- BMI_all_patients %>% 
  dplyr::filter(comorbid_hypertension == "TRUE")



DWMP_hypertension_function <- function(my_var) {
  v1 <- deparse(substitute(my_var))
  
  BMI_hypertension %>%
    tabyl({{my_var}}, DWMP)%>% 
    dplyr::mutate(total = (eligible + no_bmi + not_eligible)) %>%
    dplyr::mutate(total_bmi = eligible + not_eligible) %>%
    dplyr::mutate(prop_eligible = eligible/total) %>%
    dplyr::mutate(prop_not_eligible = not_eligible/total) %>%
    dplyr::mutate(prop_no_bmi = no_bmi/total) %>%
    dplyr::mutate(prop_eligible_bmi = eligible/total_bmi) %>% 
    dplyr::mutate(prop_not_eligible_bmi = not_eligible/total_bmi) %>%
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(group={{my_var}}) %>% 
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
} 


sex <- DWMP_hypertension_function( sex)
age_group_2 <- DWMP_hypertension_function( age_group_2)
eth_group_16 <- DWMP_hypertension_function( eth_group_16)
imd <- DWMP_hypertension_function( imd)
region <- DWMP_hypertension_function( region)
hypertension <- DWMP_hypertension_function( comorbid_hypertension)
diabetes_t1 <- DWMP_hypertension_function( comorbid_diabetes_t1)
diabetes_t2 <- DWMP_hypertension_function( comorbid_diabetes_t2)
chronic_cardiac <- DWMP_hypertension_function( comorbid_chronic_cardiac)
learning_disability <- DWMP_hypertension_function( comorbid_learning_disability)
depression <- DWMP_hypertension_function( comorbid_depression)
dementia <- DWMP_hypertension_function( comorbid_dementia)
psychosis_schiz_bipolar <- DWMP_hypertension_function( comorbid_psychosis_schiz_bipolar)
asthma <- DWMP_hypertension_function( comorbid_asthma)
COPD <- DWMP_hypertension_function( comorbid_COPD)
stroke_and_TIA <- DWMP_hypertension_function( comorbid_stroke_and_TIA)
smoking_status <- DWMP_hypertension_function( smoking_status)


complete_hypertension <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status)



#### Diabetes Type 2

BMI_diabetes_t1 <- BMI_all_patients %>% 
  dplyr::filter(comorbid_diabetes_t1 == "TRUE")



DWMP_diabetes_t1_function <- function(my_var) {
  v1 <- deparse(substitute(my_var))
  
  BMI_diabetes_t1 %>%
    tabyl({{my_var}}, DWMP)%>% 
    dplyr::mutate(total = (eligible + no_bmi + not_eligible)) %>%
    dplyr::mutate(total_bmi = eligible + not_eligible) %>%
    dplyr::mutate(prop_eligible = eligible/total) %>%
    dplyr::mutate(prop_not_eligible = not_eligible/total) %>%
    dplyr::mutate(prop_no_bmi = no_bmi/total) %>%
    dplyr::mutate(prop_eligible_bmi = eligible/total_bmi) %>% 
    dplyr::mutate(prop_not_eligible_bmi = not_eligible/total_bmi) %>%
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(group={{my_var}}) %>% 
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
} 


sex <- DWMP_diabetes_t1_function( sex)
age_group_2 <- DWMP_diabetes_t1_function( age_group_2)
eth_group_16 <- DWMP_diabetes_t1_function( eth_group_16)
imd <- DWMP_diabetes_t1_function( imd)
region <- DWMP_diabetes_t1_function( region)
hypertension <- DWMP_diabetes_t1_function( comorbid_hypertension)
diabetes_t1 <- DWMP_diabetes_t1_function( comorbid_diabetes_t1)
diabetes_t2 <- DWMP_diabetes_t1_function( comorbid_diabetes_t2)
chronic_cardiac <- DWMP_diabetes_t1_function( comorbid_chronic_cardiac)
learning_disability <- DWMP_diabetes_t1_function( comorbid_learning_disability)
depression <- DWMP_diabetes_t1_function( comorbid_depression)
dementia <- DWMP_diabetes_t1_function( comorbid_dementia)
psychosis_schiz_bipolar <- DWMP_diabetes_t1_function( comorbid_psychosis_schiz_bipolar)
asthma <- DWMP_diabetes_t1_function( comorbid_asthma)
COPD <- DWMP_diabetes_t1_function( comorbid_COPD)
stroke_and_TIA <- DWMP_diabetes_t1_function( comorbid_stroke_and_TIA)
smoking_status <- DWMP_diabetes_t1_function( smoking_status)


complete_diabetes_t1 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status)


## diabetes type 2

## CREATE DATA SETS FILTERED BY DIABETESS AND HYPERTENSION

BMI_diabetes_t2 <- BMI_all_patients %>% 
  dplyr::filter(comorbid_diabetes_t2 == "TRUE")



DWMP_diabetes_t2_function <- function(my_var) {
  v1 <- deparse(substitute(my_var))
  
  BMI_diabetes_t2 %>%
    tabyl({{my_var}}, DWMP)%>% 
    dplyr::mutate(total = (eligible + no_bmi + not_eligible)) %>%
    dplyr::mutate(total_bmi = eligible + not_eligible) %>%
    dplyr::mutate(prop_eligible = eligible/total) %>%
    dplyr::mutate(prop_not_eligible = not_eligible/total) %>%
    dplyr::mutate(prop_no_bmi = no_bmi/total) %>%
    dplyr::mutate(prop_eligible_bmi = eligible/total_bmi) %>% 
    dplyr::mutate(prop_not_eligible_bmi = not_eligible/total_bmi) %>%
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(group={{my_var}}) %>% 
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
} 


sex <- DWMP_diabetes_t2_function( sex)
age_group_2 <- DWMP_diabetes_t2_function( age_group_2)
eth_group_16 <- DWMP_diabetes_t2_function( eth_group_16)
imd <- DWMP_diabetes_t2_function( imd)
region <- DWMP_diabetes_t2_function( region)
hypertension <- DWMP_diabetes_t2_function( comorbid_hypertension)
diabetes_t2 <- DWMP_diabetes_t2_function( comorbid_diabetes_t2)
chronic_cardiac <- DWMP_diabetes_t2_function( comorbid_chronic_cardiac)
learning_disability <- DWMP_diabetes_t2_function( comorbid_learning_disability)
depression <- DWMP_diabetes_t2_function( comorbid_depression)
dementia <- DWMP_diabetes_t2_function( comorbid_dementia)
psychosis_schiz_bipolar <- DWMP_diabetes_t2_function( comorbid_psychosis_schiz_bipolar)
asthma <- DWMP_diabetes_t2_function( comorbid_asthma)
COPD <- DWMP_diabetes_t2_function( comorbid_COPD)
stroke_and_TIA <- DWMP_diabetes_t2_function( comorbid_stroke_and_TIA)
smoking_status <- DWMP_diabetes_t2_function( smoking_status)


complete_diabetes_t2 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status)


all <- all %>% 
  dplyr::mutate(n = plyr::round_any(all$n, 5))  


complete_percentages <- complete_percentages %>% 
  dplyr::mutate(eligible = plyr::round_any(complete_percentages$eligible, 5)) %>%
  dplyr::mutate(not_eligible = plyr::round_any(complete_percentages$not_eligible, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_percentages$no_bmi, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete_percentages$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_percentages$total_bmi, 5))

complete_hypertension <- complete_hypertension %>% 
  dplyr::mutate(eligible = plyr::round_any(complete_hypertension$eligible, 5)) %>%
  dplyr::mutate(not_eligible = plyr::round_any(complete_hypertension$not_eligible, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_hypertension$no_bmi, 5)) %>% 
  dplyr::mutate(total = plyr::round_any(complete_hypertension$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_hypertension$total_bmi, 5))

complete_diabetes_t1 <- complete_diabetes_t1 %>% 
  dplyr::mutate(eligible = plyr::round_any(complete_diabetes_t1$eligible, 5)) %>%
  dplyr::mutate(not_eligible = plyr::round_any(complete_diabetes_t1$not_eligible, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_diabetes_t1$no_bmi, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete_diabetes_t1$total, 5))%>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_diabetes_t1$total_bmi, 5))


complete_diabetes_t2 <- complete_diabetes_t2 %>% 
  dplyr::mutate(eligible = plyr::round_any(complete_diabetes_t2$eligible, 5)) %>%
  dplyr::mutate(not_eligible = plyr::round_any(complete_diabetes_t2$not_eligible, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_diabetes_t2$no_bmi, 5))%>%
  dplyr::mutate(total = plyr::round_any(complete_diabetes_t2$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_diabetes_t2$total_bmi, 5))



write.csv (all, here::here ("output/data","DWMP_eligible_5yr_data_years_2019.csv"))
write.csv (complete_percentages, here::here ("output/data","DWMP_eligible_5yr_data_all_2019.csv"))
write.csv (complete_hypertension, here::here ("output/data","DWMP_eligible_5yr_data_hypertension_2019.csv"))
write.csv (complete_diabetes_t1, here::here ("output/data","DWMP_eligible_5yr_data_T1DM_2019.csv"))
write.csv (complete_diabetes_t2, here::here ("output/data","DWMP_eligible_5yr_data_T2DM_2019.csv"))
