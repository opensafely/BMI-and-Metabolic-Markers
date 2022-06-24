

#### Author: M Samuel
#### Date: 15th June
####  This script looks proportion obese using BMI data from the last 5 years

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

BMI_all_patients <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))


## dplyr eligible using data from last 5 years
BMI_all_patients <- BMI_all_patients %>% 
  dplyr::filter(year == "2016")






# create a data set with all patients to join later


print('check_1')

## Can present data: proportion of population eligible for DWMP
#1. All patients
#2. those who have had a BMI

## proportions all patients: 

BMI_all_patients <- BMI_all_patients %>%  
  dplyr::mutate(obese = replace_na(obese, "no_bmi")) 






## calculate obese by exposure group

BMI_all_patients$obese[BMI_all_patients$obese == '0'] <- 'not_obese'
BMI_all_patients$obese[BMI_all_patients$obese == '1'] <- 'obese'


BMI_all <- BMI_all_patients





obese_function <- function(my_var) {
  v1 <- deparse(substitute(my_var))
  
  BMI_all_patients %>%
    tabyl({{my_var}}, obese)%>% 
    dplyr::mutate(total = (obese + no_bmi + not_obese)) %>%
    dplyr::mutate(prop_obese = obese/total) %>%
    dplyr::mutate(total_bmi = obese + not_obese) %>%
    dplyr::mutate(prop_not_obese = not_obese/total) %>%
    dplyr::mutate(prop_no_bmi = no_bmi/total) %>%
    dplyr::mutate(prop_obese_bmi = obese/total_bmi) %>% 
    dplyr::mutate(prop_not_obese_bmi = not_obese/total_bmi) %>%
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(group={{my_var}}) %>% 
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(se_prop = sqrt(((prop_obese*(1-prop_obese))/total)))  %>%
    dplyr::mutate(se_prop_bmi = sqrt(((prop_obese_bmi*(1-prop_obese_bmi))/total_bmi)))
  
} 


## Calculate obese
sex <- obese_function( sex)
age_group_2 <- obese_function( age_group_2)
eth_group_16 <- obese_function( eth_group_16)
imd <- obese_function( imd)
region <- obese_function( region)
hypertension <- obese_function( comorbid_hypertension)
diabetes_t1 <- obese_function( comorbid_diabetes_t1)
diabetes_t2 <- obese_function( comorbid_diabetes_t2)
chronic_cardiac <- obese_function( comorbid_chronic_cardiac)
learning_disability <- obese_function( comorbid_learning_disability)
depression <- obese_function( comorbid_depression)
dementia <- obese_function( comorbid_dementia)
psychosis_schiz_bipolar <- obese_function( comorbid_psychosis_schiz_bipolar)
asthma <- obese_function( comorbid_asthma)
COPD <- obese_function( comorbid_COPD)
stroke_and_TIA <- obese_function( comorbid_stroke_and_TIA)
smoking_status <- obese_function( smoking_status)


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



complete_percentages %>% 

## standard error of proportion: √(p(1−p)/n)


print('check_2')

## CREATE DATA SETS FILTERED BY DIABETESS AND HYPERTENSION

BMI_all_patients <- BMI_all %>% 
  dplyr::filter(comorbid_hypertension == "TRUE")


obese_hypertension_function <- function(my_var) {
  v1 <- deparse(substitute(my_var))
  
  BMI_all_patients %>%
    tabyl({{my_var}}, obese)%>% 
    dplyr::mutate(total = (obese + no_bmi + not_obese)) %>%
    dplyr::mutate(prop_obese = obese/total) %>%
    dplyr::mutate(total_bmi = obese + not_obese) %>%
    dplyr::mutate(prop_not_obese = not_obese/total) %>%
    dplyr::mutate(prop_no_bmi = no_bmi/total) %>%
    dplyr::mutate(prop_obese_bmi = obese/total_bmi) %>% 
    dplyr::mutate(prop_not_obese_bmi = not_obese/total_bmi) %>%
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(group={{my_var}}) %>% 
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(se_prop = sqrt(((prop_obese*(1-prop_obese))/total)))  %>%
    dplyr::mutate(se_prop_bmi = sqrt(((prop_obese_bmi*(1-prop_obese_bmi))/total_bmi)))
} 


sex <- obese_hypertension_function( sex)
age_group_2 <- obese_hypertension_function( age_group_2)
eth_group_16 <- obese_hypertension_function( eth_group_16)
imd <- obese_hypertension_function( imd)
region <- obese_hypertension_function( region)
hypertension <- obese_hypertension_function( comorbid_hypertension)
diabetes_t1 <- obese_hypertension_function( comorbid_diabetes_t1)
diabetes_t2 <- obese_hypertension_function( comorbid_diabetes_t2)
chronic_cardiac <- obese_hypertension_function( comorbid_chronic_cardiac)
learning_disability <- obese_hypertension_function( comorbid_learning_disability)
depression <- obese_hypertension_function( comorbid_depression)
dementia <- obese_hypertension_function( comorbid_dementia)
psychosis_schiz_bipolar <- obese_hypertension_function( comorbid_psychosis_schiz_bipolar)
asthma <- obese_hypertension_function( comorbid_asthma)
COPD <- obese_hypertension_function( comorbid_COPD)
stroke_and_TIA <- obese_hypertension_function( comorbid_stroke_and_TIA)
smoking_status <- obese_hypertension_function( smoking_status)


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

print('check_3')

#### Diabetes Type 2

BMI_all_patients <- BMI_all %>% 
  dplyr::filter(comorbid_diabetes_t1 == "TRUE")



obese_diabetes_t1_function <- function(my_var) {
  v1 <- deparse(substitute(my_var))
  
  BMI_all_patients %>%
    tabyl({{my_var}}, obese)%>% 
    dplyr::mutate(total = (obese + no_bmi + not_obese)) %>%
    dplyr::mutate(prop_obese = obese/total) %>%
    dplyr::mutate(total_bmi = obese + not_obese) %>%
    dplyr::mutate(prop_not_obese = not_obese/total) %>%
    dplyr::mutate(prop_no_bmi = no_bmi/total) %>%
    dplyr::mutate(prop_obese_bmi = obese/total_bmi) %>% 
    dplyr::mutate(prop_not_obese_bmi = not_obese/total_bmi) %>%
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(group={{my_var}}) %>% 
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(se_prop = sqrt(((prop_obese*(1-prop_obese))/total)))  %>%
    dplyr::mutate(se_prop_bmi = sqrt(((prop_obese_bmi*(1-prop_obese_bmi))/total_bmi)))
} 


sex <- obese_diabetes_t1_function( sex)
age_group_2 <- obese_diabetes_t1_function( age_group_2)
eth_group_16 <- obese_diabetes_t1_function( eth_group_16)
imd <- obese_diabetes_t1_function( imd)
region <- obese_diabetes_t1_function( region)
hypertension <- obese_diabetes_t1_function( comorbid_hypertension)
diabetes_t1 <- obese_diabetes_t1_function( comorbid_diabetes_t1)
diabetes_t2 <- obese_diabetes_t1_function( comorbid_diabetes_t2)
chronic_cardiac <- obese_diabetes_t1_function( comorbid_chronic_cardiac)
learning_disability <- obese_diabetes_t1_function( comorbid_learning_disability)
depression <- obese_diabetes_t1_function( comorbid_depression)
dementia <- obese_diabetes_t1_function( comorbid_dementia)
psychosis_schiz_bipolar <- obese_diabetes_t1_function( comorbid_psychosis_schiz_bipolar)
asthma <- obese_diabetes_t1_function( comorbid_asthma)
COPD <- obese_diabetes_t1_function( comorbid_COPD)
stroke_and_TIA <- obese_diabetes_t1_function( comorbid_stroke_and_TIA)
smoking_status <- obese_diabetes_t1_function( smoking_status)


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

print('check_4')
## diabetes type 2

## CREATE DATA SETS FILTERED BY DIABETESS AND HYPERTENSION

BMI_all_patients <- BMI_all %>% 
  dplyr::filter(comorbid_diabetes_t2 == "TRUE")



obese_diabetes_t2_function <- function(my_var) {
  v1 <- deparse(substitute(my_var))
  
  BMI_all_patients %>%
    tabyl({{my_var}}, obese)%>% 
    dplyr::mutate(total = (obese + no_bmi + not_obese)) %>%
    dplyr::mutate(prop_obese = obese/total) %>%
    dplyr::mutate(total_bmi = obese + not_obese) %>%
    dplyr::mutate(prop_not_obese = not_obese/total) %>%
    dplyr::mutate(prop_no_bmi = no_bmi/total) %>%
    dplyr::mutate(prop_obese_bmi = obese/total_bmi) %>% 
    dplyr::mutate(prop_not_obese_bmi = not_obese/total_bmi) %>%
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(group={{my_var}}) %>% 
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(se_prop = sqrt(((prop_obese*(1-prop_obese))/total)))  %>%
    dplyr::mutate(se_prop_bmi = sqrt(((prop_obese_bmi*(1-prop_obese_bmi))/total_bmi)))
} 


sex <- obese_diabetes_t2_function( sex)
age_group_2 <- obese_diabetes_t2_function( age_group_2)
eth_group_16 <- obese_diabetes_t2_function( eth_group_16)
imd <- obese_diabetes_t2_function( imd)
region <- obese_diabetes_t2_function( region)
hypertension <- obese_diabetes_t2_function( comorbid_hypertension)
diabetes_t2 <- obese_diabetes_t2_function( comorbid_diabetes_t2)
chronic_cardiac <- obese_diabetes_t2_function( comorbid_chronic_cardiac)
learning_disability <- obese_diabetes_t2_function( comorbid_learning_disability)
depression <- obese_diabetes_t2_function( comorbid_depression)
dementia <- obese_diabetes_t2_function( comorbid_dementia)
psychosis_schiz_bipolar <- obese_diabetes_t2_function( comorbid_psychosis_schiz_bipolar)
asthma <- obese_diabetes_t2_function( comorbid_asthma)
COPD <- obese_diabetes_t2_function( comorbid_COPD)
stroke_and_TIA <- obese_diabetes_t2_function( comorbid_stroke_and_TIA)
smoking_status <- obese_diabetes_t2_function( smoking_status)


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

print('check_5')




complete_percentages <- complete_percentages %>% 
  dplyr::mutate(obese = plyr::round_any(complete_percentages$obese, 5)) %>%
  dplyr::mutate(not_obese = plyr::round_any(complete_percentages$not_obese, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_percentages$no_bmi, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete_percentages$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_percentages$total_bmi, 5)) %>% 
  dplyr::mutate(exposure = "all")

complete_hypertension <- complete_hypertension %>% 
  dplyr::mutate(obese = plyr::round_any(complete_hypertension$obese, 5)) %>%
  dplyr::mutate(not_obese = plyr::round_any(complete_hypertension$not_obese, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_hypertension$no_bmi, 5)) %>% 
  dplyr::mutate(total = plyr::round_any(complete_hypertension$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_hypertension$total_bmi, 5)) %>% 
  dplyr::mutate(exposure = "hypertension")

complete_diabetes_t1 <- complete_diabetes_t1 %>% 
  dplyr::mutate(obese = plyr::round_any(complete_diabetes_t1$obese, 5)) %>%
  dplyr::mutate(not_obese = plyr::round_any(complete_diabetes_t1$not_obese, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_diabetes_t1$no_bmi, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete_diabetes_t1$total, 5))%>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_diabetes_t1$total_bmi, 5)) %>% 
  dplyr::mutate(exposure = "T1DM")


complete_diabetes_t2 <- complete_diabetes_t2 %>% 
  dplyr::mutate(obese = plyr::round_any(complete_diabetes_t2$obese, 5)) %>%
  dplyr::mutate(not_obese = plyr::round_any(complete_diabetes_t2$not_obese, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_diabetes_t2$no_bmi, 5))%>%
  dplyr::mutate(total = plyr::round_any(complete_diabetes_t2$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_diabetes_t2$total_bmi, 5)) %>% 
  dplyr::mutate(exposure = "T2DM")


obese_2016 <- complete_percentages %>% 
    bind_rows(complete_hypertension, complete_diabetes_t2, complete_diabetes_t1)



write.csv (obese_2016, here::here ("output/data","obese_2016.csv"))
