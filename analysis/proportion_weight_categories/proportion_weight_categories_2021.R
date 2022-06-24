

#### Author: M Samuel
#### Date: 15th June
####  This script looks proportion obese proportions each year

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

BMI_all <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))


BMI_all <- BMI_all %>% 
  dplyr::select(
    year,
    patient_id, 
    sex, 
    age_group_2, 
    eth_group_16,
    imd,
    region,
    smoking_status, 
    BMI_categories, 
    starts_with("comorbid"))

BMI_all <- BMI_all %>% 
  dplyr::mutate(overweight = as.character(BMI_categories))


BMI_all <- BMI_all %>%  
  dplyr::mutate(overweight = replace_na(overweight, "no_bmi"))



prop_function <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}, overweight) %>% 
    dplyr::mutate(total = healthy + no_bmi + obese + overweight + underweight) %>% 
    dplyr::mutate(total_bmi = total - no_bmi) %>% 
    dplyr::mutate(prop_underweight = underweight/total) %>% 
    dplyr::mutate(prop_underweight_bmi = underweight/total_bmi) %>% 
    dplyr::mutate(se_prop_uw = sqrt(((prop_underweight*(1-prop_underweight))/total)))  %>%
    dplyr::mutate(se_prop_uw_bmi = sqrt(((prop_underweight_bmi*(1-prop_underweight_bmi))/total_bmi))) %>%
    dplyr::mutate(prop_healthy = healthy/total) %>% 
    dplyr::mutate(prop_healthy_bmi = healthy/total_bmi) %>% 
    dplyr::mutate(se_prop_healthy = sqrt(((prop_healthy*(1-prop_healthy))/total)))  %>%
    dplyr::mutate(se_prop_healthy_bmi = sqrt(((prop_healthy_bmi*(1-prop_healthy_bmi))/total_bmi))) %>%
    dplyr::mutate(prop_overweight = overweight/total) %>% 
    dplyr::mutate(prop_overweight_bmi = overweight/total_bmi) %>% 
    dplyr::mutate(se_prop_ow = sqrt(((prop_overweight*(1-prop_overweight))/total)))  %>%
    dplyr::mutate(se_prop_ow_bmi = sqrt(((prop_overweight_bmi*(1-prop_overweight_bmi))/total_bmi))) %>%
    dplyr::mutate(prop_obese = obese/total) %>% 
    dplyr::mutate(prop_obese_bmi = obese/total_bmi) %>% 
    dplyr::mutate(se_prop_obese = sqrt(((prop_obese*(1-prop_obese))/total)))  %>%
    dplyr::mutate(se_prop_obese_bmi = sqrt(((prop_obese_bmi*(1-prop_obese_bmi))/total_bmi))) %>% 
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(group={{my_var}}) %>% 
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) 
  
}




## FILTER FOR YEARS

## 2021
BMI_all_patients <- BMI_all %>% 
  dplyr::filter(year == "2021")




  



## Calculate proportions
## Calculate proportions
sex <- prop_function(BMI_all_patients, sex)
age_group_2 <- prop_function(BMI_all_patients, age_group_2)
eth_group_16 <- prop_function(BMI_all_patients, eth_group_16)
imd <- prop_function(BMI_all_patients, imd)
region <- prop_function(BMI_all_patients, region)
hypertension <- prop_function(BMI_all_patients, comorbid_hypertension)
diabetes_t1 <- prop_function(BMI_all_patients, comorbid_diabetes_t1)
diabetes_t2 <- prop_function(BMI_all_patients, comorbid_diabetes_t2)
chronic_cardiac <- prop_function(BMI_all_patients, comorbid_chronic_cardiac)
learning_disability <- prop_function(BMI_all_patients, comorbid_learning_disability)
depression <- prop_function(BMI_all_patients, comorbid_depression)
dementia <- prop_function(BMI_all_patients, comorbid_dementia)
psychosis_schiz_bipolar <- prop_function(BMI_all_patients, comorbid_psychosis_schiz_bipolar)
asthma <- prop_function(BMI_all_patients, comorbid_asthma)
COPD <- prop_function(BMI_all_patients, comorbid_COPD)
stroke_and_TIA <- prop_function(BMI_all_patients, comorbid_stroke_and_TIA)
smoking_status <- prop_function(BMI_all_patients, smoking_status)




complete <- sex %>% 
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

complete <- complete %>% 
  dplyr::mutate(underweight = plyr::round_any(complete$underweight, 5)) %>%
  dplyr::mutate(healthy = plyr::round_any(complete$healthy, 5)) %>%
  dplyr::mutate(overweight = plyr::round_any(complete$overweight, 5)) %>%
  dplyr::mutate(obese = plyr::round_any(complete$obese, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete$no_bmi, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete$total_bmi, 5))  %>% 
  dplyr::mutate(year = "2021") %>% 
  dplyr::mutate(exposure = "all")


## FILTER data sets

BMI_hypertension <- BMI_all_patients %>% 
  dplyr::filter(comorbid_hypertension == TRUE)


## Calculate proportions
## Calculate proportions
sex <- prop_function(BMI_hypertension, sex)
age_group_2 <- prop_function(BMI_hypertension, age_group_2)
eth_group_16 <- prop_function(BMI_hypertension, eth_group_16)
imd <- prop_function(BMI_hypertension, imd)
region <- prop_function(BMI_hypertension, region)
hypertension <- prop_function(BMI_hypertension, comorbid_hypertension)
diabetes_t1 <- prop_function(BMI_hypertension, comorbid_diabetes_t1)
diabetes_t2 <- prop_function(BMI_hypertension, comorbid_diabetes_t2)
chronic_cardiac <- prop_function(BMI_hypertension, comorbid_chronic_cardiac)
learning_disability <- prop_function(BMI_hypertension, comorbid_learning_disability)
depression <- prop_function(BMI_hypertension, comorbid_depression)
dementia <- prop_function(BMI_hypertension, comorbid_dementia)
psychosis_schiz_bipolar <- prop_function(BMI_hypertension, comorbid_psychosis_schiz_bipolar)
asthma <- prop_function(BMI_hypertension, comorbid_asthma)
COPD <- prop_function(BMI_hypertension, comorbid_COPD)
stroke_and_TIA <- prop_function(BMI_hypertension, comorbid_stroke_and_TIA)
smoking_status <- prop_function(BMI_hypertension, smoking_status)


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

complete_hypertension <- complete_hypertension %>% 
  dplyr::mutate(underweight = plyr::round_any(complete_hypertension$underweight, 5)) %>%
  dplyr::mutate(healthy = plyr::round_any(complete_hypertension$healthy, 5)) %>%
  dplyr::mutate(overweight = plyr::round_any(complete_hypertension$overweight, 5)) %>%
  dplyr::mutate(obese = plyr::round_any(complete_hypertension$obese, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_hypertension$no_bmi, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete_hypertension$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_hypertension$total_bmi, 5))  %>% 
  dplyr::mutate(year = "2021") %>% 
  dplyr::mutate(exposure = "hypertension")

## FILTER data sets

BMI_diabetes_t1 <- BMI_all_patients %>% 
  dplyr::filter(comorbid_diabetes_t1 == TRUE)


## Calculate proportions
## Calculate proportions
sex <- prop_function(BMI_diabetes_t1, sex)
age_group_2 <- prop_function(BMI_diabetes_t1, age_group_2)
eth_group_16 <- prop_function(BMI_diabetes_t1, eth_group_16)
imd <- prop_function(BMI_diabetes_t1, imd)
region <- prop_function(BMI_diabetes_t1, region)
diabetes_t1 <- prop_function(BMI_diabetes_t1, comorbid_diabetes_t1)
diabetes_t1 <- prop_function(BMI_diabetes_t1, comorbid_diabetes_t1)
diabetes_t2 <- prop_function(BMI_diabetes_t1, comorbid_diabetes_t2)
chronic_cardiac <- prop_function(BMI_diabetes_t1, comorbid_chronic_cardiac)
learning_disability <- prop_function(BMI_diabetes_t1, comorbid_learning_disability)
depression <- prop_function(BMI_diabetes_t1, comorbid_depression)
dementia <- prop_function(BMI_diabetes_t1, comorbid_dementia)
psychosis_schiz_bipolar <- prop_function(BMI_diabetes_t1, comorbid_psychosis_schiz_bipolar)
asthma <- prop_function(BMI_diabetes_t1, comorbid_asthma)
COPD <- prop_function(BMI_diabetes_t1, comorbid_COPD)
stroke_and_TIA <- prop_function(BMI_diabetes_t1, comorbid_stroke_and_TIA)
smoking_status <- prop_function(BMI_diabetes_t1, smoking_status)


complete_diabetes_t1 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(diabetes_t1) %>%
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

complete_diabetes_t1 <- complete_diabetes_t1 %>% 
  dplyr::mutate(underweight = plyr::round_any(complete_diabetes_t1$underweight, 5)) %>%
  dplyr::mutate(healthy = plyr::round_any(complete_diabetes_t1$healthy, 5)) %>%
  dplyr::mutate(overweight = plyr::round_any(complete_diabetes_t1$overweight, 5)) %>%
  dplyr::mutate(obese = plyr::round_any(complete_diabetes_t1$obese, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_diabetes_t1$no_bmi, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete_diabetes_t1$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_diabetes_t1$total_bmi, 5))  %>% 
  dplyr::mutate(year = "2021") %>% 
  dplyr::mutate(exposure = "diabetes_t1")

## FILTER data sets

BMI_diabetes_t2 <- BMI_all_patients %>% 
  dplyr::filter(comorbid_diabetes_t2 == TRUE)


## Calculate proportions
## Calculate proportions
sex <- prop_function(BMI_diabetes_t2, sex)
age_group_2 <- prop_function(BMI_diabetes_t2, age_group_2)
eth_group_16 <- prop_function(BMI_diabetes_t2, eth_group_16)
imd <- prop_function(BMI_diabetes_t2, imd)
region <- prop_function(BMI_diabetes_t2, region)
diabetes_t2 <- prop_function(BMI_diabetes_t2, comorbid_diabetes_t2)
diabetes_t1 <- prop_function(BMI_diabetes_t2, comorbid_diabetes_t1)
diabetes_t2 <- prop_function(BMI_diabetes_t2, comorbid_diabetes_t2)
chronic_cardiac <- prop_function(BMI_diabetes_t2, comorbid_chronic_cardiac)
learning_disability <- prop_function(BMI_diabetes_t2, comorbid_learning_disability)
depression <- prop_function(BMI_diabetes_t2, comorbid_depression)
dementia <- prop_function(BMI_diabetes_t2, comorbid_dementia)
psychosis_schiz_bipolar <- prop_function(BMI_diabetes_t2, comorbid_psychosis_schiz_bipolar)
asthma <- prop_function(BMI_diabetes_t2, comorbid_asthma)
COPD <- prop_function(BMI_diabetes_t2, comorbid_COPD)
stroke_and_TIA <- prop_function(BMI_diabetes_t2, comorbid_stroke_and_TIA)
smoking_status <- prop_function(BMI_diabetes_t2, smoking_status)


complete_diabetes_t2 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(diabetes_t2) %>%
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

complete_diabetes_t2 <- complete_diabetes_t2 %>% 
  dplyr::mutate(underweight = plyr::round_any(complete_diabetes_t2$underweight, 5)) %>%
  dplyr::mutate(healthy = plyr::round_any(complete_diabetes_t2$healthy, 5)) %>%
  dplyr::mutate(overweight = plyr::round_any(complete_diabetes_t2$overweight, 5)) %>%
  dplyr::mutate(obese = plyr::round_any(complete_diabetes_t2$obese, 5)) %>%
  dplyr::mutate(no_bmi = plyr::round_any(complete_diabetes_t2$no_bmi, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete_diabetes_t2$total, 5)) %>%
  dplyr::mutate(total_bmi = plyr::round_any(complete_diabetes_t2$total_bmi, 5))  %>% 
  dplyr::mutate(year = "2021") %>% 
  dplyr::mutate(exposure = "diabetes_t2")


data_2021 <- complete %>% 
  bind_rows(complete_hypertension, complete_diabetes_t2, complete_diabetes_t1)


write.csv (data_2021, here::here ("output/data", "weight_categories_2021.csv"))
