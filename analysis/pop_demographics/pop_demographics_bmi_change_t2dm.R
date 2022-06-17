#### Author: M Samuel
#### Date: 15th June
####  This script looks at the demographics of the total population in each group


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

BMI_data <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))

function_1 <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data  %>% 
    tabyl({{var}}) %>% 
    adorn_totals() %>%
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(variable = (v1), .before=1) 
  
}

BMI_data <- BMI_data %>% 
  dplyr::filter(diabetes_t1 == "TRUE")

##  Demographics for precovid
BMI_data_precovid <- BMI_data %>% 
  dplyr::filter(pandemic_stage== "precovid")

## Apply to each year
sex <- function_1(BMI_data_precovid, sex)
age <- function_1(BMI_data_precovid, age_group_2)
eth <- function_1(BMI_data_precovid, eth_group_16)
imd <- function_1(BMI_data_precovid, imd)
region <- function_1(BMI_data_precovid, region)
smoking_status <- function_1(BMI_data_precovid, smoking_status)
diabetes_t2 <- function_1(BMI_data_precovid, diabetes_t2)
diabetes_t1 <- function_1(BMI_data_precovid, diabetes_t1)
hypertension <- function_1(BMI_data_precovid, hypertension)
chronic_cardiac <- function_1(BMI_data_precovid, chronic_cardiac)


learning_disability <- function_1(BMI_data_precovid, learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_precovid, psychosis_schiz_bipolar)
depression <- function_1(BMI_data_precovid, depression)
asthma <- function_1(BMI_data_precovid, asthma)
COPD <- function_1(BMI_data_precovid, COPD)


dementia <- function_1(BMI_data_precovid, dementia)
stroke_and_TIA <- function_1(BMI_data_precovid, stroke_and_TIA)
all_cancer <- function_1(BMI_data_precovid, all_cancer)

all_precovid <- sex %>% 
  bind_rows(age) %>% 
  bind_rows(imd) %>%
  bind_rows(eth) %>% 
  bind_rows(region) %>%
  bind_rows(smoking_status) %>% 
  bind_rows(diabetes_t2)%>% 
  bind_rows(diabetes_t1) %>% 
  bind_rows(hypertension) %>% 
  bind_rows(chronic_cardiac) %>% 
  bind_rows(learning_disability) %>%
  bind_rows(psychosis_schiz_bipolar) %>% 
  bind_rows(depression) %>% 
  bind_rows(asthma) %>% 
  bind_rows(COPD) %>% 
  bind_rows(dementia) %>% 
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(all_cancer) %>%
  dplyr::mutate(year = "precovid")


#### POSTCOVID

##  Demographics for postcovid
BMI_data_postcovid <- BMI_data %>% 
  dplyr::filter(pandemic_stage== "postcovid")

## Apply to each year
sex <- function_1(BMI_data_postcovid, sex)
age <- function_1(BMI_data_postcovid, age_group_2)
eth <- function_1(BMI_data_postcovid, eth_group_16)
imd <- function_1(BMI_data_postcovid, imd)
region <- function_1(BMI_data_postcovid, region)
smoking_status <- function_1(BMI_data_postcovid, smoking_status)
diabetes_t2 <- function_1(BMI_data_postcovid, diabetes_t2)
diabetes_t1 <- function_1(BMI_data_postcovid, diabetes_t1)
hypertension <- function_1(BMI_data_postcovid, hypertension)
chronic_cardiac <- function_1(BMI_data_postcovid, chronic_cardiac)


learning_disability <- function_1(BMI_data_postcovid, learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_postcovid, psychosis_schiz_bipolar)
depression <- function_1(BMI_data_postcovid, depression)
asthma <- function_1(BMI_data_postcovid, asthma)
COPD <- function_1(BMI_data_postcovid, COPD)


dementia <- function_1(BMI_data_postcovid, dementia)
stroke_and_TIA <- function_1(BMI_data_postcovid, stroke_and_TIA)
all_cancer <- function_1(BMI_data_postcovid, all_cancer)

all_postcovid <- sex %>% 
  bind_rows(age) %>% 
  bind_rows(imd) %>%
  bind_rows(eth) %>% 
  bind_rows(region) %>%
  bind_rows(smoking_status) %>% 
  bind_rows(diabetes_t2)%>% 
  bind_rows(diabetes_t1) %>% 
  bind_rows(hypertension) %>% 
  bind_rows(chronic_cardiac) %>% 
  bind_rows(learning_disability) %>%
  bind_rows(psychosis_schiz_bipolar) %>% 
  bind_rows(depression) %>% 
  bind_rows(asthma) %>% 
  bind_rows(COPD) %>% 
  bind_rows(dementia) %>% 
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(all_cancer) %>%
  dplyr::mutate(year = "postcovid")

##  Demographics for complete
BMI_data_complete <- BMI_data %>% 
  dplyr::filter(complete_bmi_data == "complete") %>%
  dplyr::group_by(patient_id) %>% 
  dplyr::slice_head()


## Apply to each year
sex <- function_1(BMI_data_complete, sex)
age <- function_1(BMI_data_complete, age_group_2)
eth <- function_1(BMI_data_complete, eth_group_16)
imd <- function_1(BMI_data_complete, imd)
region <- function_1(BMI_data_complete, region)
smoking_status <- function_1(BMI_data_complete, smoking_status)
diabetes_t2 <- function_1(BMI_data_complete, diabetes_t2)
diabetes_t1 <- function_1(BMI_data_complete, diabetes_t1)
hypertension <- function_1(BMI_data_complete, hypertension)
chronic_cardiac <- function_1(BMI_data_complete, chronic_cardiac)


learning_disability <- function_1(BMI_data_complete, learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_complete, psychosis_schiz_bipolar)
depression <- function_1(BMI_data_complete, depression)
asthma <- function_1(BMI_data_complete, asthma)
COPD <- function_1(BMI_data_complete, COPD)


dementia <- function_1(BMI_data_complete, dementia)
stroke_and_TIA <- function_1(BMI_data_complete, stroke_and_TIA)
all_cancer <- function_1(BMI_data_complete, all_cancer)

all_complete <- sex %>% 
  bind_rows(age) %>% 
  bind_rows(imd) %>%
  bind_rows(eth) %>% 
  bind_rows(region) %>%
  bind_rows(smoking_status) %>% 
  bind_rows(diabetes_t2)%>% 
  bind_rows(diabetes_t1) %>% 
  bind_rows(hypertension) %>% 
  bind_rows(chronic_cardiac) %>% 
  bind_rows(learning_disability) %>%
  bind_rows(psychosis_schiz_bipolar) %>% 
  bind_rows(depression) %>% 
  bind_rows(asthma) %>% 
  bind_rows(COPD) %>% 
  bind_rows(dementia) %>% 
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(all_cancer) %>%
  dplyr::mutate(year = "complete")


demographics_all <- all_precovid %>% 
  bind_rows(all_postcovid) %>% 
  bind_rows(all_complete)

demographics_all <- demographics_all %>% 
  dplyr::mutate(n = plyr::round_any(demographics_all$n, 5))

####################  STAGE_2

## FILTER OUT LOW WEIGHT AND CANCER TO SEE POPULATION IN FINAL ANALYSIS
BMI_data <- BMI_data %>% 
  dplyr::filter(precovid_bmi_category != "underweight") %>% 
  dplyr::filter(all_cancer != "TRUE") 



function_1 <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data  %>% 
    tabyl({{var}}) %>% 
    adorn_totals() %>%
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(variable = (v1), .before=1) 
  
}

##  Demographics for precovid
BMI_data_precovid <- BMI_data %>% 
  dplyr::filter(pandemic_stage== "precovid")

## Apply to each year
sex <- function_1(BMI_data_precovid, sex)
age <- function_1(BMI_data_precovid, age_group_2)
eth <- function_1(BMI_data_precovid, eth_group_16)
imd <- function_1(BMI_data_precovid, imd)
region <- function_1(BMI_data_precovid, region)
smoking_status <- function_1(BMI_data_precovid, smoking_status)
diabetes_t2 <- function_1(BMI_data_precovid, diabetes_t2)
diabetes_t1 <- function_1(BMI_data_precovid, diabetes_t1)
hypertension <- function_1(BMI_data_precovid, hypertension)
chronic_cardiac <- function_1(BMI_data_precovid, chronic_cardiac)


learning_disability <- function_1(BMI_data_precovid, learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_precovid, psychosis_schiz_bipolar)
depression <- function_1(BMI_data_precovid, depression)
asthma <- function_1(BMI_data_precovid, asthma)
COPD <- function_1(BMI_data_precovid, COPD)


dementia <- function_1(BMI_data_precovid, dementia)
stroke_and_TIA <- function_1(BMI_data_precovid, stroke_and_TIA)
all_cancer <- function_1(BMI_data_precovid, all_cancer)

all_precovid <- sex %>% 
  bind_rows(age) %>% 
  bind_rows(imd) %>%
  bind_rows(eth) %>% 
  bind_rows(region) %>%
  bind_rows(smoking_status) %>% 
  bind_rows(diabetes_t2)%>% 
  bind_rows(diabetes_t1) %>% 
  bind_rows(hypertension) %>% 
  bind_rows(chronic_cardiac) %>% 
  bind_rows(learning_disability) %>%
  bind_rows(psychosis_schiz_bipolar) %>% 
  bind_rows(depression) %>% 
  bind_rows(asthma) %>% 
  bind_rows(COPD) %>% 
  bind_rows(dementia) %>% 
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(all_cancer) %>%
  dplyr::mutate(year = "precovid")


#### POSTCOVID

##  Demographics for postcovid
BMI_data_postcovid <- BMI_data %>% 
  dplyr::filter(pandemic_stage== "postcovid")

## Apply to each year
sex <- function_1(BMI_data_postcovid, sex)
age <- function_1(BMI_data_postcovid, age_group_2)
eth <- function_1(BMI_data_postcovid, eth_group_16)
imd <- function_1(BMI_data_postcovid, imd)
region <- function_1(BMI_data_postcovid, region)
smoking_status <- function_1(BMI_data_postcovid, smoking_status)
diabetes_t2 <- function_1(BMI_data_postcovid, diabetes_t2)
diabetes_t1 <- function_1(BMI_data_postcovid, diabetes_t1)
hypertension <- function_1(BMI_data_postcovid, hypertension)
chronic_cardiac <- function_1(BMI_data_postcovid, chronic_cardiac)


learning_disability <- function_1(BMI_data_postcovid, learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_postcovid, psychosis_schiz_bipolar)
depression <- function_1(BMI_data_postcovid, depression)
asthma <- function_1(BMI_data_postcovid, asthma)
COPD <- function_1(BMI_data_postcovid, COPD)


dementia <- function_1(BMI_data_postcovid, dementia)
stroke_and_TIA <- function_1(BMI_data_postcovid, stroke_and_TIA)
all_cancer <- function_1(BMI_data_postcovid, all_cancer)

all_postcovid <- sex %>% 
  bind_rows(age) %>% 
  bind_rows(imd) %>%
  bind_rows(eth) %>% 
  bind_rows(region) %>%
  bind_rows(smoking_status) %>% 
  bind_rows(diabetes_t2)%>% 
  bind_rows(diabetes_t1) %>% 
  bind_rows(hypertension) %>% 
  bind_rows(chronic_cardiac) %>% 
  bind_rows(learning_disability) %>%
  bind_rows(psychosis_schiz_bipolar) %>% 
  bind_rows(depression) %>% 
  bind_rows(asthma) %>% 
  bind_rows(COPD) %>% 
  bind_rows(dementia) %>% 
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(all_cancer) %>%
  dplyr::mutate(year = "postcovid")

##  Demographics for complete
BMI_data_complete <- BMI_data %>% 
  dplyr::filter(complete_bmi_data == "complete") %>%
  dplyr::group_by(patient_id) %>% 
  dplyr::slice_head()


## Apply to each year
sex <- function_1(BMI_data_complete, sex)
age <- function_1(BMI_data_complete, age_group_2)
eth <- function_1(BMI_data_complete, eth_group_16)
imd <- function_1(BMI_data_complete, imd)
region <- function_1(BMI_data_complete, region)
smoking_status <- function_1(BMI_data_complete, smoking_status)
diabetes_t2 <- function_1(BMI_data_complete, diabetes_t2)
diabetes_t1 <- function_1(BMI_data_complete, diabetes_t1)
hypertension <- function_1(BMI_data_complete, hypertension)
chronic_cardiac <- function_1(BMI_data_complete, chronic_cardiac)


learning_disability <- function_1(BMI_data_complete, learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_complete, psychosis_schiz_bipolar)
depression <- function_1(BMI_data_complete, depression)
asthma <- function_1(BMI_data_complete, asthma)
COPD <- function_1(BMI_data_complete, COPD)


dementia <- function_1(BMI_data_complete, dementia)
stroke_and_TIA <- function_1(BMI_data_complete, stroke_and_TIA)
all_cancer <- function_1(BMI_data_complete, all_cancer)

all_complete <- sex %>% 
  bind_rows(age) %>% 
  bind_rows(imd) %>%
  bind_rows(eth) %>% 
  bind_rows(region) %>%
  bind_rows(smoking_status) %>% 
  bind_rows(diabetes_t2)%>% 
  bind_rows(diabetes_t1) %>% 
  bind_rows(hypertension) %>% 
  bind_rows(chronic_cardiac) %>% 
  bind_rows(learning_disability) %>%
  bind_rows(psychosis_schiz_bipolar) %>% 
  bind_rows(depression) %>% 
  bind_rows(asthma) %>% 
  bind_rows(COPD) %>% 
  bind_rows(dementia) %>% 
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(all_cancer) %>%
  dplyr::mutate(year = "complete")


demographics <- all_precovid %>% 
  bind_rows(all_postcovid) %>% 
  bind_rows(all_complete)

demographics <- demographics %>% 
  dplyr::mutate(n = plyr::round_any(demographics$n, 5))


write.csv (demographics, here::here ("output/data","demographics_bmi_change_analysis_lowbmiexc_t2dm.csv"))



write.csv (demographics_all, here::here ("output/data","demographics_bmi_change_analysis_all_t2dm.csv"))

