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

BMI_data <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))


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

##  Demographics for 2021
BMI_data_2021 <- BMI_data %>% 
  dplyr::filter(year==2021)

## Apply to each year
sex <- function_1(BMI_data_2021, sex)
age <- function_1(BMI_data_2021, age_group_2)
eth <- function_1(BMI_data_2021, eth_group_16)
imd <- function_1(BMI_data_2021, imd)
region <- function_1(BMI_data_2021, region)
smoking_status <- function_1(BMI_data_2021, smoking_status)
diabetes_t2 <- function_1(BMI_data_2021, comorbid_diabetes_t2)
diabetes_t1 <- function_1(BMI_data_2021, comorbid_diabetes_t1)
hypertension <- function_1(BMI_data_2021, comorbid_hypertension)
chronic_cardiac <- function_1(BMI_data_2021, comorbid_chronic_cardiac)


learning_disability <- function_1(BMI_data_2021, comorbid_learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_2021, comorbid_psychosis_schiz_bipolar)
depression <- function_1(BMI_data_2021, comorbid_depression)
asthma <- function_1(BMI_data_2021, comorbid_asthma)
COPD <- function_1(BMI_data_2021, comorbid_COPD)


dementia <- function_1(BMI_data_2021, comorbid_dementia)
stroke_and_TIA <- function_1(BMI_data_2021, comorbid_stroke_and_TIA)
all_cancer <- function_1(BMI_data_2021, comorbid_all_cancer)


all_2021 <- sex %>% 
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
  dplyr::mutate(year = "2021")

## Apply to each year 2020

##  Demographics for 2020
BMI_data_2020 <- BMI_data %>% 
  dplyr::filter(year==2020)

## Apply to each year
sex <- function_1(BMI_data_2020, sex)
age <- function_1(BMI_data_2020, age_group_2)
eth <- function_1(BMI_data_2020, eth_group_16)
imd <- function_1(BMI_data_2020, imd)
region <- function_1(BMI_data_2020, region)
smoking_status <- function_1(BMI_data_2020, smoking_status)
diabetes_t2 <- function_1(BMI_data_2020, comorbid_diabetes_t2)
diabetes_t1 <- function_1(BMI_data_2020, comorbid_diabetes_t1)
hypertension <- function_1(BMI_data_2020, comorbid_hypertension)
chronic_cardiac <- function_1(BMI_data_2020, comorbid_chronic_cardiac)


learning_disability <- function_1(BMI_data_2020, comorbid_learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_2020, comorbid_psychosis_schiz_bipolar)
depression <- function_1(BMI_data_2020, comorbid_depression)
asthma <- function_1(BMI_data_2020, comorbid_asthma)
COPD <- function_1(BMI_data_2020, comorbid_COPD)


dementia <- function_1(BMI_data_2020, comorbid_dementia)
stroke_and_TIA <- function_1(BMI_data_2020, comorbid_stroke_and_TIA)
all_cancer <- function_1(BMI_data_2020, comorbid_all_cancer)


all_2020 <- sex %>% 
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
  dplyr::mutate(year = "2020")



## Apply to each year 2019

##  Demographics for 2019
BMI_data_2019 <- BMI_data %>% 
  dplyr::filter(year==2019)

## Apply to each year
sex <- function_1(BMI_data_2019, sex)
age <- function_1(BMI_data_2019, age_group_2)
eth <- function_1(BMI_data_2019, eth_group_16)
imd <- function_1(BMI_data_2019, imd)
region <- function_1(BMI_data_2019, region)
smoking_status <- function_1(BMI_data_2019, smoking_status)
diabetes_t2 <- function_1(BMI_data_2019, comorbid_diabetes_t2)
diabetes_t1 <- function_1(BMI_data_2019, comorbid_diabetes_t1)
hypertension <- function_1(BMI_data_2019, comorbid_hypertension)
chronic_cardiac <- function_1(BMI_data_2019, comorbid_chronic_cardiac)


learning_disability <- function_1(BMI_data_2019, comorbid_learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_2019, comorbid_psychosis_schiz_bipolar)
depression <- function_1(BMI_data_2019, comorbid_depression)
asthma <- function_1(BMI_data_2019, comorbid_asthma)
COPD <- function_1(BMI_data_2019, comorbid_COPD)


dementia <- function_1(BMI_data_2019, comorbid_dementia)
stroke_and_TIA <- function_1(BMI_data_2019, comorbid_stroke_and_TIA)
all_cancer <- function_1(BMI_data_2019, comorbid_all_cancer)


all_2019 <- sex %>% 
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
  dplyr::mutate(year = "2019")


## Apply to each year 2018

##  Demographics for 2018
BMI_data_2018 <- BMI_data %>% 
  dplyr::filter(year==2018)

## Apply to each year
sex <- function_1(BMI_data_2018, sex)
age <- function_1(BMI_data_2018, age_group_2)
eth <- function_1(BMI_data_2018, eth_group_16)
imd <- function_1(BMI_data_2018, imd)
region <- function_1(BMI_data_2018, region)
smoking_status <- function_1(BMI_data_2018, smoking_status)
diabetes_t2 <- function_1(BMI_data_2018, comorbid_diabetes_t2)
diabetes_t1 <- function_1(BMI_data_2018, comorbid_diabetes_t1)
hypertension <- function_1(BMI_data_2018, comorbid_hypertension)
chronic_cardiac <- function_1(BMI_data_2018, comorbid_chronic_cardiac)


learning_disability <- function_1(BMI_data_2018, comorbid_learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_2018, comorbid_psychosis_schiz_bipolar)
depression <- function_1(BMI_data_2018, comorbid_depression)
asthma <- function_1(BMI_data_2018, comorbid_asthma)
COPD <- function_1(BMI_data_2018, comorbid_COPD)


dementia <- function_1(BMI_data_2018, comorbid_dementia)
stroke_and_TIA <- function_1(BMI_data_2018, comorbid_stroke_and_TIA)
all_cancer <- function_1(BMI_data_2018, comorbid_all_cancer)


all_2018 <- sex %>% 
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
  dplyr::mutate(year = "2018")


## Apply to each year 2017

##  Demographics for 2017
BMI_data_2017 <- BMI_data %>% 
  dplyr::filter(year==2017)

## Apply to each year
sex <- function_1(BMI_data_2017, sex)
age <- function_1(BMI_data_2017, age_group_2)
eth <- function_1(BMI_data_2017, eth_group_16)
imd <- function_1(BMI_data_2017, imd)
region <- function_1(BMI_data_2017, region)
smoking_status <- function_1(BMI_data_2017, smoking_status)
diabetes_t2 <- function_1(BMI_data_2017, comorbid_diabetes_t2)
diabetes_t1 <- function_1(BMI_data_2017, comorbid_diabetes_t1)
hypertension <- function_1(BMI_data_2017, comorbid_hypertension)
chronic_cardiac <- function_1(BMI_data_2017, comorbid_chronic_cardiac)


learning_disability <- function_1(BMI_data_2017, comorbid_learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_2017, comorbid_psychosis_schiz_bipolar)
depression <- function_1(BMI_data_2017, comorbid_depression)
asthma <- function_1(BMI_data_2017, comorbid_asthma)
COPD <- function_1(BMI_data_2017, comorbid_COPD)


dementia <- function_1(BMI_data_2017, comorbid_dementia)
stroke_and_TIA <- function_1(BMI_data_2017, comorbid_stroke_and_TIA)
all_cancer <- function_1(BMI_data_2017, comorbid_all_cancer)


all_2017 <- sex %>% 
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
  dplyr::mutate(year = "2017")



## Apply to each year 2016

##  Demographics for 2016
BMI_data_2016 <- BMI_data %>% 
  dplyr::filter(year==2016)

## Apply to each year
sex <- function_1(BMI_data_2016, sex)
age <- function_1(BMI_data_2016, age_group_2)
eth <- function_1(BMI_data_2016, eth_group_16)
imd <- function_1(BMI_data_2016, imd)
region <- function_1(BMI_data_2016, region)
smoking_status <- function_1(BMI_data_2016, smoking_status)
diabetes_t2 <- function_1(BMI_data_2016, comorbid_diabetes_t2)
diabetes_t1 <- function_1(BMI_data_2016, comorbid_diabetes_t1)
hypertension <- function_1(BMI_data_2016, comorbid_hypertension)
chronic_cardiac <- function_1(BMI_data_2016, comorbid_chronic_cardiac)


learning_disability <- function_1(BMI_data_2016, comorbid_learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_2016, comorbid_psychosis_schiz_bipolar)
depression <- function_1(BMI_data_2016, comorbid_depression)
asthma <- function_1(BMI_data_2016, comorbid_asthma)
COPD <- function_1(BMI_data_2016, comorbid_COPD)


dementia <- function_1(BMI_data_2016, comorbid_dementia)
stroke_and_TIA <- function_1(BMI_data_2016, comorbid_stroke_and_TIA)
all_cancer <- function_1(BMI_data_2016, comorbid_all_cancer)


all_2016 <- sex %>% 
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
  dplyr::mutate(year = "2016")



## Apply to each year 2015

##  Demographics for 2015
BMI_data_2015 <- BMI_data %>% 
  dplyr::filter(year==2015)

## Apply to each year
sex <- function_1(BMI_data_2015, sex)
age <- function_1(BMI_data_2015, age_group_2)
eth <- function_1(BMI_data_2015, eth_group_16)
imd <- function_1(BMI_data_2015, imd)
region <- function_1(BMI_data_2015, region)
smoking_status <- function_1(BMI_data_2015, smoking_status)
diabetes_t2 <- function_1(BMI_data_2015, comorbid_diabetes_t2)
diabetes_t1 <- function_1(BMI_data_2015, comorbid_diabetes_t1)
hypertension <- function_1(BMI_data_2015, comorbid_hypertension)
chronic_cardiac <- function_1(BMI_data_2015, comorbid_chronic_cardiac)


learning_disability <- function_1(BMI_data_2015, comorbid_learning_disability)
psychosis_schiz_bipolar <- function_1(BMI_data_2015, comorbid_psychosis_schiz_bipolar)
depression <- function_1(BMI_data_2015, comorbid_depression)
asthma <- function_1(BMI_data_2015, comorbid_asthma)
COPD <- function_1(BMI_data_2015, comorbid_COPD)


dementia <- function_1(BMI_data_2015, comorbid_dementia)
stroke_and_TIA <- function_1(BMI_data_2015, comorbid_stroke_and_TIA)
all_cancer <- function_1(BMI_data_2015, comorbid_all_cancer)


all_2015 <- sex %>% 
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
  dplyr::mutate(year = "2015")



demographics <- all_2021 %>% 
  bind_rows(all_2020) %>% 
  bind_rows(all_2019) %>% 
  bind_rows(all_2018) %>% 
  bind_rows(all_2017) %>%
  bind_rows(all_2016) %>%
  bind_rows(all_2015)  

demographics <- demographics %>% 
  dplyr::mutate(n = plyr::round_any(demographics$n, 5))


write.csv (demographics, here::here ("output/data","demographics_study_population.csv"))

