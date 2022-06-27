

#### Author: M Samuel
#### Date: 15th June
####  This script looks at whether exposurs were associated with having compelte BMI data

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


trajectory_population  <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))



# BMI_data <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))


# # Filter for data that had BMI_trajectory data in the precovid period.  This is my population
trajectory_population <- trajectory_population %>% 
  dplyr::filter(period1_missing == FALSE)

## Filter out patients who were underweight precovid and those with cancer

trajectory_population <- trajectory_population %>%
  dplyr::filter(all_cancer == "FALSE") %>% 
  dplyr::filter(precovid_bmi_category != "underweight")


## Create a flag for COVID
trajectory_population <- trajectory_population %>% 
  dplyr::mutate(complete = case_when(
    complete_bmi_data == "complete" ~ 1, 
    complete_bmi_data == "incomplete" ~ 0)
  )


 

## drop empty levels in imd
trajectory_population$imd <- droplevels(trajectory_population$imd )





chi_function <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data  %>% 
    tabyl({{var}}, complete_bmi_data) %>% 
    select(-1) %>% 
    chisq_test()  %>% 
    dplyr::mutate(variable = (v1), .before=1)  
}


sex <- chi_function(trajectory_population, sex)
age_group_2 <- chi_function(trajectory_population, age_group_2)
eth_group_16 <- chi_function(trajectory_population, eth_group_16)
imd <- chi_function(trajectory_population, imd)
precovid_bmi_category <- chi_function(trajectory_population, precovid_bmi_category)
region <- chi_function(trajectory_population, region)
hypertension <- chi_function(trajectory_population, hypertension)
diabetes_t1 <- chi_function(trajectory_population, diabetes_t1)
diabetes_t2 <- chi_function(trajectory_population, diabetes_t2)
chronic_cardiac <- chi_function(trajectory_population, chronic_cardiac)
learning_disability <- chi_function(trajectory_population, learning_disability)
depression <- chi_function(trajectory_population, depression)
dementia <- chi_function(trajectory_population, dementia)
psychosis_schiz_bipolar <- chi_function(trajectory_population, psychosis_schiz_bipolar)
asthma <- chi_function(trajectory_population, asthma)
COPD <- chi_function(trajectory_population, COPD)
stroke_and_TIA <- chi_function(trajectory_population, stroke_and_TIA)
smoking_status <- chi_function(trajectory_population, smoking_status)


complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  dplyr::mutate(n = plyr::round_any(complete$n, 5))

write.csv (complete, here::here ("output/data","complete_traj_data_prop_chi.csv"))