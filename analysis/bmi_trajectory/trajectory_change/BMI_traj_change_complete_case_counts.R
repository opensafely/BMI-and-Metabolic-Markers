## Author: M Samuel
## This script calculate the proportion of patients who had precovid BMI trajectory data who also had post covid data
## This will identify those with missing post covid data

## Load libraries
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
library(gtsummary)

BMI_data <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))


## Filter for data that had BMI_trajectory data in the precovid period.  This is my population
trajectory_population <- BMI_data %>% 
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
 


 
## Calculate odds of having complete data by each exposure
 numbers_function <- function(data, var){
   v1 <- deparse(substitute(var))
   
   data  %>% 
     tabyl({{var}}, complete_bmi_data) %>% 
     dplyr::rename(group = {{var}}) %>%
     dplyr::rename(complete_n = 'complete') %>%
     ungroup() %>%
     dplyr::mutate(group = as.character(group)) %>%
     dplyr::mutate(variable = (v1), .before=1) %>%
     dplyr::mutate(N_total = complete_n + incomplete) 
 }
 
 numbers_function(trajectory_population, sex)
 
 
 
 trajectory_population %>% 
   tabyl(sex, complete_bmi_data) %>% 
   adorn_percentages(denominator = "row")
 
 percentages_function <- function(data, var){
   v1 <- deparse(substitute(var))
   
   data  %>% 
     tabyl({{var}}, complete_bmi_data) %>% 
     adorn_percentages(denominator = "row") %>%
     dplyr::rename(complete_prop = 'complete') %>%
     dplyr::rename(group = {{var}}) %>%
     ungroup() %>%
     dplyr::mutate(group = as.character(group)) %>%
     dplyr::mutate(variable = (v1), .before=1)  
 }
 
percentages_function(trajectory_population, sex)


## Calculate percentages for each
sex <- percentages_function(trajectory_population, sex)
age_group_2 <- percentages_function(trajectory_population, age_group_2)
eth_group_16 <- percentages_function(trajectory_population, eth_group_16)
imd <- percentages_function(trajectory_population, imd)
precovid_bmi_category <- percentages_function(trajectory_population, precovid_bmi_category)
region <- percentages_function(trajectory_population, region)
hypertension <- percentages_function(trajectory_population, hypertension)
diabetes_t1 <- percentages_function(trajectory_population, diabetes_t1)
diabetes_t2 <- percentages_function(trajectory_population, diabetes_t2)
chronic_cardiac <- percentages_function(trajectory_population, chronic_cardiac)
learning_disability <- percentages_function(trajectory_population, learning_disability)
depression <- percentages_function(trajectory_population, depression)
dementia <- percentages_function(trajectory_population, dementia)
psychosis_schiz_bipolar <- percentages_function(trajectory_population, psychosis_schiz_bipolar)
asthma <- percentages_function(trajectory_population, asthma)
COPD <- percentages_function(trajectory_population, COPD)
stroke_and_TIA <- percentages_function(trajectory_population, stroke_and_TIA)
smoking_status <- percentages_function(trajectory_population, smoking_status)

complete_percentages <- sex %>% 
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



## Calculate numbers 
sex <- numbers_function(trajectory_population, sex)
age_group_2 <- numbers_function(trajectory_population, age_group_2)
eth_group_16 <- numbers_function(trajectory_population, eth_group_16)
imd <- numbers_function(trajectory_population, imd)
precovid_bmi_category <- numbers_function(trajectory_population, precovid_bmi_category)
region <- numbers_function(trajectory_population, region)
hypertension <- numbers_function(trajectory_population, hypertension)
diabetes_t1 <- numbers_function(trajectory_population, diabetes_t1)
diabetes_t2 <- numbers_function(trajectory_population, diabetes_t2)
chronic_cardiac <- numbers_function(trajectory_population, chronic_cardiac)
learning_disability <- numbers_function(trajectory_population, learning_disability)
depression <- numbers_function(trajectory_population, depression)
dementia <- numbers_function(trajectory_population, dementia)
psychosis_schiz_bipolar <- numbers_function(trajectory_population, psychosis_schiz_bipolar)
asthma <- numbers_function(trajectory_population, asthma)
COPD <- numbers_function(trajectory_population, COPD)
stroke_and_TIA <- numbers_function(trajectory_population, stroke_and_TIA)
smoking_status <- numbers_function(trajectory_population, smoking_status)

complete_numbers <- sex %>% 
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


all <- trajectory_population %>% 
  tabyl(complete_bmi_data) %>% 
  dplyr::mutate(N_total = n/percent) %>% 
  dplyr::rename(complete_n = n) %>% 
  dplyr::rename(complete_prop = percent) %>% 
  dplyr::filter(complete_bmi_data == "complete") %>% 
  dplyr::mutate(group = "all") %>% 
  dplyr::mutate(variable = "all") %>% 
  dplyr::select("variable","group", "complete_n", "complete_prop", "N_total") 


complete <- complete_numbers %>% 
  bind_cols(complete_percentages) %>% 
  dplyr::rename(variable = "variable...1") %>%
  dplyr::rename(group = "group...2") %>%
  dplyr::select("variable","group", "complete_n", "complete_prop", "N_total") 

complete <- all %>%
  dplyr::bind_rows(complete)

complete <- complete %>%
  dplyr::mutate(N_total = plyr::round_any(complete$N_total, 5))  %>% 
  dplyr::mutate(complete_n = plyr::round_any(complete$complete_n, 5))  



write.csv (complete, here::here ("output/data","complete_traj_data_proportions.csv"))