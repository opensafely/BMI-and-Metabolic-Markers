## M Samuel 
## 5th May 2022
## This script ensures that the demographic data used is the most recent recorded for the patient (e.g. age)


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

# webshot::install_phantomjs()



## Read in files
BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))

colnames(BMI_trajectories)

   



BMI_trajectories <- BMI_trajectories %>% 
  dplyr::select("sex",
                "age_group_2", 
                "region",                
                "imd",
                "hypertension",
                "diabetes_t1", 
                "diabetes_t2",
                "learning_disability", 
                "depression",               
                "psychosis_schiz_bipolar", 
                "dementia", 
                "asthma",
                "COPD",
                "stroke_and_TIA",          
                "chronic_cardiac",                 
                "all_cancer",                           
                "smoking_status", 
                "ethnic_no_miss",         
                "eth_group_16",           
                "complete_bmi_data", 
                "bmi_change_cat", 
                "precovid_bmi_category", 
                "pandemic_stage")

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(rapid_bmi_change = case_when(
    bmi_change_cat == 'over 0.5' ~ 1, 
    bmi_change_cat != 'over 0.5' ~ 0, 
  ))


explanatory_vars <- c("sex",
                       "age_group_2", 
                       "region",                
                       "imd",
                       "hypertension",
                       "diabetes_t1", 
                       "diabetes_t2",
                       "learning_disability", 
                       "depression",               
                       "psychosis_schiz_bipolar", 
                       "dementia", 
                       "asthma",
                       "COPD",
                       "stroke_and_TIA",          
                       "chronic_cardiac",                 
                       "all_cancer",                           
                       "smoking_status", 
                       "ethnic_no_miss",         
                       "eth_group_16",           
                       "precovid_bmi_category")


### Precovid analysis
precovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "precovid")

models_precov_rapidinc_bmi_univar <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = BMI_trajectories)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))



### Postcovid Analysis

postcovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "postcovid")

models_postcov_rapidinc_bmi_univar <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = BMI_trajectories)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))


### Write outputs

write_csv (models_precov_rapidinc_bmi_univar, here::here ("output/data","rapid_bmi_change_univariate_precovid.csv"))
write_csv (models_postcov_rapidinc_bmi_univar, here::here ("output/data","rapid_bmi_change_univariate_postcovid.csv"))

