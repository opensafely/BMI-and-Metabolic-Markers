## This script will develop linear regression models of predictors of change in BMI trajectory
## Low BMI and Cancer Excluded
## Author: Miriam Samuel 
## Date: 5th May


## Load libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)
library(lubridate)
library(skimr)
library(ggplot2)



BMI_DT <- read_feather (here::here ("output/data", "BMI_trajectory_models_data.feather"))

##*** Change to code.  FILTER OUT UNDERWEIGHT AND THOSE WITH CANCER
BMI_DT <- BMI_DT %>% 
  dplyr::filter(all_cancer == FALSE)

BMI_DT <- BMI_DT %>% 
  dplyr::filter(precovid_bmi_category != "underweight")

BMI_DT %>% 
  tabyl(all_cancer)

BMI_DT %>% 
  tabyl(precovid_bmi_category)


## LINEAR REGRESSION MODELLING
### Predictors of trajectory change


# develop a vector of explanatory variables
explanatory_vars <- c("sex", 
                      "age_group_2", 
                      "precovid_bmi_category", 
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
                      "smoking_status")



models_trajectory_change <- explanatory_vars %>%       # begin with variables of interest
  str_c("trajectory_change ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 4))




models_trajectory_change

## Limit to patients with hypertension
BMI_DT_hypertension <-  BMI_DT %>% 
dplyr::filter(hypertension == TRUE)




## LINEAR REGRESSION MODELLING
### Predictors of trajectory change


# develop a vector of explanatory variables
explanatory_vars_hypertension <- c("sex", 
                      "age_group_2", 
                      "precovid_bmi_category", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "imd", 
                      "region", 
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
                      "smoking_status")



models_trajectory_change_hypertension <- explanatory_vars_hypertension %>%       # begin with variables of interest
  str_c("trajectory_change ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_hypertension)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 4))




models_trajectory_change_hypertension


## Limit to patients with T2 diabetes
BMI_DT_T2DM <-  BMI_DT %>% 
dplyr::filter(diabetes_t2 == TRUE)




## LINEAR REGRESSION MODELLING
### Predictors of trajectory change


# develop a vector of explanatory variables
explanatory_vars_diabetes <- c("sex", 
                      "age_group_2", 
                      "precovid_bmi_category", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "imd", 
                      "region", 
                      "hypertension",           
                      "chronic_cardiac", 
                      "learning_disability",     
                      "depression",            
                      "dementia",               
                      "psychosis_schiz_bipolar",
                      "asthma",                
                      "COPD",                   
                      "stroke_and_TIA",       
                      "smoking_status")



models_trajectory_change_T2DM <- explanatory_vars_diabetes %>%       # begin with variables of interest
  str_c("trajectory_change ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_T2DM)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 4))




models_trajectory_change_T2DM

BMI_DT_T1DM <-  BMI_DT %>% 
dplyr::filter(diabetes_t1 == TRUE)




## LINEAR REGRESSION MODELLING
### Predictors of trajectory change


models_trajectory_change_T1DM <- explanatory_vars_diabetes %>%       # begin with variables of interest
  str_c("trajectory_change ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_T1DM)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 4))




models_trajectory_change_T1DM








write.csv (models_trajectory_change, here::here ("output/data","univariate_lowbmi_excluded_bmi_trajectory_change.csv"))
write.csv (models_trajectory_change_hypertension, here::here ("output/data","univariate_lowbmi_excluded_bmi_trajectory_change_hypertension.csv"))
write.csv (models_trajectory_change_T2DM, here::here ("output/data","univariate_lowbmi_excluded_bmi_trajectory_change_T2DM.csv"))
write.csv (models_trajectory_change_T1DM, here::here ("output/data","univariate_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))


