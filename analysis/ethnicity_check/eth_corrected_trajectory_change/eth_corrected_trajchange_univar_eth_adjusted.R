## This script looks for predictors of trajectory change in BMI after correcting ethnicity
## Population limited to those with T2DM
# M Samuel 
# 16th Nov




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

## from: bmi_trajectory_change_summary

# BMI_trajectories <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "BMI_trajectory_models_eth_corrected_data.feather"))

BMI_DT <- read_feather (here::here ("output/data", "BMI_trajectory_models_eth_corrected_data.feather"))

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
explanatory_vars <- c("age_group_2", 
                    "sex",
                    "imd",
                      "precovid_bmi_category", 
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
  str_c("trajectory_change ~ eth_group_16 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each ethnic_adjusted formula
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




## Limit to patients with T2 diabetes
BMI_DT_T2DM <-  BMI_DT %>% 
dplyr::filter(diabetes_t2 == TRUE)




## LINEAR REGRESSION MODELLING
### Predictors of trajectory change


# develop a vector of explanatory variables
explanatory_vars_diabetes <- c("age_group_2",  "sex", "imd",
                      "precovid_bmi_category", 
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
  str_c("trajectory_change ~ eth_group_16 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each ethnic_adjusted formula
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

write.csv (models_trajectory_change, here::here ("output/data","ethnic_adjusted_lowbmi_excluded_bmi_trajectory_change_eth_corrected.csv"))
write.csv (models_trajectory_change_T2DM, here::here ("output/data","ethnic_adjusted_lowbmi_excluded_bmi_trajectory_change_T2DM_eth_corrected.csv"))




