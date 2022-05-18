## This script will develop linear regression models of predictors of change in BMI trajectory adjusted for age
## Author: Miriam Samuel 
## Date: 17th May


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
                      "all_cancer", 
                      "smoking_status")



models_trajectory_change <- explanatory_vars %>%       # begin with variables of interest
  str_c("trajectory_change ~ age_group_2 +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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
  mutate(across(where(is.numeric), round, digits = 2))




models_trajectory_change











write.csv (models_trajectory_change, here::here ("output/data","age_adjusted_bmi_trajectory_change.csv"))

