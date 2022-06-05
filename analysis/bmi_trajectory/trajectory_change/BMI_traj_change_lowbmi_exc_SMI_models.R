

## This script will develop linear regression models of predictors of change in BMI trajectory
## All cancer and low BMI prepandemic filtered out
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
## Limit to patients with learning_disability
BMI_DT_SMI <-  BMI_DT %>% 
dplyr::filter(psychosis_schiz_bipolar == TRUE)



# develop a vector of explanatory variables
explanatory_vars <- c("age_group_2", 
                        "sex",
                      "precovid_bmi_category", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "imd", 
                      "region", 
                      "hypertension",  
                      "diabetes_t1",           
                      "diabetes_t2",
                      "chronic_cardiac", 
                      "depression",          
                      "dementia",               
                      "learning_disability",
                      "asthma",                
                      "COPD",                   
                      "stroke_and_TIA",       
                      "smoking_status")


explanatory_vars_sex <- c("age_group_2", 
                      "precovid_bmi_category", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "imd", 
                      "region", 
                      "hypertension",  
                      "diabetes_t1",           
                      "diabetes_t2",
                      "chronic_cardiac",  
                      "depression",            
                      "dementia",               
                      "learning_disability",
                      "asthma",                
                      "COPD",                   
                      "stroke_and_TIA",       
                      "smoking_status")


explanatory_vars_age <- c("sex",
                      "precovid_bmi_category", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "imd", 
                      "region", 
                      "hypertension",  
                      "diabetes_t1",           
                      "diabetes_t2",
                      "chronic_cardiac", 
                      "depression",     
                      "depression",            
                      "dementia", 
                      "asthma",                
                      "COPD",                   
                      "stroke_and_TIA",       
                      "smoking_status")

explanatory_vars_imd <- c("age_group_2", 
                        "sex",
                      "precovid_bmi_category", 
                      "ethnic_no_miss", 
                      "eth_group_16", 
                      "region", 
                      "hypertension",  
                      "diabetes_t1",           
                      "diabetes_t2",
                      "chronic_cardiac", 
                      "depression",          
                      "dementia", 
                      "asthma",                
                      "COPD",                   
                      "stroke_and_TIA",       
                      "smoking_status")

explanatory_vars_eth <- c("age_group_2", 
                        "sex",
                      "precovid_bmi_category", 
                      "imd", 
                      "region", 
                      "hypertension",  
                      "diabetes_t1",           
                      "diabetes_t2",
                      "chronic_cardiac", 
                      "depression",          
                      "dementia",               
                      "learning_disability",
                      "asthma",                
                      "COPD",                   
                      "stroke_and_TIA",       
                      "smoking_status")

explanatory_vars_sexage <- c("precovid_bmi_category", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "imd", 
                      "region", 
                      "hypertension",  
                      "diabetes_t1",           
                      "diabetes_t2",
                      "chronic_cardiac", 
                      "depression",     
                      "depression",            
                      "dementia", 
                      "asthma",                
                      "COPD",                   
                      "stroke_and_TIA",       
                      "smoking_status")







models_SMI_univariate <- explanatory_vars %>%       # begin with variables of interest
  str_c("trajectory_change ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each sex_adjusted formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_SMI)) %>%          # dataset
  
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

##
## Limit to patients with depression >> sex adjusted

models_SMI_sex <- explanatory_vars_sex %>%       # begin with variables of interest
  str_c("trajectory_change ~ sex + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each sex_adjusted formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_SMI)) %>%          # dataset
  
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

##
## Limit to patients with depression >>age adjusted

models_SMI_age <- explanatory_vars_age %>%       # begin with variables of interest
  str_c("trajectory_change ~ age_group_2 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each sex_adjusted formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_SMI)) %>%          # dataset
  
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

##
## Limit to patients with depression >> imd

models_SMI_imd <- explanatory_vars_imd %>%       # begin with variables of interest
  str_c("trajectory_change ~ imd + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each sex_adjusted formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_SMI)) %>%          # dataset
  
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

## Limit to patients with depression >> ethnicity adjusted

models_SMI_eth <- explanatory_vars_eth %>%       # begin with variables of interest
  str_c("trajectory_change ~ eth_group_16 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each sex_adjusted formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_SMI)) %>%          # dataset
  
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



## Limit to patients with depression >> sex age adjusted

models_SMI_sexage <- explanatory_vars_sexage %>%       # begin with variables of interest
  str_c("trajectory_change ~ sex + age_group_2 +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each sex_adjusted formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_SMI)) %>%          # dataset
  
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



## Limit to patients with depression >> sex age imd adjusted

models_SMI_sexageimd <- explanatory_vars_sexage %>%       # begin with variables of interest
  str_c("trajectory_change ~ sex + age_group_2 + imd +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each sex_adjusted formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_SMI)) %>%          # dataset
  
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

## Limit to patients with depression >> sex age ethnicity adjusted

models_SMI_sexageeth <- explanatory_vars_sexage %>%       # begin with variables of interest
  str_c("trajectory_change ~ sex + age_group_2 + eth_group_16 +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each sex_adjusted formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_SMI)) %>%          # dataset
  
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



## Limit to patients with depression >> sex age ethnicity imd adjusted

models_SMI_sexageethimd <- explanatory_vars_sexage %>%       # begin with variables of interest
  str_c("trajectory_change ~ sex + age_group_2 + eth_group_16 + imd +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each sex_adjusted formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = BMI_DT_SMI)) %>%          # dataset
  
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




write.csv (models_SMI_univariate, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_univariate.csv"))
write.csv (models_SMI_sex, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sex_adj.csv"))
write.csv (models_SMI_age, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_age_adj.csv"))
write.csv (models_SMI_eth, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_ethnicity_adj.csv"))
write.csv (models_SMI_imd, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_imd_adj.csv"))
write.csv (models_SMI_sexage, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sexage_adj.csv"))
write.csv (models_SMI_sexageimd, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sexageimd_adj.csv"))
write.csv (models_SMI_sexageeth, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sexageeth_adj.csv"))
write.csv (models_SMI_sexageethimd, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sexageethimd_adj.csv"))











