## This script looks at sex  adjusted predictors rapid bmi change 
## Author: M Samuel
## Date: 4th May 2022



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

BMI_trajectories$precovid_bmi_category <- factor(BMI_trajectories$precovid_bmi_category, levels = c("healthy","overweight", "obese", "underweight"))



explanatory_vars_2 <- c("age_group_2",                 
                        "imd",
                        "region",
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
                      "smoking_status", 
                      "ethnic_no_miss",         
                      "eth_group_16",           
                      "precovid_bmi_category")

explanatory_vars_3 <- c("pandemic_stage")


##*** Change to code.  FILTER OUT UNDERWEIGHT AND THOSE WITH CANCER
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(all_cancer == FALSE)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(precovid_bmi_category != "underweight")

BMI_trajectories %>% 
  tabyl(all_cancer)

BMI_trajectories %>% 
  tabyl(precovid_bmi_category)

##  ALL population - does pandemic affect odds
models_all <- explanatory_vars_3 %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ sex +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
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
  mutate(across(where(is.numeric), round, digits = 4))




models_all <- models_all %>%
  dplyr::mutate(stage = "all", .before = 1)




## population composition


### Precovid analysis
precovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "precovid")

models_precov <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ sex +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = precovid_change)) %>%          # dataset
  
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


## population composition

models_precov <- models_precov %>%
  dplyr::mutate(stage = "precovid", .before = 1)



###########################################################

### Precovid analysis
postcovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "postcovid")

models_postcov <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ sex +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = postcovid_change)) %>%          # dataset
  
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


## population composition

models_postcov <- models_postcov %>%
  dplyr::mutate(stage = "postcovid", .before = 1)


models <-  models_all %>%
  bind_rows (models_precov) %>% 
  bind_rows(models_postcov)


write_csv (models, here::here ("output/data","rapid_bmi_change_cancerandlowbmi_removed_sex_adjusted.csv"))

