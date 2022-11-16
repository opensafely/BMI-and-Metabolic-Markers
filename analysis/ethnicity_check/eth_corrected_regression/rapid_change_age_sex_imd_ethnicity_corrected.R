
## M Samuel 
## 5th May 2022
## This script checks factors associated with rapid weight gain if patients with cancer and a low BMI prepandemic are removed


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



## Read in files
BMI_2 <- read_feather (here::here ("output/data", "BMI_trajectory_data_long_eth_corrected.feather"))



colnames(BMI_2)





BMI_trajectories <- BMI_2 %>% 
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
                "eth_16_corrected",
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


explanatory_vars <- c("eth_16_corrected")


explanatory_vars_2 <- c("sex",
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
                      "smoking_status", 
                      "eth_16_corrected",           
                      "precovid_bmi_category")





##*** Change to code.  FILTER OUT UNDERWEIGHT AND THOSE WITH CANCER
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(all_cancer == FALSE)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(precovid_bmi_category != "underweight")

BMI_trajectories %>% 
  tabyl(all_cancer)

BMI_trajectories %>% 
  tabyl(precovid_bmi_category)

## *** Change to code complete







## Precovid analysis proportions in groups


### Precovid analysis


precovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "precovid")

# age adjusted
models_precov_rapidinc_bmi_age <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ age_group_2 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_precov_rapidinc_bmi_age <- models_precov_rapidinc_bmi_age %>%
  dplyr::mutate(stage = "precovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "age_adjusted")


##
# sex adjusted
models_precov_rapidinc_bmi_sex <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ sex + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_precov_rapidinc_bmi_sex <- models_precov_rapidinc_bmi_sex %>%
  dplyr::mutate(stage = "precovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "sex_adjusted")



## imd adjusted

models_precov_rapidinc_bmi_imd <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ imd + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_precov_rapidinc_bmi_imd <- models_precov_rapidinc_bmi_imd %>%
  dplyr::mutate(stage = "precovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "imd_adjusted")



# age/sex adjusted
models_precov_rapidinc_bmi_age_sex <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ age_group_2 + sex + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_precov_rapidinc_bmi_age_sex <- models_precov_rapidinc_bmi_age_sex %>%
  dplyr::mutate(stage = "precovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "age_sex_adjusted")




# age/sex/imd adjusted
models_precov_rapidinc_bmi_age_sex_imd <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ age_group_2 + sex + imd + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_precov_rapidinc_bmi_age_sex_imd <- models_precov_rapidinc_bmi_age_sex_imd %>%
  dplyr::mutate(stage = "precovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "age_sex_imd_adjusted")








models_precov <- models_precov_rapidinc_bmi_age %>%
  bind_rows(models_precov_rapidinc_bmi_sex, 
            models_precov_rapidinc_bmi_imd, 
            models_precov_rapidinc_bmi_age_sex, 
            models_precov_rapidinc_bmi_age_sex_imd)







## postcovid analysis proportions in groups


### postcovid analysis


postcovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "postcovid")

# age adjusted
models_postcov_rapidinc_bmi_age <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ age_group_2 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_postcov_rapidinc_bmi_age <- models_postcov_rapidinc_bmi_age %>%
  dplyr::mutate(stage = "postcovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "age_adjusted")


##
# sex adjusted
models_postcov_rapidinc_bmi_sex <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ sex + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_postcov_rapidinc_bmi_sex <- models_postcov_rapidinc_bmi_sex %>%
  dplyr::mutate(stage = "postcovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "sex_adjusted")



## imd adjusted

models_postcov_rapidinc_bmi_imd <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ imd + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_postcov_rapidinc_bmi_imd <- models_postcov_rapidinc_bmi_imd %>%
  dplyr::mutate(stage = "postcovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "imd_adjusted")



# age/sex adjusted
models_postcov_rapidinc_bmi_age_sex <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ age_group_2 + sex + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_postcov_rapidinc_bmi_age_sex <- models_postcov_rapidinc_bmi_age_sex %>%
  dplyr::mutate(stage = "postcovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "age_sex_adjusted")




# age/sex/imd adjusted
models_postcov_rapidinc_bmi_age_sex_imd <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ age_group_2 + sex + imd + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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

models_postcov_rapidinc_bmi_age_sex_imd <- models_postcov_rapidinc_bmi_age_sex_imd %>%
  dplyr::mutate(stage = "postcovid", .before = 1) %>% 
  
  ## add label
  mutate(model = "age_sex_imd_adjusted")








models_postcov <- models_postcov_rapidinc_bmi_age %>%
  bind_rows(models_postcov_rapidinc_bmi_sex, 
            models_postcov_rapidinc_bmi_imd, 
            models_postcov_rapidinc_bmi_age_sex, 
            models_postcov_rapidinc_bmi_age_sex_imd)



models <- models_precov %>% 
  bind_rows(models_postcov)


write_csv (models, here::here ("output/data","rapid_bmi_change_cancerandlowbmi_removed_eth_corrected_age_sex_imd.csv"))
