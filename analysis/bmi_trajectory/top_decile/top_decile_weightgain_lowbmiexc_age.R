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

postcovid_change <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))







postcovid_change <- postcovid_change %>% 
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
                "pandemic_stage", 
                "yearly_bmi_change")



postcovid_change$precovid_bmi_category <- factor(postcovid_change$precovid_bmi_category, levels = c("healthy","overweight", "obese", "underweight"))



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












### Postcovid Analysis

postcovid_change <- postcovid_change %>% 
  dplyr::filter(pandemic_stage == "postcovid")


postcovid_change <- postcovid_change %>% 
  dplyr::filter(all_cancer == FALSE)

postcovid_change <- postcovid_change %>% 
  dplyr::filter(precovid_bmi_category != "underweight")


##postcovid_quantiles
postcovid_quantiles <- as.data.frame(quantile(postcovid_change$yearly_bmi_change, probs = seq(.1, .9, by = .1))) %>%
  dplyr::rename( postcovid_yearly_bmi_change = 1)


## create a column for deciles
postcovid_change$decile <- ntile(postcovid_change$yearly_bmi_change, 10)

## create a flag for top 10% weight gain

postcovid_change <- postcovid_change %>% 
  dplyr::mutate(weightgain_90th = case_when(
    decile == 10 ~ 1,
    decile != 10 ~ 0
  ))

models_postcov_rapidinc_bmi_univar <- explanatory_vars %>%       # begin with variables of interest
  str_c("weightgain_90th ~ age_group_2 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
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


models_postcov_rapidinc_bmi_univar <- models_postcov_rapidinc_bmi_univar %>% 
  dplyr::mutate(stage="postcovid", .before=1)






write_csv (models_postcov_rapidinc_bmi_univar, here::here ("output/data","weightgain_lowbmiexc_90th_age_adj.csv"))















