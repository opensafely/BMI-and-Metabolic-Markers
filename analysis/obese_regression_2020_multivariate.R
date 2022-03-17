##########################################
## Author: Miriam Samuel
## Updated: 17th March 2022
## Multivariate analysis of who was obese in 2020



##  packages
library(broom)
library(purrr)
library(dplyr)
library(janitor)
library(tidyverse)
library(arrow)

#read in file
BMI_complete_categories <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))




## select the variables needed for analysis

BMI_complete_categories_2020 <- BMI_complete_categories


##  Filter by year
BMI_complete_categories_2020 <- BMI_complete_categories_2020 %>%
  ungroup %>%
  dplyr::filter(year==2020) %>%
  dplyr::select(patient_id,
                obese, 
                sex, 
                age_group, 
                region, 
                imd, 
                ethnic_no_miss, 
                eth_group_16,
                precovid_obese_flag, 
                starts_with("comorbid_"))








## convert obese to a logical output
BMI_complete_categories_2020 %>%
  dplyr::mutate(obese = as.logical(obese))




## Try to change base level  >>  NOTE:  co-efficient for base group = log.odds of event in base group
BMI_complete_categories_2020 <- BMI_complete_categories_2020 %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", after = 0)) 


## instructions from Epi R handbook
# create a new vector of explanatory variables - exclude age, gender, ethnicity

explanatory_vars_multivariate <- c("region", 
                                   "imd",
                                   "comorbid_learning_disability",     
                                   "comorbid_depression",            
                                   "comorbid_dementia",               
                                   "comorbid_psychosis_schiz_bipolar",
                                   "comorbid_diabetes_type",
                                   "comorbid_diabetes_t1",           
                                   "comorbid_diabetes_t2",             
                                   "comorbid_asthma",                
                                   "comorbid_COPD",                   
                                   "comorbid_stroke_and_TIA",         
                                   "comorbid_chronic_cardiac", 
                                   "comorbid_hypertension",           
                                   "comorbid_all_cancer")


# >> Example model
# Multivariate model with age +gender
# obese_age_sex_m <- glm(obese ~ age_group + sex, data=BMI_complete_categories_2020, family=binomial) %>%
# broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
# dplyr::mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns



#>> Use PURR to loop over the different exposures in a univariate analysis and create a combined table

#>>  use stringer to create a vector listing each item to run the logistic regression over
models_age_sex_ethnic_2020 <- explanatory_vars_multivariate %>%       # begin with variables of interest
  str_c("obese ~ age_group + sex + eth_group_16 + ", .)  %>%      ## creates a vector of character with terms for age, gender and ethnicity regression
  
  # iterate through each univariate formula ... using map function from purr
  map(                               #  Map each element of the preceding vector the following formula
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = BMI_complete_categories_2020))  %>%        # dataset
  
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




####################################################################################
## OUTPUTS
write.csv (models_age_sex_ethnic_2020, here::here ("output/data","multivariate_regression_obese_2020.csv"))
