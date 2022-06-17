#### Author: M Samuel
#### Date: 15th June
####  This script looks at the demographics of the total population in each group


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
library(skimr)

BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_models_data.feather"))

##1.  exclude low bmi and cancer
##2. run analysis - write yaml action



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
                "pandemic_stage", 
                "trajectory_change")



BMI_trajectories$precovid_bmi_category <- factor(BMI_trajectories$precovid_bmi_category, levels = c("healthy","overweight", "obese", "underweight"))



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


## Analysis of predictors for top decile of trajectory change



traj_change <- BMI_trajectories 

quantiles <- as.data.frame(quantile(traj_change$trajectory_change, probs = seq(.1, .9, by = .1))) %>% 
  dplyr::rename(trajectory_change = 1)


## create a column for deciles
traj_change$decile <- ntile(traj_change$trajectory_change, 10)

## create a flag for top 10% weight gain

traj_change <- traj_change %>% 
  dplyr::mutate(change_90th = case_when(
    decile == 10 ~ 1,
    decile != 10 ~ 0
  ))


models_univar <- explanatory_vars %>%       # begin with variables of interest
  str_c("change_90th ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = traj_change)) %>%          # dataset
  
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



population_demog_function2 <- function(my_var) {
  traj_change %>%
    tabyl({{my_var}}, change_90th)%>% 
    dplyr::rename(not_90th = '0') %>% 
    dplyr::rename(top_decile = '1') %>% 
    dplyr::mutate(total = not_90th + top_decile) %>% 
    dplyr::mutate(percent = top_decile/total) %>% 
    dplyr::select(-('not_90th')) %>% 
    dplyr::rename(group={{my_var}})
} 


  

sex <- population_demog_function2(sex) %>% 
  dplyr::mutate(variable = 'sex') %>% 
  dplyr::mutate(group = as.factor(group))


age_group_2 <- population_demog_function2(age_group_2) %>% 
  dplyr::mutate(variable = 'age_group_2')%>% 
  dplyr::mutate(group = as.factor(group))

region <- population_demog_function2(region) %>% 
  dplyr::mutate(variable = 'region')%>% 
  dplyr::mutate(group = as.factor(group))

imd <- population_demog_function2(imd) %>% 
  dplyr::mutate(variable = 'imd') %>% 
  dplyr::mutate(group = as.factor(group))


hypertension <- population_demog_function2(hypertension) %>% 
  dplyr::mutate(variable = 'hypertension') %>% 
  dplyr::mutate(group = as.factor(group)) 

diabetes_t1 <- population_demog_function2(diabetes_t1) %>% 
  dplyr::mutate(variable = 'diabetes_t1')%>% 
  dplyr::mutate(group = as.factor(group))

diabetes_t2 <- population_demog_function2(diabetes_t1) %>% 
  dplyr::mutate(variable = 'diabetes_t1')%>% 
  dplyr::mutate(group = as.factor(group))

learning_disability <- population_demog_function2(learning_disability) %>% 
  dplyr::mutate(variable = 'learning_disability')%>% 
  dplyr::mutate(group = as.factor(group))

depression <- population_demog_function2(depression) %>% 
  dplyr::mutate(variable = 'depression')%>% 
  dplyr::mutate(group = as.factor(group))

psychosis_schiz_bipolar <- population_demog_function2(psychosis_schiz_bipolar) %>% 
  dplyr::mutate(variable = 'psychosis_schiz_bipolar')%>% 
  dplyr::mutate(group = as.factor(group))

dementia <- population_demog_function2(dementia) %>% 
  dplyr::mutate(variable = 'dementia')%>% 
  dplyr::mutate(group = as.factor(group))

asthma <- population_demog_function2(asthma) %>% 
  dplyr::mutate(variable = 'asthma')%>% 
  dplyr::mutate(group = as.factor(group))


COPD <- population_demog_function2(COPD) %>% 
  dplyr::mutate(variable = 'COPD')%>% 
  dplyr::mutate(group = as.factor(group))

stroke_and_TIA <- population_demog_function2(stroke_and_TIA) %>% 
  dplyr::mutate(variable = 'stroke_and_TIA')%>% 
  dplyr::mutate(group = as.factor(group))

chronic_cardiac <- population_demog_function2(chronic_cardiac) %>% 
  dplyr::mutate(variable = 'chronic_cardiac')%>% 
  dplyr::mutate(group = as.factor(group))


all_cancer <- population_demog_function2(all_cancer) %>% 
  dplyr::mutate(variable = 'all_cancer')%>% 
  dplyr::mutate(group = as.factor(group))

smoking_status <- population_demog_function2(smoking_status) %>% 
  dplyr::mutate(variable = 'smoking_status')%>% 
  dplyr::mutate(group = as.factor(group))

ethnic_no_miss <- population_demog_function2(ethnic_no_miss) %>% 
  dplyr::mutate(variable = 'ethnic_no_miss')%>% 
  dplyr::mutate(group = as.factor(group))


eth_group_16 <- population_demog_function2(eth_group_16) %>% 
  dplyr::mutate(variable = 'eth_group_16')%>% 
  dplyr::mutate(group = as.factor(group))

precovid_bmi_category <- population_demog_function2(precovid_bmi_category) %>% 
  dplyr::mutate(variable = 'precovid_bmi_category')%>% 
  dplyr::mutate(group = as.factor(group))




change_demog <- sex %>% 
  bind_rows(
                             age_group_2, 
                             ethnic_no_miss,
                             eth_group_16,
                             region, 
                             imd, 
                             hypertension,
                             diabetes_t1,
                             diabetes_t2,
                             learning_disability,
                             depression,
                             psychosis_schiz_bipolar,
                             dementia,
                             asthma,
                             COPD,
                             stroke_and_TIA,
                             chronic_cardiac,
                             all_cancer,
                             smoking_status,
                             precovid_bmi_category)


change_demog <- change_demog %>% 
  dplyr::mutate(top_decile = plyr::round_any(change_demog$top_decile, 5))







### Write outputs



write_csv (models_univar, here::here ("output/data","change_90th_univariate.csv"))

write_csv (quantiles, here::here ("output/data","change_deciles.csv"))

write_csv (change_demog, here::here ("output/data","change_90th_counts.csv"))














