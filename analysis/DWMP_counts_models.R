## This script looks at proportion eligible for DWMP 
## Author: M Samuel
## Date: 24th May 2022



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
library(ggplot2)


DWMP <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

## order the age-groups for ordered plots
DWMP <- DWMP# Replicate data
DWMP$age_group_2 <- factor(DWMP$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))

DWMP$age_group <- factor(DWMP$age_group,      # Reordering group factor levels
                                     levels = c("18-39", "40-65", "65-80", "80+"))


DWMP$smoking_status <- factor(DWMP$smoking_status, 
                                          levels = c('N',"S", "E", "M"))


## selected the variables for analysis


DWMP2 <- DWMP %>% 
  dplyr::filter(year==2017| year==2018| year==2019| year==2020| year==2021)


DWMP2 <- DWMP2 %>%
  rename_all(~stringr::str_replace(.,"^comorbid_",""))

DWMP2 <- DWMP2 %>% 
  dplyr::select(
    c("patient_id",
     "median_bmi",
      "sex", 
     "age_group_2", 
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
     "smoking_status", 
     "year", 
     "DWMP")
  )

## drop NA in BMI
DWMP2 <- DWMP2 %>% 
  drop_na(median_bmi)


DWMP2 <- DWMP2 %>% 
  dplyr::mutate(DWMP = case_when(
    DWMP == "eligible" ~ 1, 
    DWMP == "not_eligible" ~ 0
  )) 
DWMP2 <- DWMP2 %>% 
  dplyr::mutate(DWMP = as.logical(DWMP))



explanatory_vars_hypertension <- c(
      "sex", 
      "age_group_2", 
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
      "all_cancer", 
      "smoking_status", 
      "year")

explanatory_vars_diabetes <- c(
  "sex", 
  "age_group_2", 
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
  "all_cancer", 
  "smoking_status", 
  "year")

## univariate models predictors of being DWMP eligible in subgroups








###
## Hypertension

## Limit to patients with hypertension
DWMP2_hypertension <-  DWMP2 %>% 
dplyr::filter(hypertension == TRUE)



models_DWMP_hypertension <- explanatory_vars_hypertension %>%       # begin with variables of interest
  str_c("DWMP ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x), 
      family = "binomial",          # within glm(), the string formula is .x
      data = DWMP2_hypertension)) %>%          # dataset
  
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




models_DWMP_hypertension


## T2DM
## Limit to patients with T2 diabetes
DWMP2_T2DM <-  DWMP2 %>% 
dplyr::filter(diabetes_t2 == TRUE)








models_DWMP_T2DM <- explanatory_vars_diabetes %>%       # begin with variables of interest
  str_c("DWMP ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",  
      data = DWMP2_T2DM)) %>%          # dataset
  
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




models_DWMP_T2DM

## T1DM

DWMP2_T1DM <-  DWMP2 %>% 
dplyr::filter(diabetes_t1 == TRUE)






models_DWMP_T1DM <- explanatory_vars_diabetes %>%       # begin with variables of interest
  str_c("DWMP ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",  
      data = DWMP2_T1DM)) %>%          # dataset
  
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




models_DWMP_T1DM

##### Age adjusted models



## Limit to patients with hypertension

models_DWMP_hypertension_2 <- explanatory_vars_hypertension %>%       # begin with variables of interest
  str_c("DWMP ~ age_group_2 +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each age_adjusted formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x), 
      family = "binomial",          # within glm(), the string formula is .x
      data = DWMP2_hypertension)) %>%          # dataset
  
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




models_DWMP_hypertension_2



## limite to patient withT2DM

models_DWMP_T2DM_2 <- explanatory_vars_diabetes %>%       # begin with variables of interest
  str_c("DWMP ~ age_group_2 +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each age_adjusted formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",  
      data = DWMP2_T2DM)) %>%          # dataset
  
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




models_DWMP_T2DM_2





## Limit to patients with T1DM

models_DWMP_T1DM_2 <- explanatory_vars_diabetes %>%       # begin with variables of interest
  str_c("DWMP ~ age_group_2 +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each age_adjusted formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",  
      data = DWMP2_T1DM)) %>%          # dataset
  
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




models_DWMP_T1DM_2









write.csv (models_DWMP_hypertension_2, here::here ("output/data","age_adjusted_DWMP_hypertension.csv"))
write.csv (models_DWMP_T2DM_2, here::here ("output/data","age_adjusted_DWMP_T2DM.csv"))
write.csv (models_DWMP_T1DM_2, here::here ("output/data","age_adjusted_DWMP_T1DM.csv"))




write.csv (models_DWMP_hypertension, here::here ("output/data","univariate_DWMP_hypertension.csv"))
write.csv (models_DWMP_T2DM, here::here ("output/data","univariate_DWMP_T2DM.csv"))
write.csv (models_DWMP_T1DM, here::here ("output/data","univariate_DWMP_T1DM.csv"))



