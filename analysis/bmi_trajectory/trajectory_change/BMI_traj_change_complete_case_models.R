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

BMI_data <- read_feather (here::here ("/Users/miriamsamuel/Documents/Academic GP/Open Safely/Dummy Data", "BMI_trajectory_data_long.feather"))


## Filter for data that had BMI_trajectory data in the precovid period.  This is my population
trajectory_population <- BMI_data %>% 
  dplyr::filter(period1_missing == FALSE)

## Filter out patients who were underweight precovid and those with cancer

trajectory_population <- trajectory_population %>%
  dplyr::filter(all_cancer == "FALSE") %>% 
  dplyr::filter(precovid_bmi_category != "underweight")


## Create a flag for COVID
trajectory_population <- trajectory_population %>% 
  dplyr::mutate(complete = case_when(
    complete_bmi_data == "complete" ~ 1, 
    complete_bmi_data == "incomplete" ~ 0)
  )


## Univariate analysis _ predictors

explanatory_vars_2 <- c("age_group_2",   
                        "sex",
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






##  ALL population - does pandemic affect odds
univariate <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("complete ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = trajectory_population)) %>%          # dataset
  
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


age_adjusted <-  explanatory_vars_2 %>%       # begin with variables of interest
  str_c("complete ~ age_group_2 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = trajectory_population)) %>%          # dataset
  
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


sex_adjusted <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("complete ~ sex +  ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = trajectory_population)) %>%          # dataset
  
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



imd_adjusted <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("complete ~ imd + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = trajectory_population)) %>%          # dataset
  
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






eth_adjusted <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("complete ~ eth_group_16 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = trajectory_population)) %>%          # dataset
  
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




sexage_adjusted <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("complete ~ sex + age_group_2 + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = trajectory_population)) %>%          # dataset
  
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


sexageimd_adjusted <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("complete ~ sex + age_group_2 + imd + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = trajectory_population)) %>%          # dataset
  
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


sexageeth_adjusted <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("complete ~ sex + age_group_2 + eth_group_16 +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = trajectory_population)) %>%          # dataset
  
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

sexageethimd_adjusted <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("complete ~ sex + age_group_2 + eth_group_16 + imd + ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = trajectory_population)) %>%          # dataset
  
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


write.csv (univariate, here::here ("output/data","univariate_complete_cases.csv"))
write.csv (age_adjusted, here::here ("output/data","age_adjusted_complete_cases.csv"))
write.csv (sex_adjusted, here::here ("output/data","sex_adjusted_complete_cases.csv"))
write.csv (sexage_adjusted, here::here ("output/data","sexage_adjusted_complete_cases.csv"))
write.csv (eth_adjusted, here::here ("output/data","eth_adjusted_complete_cases.csv"))
write.csv (imd_adjusted, here::here ("output/data","imd_adjusted_complete_cases.csv"))
write.csv (sexageeth_adjusted, here::here ("output/data","sexageeth_adjusted_complete_cases.csv"))
write.csv (sexageimd_adjusted, here::here ("output/data","sexageimd_adjusted_complete_cases.csv"))
write.csv (sexageethimd_adjusted, here::here ("output/data","sexageethimd_adjusted_complete_cases.csv"))







