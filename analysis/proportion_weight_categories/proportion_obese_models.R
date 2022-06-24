

#### Author: M Samuel
#### Date: 15th June
####  This script looks proportion obese proportions each year

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

BMI_all <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))


BMI_all <- BMI_all %>% 
  dplyr::select(
    year,
    patient_id, 
    sex, 
    age_group_2, 
    eth_group_16,
    imd,
    region,
    smoking_status, 
    BMI_categories, 
    starts_with("comorbid"))

BMI_all <- BMI_all %>% 
  dplyr::mutate(obese = as.character(BMI_categories))


BMI_all <- BMI_all %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2019"| year == "2021" ) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group_2 = as.factor(age_group_2)) %>%
  dplyr::mutate(age_group_2 = fct_relevel(age_group_2, "18-29", "30-39", "40-49", "50-59", "60-69", "70-79","80+", "missing")) %>% 
  dplyr::mutate(sex = fct_relevel(sex, 'F', "M")) 


BMI_all$obese[BMI_all$obese == "obese"] <- 'TRUE'
BMI_all$obese[BMI_all$obese == "healthy"] <- 'FALSE'
BMI_all$obese[BMI_all$obese == "overweight"] <- 'FALSE'
BMI_all$obese[BMI_all$obese == "underweight"] <- 'FALSE'


BMI_all <- BMI_all %>% 
  dplyr::mutate(obese = as.logical(obese))


###
model_year <- BMI_all %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year")



model_age <- BMI_all %>% 
  glm(obese ~ age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "age")

model_imd <- BMI_all %>% 
  glm(obese ~ imd, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::mutate(predictor = "imd")


model_sex <- BMI_all %>% 
  glm(obese ~ sex, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "sex")


model_eth <- BMI_all %>% 
  glm(obese ~ eth_group_16, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "ethnicity")

model_region <- BMI_all %>% 
  glm(obese ~ region, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "region")


model_year_age <- BMI_all %>% 
  glm(obese ~ year+ age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::mutate(predictor = "year_age")

models_complete <- model_year %>% 
  bind_rows(model_age) %>% 
  bind_rows(model_sex) %>% 
  bind_rows(model_imd) %>% 
  bind_rows(model_eth) %>% 
  bind_rows(model_region) %>% 
  bind_rows(model_year_age) %>% 
  dplyr::mutate(across(where(is.numeric), round, 5)) 


write.csv (models_complete, here::here ("output/data", "obese_2019_21_models_1.csv"))




