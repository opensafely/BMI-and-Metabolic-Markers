#### Author: M Samuel
#### Date: 15th June
####  This script looks at the odds of having a BMI in different groups


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

BMI_median_2 <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

BMI_complete_categories <- BMI_median_2 %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2019"| year == "2021" ) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group_2 = as.factor(age_group_2)) %>%
  dplyr::mutate(age_group_2 = fct_relevel(age_group_2, "18-29", "30_39", "40-49", "50_59", "60-69", "70-79","80+")) %>% 
  dplyr::mutate(sex = fct_relevel(sex, 'F', "M"))



model_year <- BMI_complete_categories %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year")



model_age <- BMI_complete_categories %>% 
  glm(had_bmi ~ age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "age")

model_imd <- BMI_complete_categories %>% 
  glm(had_bmi ~ imd, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::mutate(predictor = "imd")


model_sex <- BMI_complete_categories %>% 
  glm(had_bmi ~ sex, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "sex")


model_eth <- BMI_complete_categories %>% 
  glm(had_bmi ~ eth_group_16, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "ethnicity")

model_region <- BMI_complete_categories %>% 
  glm(had_bmi ~ region, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "region")


model_year_age <- BMI_complete_categories %>% 
  glm(had_bmi ~ year+ age_group_2, data=., family=binomial) %>% 
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








  write.csv (models_complete, here::here ("output/data","had_bmi_2019_21_models_1.csv"))
