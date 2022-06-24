

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


### MODELS



model_T1DM <- BMI_all %>%
  dplyr::filter(comorbid_diabetes_t1 == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "diabetes_t1")

model_T1DM_2 <- BMI_all %>%
  dplyr::filter(comorbid_diabetes_t1 == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "diabetes_t1")


model_T2DM <- BMI_all %>%
  dplyr::filter(comorbid_diabetes_t2 == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "diabetes_t2")

model_T2DM_2 <- BMI_all %>%
  dplyr::filter(comorbid_diabetes_t2 == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "diabetes_t2")


model_hypertension <- BMI_all %>%
  dplyr::filter(comorbid_hypertension == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "hypertension")

model_hypertension_2 <- BMI_all %>%
  dplyr::filter(comorbid_hypertension == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "hypertension")



model_learning_disability <- BMI_all %>%
  dplyr::filter(comorbid_learning_disability == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "learning_disability")

model_learning_disability_2 <- BMI_all %>%
  dplyr::filter(comorbid_learning_disability == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "learning_disability")





model_depression <- BMI_all %>%
  dplyr::filter(comorbid_depression == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "depression")


model_depression_2 <- BMI_all %>%
  dplyr::filter(comorbid_depression == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "depression")



model_psychosis_schiz_bipolar <- BMI_all %>%
  dplyr::filter(comorbid_psychosis_schiz_bipolar == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "psychosis_schiz_bipolar")


model_psychosis_schiz_bipolar_2 <- BMI_all %>%
  dplyr::filter(comorbid_psychosis_schiz_bipolar == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "psychosis_schiz_bipolar")


model_asthma <- BMI_all %>%
  dplyr::filter(comorbid_asthma == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "asthma")


model_asthma_2 <- BMI_all %>%
  dplyr::filter(comorbid_asthma == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "asthma")




model_COPD <- BMI_all %>%
  dplyr::filter(comorbid_COPD == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "COPD")


model_COPD_2 <- BMI_all %>%
  dplyr::filter(comorbid_COPD == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "COPD")


model_stroke_and_TIA <- BMI_all %>%
  dplyr::filter(comorbid_stroke_and_TIA == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "stroke_and_TIA")


model_stroke_and_TIA_2 <- BMI_all %>%
  dplyr::filter(comorbid_stroke_and_TIA == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "stroke_and_TIA")




model_dementia <- BMI_all %>%
  dplyr::filter(comorbid_dementia == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "dementia")


model_dementia_2 <- BMI_all %>%
  dplyr::filter(comorbid_dementia == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "dementia")




model_chronic_cardiac <- BMI_all %>%
  dplyr::filter(comorbid_chronic_cardiac == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "chronic_cardiac")


model_chronic_cardiac_2 <- BMI_all %>%
  dplyr::filter(comorbid_chronic_cardiac == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "chronic_cardiac")


model_all_cancer <- BMI_all %>%
  dplyr::filter(comorbid_all_cancer == TRUE) %>% 
  glm(obese ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "all_cancer")


model_all_cancer_2 <- BMI_all %>%
  dplyr::filter(comorbid_all_cancer == TRUE) %>% 
  glm(obese ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "all_cancer")


models_all <- model_T2DM %>%
  bind_rows(model_T2DM_2) %>%
  bind_rows(model_T1DM) %>% 
  bind_rows(model_T1DM_2) %>% 
  bind_rows(model_hypertension) %>% 
  bind_rows(model_hypertension_2) %>% 
  bind_rows(model_chronic_cardiac) %>% 
  bind_rows(model_chronic_cardiac_2) %>% 
  bind_rows(model_learning_disability) %>% 
  bind_rows(model_learning_disability_2) %>% 
  bind_rows(model_psychosis_schiz_bipolar) %>% 
  bind_rows(model_psychosis_schiz_bipolar_2) %>% 
  bind_rows(model_depression) %>% 
  bind_rows(model_depression_2) %>% 
  bind_rows(model_asthma) %>% 
  bind_rows(model_asthma_2) %>% 
  bind_rows(model_COPD) %>% 
  bind_rows(model_COPD_2) %>% 
  bind_rows(model_stroke_and_TIA) %>%
  bind_rows(model_stroke_and_TIA_2) %>% 
  bind_rows(model_dementia) %>% 
  bind_rows(model_dementia_2) %>% 
  bind_rows(model_all_cancer) %>% 
  bind_rows(model_all_cancer_2)


models_all <- models_all  %>% 
  dplyr::mutate(across(where(is.numeric), round, 5)) 


write.csv (models_all, here::here ("output/data","obese_2019_21_comorbidity_models.csv"))
