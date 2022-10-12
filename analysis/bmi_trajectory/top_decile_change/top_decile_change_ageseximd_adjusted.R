
#### Author: M Samuel
#### Date: 15th June
####  This script looks at predictors of being in the top decile of change in rate of BMI change


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
library(lmtest)

BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_models_data.feather"))



##1.  exclude low bmi and cancer

##*** Change to code.  FILTER OUT UNDERWEIGHT AND THOSE WITH CANCER
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(all_cancer == FALSE)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(precovid_bmi_category != "underweight")



# select relevant variables
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
                "precovid_bmi_category", 
                "trajectory_change")



BMI_trajectories$precovid_bmi_category <- factor(BMI_trajectories$precovid_bmi_category, levels = c("healthy","overweight", "obese"))



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

quantiles <- quantiles %>%
  cbind(rownames(quantiles), data.frame(quantiles, row.names=NULL)) %>% 
  dplyr::select(-c(1))


## create a column for deciles
traj_change$decile <- ntile(traj_change$trajectory_change, 10)

## create a flag for top 10% weight gain

traj_change <- traj_change %>% 
  dplyr::mutate(change_90th = case_when(
    decile == 10 ~ 1,
    decile != 10 ~ 0
  ))





DT_traj_change <- as.data.table(traj_change)



## create a data set that is not missing for sex - for Wald tests

DT_traj_sex <- DT_traj_change %>% 
  drop_na(sex)







eth_group_16_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + eth_group_16, family = binomial)] 

ethnicity_sex <- eth_group_16_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)






imd_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + imd, family = binomial)] 

imd_sex <- imd_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)






region_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + region, family = binomial)] 

region_sex <- region_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)





hypertension_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + hypertension, family = binomial)] 

hypertension_sex <- hypertension_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)




##
diabetes_t1_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + diabetes_t1, family = binomial)] 

diabetes_t1_sex <- diabetes_t1_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



##

diabetes_t2_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + diabetes_t2, family = binomial)] 

diabetes_t2_sex <- diabetes_t2_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



##
chronic_cardiac_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + chronic_cardiac, family = binomial)] 

chronic_cardiac_sex <- chronic_cardiac_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



##

learning_disability_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + learning_disability, family = binomial)] 

learning_disability_sex <- learning_disability_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



##
depression_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + depression, family = binomial)] 

depression_sex <- depression_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



##
psychosis_schiz_bipolar_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + psychosis_schiz_bipolar, family = binomial)] 

SMI_sex <- psychosis_schiz_bipolar_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



##
asthma_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + asthma, family = binomial)] 

asthma_sex <- asthma_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



##
COPD_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + COPD, family = binomial)] 

COPD_sex <- COPD_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



##
stroke_and_TIA_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + stroke_and_TIA, family = binomial)] 

stroke_sex <- stroke_and_TIA_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



##
dementia_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + dementia, family = binomial)] 

dementia_sex <- dementia_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)




##
all_cancer_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + all_cancer, family = binomial)] 

all_cancer_sex <- all_cancer_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)




##
smoking_status_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + smoking_status, family = binomial)] 

smoking_status_sex <- smoking_status_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)




##
precovid_bmi_category_sex_model <- DT_traj_sex [, glm(change_90th ~ imd +  age_group_2 + sex + precovid_bmi_category, family = binomial)] 

precovid_bmi_category_sex <- precovid_bmi_category_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)





age_sex_adjusted <- imd_sex %>%
  bind_rows(
    ethnicity_sex, 
    region_sex, 
    diabetes_t2_sex,
    diabetes_t1_sex, 
    hypertension_sex, 
    chronic_cardiac_sex, 
    depression_sex,
    learning_disability_sex, 
    SMI_sex, 
    COPD_sex, 
    asthma_sex, 
    dementia_sex,
    stroke_sex, 
    all_cancer_sex, 
    precovid_bmi_category_sex, 
    smoking_status_sex)



write.csv (age_sex_adjusted, here::here ("output/data","change_90th_age_sex_imd_adjusted_lowbmiexc.csv"))




