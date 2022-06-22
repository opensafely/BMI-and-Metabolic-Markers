
#### Author: M Samuel
#### Date: 15th June
####  This script looks at those in the top decile of post covid change in BMI rate of change


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



# BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_models_data.feather"))

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







eth_group_16_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + eth_group_16, family = binomial)] 
eth_group_16_model <- DT_traj_sex[, glm(change_90th ~ eth_group_16, family = binomial)] 

ethnicity_sex <- eth_group_16_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

ethnicity_sex_wald <- lrtest(eth_group_16_sex_model, eth_group_16_model) %>% 
  dplyr::mutate(variable = 'ethnicity')




imd_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + imd, family = binomial)] 
imd_model <- DT_traj_sex[, glm(change_90th ~ imd, family = binomial)] 

imd_sex <- imd_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

imd_sex_wald <- lrtest(imd_sex_model, imd_model) %>% 
  dplyr::mutate(variable = 'imd')




region_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + region, family = binomial)] 
region_model <- DT_traj_sex[, glm(change_90th ~ region, family = binomial)] 

region_sex <- region_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

region_sex_wald <- lrtest(region_sex_model, region_model) %>% 
  dplyr::mutate(variable = 'region')



hypertension_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + hypertension, family = binomial)] 
hypertension_model <- DT_traj_sex[, glm(change_90th ~ hypertension, family = binomial)] 

hypertension_sex <- hypertension_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

hypertension_sex_wald <- lrtest(hypertension_sex_model, hypertension_model) %>% 
  dplyr::mutate(variable = 'hypertension')


##
diabetes_t1_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + diabetes_t1, family = binomial)] 
diabetes_t1_model <- DT_traj_sex[, glm(change_90th ~ diabetes_t1, family = binomial)] 

diabetes_t1_sex <- diabetes_t1_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

diabetes_t1_sex_wald <- lrtest(diabetes_t1_sex_model, diabetes_t1_model) %>% 
  dplyr::mutate(variable = 'T1DM')

##

diabetes_t2_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + diabetes_t2, family = binomial)] 
diabetes_t2_model <- DT_traj_sex[, glm(change_90th ~ diabetes_t2, family = binomial)] 

diabetes_t2_sex <- diabetes_t2_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

diabetes_t2_sex_wald <- lrtest(diabetes_t2_sex_model, diabetes_t2_model) %>% 
  dplyr::mutate(variable = 'T2DM')

##
chronic_cardiac_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + chronic_cardiac, family = binomial)] 
chronic_cardiac_model <- DT_traj_sex[, glm(change_90th ~ chronic_cardiac, family = binomial)] 

chronic_cardiac_sex <- chronic_cardiac_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

chronic_cardiac_sex_wald <- lrtest(chronic_cardiac_sex_model, chronic_cardiac_model) %>% 
  dplyr::mutate(variable = 'chronic_cardiac')

##

learning_disability_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + learning_disability, family = binomial)] 
learning_disability_model <- DT_traj_sex[, glm(change_90th ~ learning_disability, family = binomial)] 

learning_disability_sex <- learning_disability_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

learning_disability_sex_wald <- lrtest(learning_disability_sex_model, learning_disability_model) %>% 
  dplyr::mutate(variable = 'learning_disability')

##
depression_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + depression, family = binomial)] 
depression_model <- DT_traj_sex[, glm(change_90th ~ depression, family = binomial)] 

depression_sex <- depression_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

depression_sex_wald <- lrtest(depression_sex_model, depression_model) %>% 
  dplyr::mutate(variable = 'depression')

##
psychosis_schiz_bipolar_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + psychosis_schiz_bipolar, family = binomial)] 
psychosis_schiz_bipolar_model <- DT_traj_sex[, glm(change_90th ~ psychosis_schiz_bipolar, family = binomial)] 

SMI_sex <- psychosis_schiz_bipolar_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

SMI_sex_wald <- lrtest(psychosis_schiz_bipolar_sex_model, psychosis_schiz_bipolar_model) %>% 
  dplyr::mutate(variable = 'SMI')

##
asthma_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + asthma, family = binomial)] 
asthma_model <- DT_traj_sex[, glm(change_90th ~ asthma, family = binomial)] 

asthma_sex <- asthma_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

asthma_sex_wald <- lrtest(asthma_sex_model, asthma_model) %>% 
  dplyr::mutate(variable = 'asthma')

##
COPD_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + COPD, family = binomial)] 
COPD_model <- DT_traj_sex[, glm(change_90th ~ COPD, family = binomial)] 

COPD_sex <- COPD_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

COPD_sex_wald <- lrtest(COPD_sex_model, COPD_model) %>% 
  dplyr::mutate(variable = 'COPD')


##
stroke_and_TIA_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + stroke_and_TIA, family = binomial)] 
stroke_and_TIA_model <- DT_traj_sex[, glm(change_90th ~ stroke_and_TIA, family = binomial)] 

stroke_sex <- stroke_and_TIA_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

stroke_sex_wald <- lrtest(stroke_and_TIA_sex_model, stroke_and_TIA_model) %>% 
  dplyr::mutate(variable = 'stroke_TIA')

##
dementia_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + dementia, family = binomial)] 
dementia_model <- DT_traj_sex[, glm(change_90th ~ dementia, family = binomial)] 

dementia_sex <- dementia_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

dementia_sex_wald <- lrtest(dementia_sex_model, dementia_model) %>% 
  dplyr::mutate(variable = 'dementia')


##
all_cancer_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + all_cancer, family = binomial)] 
all_cancer_model <- DT_traj_sex[, glm(change_90th ~ all_cancer, family = binomial)] 

all_cancer_sex <- all_cancer_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

all_cancer_sex_wald <- lrtest(all_cancer_sex_model, all_cancer_model) %>% 
  dplyr::mutate(variable = 'all_cancer')


##
smoking_status_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + smoking_status, family = binomial)] 
smoking_status_model <- DT_traj_sex[, glm(change_90th ~ smoking_status, family = binomial)] 

smoking_status_sex <- smoking_status_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

smoking_status_sex_wald <- lrtest(smoking_status_sex_model, smoking_status_model) %>% 
  dplyr::mutate(variable = 'smoking')


##
precovid_bmi_category_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + precovid_bmi_category, family = binomial)] 
precovid_bmi_category_model <- DT_traj_sex[, glm(change_90th ~ precovid_bmi_category, family = binomial)] 

precovid_bmi_category_sex <- precovid_bmi_category_sex_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

precovid_bmi_category_sex_wald <- lrtest(precovid_bmi_category_sex_model, precovid_bmi_category_model) %>% 
  dplyr::mutate(variable = 'precovid_bmi')


##
age_sex_model <- DT_traj_sex [, glm(change_90th ~ sex + age_group_2, family = binomial)] 
age_model <- DT_traj_sex[, glm(change_90th ~ age_group_2, family = binomial)] 

age_sex_wald <- lrtest(age_sex_model, age_model) %>% 
  dplyr::mutate(variable = 'age')


sex_adjusted <- imd_sex %>%
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

wald_sex <- age_sex_wald %>% 
  bind_rows(
    imd_sex_wald,
    ethnicity_sex_wald, 
    region_sex_wald, 
    diabetes_t2_sex_wald,
    diabetes_t1_sex_wald, 
    hypertension_sex_wald, 
    chronic_cardiac_sex_wald, 
    depression_sex_wald,
    learning_disability_sex_wald, 
    SMI_sex_wald, 
    COPD_sex_wald, 
    asthma_sex_wald, 
    dementia_sex_wald,
    stroke_sex_wald, 
    all_cancer_sex_wald, 
    precovid_bmi_category_sex_wald, 
    smoking_status_sex_wald)

write.csv (sex_adjusted, here::here ("output/data","change_90th_sex_adjusted_lowbmiexc.csv"))
write.csv (wald_sex, here::here ("output/data","change_90th_wald_sex.csv"))




