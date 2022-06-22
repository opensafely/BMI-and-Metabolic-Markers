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


### Age adjuted models


## create a data set that is not missing for age - for Wald tests

DT_traj_age <- DT_traj_change %>% 
  drop_na(age_group_2)




sex_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + sex, family = binomial)] 
sex_model <- DT_traj_age[, glm(change_90th ~ sex, family = binomial)] 

sex_age <- sex_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

sex_age_wald <- lrtest(sex_age_model, sex_model) %>% 
  dplyr::mutate(variable = 'sex')



eth_group_16_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + eth_group_16, family = binomial)] 
eth_group_16_model <- DT_traj_age[, glm(change_90th ~ eth_group_16, family = binomial)] 

ethnicity_age <- eth_group_16_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

ethnicity_age_wald <- lrtest(eth_group_16_age_model, eth_group_16_model) %>% 
  dplyr::mutate(variable = 'ethnicity')




imd_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + imd, family = binomial)] 
imd_model <- DT_traj_age[, glm(change_90th ~ imd, family = binomial)] 

imd_age <- imd_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

imd_age_wald <- lrtest(imd_age_model, imd_model) %>% 
  dplyr::mutate(variable = 'imd')




region_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + region, family = binomial)] 
region_model <- DT_traj_age[, glm(change_90th ~ region, family = binomial)] 

region_age <- region_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

region_age_wald <- lrtest(region_age_model, region_model) %>% 
  dplyr::mutate(variable = 'region')



hypertension_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + hypertension, family = binomial)] 
hypertension_model <- DT_traj_age[, glm(change_90th ~ hypertension, family = binomial)] 

hypertension_age <- hypertension_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

hypertension_age_wald <- lrtest(hypertension_age_model, hypertension_model) %>% 
  dplyr::mutate(variable = 'hypertension')


##
diabetes_t1_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + diabetes_t1, family = binomial)] 
diabetes_t1_model <- DT_traj_age[, glm(change_90th ~ diabetes_t1, family = binomial)] 

diabetes_t1_age <- diabetes_t1_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

diabetes_t1_age_wald <- lrtest(diabetes_t1_age_model, diabetes_t1_model) %>% 
  dplyr::mutate(variable = 'T1DM')

##

diabetes_t2_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + diabetes_t2, family = binomial)] 
diabetes_t2_model <- DT_traj_age[, glm(change_90th ~ diabetes_t2, family = binomial)] 

diabetes_t2_age <- diabetes_t2_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

diabetes_t2_age_wald <- lrtest(diabetes_t2_age_model, diabetes_t2_model) %>% 
  dplyr::mutate(variable = 'T2DM')

##
chronic_cardiac_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + chronic_cardiac, family = binomial)] 
chronic_cardiac_model <- DT_traj_age[, glm(change_90th ~ chronic_cardiac, family = binomial)] 

chronic_cardiac_age <- chronic_cardiac_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

chronic_cardiac_age_wald <- lrtest(chronic_cardiac_age_model, chronic_cardiac_model) %>% 
  dplyr::mutate(variable = 'chronic_cardiac')

##

learning_disability_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + learning_disability, family = binomial)] 
learning_disability_model <- DT_traj_age[, glm(change_90th ~ learning_disability, family = binomial)] 

learning_disability_age <- learning_disability_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

learning_disability_age_wald <- lrtest(learning_disability_age_model, learning_disability_model) %>% 
  dplyr::mutate(variable = 'learning_disability')

##
depression_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + depression, family = binomial)] 
depression_model <- DT_traj_age[, glm(change_90th ~ depression, family = binomial)] 

depression_age <- depression_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

depression_age_wald <- lrtest(depression_age_model, depression_model) %>% 
  dplyr::mutate(variable = 'depression')

##
psychosis_schiz_bipolar_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + psychosis_schiz_bipolar, family = binomial)] 
psychosis_schiz_bipolar_model <- DT_traj_age[, glm(change_90th ~ psychosis_schiz_bipolar, family = binomial)] 

SMI_age <- psychosis_schiz_bipolar_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

SMI_age_wald <- lrtest(psychosis_schiz_bipolar_age_model, psychosis_schiz_bipolar_model) %>% 
  dplyr::mutate(variable = 'SMI')

##
asthma_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + asthma, family = binomial)] 
asthma_model <- DT_traj_age[, glm(change_90th ~ asthma, family = binomial)] 

asthma_age <- asthma_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

asthma_age_wald <- lrtest(asthma_age_model, asthma_model) %>% 
  dplyr::mutate(variable = 'asthma')

##
COPD_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + COPD, family = binomial)] 
COPD_model <- DT_traj_age[, glm(change_90th ~ COPD, family = binomial)] 

COPD_age <- COPD_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

COPD_age_wald <- lrtest(COPD_age_model, COPD_model) %>% 
  dplyr::mutate(variable = 'COPD')


##
stroke_and_TIA_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + stroke_and_TIA, family = binomial)] 
stroke_and_TIA_model <- DT_traj_age[, glm(change_90th ~ stroke_and_TIA, family = binomial)] 

stroke_age <- stroke_and_TIA_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

stroke_age_wald <- lrtest(stroke_and_TIA_age_model, stroke_and_TIA_model) %>% 
  dplyr::mutate(variable = 'stroke_TIA')

##
dementia_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + dementia, family = binomial)] 
dementia_model <- DT_traj_age[, glm(change_90th ~ dementia, family = binomial)] 

dementia_age <- dementia_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

dementia_age_wald <- lrtest(dementia_age_model, dementia_model) %>% 
  dplyr::mutate(variable = 'dementia')


##
all_cancer_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + all_cancer, family = binomial)] 
all_cancer_model <- DT_traj_age[, glm(change_90th ~ all_cancer, family = binomial)] 

all_cancer_age <- all_cancer_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

all_cancer_age_wald <- lrtest(all_cancer_age_model, all_cancer_model) %>% 
  dplyr::mutate(variable = 'all_cancer')


##
smoking_status_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + smoking_status, family = binomial)] 
smoking_status_model <- DT_traj_age[, glm(change_90th ~ smoking_status, family = binomial)] 

smoking_status_age <- smoking_status_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

smoking_status_age_wald <- lrtest(smoking_status_age_model, smoking_status_model) %>% 
  dplyr::mutate(variable = 'smoking')


##
precovid_bmi_category_age_model <- DT_traj_age [, glm(change_90th ~ age_group_2 + precovid_bmi_category, family = binomial)] 
precovid_bmi_category_model <- DT_traj_age[, glm(change_90th ~ precovid_bmi_category, family = binomial)] 

precovid_bmi_category_age <- precovid_bmi_category_age_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

precovid_bmi_category_age_wald <- lrtest(precovid_bmi_category_age_model, precovid_bmi_category_model) %>% 
  dplyr::mutate(variable = 'precovid_bmi')



age_adjusted <- sex_age %>% 
  bind_rows(
            ethnicity_age, 
            imd_age, 
            region_age, 
            diabetes_t2_age,
            diabetes_t1_age, 
            hypertension_age, 
            chronic_cardiac_age, 
            depression_age,
            learning_disability_age, 
            SMI_age, 
            COPD_age, 
            asthma_age, 
            dementia_age,
            stroke_age, 
            all_cancer_age, 
            precovid_bmi_category_age, 
            smoking_status_age)

wald_age <- sex_age_wald %>% 
  bind_rows(
    ethnicity_age_wald, 
    imd_age_wald, 
    region_age_wald, 
    diabetes_t2_age_wald,
    diabetes_t1_age_wald, 
    hypertension_age_wald, 
    chronic_cardiac_age_wald, 
    depression_age_wald,
    learning_disability_age_wald, 
    SMI_age_wald, 
    COPD_age_wald, 
    asthma_age_wald, 
    dementia_age_wald,
    stroke_age_wald, 
    all_cancer_age_wald, 
    precovid_bmi_category_age_wald, 
    smoking_status_age_wald)

write.csv (age_adjusted, here::here ("output/data","change_90th_age_adjusted_lowbmiexc.csv"))
write.csv (wald_age, here::here ("output/data","change_90th_wald_age.csv"))

                          


