
## This script looks proportion in the top_decile of weight gain acceleration in patients with T2DM
## Population limited to those with T2DM
# M Samuel 
# 16th Nov




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

## from: bmi_trajectory_change_summary

# BMI_trajectories <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "BMI_trajectory_models_eth_corrected_data.feather"))

BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_models_eth_corrected_data.feather"))





##1.  exclude low bmi and cancer

##*** Change to code.  FILTER OUT UNDERWEIGHT AND THOSE WITH CANCER
##* JUST KEEP PATIENTS WITH T2DM
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(all_cancer == FALSE)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(diabetes_t2 == TRUE)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(precovid_bmi_category != "underweight")



# select relevant variables
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::select("sex",
                "age_group_2", 
                "region",                
                "imd",
                "hypertension",
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
                "eth_16_corrected",
                "complete_bmi_data", 
                "precovid_bmi_category", 
                "trajectory_change")



BMI_trajectories$precovid_bmi_category <- factor(BMI_trajectories$precovid_bmi_category, levels = c("healthy","overweight", "obese"))





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


diabetes_t2 <- population_demog_function2(diabetes_t2) %>% 
  dplyr::mutate(variable = 'diabetes_t2')%>% 
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



eth_16_corrected <- population_demog_function2(eth_16_corrected) %>% 
  dplyr::mutate(variable = 'eth_group_16')%>% 
  dplyr::mutate(group = as.factor(group))

precovid_bmi_category <- population_demog_function2(precovid_bmi_category) %>% 
  dplyr::mutate(variable = 'precovid_bmi_category')%>% 
  dplyr::mutate(group = as.factor(group))




change_demog <- sex %>% 
  bind_rows(
    age_group_2,
    eth_16_corrected,
    region, 
    imd, 
    hypertension,
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


colnames(change_demog)

change_demog <- change_demog %>% 
  dplyr::mutate(top_decile = plyr::round_any(change_demog$top_decile, 5)) %>%
  dplyr::mutate(total = plyr::round_any(change_demog$total, 5)) %>% 
  dplyr::select("variable", "group", "top_decile", "total", "percent")







### Write outputs


write_csv (change_demog, here::here ("output/data","change_90th_counts_lowbmiexc_eth_corrected_t2dm.csv"))
