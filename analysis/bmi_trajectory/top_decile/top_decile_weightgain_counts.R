
## Counts for top decile of weightgain.  

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
library(data.table)


BMI_data <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))

BMI_data <- BMI_data %>% 
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
                "yearly_bmi_change")



BMI_data$precovid_bmi_category <- factor(BMI_data$precovid_bmi_category, levels = c("healthy","overweight", "obese", "underweight"))

### Postcovid Analysis

postcovid_change <- BMI_data %>% 
  dplyr::filter(pandemic_stage == "postcovid")




##postcovid_quantiles
postcovid_quantiles <- as.data.frame(quantile(postcovid_change$yearly_bmi_change, probs = seq(.1, .9, by = .1))) %>%
  dplyr::rename( postcovid_yearly_bmi_change = 1)


## create a column for deciles
postcovid_change$decile <- ntile(postcovid_change$yearly_bmi_change, 10)

## create a flag for top 10% weight gain

postcovid_change <- postcovid_change %>% 
  dplyr::mutate(weightgain_90th = case_when(
    decile == 10 ~ 1,
    decile != 10 ~ 0
  ))











prop_function <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))

  data %>%
tabyl({{my_var}}, weightgain_90th) %>% 
  dplyr::rename(top_decile = 3) %>% 
  dplyr::rename(bott_deciles = 2) %>% 
  dplyr::mutate(total = top_decile + bott_deciles) %>%
  dplyr::mutate(top_decile_prop = top_decile/total) %>%
  dplyr::mutate(se_prop = sqrt(((top_decile_prop * (1-top_decile_prop))/total))) %>% 
  dplyr::rename(group = {{my_var}}) %>% 
  dplyr::mutate(variable = (v1), .before=1)  %>%   
  dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
  dplyr::mutate(group = as.character(group)) 
}


sex <- prop_function(postcovid_change, sex)
age_group_2 <- prop_function(postcovid_change, age_group_2)
eth_group_16 <- prop_function(postcovid_change, eth_group_16)
imd <- prop_function(postcovid_change, imd)
region <- prop_function(postcovid_change, region)
hypertension <- prop_function(postcovid_change,  hypertension)
diabetes_t1 <- prop_function(postcovid_change,  diabetes_t1)
diabetes_t2 <- prop_function(postcovid_change,  diabetes_t2)
chronic_cardiac <- prop_function(postcovid_change,  chronic_cardiac)
learning_disability <- prop_function(postcovid_change,  learning_disability)
depression <- prop_function(postcovid_change,  depression)
dementia <- prop_function(postcovid_change,  dementia)
psychosis_schiz_bipolar <- prop_function(postcovid_change,  psychosis_schiz_bipolar)
asthma <- prop_function(postcovid_change,  asthma)
COPD <- prop_function(postcovid_change,  COPD)
stroke_and_TIA <- prop_function(postcovid_change,  stroke_and_TIA)
smoking_status <- prop_function(postcovid_change, smoking_status)


complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>% 
  dplyr::mutate(population = "all", .before=1)

complete <- complete %>% 
  dplyr::mutate(bott_deciles = plyr::round_any(complete$bott_deciles, 5)) %>%
  dplyr::mutate(top_decile = plyr::round_any(complete$top_decile, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete$total, 5)) 


#####################
## Exclude cancer and low bmi
postcovid_change <- postcovid_change %>% 
  dplyr::filter(all_cancer == FALSE)

postcovid_change <- postcovid_change %>% 
  dplyr::filter(precovid_bmi_category != "underweight")



sex <- prop_function(postcovid_change, sex)
age_group_2 <- prop_function(postcovid_change, age_group_2)
eth_group_16 <- prop_function(postcovid_change, eth_group_16)
imd <- prop_function(postcovid_change, imd)
region <- prop_function(postcovid_change, region)
hypertension <- prop_function(postcovid_change,  hypertension)
diabetes_t1 <- prop_function(postcovid_change,  diabetes_t1)
diabetes_t2 <- prop_function(postcovid_change,  diabetes_t2)
chronic_cardiac <- prop_function(postcovid_change,  chronic_cardiac)
learning_disability <- prop_function(postcovid_change,  learning_disability)
depression <- prop_function(postcovid_change,  depression)
dementia <- prop_function(postcovid_change,  dementia)
psychosis_schiz_bipolar <- prop_function(postcovid_change,  psychosis_schiz_bipolar)
asthma <- prop_function(postcovid_change,  asthma)
COPD <- prop_function(postcovid_change,  COPD)
stroke_and_TIA <- prop_function(postcovid_change,  stroke_and_TIA)
smoking_status <- prop_function(postcovid_change, smoking_status)


complete_lowbmiexc <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>% 
  dplyr::mutate(population = "lowbmi_exc", .before=1)

complete_lowbmiexc <- complete_lowbmiexc %>% 
  dplyr::mutate(bott_deciles = plyr::round_any(complete_lowbmiexc$bott_deciles, 5)) %>%
  dplyr::mutate(top_decile = plyr::round_any(complete_lowbmiexc$top_decile, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete_lowbmiexc$total, 5)) 

complete <- complete %>% 
  bind_rows(complete_lowbmiexc) %>% 
  dplyr::mutate(pandemic = "postcovid", .before=1)


### Precovid Analysis

precovid_change <- BMI_data %>% 
  dplyr::filter(pandemic_stage == "precovid")




##precovid_quantiles
precovid_quantiles <- as.data.frame(quantile(precovid_change$yearly_bmi_change, probs = seq(.1, .9, by = .1))) %>%
  dplyr::rename( precovid_yearly_bmi_change = 1)


## create a column for deciles
precovid_change$decile <- ntile(precovid_change$yearly_bmi_change, 10)

## create a flag for top 10% weight gain

precovid_change <- precovid_change %>% 
  dplyr::mutate(weightgain_90th = case_when(
    decile == 10 ~ 1,
    decile != 10 ~ 0
  ))


prop_function <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))

  data %>%
tabyl({{my_var}}, weightgain_90th) %>% 
  dplyr::rename(top_decile = 3) %>% 
  dplyr::rename(bott_deciles = 2) %>% 
  dplyr::mutate(total = top_decile + bott_deciles) %>%
  dplyr::mutate(top_decile_prop = top_decile/total) %>%
  dplyr::mutate(se_prop = sqrt(((top_decile_prop * (1-top_decile_prop))/total))) %>% 
  dplyr::rename(group = {{my_var}}) %>% 
  dplyr::mutate(variable = (v1), .before=1)  %>%   
  dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
  dplyr::mutate(group = as.character(group)) 
}


sex <- prop_function(precovid_change, sex)
age_group_2 <- prop_function(precovid_change, age_group_2)
eth_group_16 <- prop_function(precovid_change, eth_group_16)
imd <- prop_function(precovid_change, imd)
region <- prop_function(precovid_change, region)
hypertension <- prop_function(precovid_change,  hypertension)
diabetes_t1 <- prop_function(precovid_change,  diabetes_t1)
diabetes_t2 <- prop_function(precovid_change,  diabetes_t2)
chronic_cardiac <- prop_function(precovid_change,  chronic_cardiac)
learning_disability <- prop_function(precovid_change,  learning_disability)
depression <- prop_function(precovid_change,  depression)
dementia <- prop_function(precovid_change,  dementia)
psychosis_schiz_bipolar <- prop_function(precovid_change,  psychosis_schiz_bipolar)
asthma <- prop_function(precovid_change,  asthma)
COPD <- prop_function(precovid_change,  COPD)
stroke_and_TIA <- prop_function(precovid_change,  stroke_and_TIA)
smoking_status <- prop_function(precovid_change, smoking_status)


precovid_complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>% 
  dplyr::mutate(population = "all", .before=1)

precovid_complete <- precovid_complete %>% 
  dplyr::mutate(bott_deciles = plyr::round_any(precovid_complete$bott_deciles, 5)) %>%
  dplyr::mutate(top_decile = plyr::round_any(precovid_complete$top_decile, 5)) %>%
  dplyr::mutate(total = plyr::round_any(precovid_complete$total, 5)) 


#####################
## Exclude cancer and low bmi
precovid_change <- precovid_change %>% 
  dplyr::filter(all_cancer == FALSE)

precovid_change <- precovid_change %>% 
  dplyr::filter(precovid_bmi_category != "underweight")



sex <- prop_function(precovid_change, sex)
age_group_2 <- prop_function(precovid_change, age_group_2)
eth_group_16 <- prop_function(precovid_change, eth_group_16)
imd <- prop_function(precovid_change, imd)
region <- prop_function(precovid_change, region)
hypertension <- prop_function(precovid_change,  hypertension)
diabetes_t1 <- prop_function(precovid_change,  diabetes_t1)
diabetes_t2 <- prop_function(precovid_change,  diabetes_t2)
chronic_cardiac <- prop_function(precovid_change,  chronic_cardiac)
learning_disability <- prop_function(precovid_change,  learning_disability)
depression <- prop_function(precovid_change,  depression)
dementia <- prop_function(precovid_change,  dementia)
psychosis_schiz_bipolar <- prop_function(precovid_change,  psychosis_schiz_bipolar)
asthma <- prop_function(precovid_change,  asthma)
COPD <- prop_function(precovid_change,  COPD)
stroke_and_TIA <- prop_function(precovid_change,  stroke_and_TIA)
smoking_status <- prop_function(precovid_change, smoking_status)


precovid_complete_lowbmiexc <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>% 
  dplyr::mutate(population = "lowbmi_exc", .before=1)

precovid_complete_lowbmiexc <- precovid_complete_lowbmiexc %>% 
  dplyr::mutate(bott_deciles = plyr::round_any(precovid_complete_lowbmiexc$bott_deciles, 5)) %>%
  dplyr::mutate(top_decile = plyr::round_any(precovid_complete_lowbmiexc$top_decile, 5)) %>%
  dplyr::mutate(total = plyr::round_any(precovid_complete_lowbmiexc$total, 5)) 

precovid_complete <- precovid_complete %>% 
  bind_rows(precovid_complete_lowbmiexc) %>% 
  dplyr::mutate(pandemic = "precovid", .before=1)


complete <- complete %>% 
dplyr::bind_rows(precovid_complete)



write_csv (complete, here::here ("output/data","weightgain_90th_counts_total_and_lowbmiexc.csv"))
