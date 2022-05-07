## M Samuel 
## 5th May 2022
## This script ensures that the demographic data used is the most recent recorded for the patient (e.g. age)


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

# webshot::install_phantomjs()



## Read in files
BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))

colnames(BMI_trajectories)

   



BMI_precovid_change_table <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "precovid") %>%
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
                "precovid_bmi_category")

exposure_variates <- c("sex",
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
                       "precovid_bmi_category")

precovid_bmi_change <- BMI_precovid_change_table %>% 
  tabyl(sex, bmi_change_cat) %>%
  adorn_totals() %>% 
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "all", .before=1) %>% 
  dplyr::rename(group = sex) %>% 
  dplyr::filter(group == 'Total') %>%
  dplyr::mutate(group = as.character(group))

precovid_bmichange_sex <- BMI_precovid_change_table %>% 
  tabyl(sex, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "sex", .before=1) %>% 
  dplyr::rename (group = sex)  %>%
  dplyr::mutate(group = as.character(group))
  

precovid_bmichange_age_group_2 <- BMI_precovid_change_table %>% 
  tabyl(age_group_2, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "age_group_2", .before=1) %>% 
  dplyr::rename (group = age_group_2) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_region <- BMI_precovid_change_table %>% 
  tabyl(region, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "region", .before=1) %>% 
  dplyr::rename (group = region) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_imd <- BMI_precovid_change_table %>% 
  tabyl(imd, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "imd", .before=1) %>% 
  dplyr::rename (group = imd) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_hypertension <- BMI_precovid_change_table %>% 
  tabyl(hypertension, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "hypertension", .before=1) %>% 
  dplyr::rename (group = hypertension) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_diabetes_t1 <- BMI_precovid_change_table %>% 
  tabyl(diabetes_t1, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "diabetes_t1", .before=1) %>% 
  dplyr::rename (group = diabetes_t1) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_diabetes_t2 <- BMI_precovid_change_table %>% 
  tabyl(diabetes_t2, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "diabetes_t2", .before=1) %>% 
  dplyr::rename (group = diabetes_t2) %>%
  dplyr::mutate(group = as.character(group))



precovid_bmichange_learning_disability <- BMI_precovid_change_table %>% 
  tabyl(learning_disability, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "learning_disability", .before=1) %>% 
  dplyr::rename (group = learning_disability) %>%
  dplyr::mutate(group = as.character(group))



precovid_bmichange_depression <- BMI_precovid_change_table %>% 
  tabyl(depression, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "depression", .before=1) %>% 
  dplyr::rename (group = depression) %>%
  dplyr::mutate(group = as.character(group))



precovid_bmichange_psychosis_schiz_bipolar <- BMI_precovid_change_table %>% 
  tabyl(psychosis_schiz_bipolar, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "psychosis_schiz_bipolar", .before=1) %>% 
  dplyr::rename (group = psychosis_schiz_bipolar) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_dementia <- BMI_precovid_change_table %>% 
  tabyl(dementia, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "dementia", .before=1) %>% 
  dplyr::rename (group = dementia) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_COPD <- BMI_precovid_change_table %>% 
  tabyl(COPD, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "COPD", .before=1) %>% 
  dplyr::rename (group = COPD) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_asthma <- BMI_precovid_change_table %>% 
  tabyl(asthma, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "asthma", .before=1) %>% 
  dplyr::rename (group = asthma) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_stroke_and_TIA <- BMI_precovid_change_table %>% 
  tabyl(stroke_and_TIA, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "stroke_and_TIA", .before=1) %>% 
  dplyr::rename (group = stroke_and_TIA) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_chronic_cardiac <- BMI_precovid_change_table %>% 
  tabyl(chronic_cardiac, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "chronic_cardiac", .before=1) %>% 
  dplyr::rename (group = chronic_cardiac) %>%
  dplyr::mutate(group = as.character(group))


precovid_bmichange_all_cancer<- BMI_precovid_change_table %>% 
  tabyl(all_cancer, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "all_cancer", .before=1) %>% 
  dplyr::rename (group = all_cancer) %>%
  dplyr::mutate(group = as.character(group))



precovid_bmichange_smoking_status<- BMI_precovid_change_table %>% 
  tabyl(smoking_status, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "smoking_status", .before=1) %>% 
  dplyr::rename (group = smoking_status) %>%
  dplyr::mutate(group = as.character(group))



precovid_bmichange_eth_group_16<- BMI_precovid_change_table %>% 
  tabyl(eth_group_16, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "eth_group_16", .before=1) %>% 
  dplyr::rename (group = eth_group_16) %>%
  dplyr::mutate(group = as.character(group))
 

precovid_bmichange_precovid_bmi_category<- BMI_precovid_change_table %>% 
  tabyl(precovid_bmi_category, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "precovid_bmi_category", .before=1) %>% 
  dplyr::rename (group = precovid_bmi_category) %>%
  dplyr::mutate(group = as.character(group))





precovid_bmi_trajectories <- precovid_bmi_change %>% 
  dplyr::bind_rows(precovid_bmichange_age_group_2)  %>% 
  dplyr::bind_rows(precovid_bmichange_sex) %>% 
  dplyr::bind_rows(precovid_bmichange_eth_group_16)  %>% 
  dplyr::bind_rows(precovid_bmichange_imd)  %>% 
  dplyr::bind_rows(precovid_bmichange_region)  %>%
  dplyr::bind_rows(precovid_bmichange_hypertension)  %>% 
  dplyr::bind_rows(precovid_bmichange_diabetes_t1)  %>% 
  dplyr::bind_rows(precovid_bmichange_diabetes_t2)  %>% 
  dplyr::bind_rows(precovid_bmichange_learning_disability)  %>% 
  dplyr::bind_rows(precovid_bmichange_depression)  %>% 
  dplyr::bind_rows(precovid_bmichange_psychosis_schiz_bipolar)  %>% 
  dplyr::bind_rows(precovid_bmichange_dementia)  %>% 
  dplyr::bind_rows(precovid_bmichange_asthma)  %>% 
  dplyr::bind_rows(precovid_bmichange_COPD)  %>% 
  dplyr::bind_rows(precovid_bmichange_stroke_and_TIA)  %>% 
  dplyr::bind_rows(precovid_bmichange_chronic_cardiac)  %>% 
  dplyr::bind_rows(precovid_bmichange_all_cancer)  %>% 
  dplyr::bind_rows(precovid_bmichange_smoking_status)  %>%  
  dplyr::bind_rows(precovid_bmichange_precovid_bmi_category)   
                      
         
          
##



BMI_postcovid_change_table <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "postcovid") %>%
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
                "postcovid_bmi_category")

exposure_variates <- c("sex",
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
                       "postcovid_bmi_category")

postcovid_bmi_change <- BMI_postcovid_change_table %>% 
  tabyl(sex, bmi_change_cat) %>%
  adorn_totals() %>% 
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "all", .before=1) %>% 
  dplyr::rename(group = sex) %>% 
  dplyr::filter(group == 'Total') %>%
  dplyr::mutate(group = as.character(group))

postcovid_bmichange_sex <- BMI_postcovid_change_table %>% 
  tabyl(sex, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "sex", .before=1) %>% 
  dplyr::rename (group = sex)  %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_age_group_2 <- BMI_postcovid_change_table %>% 
  tabyl(age_group_2, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "age_group_2", .before=1) %>% 
  dplyr::rename (group = age_group_2) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_region <- BMI_postcovid_change_table %>% 
  tabyl(region, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "region", .before=1) %>% 
  dplyr::rename (group = region) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_imd <- BMI_postcovid_change_table %>% 
  tabyl(imd, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "imd", .before=1) %>% 
  dplyr::rename (group = imd) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_hypertension <- BMI_postcovid_change_table %>% 
  tabyl(hypertension, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "hypertension", .before=1) %>% 
  dplyr::rename (group = hypertension) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_diabetes_t1 <- BMI_postcovid_change_table %>% 
  tabyl(diabetes_t1, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "diabetes_t1", .before=1) %>% 
  dplyr::rename (group = diabetes_t1) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_diabetes_t2 <- BMI_postcovid_change_table %>% 
  tabyl(diabetes_t2, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "diabetes_t2", .before=1) %>% 
  dplyr::rename (group = diabetes_t2) %>%
  dplyr::mutate(group = as.character(group))



postcovid_bmichange_learning_disability <- BMI_postcovid_change_table %>% 
  tabyl(learning_disability, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "learning_disability", .before=1) %>% 
  dplyr::rename (group = learning_disability) %>%
  dplyr::mutate(group = as.character(group))



postcovid_bmichange_depression <- BMI_postcovid_change_table %>% 
  tabyl(depression, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "depression", .before=1) %>% 
  dplyr::rename (group = depression) %>%
  dplyr::mutate(group = as.character(group))



postcovid_bmichange_psychosis_schiz_bipolar <- BMI_postcovid_change_table %>% 
  tabyl(psychosis_schiz_bipolar, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "psychosis_schiz_bipolar", .before=1) %>% 
  dplyr::rename (group = psychosis_schiz_bipolar) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_dementia <- BMI_postcovid_change_table %>% 
  tabyl(dementia, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "dementia", .before=1) %>% 
  dplyr::rename (group = dementia) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_COPD <- BMI_postcovid_change_table %>% 
  tabyl(COPD, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "COPD", .before=1) %>% 
  dplyr::rename (group = COPD) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_asthma <- BMI_postcovid_change_table %>% 
  tabyl(asthma, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "asthma", .before=1) %>% 
  dplyr::rename (group = asthma) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_stroke_and_TIA <- BMI_postcovid_change_table %>% 
  tabyl(stroke_and_TIA, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "stroke_and_TIA", .before=1) %>% 
  dplyr::rename (group = stroke_and_TIA) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_chronic_cardiac <- BMI_postcovid_change_table %>% 
  tabyl(chronic_cardiac, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "chronic_cardiac", .before=1) %>% 
  dplyr::rename (group = chronic_cardiac) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_all_cancer<- BMI_postcovid_change_table %>% 
  tabyl(all_cancer, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "all_cancer", .before=1) %>% 
  dplyr::rename (group = all_cancer) %>%
  dplyr::mutate(group = as.character(group))



postcovid_bmichange_smoking_status<- BMI_postcovid_change_table %>% 
  tabyl(smoking_status, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "smoking_status", .before=1) %>% 
  dplyr::rename (group = smoking_status) %>%
  dplyr::mutate(group = as.character(group))



postcovid_bmichange_eth_group_16<- BMI_postcovid_change_table %>% 
  tabyl(eth_group_16, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "eth_group_16", .before=1) %>% 
  dplyr::rename (group = eth_group_16) %>%
  dplyr::mutate(group = as.character(group))


postcovid_bmichange_postcovid_bmi_category<- BMI_postcovid_change_table %>% 
  tabyl(postcovid_bmi_category, bmi_change_cat) %>%
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>% 
  dplyr::mutate(variable = "postcovid_bmi_category", .before=1) %>% 
  dplyr::rename (group = postcovid_bmi_category) %>%
  dplyr::mutate(group = as.character(group))





postcovid_bmi_trajectories <- postcovid_bmi_change %>% 
  dplyr::bind_rows(postcovid_bmichange_age_group_2)  %>% 
  dplyr::bind_rows(postcovid_bmichange_sex) %>% 
  dplyr::bind_rows(postcovid_bmichange_eth_group_16)  %>% 
  dplyr::bind_rows(postcovid_bmichange_imd)  %>% 
  dplyr::bind_rows(postcovid_bmichange_region)  %>%
  dplyr::bind_rows(postcovid_bmichange_hypertension)  %>% 
  dplyr::bind_rows(postcovid_bmichange_diabetes_t1)  %>% 
  dplyr::bind_rows(postcovid_bmichange_diabetes_t2)  %>% 
  dplyr::bind_rows(postcovid_bmichange_learning_disability)  %>% 
  dplyr::bind_rows(postcovid_bmichange_depression)  %>% 
  dplyr::bind_rows(postcovid_bmichange_psychosis_schiz_bipolar)  %>% 
  dplyr::bind_rows(postcovid_bmichange_dementia)  %>% 
  dplyr::bind_rows(postcovid_bmichange_asthma)  %>% 
  dplyr::bind_rows(postcovid_bmichange_COPD)  %>% 
  dplyr::bind_rows(postcovid_bmichange_stroke_and_TIA)  %>% 
  dplyr::bind_rows(postcovid_bmichange_chronic_cardiac)  %>% 
  dplyr::bind_rows(postcovid_bmichange_all_cancer)  %>% 
  dplyr::bind_rows(postcovid_bmichange_smoking_status)  %>%  
  dplyr::bind_rows(postcovid_bmichange_postcovid_bmi_category)   



write_csv (postcovid_bmi_trajectories, here::here ("output/data","postcovid_bmi_trajectories.csv"))

write_csv (precovid_bmi_trajectories, here::here ("output/data","precovid_bmi_trajectories.csv"))


