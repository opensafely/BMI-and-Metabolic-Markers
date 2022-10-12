## M Samuel 
## 5th May 2022
## This script checks factors associated with rapid weight gain if patients with cancer and a low BMI prepandemic are removed


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
                "bmi_change_cat", 
                "precovid_bmi_category", 
                "pandemic_stage")

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(rapid_bmi_change = case_when(
    bmi_change_cat == 'over 0.5' ~ 1, 
    bmi_change_cat != 'over 0.5' ~ 0, 
  ))

BMI_trajectories$precovid_bmi_category <- factor(BMI_trajectories$precovid_bmi_category, levels = c("healthy","overweight", "obese", "underweight"))



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
                      "smoking_status", 
                      "ethnic_no_miss",         
                      "eth_group_16",           
                      "precovid_bmi_category")

explanatory_vars_3 <- c("sex",
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
                      "smoking_status", 
                      "ethnic_no_miss",         
                      "eth_group_16",           
                      "precovid_bmi_category", 
                      "pandemic_stage")



##*** Change to code.  FILTER OUT UNDERWEIGHT AND THOSE WITH CANCER
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(all_cancer == FALSE)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(precovid_bmi_category != "underweight")

BMI_trajectories %>% 
  tabyl(all_cancer)

BMI_trajectories %>% 
  tabyl(precovid_bmi_category)

## *** Change to code complete


## population composition

# function to calculate
population_demog_function2 <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data %>%
    tabyl({{var}}, rapid_bmi_change) %>% 
    dplyr::mutate(N_total = c(not_rapid) + c(rapid)) %>% 
    dplyr::mutate(percent_rapid = rapid/N_total*100) %>% 
    dplyr::mutate(rapid = plyr::round_any(rapid, 5)) %>% 
    dplyr::mutate(N_total = plyr::round_any(N_total, 5)) %>% 
    dplyr::mutate(not_rapid = plyr::round_any(not_rapid, 5)) %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(variable = (v1))
}

## PRECOVID

precovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "precovid")

## age sex imd adjusted model


imd <- lm(rapid_bmi_change ~ sex +  age_group_2 +  imd, data=precovid_change) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)

precovid_imd <- imd %>%
    dplyr::mutate(stage = "precovid", .before=1)


## precovid population counts 

precovid_change$rapid_bmi_change[precovid_change$rapid_bmi_change==0] <- "not_rapid"
precovid_change$rapid_bmi_change[precovid_change$rapid_bmi_change==1] <- "rapid"


precovid_change %>% 
  tabyl(sex, rapid_bmi_change) %>% 
  dplyr::mutate(N_total = c(not_rapid) + c(rapid)) %>% 
  dplyr::mutate(percent_rapid = rapid/N_total*100) %>% 
  dplyr::mutate(rapid = plyr::round_any(rapid, 5)) %>% 
  dplyr::mutate(N_total = plyr::round_any(N_total, 5)) 






diabetes_t2 <- population_demog_function2(precovid_change, diabetes_t2) %>% 
  dplyr::mutate(group = as.factor(group))

age <- population_demog_function2(precovid_change, age_group_2) %>% 
  dplyr::mutate(group = as.factor(group))

sex <- population_demog_function2(precovid_change, sex) %>% 
  dplyr::mutate(group = as.factor(group))

imd <- population_demog_function2(precovid_change, imd) %>% 
  dplyr::mutate(group = as.factor(group))



precovid_demog <- bind_rows (diabetes_t2, 
                age,
                sex, 
                imd) %>%
  dplyr::mutate(stage = "precovid", .before=1)






## postcovid

postcovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "postcovid")


## age sex imd adjusted model


imd <- lm(rapid_bmi_change ~ sex +  age_group_2 +  imd, data=postcovid_change) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)

postcovid_imd <- imd %>%
    dplyr::mutate(stage = "postcovid", .before=1)

postcovid_change$rapid_bmi_change[postcovid_change$rapid_bmi_change==0] <- "not_rapid"
postcovid_change$rapid_bmi_change[postcovid_change$rapid_bmi_change==1] <- "rapid"


postcovid_change %>% 
  tabyl(sex, rapid_bmi_change) %>% 
  dplyr::mutate(N_total = c(not_rapid) + c(rapid)) %>% 
  dplyr::mutate(percent_rapid = rapid/N_total*100) %>% 
  dplyr::mutate(rapid = plyr::round_any(rapid, 5)) %>% 
  dplyr::mutate(N_total = plyr::round_any(N_total, 5)) 


diabetes_t2 <- population_demog_function2(postcovid_change, diabetes_t2) %>% 
  dplyr::mutate(group = as.factor(group))

age <- population_demog_function2(postcovid_change, age_group_2) %>% 
  dplyr::mutate(group = as.factor(group))

sex <- population_demog_function2(postcovid_change, sex) %>% 
  dplyr::mutate(group = as.factor(group))

imd <- population_demog_function2(postcovid_change, imd) %>% 
  dplyr::mutate(group = as.factor(group))


postcovid_demog <- bind_rows (diabetes_t2,
                            age, 
                            sex, 
                            imd) %>% 
  dplyr::mutate(stage = "postcovid", .before=1) 




## demographic of total data set




demog <- precovid_demog %>% 
  bind_rows(postcovid_demog) %>% 
  dplyr::select(stage,variable,group,not_rapid,rapid,N_total,percent_rapid) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 4))


models <- precovid_imd %>% 
    bind_rows (postcovid_imd)

### Write outputs



write_csv (models, here::here ("output/data","rapid_bmi_change_cancerandlowbmi_removed_IMD_agesex.csv"))
write_csv (demog, here::here ("output/data","rapid_bmi_change_cancerandlowbmi_removed_T2DM_popcharac.csv"))