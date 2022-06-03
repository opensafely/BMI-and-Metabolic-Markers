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

BMI_data <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))

postcovid_data <- BMI_data %>% 
  dplyr::filter(pandemic_stage == "postcovid")

precovid_data <- BMI_data %>% 
  dplyr::filter(pandemic_stage == "precovid")



percentages <- postcovid_data  %>% 
  tabyl(sex, bmi_change_cat) %>% 
  adorn_percentages(denominator = "col")

numbers <- postcovid_data  %>% 
  tabyl(sex, bmi_change_cat) 


check <- percentages %>% 
  bind_cols(numbers)


percentages_function <- function(data, var){
  v1 <- deparse(substitute(var))
  
 data  %>% 
    tabyl({{var}}, bmi_change_cat) %>% 
    adorn_percentages(denominator = "col") %>%
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
   dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(variable = (v1), .before=1) %>% 
   dplyr::rename(weightloss=  ">0.1 loss" ) %>%
   dplyr::rename(stable =  "-0.1 to <0.1" ) %>%
   dplyr::rename(slow_gain =  "0.1 to <0.3" ) %>%
   dplyr::rename(mod_gain =  "0.3 to <0.5" ) %>%
   dplyr::rename(rapid_gain =  "over 0.5" )
}




numbers_function <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data  %>% 
    tabyl({{var}}, bmi_change_cat) %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(variable = (v1), .before=1) %>%
    dplyr::rename(N_weightloss=  ">0.1 loss" ) %>%
    dplyr::rename(N_stable =  "-0.1 to <0.1" ) %>%
    dplyr::rename(N_slow_gain =  "0.1 to <0.3" ) %>%
    dplyr::rename(N_mod_gain =  "0.3 to <0.5" ) %>%
    dplyr::rename(N_rapid_gain =  "over 0.5" )
}

sex <- percentages_function(postcovid_data, sex)
age_group_2 <- percentages_function(postcovid_data, age_group_2)
eth_group_16 <- percentages_function(postcovid_data, eth_group_16)
imd <- percentages_function(postcovid_data, imd)
precovid_bmi_category <- percentages_function(postcovid_data, precovid_bmi_category)
region <- percentages_function(postcovid_data, region)
hypertension <- percentages_function(postcovid_data, hypertension)
diabetes_t1 <- percentages_function(postcovid_data, diabetes_t1)
diabetes_t2 <- percentages_function(postcovid_data, diabetes_t2)
chronic_cardiac <- percentages_function(postcovid_data, chronic_cardiac)
learning_disability <- percentages_function(postcovid_data, learning_disability)
depression <- percentages_function(postcovid_data, depression)
dementia <- percentages_function(postcovid_data, dementia)
psychosis_schiz_bipolar <- percentages_function(postcovid_data, psychosis_schiz_bipolar)
asthma <- percentages_function(postcovid_data, asthma)
COPD <- percentages_function(postcovid_data, COPD)
stroke_and_TIA <- percentages_function(postcovid_data, stroke_and_TIA)
smoking_status <- percentages_function(postcovid_data, smoking_status)

postcovid_percentages <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  bind_rows(smoking_status) 


sex <- numbers_function(postcovid_data, sex)
age_group_2 <- numbers_function(postcovid_data, age_group_2)
eth_group_16 <- numbers_function(postcovid_data, eth_group_16)
imd <- numbers_function(postcovid_data, imd)
postcovid_bmi_category <- numbers_function(postcovid_data, postcovid_bmi_category)
region <- numbers_function(postcovid_data, region)
hypertension <- numbers_function(postcovid_data, hypertension)
diabetes_t1 <- numbers_function(postcovid_data, diabetes_t1)
diabetes_t2 <- numbers_function(postcovid_data, diabetes_t2)
chronic_cardiac <- numbers_function(postcovid_data, chronic_cardiac)
learning_disability <- numbers_function(postcovid_data, learning_disability)
depression <- numbers_function(postcovid_data, depression)
dementia <- numbers_function(postcovid_data, dementia)
psychosis_schiz_bipolar <- numbers_function(postcovid_data, psychosis_schiz_bipolar)
asthma <- numbers_function(postcovid_data, asthma)
COPD <- numbers_function(postcovid_data, COPD)
stroke_and_TIA <- numbers_function(postcovid_data, stroke_and_TIA)
smoking_status <- numbers_function(postcovid_data, smoking_status)


postcovid_numbers <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(postcovid_bmi_category) %>%
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
  bind_rows(smoking_status) 


##########################3
### Precovid  DATA
sex <- percentages_function(precovid_data, sex)
age_group_2 <- percentages_function(precovid_data, age_group_2)
eth_group_16 <- percentages_function(precovid_data, eth_group_16)
imd <- percentages_function(precovid_data, imd)
precovid_bmi_category <- percentages_function(precovid_data, precovid_bmi_category)
region <- percentages_function(precovid_data, region)
hypertension <- percentages_function(precovid_data, hypertension)
diabetes_t1 <- percentages_function(precovid_data, diabetes_t1)
diabetes_t2 <- percentages_function(precovid_data, diabetes_t2)
chronic_cardiac <- percentages_function(precovid_data, chronic_cardiac)
learning_disability <- percentages_function(precovid_data, learning_disability)
depression <- percentages_function(precovid_data, depression)
dementia <- percentages_function(precovid_data, dementia)
psychosis_schiz_bipolar <- percentages_function(precovid_data, psychosis_schiz_bipolar)
asthma <- percentages_function(precovid_data, asthma)
COPD <- percentages_function(precovid_data, COPD)
stroke_and_TIA <- percentages_function(precovid_data, stroke_and_TIA)
smoking_status <- percentages_function(precovid_data, smoking_status)


precovid_percentages <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  bind_rows(smoking_status) 



## Precovid numbers in each group
sex <- numbers_function(precovid_data, sex)
age_group_2 <- numbers_function(precovid_data, age_group_2)
eth_group_16 <- numbers_function(precovid_data, eth_group_16)
imd <- numbers_function(precovid_data, imd)
precovid_bmi_category <- numbers_function(precovid_data, precovid_bmi_category)
region <- numbers_function(precovid_data, region)
hypertension <- numbers_function(precovid_data, hypertension)
diabetes_t1 <- numbers_function(precovid_data, diabetes_t1)
diabetes_t2 <- numbers_function(precovid_data, diabetes_t2)
chronic_cardiac <- numbers_function(precovid_data, chronic_cardiac)
learning_disability <- numbers_function(precovid_data, learning_disability)
depression <- numbers_function(precovid_data, depression)
dementia <- numbers_function(precovid_data, dementia)
psychosis_schiz_bipolar <- numbers_function(precovid_data, psychosis_schiz_bipolar)
asthma <- numbers_function(precovid_data, asthma)
COPD <- numbers_function(precovid_data, COPD)
stroke_and_TIA <- numbers_function(precovid_data, stroke_and_TIA)
smoking_status <- numbers_function(precovid_data, smoking_status)


precovid_numbers <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  bind_rows(smoking_status) 

### ROund data

precovid_numbers <- precovid_numbers %>% 
  dplyr::mutate(N_weightloss = plyr::round_any(precovid_numbers$N_weightloss, 5)) %>% 
  dplyr::mutate(N_stable = plyr::round_any(precovid_numbers$N_stable, 5)) %>%
  dplyr::mutate(N_slow_gain = plyr::round_any(precovid_numbers$N_slow_gain, 5)) %>%
  dplyr::mutate(N_mod_gain = plyr::round_any(precovid_numbers$N_mod_gain, 5)) %>%
  dplyr::mutate(N_rapid_gain = plyr::round_any(precovid_numbers$N_rapid_gain, 5))


postcovid_numbers <- postcovid_numbers %>% 
  dplyr::mutate(N_weightloss = plyr::round_any(postcovid_numbers$N_weightloss, 5)) %>% 
  dplyr::mutate(N_stable = plyr::round_any(postcovid_numbers$N_stable, 5)) %>%
  dplyr::mutate(N_slow_gain = plyr::round_any(postcovid_numbers$N_slow_gain, 5)) %>%
  dplyr::mutate(N_mod_gain = plyr::round_any(postcovid_numbers$N_mod_gain, 5)) %>%
  dplyr::mutate(N_rapid_gain = plyr::round_any(postcovid_numbers$N_rapid_gain, 5))


precovid <- precovid_numbers %>% 
  bind_cols(precovid_percentages) %>% 
  dplyr::select(-c("variable...8", "group...9"))


postcovid <- postcovid_numbers %>% 
  bind_cols(postcovid_percentages) %>% 
  dplyr::select(-c("variable...8", "group...9"))

write_csv (precovid, here::here ("output/data","precovid_bmichange_description.csv"))
write_csv (postcovid, here::here ("output/data","postcovid_bmichange_description.csv"))
