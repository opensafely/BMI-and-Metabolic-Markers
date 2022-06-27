

## Author: M Samuel
## This script calculate the proportion of patients who had precovid BMI trajectory data who also had post covid data
## This will identify those with missing post covid data

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

BMI_data <- read_feather (here::here ("output/data", "BMI_trajectory_models_data.feather"))



BMI_data <- BMI_data %>% 
  dplyr::select(
    sex, 
    age_group_2, 
    eth_group_16, 
    imd, 
    region, 
    hypertension, 
    diabetes_t2, 
    diabetes_t1, 
    chronic_cardiac,
    learning_disability, 
    psychosis_schiz_bipolar, 
    depression, 
    asthma, 
    COPD, 
    stroke_and_TIA, 
    dementia, 
    all_cancer,
    smoking_status, 
    precovid_bmi_category, 
    precovid_change, 
    trajectory_change
  )





## categorise the precovid rate of change
BMI_data <- BMI_data %>% 
  dplyr::mutate ( precovid_change_cat = cut(
    precovid_change, 
    breaks = c(-999,-0.1, 0.1, 0.3, 0.5, 999), 
    labels = c(">0.1 loss", "-0.1 to <0.1", "0.1 to <0.3", "0.3 to <0.5", "over 0.5"),
    include.lowest = TRUE))


## create a variable to show if trajectory change was increasing post covid vs stable/decreasing
BMI_data <- BMI_data %>% 
  dplyr::mutate(positive_traj_change = case_when(
    trajectory_change < 0 ~ FALSE, 
    trajectory_change == 0 ~ FALSE, 
    trajectory_change > 0 ~ TRUE
  ))



##*** Change to code.  FILTER OUT UNDERWEIGHT AND THOSE WITH CANCER ## Data set for lowbmi_excluded analysis
lowbmi_exc <- BMI_data %>% 
  dplyr::filter(all_cancer == FALSE)

lowbmi_exc <- lowbmi_exc %>% 
  dplyr::filter(precovid_bmi_category != "underweight")







## create a function for proportions increasing
prop_function <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}, positive_traj_change) %>% 
    dplyr::rename(stable_decrease = 'FALSE')  %>% 
    dplyr::rename(increase = 'TRUE')  %>% 
    dplyr::mutate(total = stable_decrease + increase) %>% 
    dplyr::mutate(prop_increase = increase/total) %>% 
    dplyr::mutate(se_prop_increase = sqrt(((prop_increase * (1-prop_increase))/total))) %>% 
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) 
}


sex <- prop_function(BMI_data, sex)
age_group_2 <- prop_function(BMI_data, age_group_2)
eth_group_16 <- prop_function(BMI_data, eth_group_16)
imd <- prop_function(BMI_data, imd)
region <- prop_function(BMI_data, region)
hypertension <- prop_function(BMI_data,  hypertension)
diabetes_t1 <- prop_function(BMI_data,  diabetes_t1)
diabetes_t2 <- prop_function(BMI_data,  diabetes_t2)
chronic_cardiac <- prop_function(BMI_data,  chronic_cardiac)
learning_disability <- prop_function(BMI_data,  learning_disability)
depression <- prop_function(BMI_data,  depression)
dementia <- prop_function(BMI_data,  dementia)
psychosis_schiz_bipolar <- prop_function(BMI_data,  psychosis_schiz_bipolar)
asthma <- prop_function(BMI_data,  asthma)
COPD <- prop_function(BMI_data,  COPD)
stroke_and_TIA <- prop_function(BMI_data,  stroke_and_TIA)
smoking_status <- prop_function(BMI_data, smoking_status)
precovid_change_cat <- prop_function(BMI_data, precovid_change_cat)


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
  bind_rows(precovid_change_cat)

complete <- complete %>% 
  dplyr::mutate(stable_decrease = plyr::round_any(complete$stable_decrease, 5)) %>%
  dplyr::mutate(increase = plyr::round_any(complete$increase, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete$total, 5)) %>%
  dplyr::mutate(population = "all", .before=1)


## repeat on population with cancer and lowbmi excluded
sex <- prop_function(lowbmi_exc , sex)
age_group_2 <- prop_function(lowbmi_exc , age_group_2)
eth_group_16 <- prop_function(lowbmi_exc , eth_group_16)
imd <- prop_function(lowbmi_exc , imd)
region <- prop_function(lowbmi_exc , region)
hypertension <- prop_function(lowbmi_exc ,  hypertension)
diabetes_t1 <- prop_function(lowbmi_exc ,  diabetes_t1)
diabetes_t2 <- prop_function(lowbmi_exc ,  diabetes_t2)
chronic_cardiac <- prop_function(lowbmi_exc ,  chronic_cardiac)
learning_disability <- prop_function(lowbmi_exc ,  learning_disability)
depression <- prop_function(lowbmi_exc ,  depression)
dementia <- prop_function(lowbmi_exc ,  dementia)
psychosis_schiz_bipolar <- prop_function(lowbmi_exc ,  psychosis_schiz_bipolar)
asthma <- prop_function(lowbmi_exc ,  asthma)
COPD <- prop_function(lowbmi_exc ,  COPD)
stroke_and_TIA <- prop_function(lowbmi_exc ,  stroke_and_TIA)
smoking_status <- prop_function(lowbmi_exc , smoking_status)
precovid_change_cat <- prop_function(lowbmi_exc , precovid_change_cat)



complete_2 <- sex %>% 
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
  bind_rows(precovid_change_cat)

complete_2 <- complete_2 %>% 
  dplyr::mutate(stable_decrease = plyr::round_any(complete_2$stable_decrease, 5)) %>%
  dplyr::mutate(increase = plyr::round_any(complete_2$increase, 5)) %>%
  dplyr::mutate(total = plyr::round_any(complete_2$total, 5)) %>%
  dplyr::mutate(population = "lowbmi_exc", .before=1)


complete <- complete %>% 
  bind_rows(complete_2)

write_csv (complete, here::here ("output/data","bmi_trajectory_positive_change_proportions.csv"))



