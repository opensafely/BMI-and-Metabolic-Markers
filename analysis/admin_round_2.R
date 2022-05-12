## This script rounds and redacts tables to ensure no counts <5 and all values rounded to 5
## 12th May
## Author: M Samuel


## Add libraries
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
library(stringr)

data2 <- read_feather (here::here ("output/data", "BMI_trajectories_final_demog.feather"))

       
all_data <- data2 %>% 
  tabyl(complete_bmi_data) %>%
  dplyr::filter(complete_bmi_data == 'complete') %>% 
  dplyr::mutate(group = 'all', .before=1) %>% 
  dplyr::mutate(variable = 'all', .before=1) %>% 
  dplyr::rename(complete=n) %>% 
  dplyr::select(-('complete_bmi_data')) %>% 
  dplyr::mutate(percent = (percent*100))


  
  
       
complete_sex <- data2 %>% 
  tabyl(sex, complete_bmi_data) %>% 
  dplyr::mutate(variable="sex", .before=1) %>% 
  dplyr::rename(group = sex) %>% 
  dplyr::mutate(group = as.character(group))


complete_age_group <- data2 %>% 
  tabyl(age_group, complete_bmi_data) %>% 
  dplyr::mutate(variable="age_group", .before=1) %>% 
  dplyr::rename(group = age_group)%>% 
  dplyr::mutate(group = as.character(group))


complete_age_group_2 <- data2 %>% 
  tabyl(age_group_2, complete_bmi_data) %>% 
  dplyr::mutate(variable="age_group_2", .before=1) %>% 
  dplyr::rename(group = age_group_2)%>% 
  dplyr::mutate(group = as.character(group))

complete_region <- data2 %>% 
  tabyl(region, complete_bmi_data) %>% 
  dplyr::mutate(variable="region", .before=1) %>% 
  dplyr::rename(group = region)%>% 
  dplyr::mutate(group = as.character(group))


complete_imd <- data2 %>% 
  tabyl(imd, complete_bmi_data) %>% 
  dplyr::mutate(variable="imd", .before=1) %>% 
  dplyr::rename(group = imd)%>% 
  dplyr::mutate(group = as.character(group))

complete_learning_disability <- data2 %>% 
  tabyl(learning_disability, complete_bmi_data) %>% 
  dplyr::mutate(variable="learning_disability", .before=1) %>% 
  dplyr::rename(group = learning_disability)%>% 
  dplyr::mutate(group = as.character(group))

complete_dementia <- data2 %>% 
  tabyl(dementia, complete_bmi_data) %>% 
  dplyr::mutate(variable="dementia", .before=1) %>% 
  dplyr::rename(group = dementia)%>% 
  dplyr::mutate(group = as.character(group))

complete_depression <- data2 %>% 
  tabyl(depression, complete_bmi_data) %>% 
  dplyr::mutate(variable="depression", .before=1) %>% 
  dplyr::rename(group = depression)%>% 
  dplyr::mutate(group = as.character(group))

complete_psychosis_schiz_bipolar <- data2 %>% 
  tabyl(psychosis_schiz_bipolar, complete_bmi_data) %>% 
  dplyr::mutate(variable="psychosis_schiz_bipolar", .before=1) %>% 
  dplyr::rename(group = psychosis_schiz_bipolar)%>% 
  dplyr::mutate(group = as.character(group))

complete_diabetes_t1 <- data2 %>% 
  tabyl(diabetes_t1, complete_bmi_data) %>% 
  dplyr::mutate(variable="diabetes_t1", .before=1) %>% 
  dplyr::rename(group = diabetes_t1)%>% 
  dplyr::mutate(group = as.character(group))

complete_diabetes_t2 <- data2 %>% 
  tabyl(diabetes_t2, complete_bmi_data) %>% 
  dplyr::mutate(variable="diabetes_t2", .before=1) %>% 
  dplyr::rename(group = diabetes_t2)%>% 
  dplyr::mutate(group = as.character(group))

complete_asthma <- data2 %>% 
  tabyl(asthma, complete_bmi_data) %>% 
  dplyr::mutate(variable="asthma", .before=1) %>% 
  dplyr::rename(group = asthma)%>% 
  dplyr::mutate(group = as.character(group))

complete_COPD <- data2 %>% 
  tabyl(COPD, complete_bmi_data) %>% 
  dplyr::mutate(variable="COPD", .before=1) %>% 
  dplyr::rename(group = COPD)%>% 
  dplyr::mutate(group = as.character(group))

complete_stroke_and_TIA <- data2 %>% 
  tabyl(stroke_and_TIA, complete_bmi_data) %>% 
  dplyr::mutate(variable="stroke_and_TIA", .before=1) %>% 
  dplyr::rename(group = stroke_and_TIA)%>% 
  dplyr::mutate(group = as.character(group))

complete_chronic_cardiac <- data2 %>% 
  tabyl(chronic_cardiac, complete_bmi_data) %>% 
  dplyr::mutate(variable="chronic_cardiac", .before=1) %>% 
  dplyr::rename(group = chronic_cardiac)%>% 
  dplyr::mutate(group = as.character(group))

complete_hypertension <- data2 %>% 
  tabyl(hypertension, complete_bmi_data) %>% 
  dplyr::mutate(variable="hypertension", .before=1) %>% 
  dplyr::rename(group = hypertension)%>% 
  dplyr::mutate(group = as.character(group))

complete_all_cancer <- data2 %>% 
  tabyl(all_cancer, complete_bmi_data) %>% 
  dplyr::mutate(variable="all_cancer", .before=1) %>% 
  dplyr::rename(group = all_cancer)%>% 
  dplyr::mutate(group = as.character(group))

complete_ethnic_no_miss <- data2 %>% 
  tabyl(ethnic_no_miss, complete_bmi_data) %>% 
  dplyr::mutate(variable="ethnic_no_miss", .before=1) %>% 
  dplyr::rename(group = ethnic_no_miss)%>% 
  dplyr::mutate(group = as.character(group))

complete_eth_group_16 <- data2 %>% 
  tabyl(eth_group_16, complete_bmi_data) %>% 
  dplyr::mutate(variable="eth_group_16", .before=1) %>% 
  dplyr::rename(group = eth_group_16)%>% 
  dplyr::mutate(group = as.character(group))



data <- complete_sex %>% 
  bind_rows(complete_age_group) %>% 
  bind_rows(complete_age_group_2) %>%
  bind_rows(complete_region) %>%
  bind_rows(complete_imd) %>%
  bind_rows(complete_ethnic_no_miss) %>%
  bind_rows(complete_eth_group_16) %>%
  bind_rows(complete_hypertension) %>%
  bind_rows(complete_diabetes_t2) %>%
  bind_rows(complete_diabetes_t1) %>%
  bind_rows(complete_learning_disability) %>%
  bind_rows(complete_dementia) %>%
  bind_rows(complete_depression) %>%
  bind_rows(complete_psychosis_schiz_bipolar) %>%
  bind_rows(complete_asthma) %>%
  bind_rows(complete_COPD) %>%
  bind_rows(complete_stroke_and_TIA) %>%
  bind_rows(complete_chronic_cardiac) %>%
  bind_rows(complete_all_cancer) %>% 
  dplyr::mutate(percent = (complete/(complete+incomplete)*100)) %>% 
  dplyr::select(-("incomplete")) 

data <- all_data %>% 
  bind_rows(data)  %>%
  dplyr::mutate((across(where(is.numeric), round, 2)))

data <- as.data.frame(data)
  


data <- data %>% 
  dplyr::mutate(complete = plyr::round_any(data$complete, 5))

write.csv (data, here::here ("output/data", "complete_bmi_trajectory_data.csv"))

