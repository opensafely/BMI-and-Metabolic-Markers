## This script looks at the average weight change in the prepandemic periods by exposure groups
## Author: M Samuel
## Date: 4th May 2022



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

BMI_DT <- read_feather (here::here ("output/data", "BMI_trajectory_models_data.feather"))




## filter out missing
BMI_hypertension_DT <- BMI_DT %>%
dplyr::filter(complete_bmi_data == "complete")  %>% 
dplyr::filter(hypertension == TRUE)

BMI_hypertension_DT %>%
tabyl(age_group_2, sex)

BMI_hypertension_DT_summ <- BMI_hypertension_DT %>% 
  dplyr::summarise( n = n(), mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::mutate(group = "all", .before=1) %>% 
  dplyr::mutate(variable = "all", .before =1)
  




## create summaries to assess for change in mean in sd per subgroup
 #age_group_2
BMI_hypertension_summ_age_group_2 <- BMI_hypertension_DT %>%
  dplyr::group_by(age_group_2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = age_group_2) %>% 
  dplyr::mutate(variable = 'age_group_2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))
 


# sex
BMI_hypertension_summ_sex <- BMI_hypertension_DT%>%
  dplyr::group_by(sex)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = sex) %>% 
  dplyr::mutate(variable = 'sex', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# region
BMI_hypertension_summ_region <- BMI_hypertension_DT%>%
  dplyr::group_by(region)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = region) %>% 
  dplyr::mutate(variable = 'region', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# imd
BMI_hypertension_summ_imd <- BMI_hypertension_DT%>%
  dplyr::group_by(imd)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = imd) %>% 
  dplyr::mutate(variable = 'imd', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))




# diabetes_t1
BMI_hypertension_summ_diabetes_t1 <- BMI_hypertension_DT%>%
  dplyr::group_by(diabetes_t1)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t1) %>% 
  dplyr::mutate(variable = 'diabetes_t1', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# diabetes_t2
BMI_hypertension_summ_diabetes_t2 <- BMI_hypertension_DT%>%
  dplyr::group_by(diabetes_t2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t2) %>% 
  dplyr::mutate(variable = 'diabetes_t2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# learning_disability
BMI_hypertension_summ_learning_disability <- BMI_hypertension_DT%>%
  dplyr::group_by(learning_disability)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = learning_disability) %>% 
  dplyr::mutate(variable = 'learning_disability', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# depression
BMI_hypertension_summ_depression <- BMI_hypertension_DT%>%
  dplyr::group_by(depression)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = depression) %>% 
  dplyr::mutate(variable = 'depression', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# psychosis_schiz_bipolar
BMI_hypertension_summ_psychosis_schiz_bipolar <- BMI_hypertension_DT%>%
  dplyr::group_by(psychosis_schiz_bipolar)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = psychosis_schiz_bipolar) %>% 
  dplyr::mutate(variable = 'psychosis_schiz_bipolar', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# dementia
BMI_hypertension_summ_dementia <- BMI_hypertension_DT%>%
  dplyr::group_by(dementia)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = dementia) %>% 
  dplyr::mutate(variable = 'dementia', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))



# asthma
BMI_hypertension_summ_asthma <- BMI_hypertension_DT%>%
  dplyr::group_by(asthma)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = asthma) %>% 
  dplyr::mutate(variable = 'asthma', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# COPD
BMI_hypertension_summ_COPD <- BMI_hypertension_DT%>%
  dplyr::group_by(COPD)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = COPD) %>% 
  dplyr::mutate(variable = 'COPD', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# stroke_and_TIA
BMI_hypertension_summ_stroke_and_TIA <- BMI_hypertension_DT%>%
  dplyr::group_by(stroke_and_TIA)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = stroke_and_TIA) %>% 
  dplyr::mutate(variable = 'stroke_and_TIA', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# all_cancer
BMI_hypertension_summ_all_cancer <- BMI_hypertension_DT%>%
  dplyr::group_by(all_cancer)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = all_cancer) %>% 
  dplyr::mutate(variable = 'all_cancer', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# smoking_status
BMI_hypertension_summ_smoking_status <- BMI_hypertension_DT%>%
  dplyr::group_by(smoking_status)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = smoking_status) %>% 
  dplyr::mutate(variable = 'smoking_status', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# ethnic_no_miss
BMI_hypertension_summ_ethnic_no_miss <- BMI_hypertension_DT%>%
  dplyr::group_by(ethnic_no_miss)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = ethnic_no_miss) %>% 
  dplyr::mutate(variable = 'ethnic_no_miss', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# eth_group_16
BMI_hypertension_summ_eth_group_16 <- BMI_hypertension_DT%>%
  dplyr::group_by(eth_group_16)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = eth_group_16) %>% 
  dplyr::mutate(variable = 'eth_group_16', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# precovid_BMI_hypertension_category
BMI_hypertension_summ_precovid_BMI_category <- BMI_hypertension_DT%>%
   dplyr::group_by(precovid_bmi_category)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = precovid_bmi_category) %>% 
  dplyr::mutate(variable = 'precovid_bmi_category', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# chronic_cardiac
BMI_hypertension_summ_chronic_cardiac <- BMI_hypertension_DT%>%
  dplyr::group_by(chronic_cardiac)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = chronic_cardiac) %>% 
  dplyr::mutate(variable = 'chronic_cardiac', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

BMI_hypertension_traj_change_summary <- BMI_hypertension_DT_summ %>% 
  bind_rows(BMI_hypertension_summ_age_group_2) %>% 
  bind_rows(BMI_hypertension_summ_sex) %>% 
  bind_rows(BMI_hypertension_summ_ethnic_no_miss) %>% 
  bind_rows(BMI_hypertension_summ_eth_group_16) %>% 
  bind_rows(BMI_hypertension_summ_imd) %>%
  bind_rows(BMI_hypertension_summ_region) %>%
  bind_rows(BMI_hypertension_summ_precovid_BMI_category) %>% 
  bind_rows(BMI_hypertension_summ_diabetes_t2) %>% 
  bind_rows(BMI_hypertension_summ_diabetes_t1) %>% 
  bind_rows(BMI_hypertension_summ_learning_disability) %>%
  bind_rows(BMI_hypertension_summ_depression) %>% 
  bind_rows(BMI_hypertension_summ_psychosis_schiz_bipolar) %>% 
  bind_rows(BMI_hypertension_summ_chronic_cardiac) %>% 
  bind_rows(BMI_hypertension_summ_COPD) %>%
  bind_rows(BMI_hypertension_summ_asthma)%>%
  bind_rows(BMI_hypertension_summ_dementia) %>% 
  bind_rows(BMI_hypertension_summ_all_cancer) %>% 
  bind_rows(BMI_hypertension_summ_stroke_and_TIA) %>% 
  bind_rows(BMI_hypertension_summ_smoking_status) %>% 
  dplyr::mutate(across(where(is.numeric), round, digits=2))

write.csv (BMI_hypertension_traj_change_summary, here::here ("output/data","mean_bmi_traj_change_hypertension.csv"))
