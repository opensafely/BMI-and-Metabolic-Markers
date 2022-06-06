## This script looks at the average weight change by exposure groups, 
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




## filter out missing + depression
BMI_depression_DT <- BMI_DT %>%
dplyr::filter(complete_bmi_data == "complete")  %>% 
dplyr::filter(depression == TRUE)

BMI_depression_DT %>%
tabyl(age_group_2, sex)

BMI_depression_DT_summ <- BMI_depression_DT %>% 
  dplyr::summarise( n = n(), mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::mutate(group = "all", .before=1) %>% 
  dplyr::mutate(variable = "all", .before =1)
  




## create summaries to assess for change in mean in sd per subgroup
 #age_group_2
BMI_depression_summ_age_group_2 <- BMI_depression_DT %>%
  dplyr::group_by(age_group_2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = age_group_2) %>% 
  dplyr::mutate(variable = 'age_group_2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))
 


# sex
BMI_depression_summ_sex <- BMI_depression_DT%>%
  dplyr::group_by(sex)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = sex) %>% 
  dplyr::mutate(variable = 'sex', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# region
BMI_depression_summ_region <- BMI_depression_DT%>%
  dplyr::group_by(region)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = region) %>% 
  dplyr::mutate(variable = 'region', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# imd
BMI_depression_summ_imd <- BMI_depression_DT%>%
  dplyr::group_by(imd)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = imd) %>% 
  dplyr::mutate(variable = 'imd', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))




# diabetes_t1
BMI_depression_summ_diabetes_t1 <- BMI_depression_DT%>%
  dplyr::group_by(diabetes_t1)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t1) %>% 
  dplyr::mutate(variable = 'diabetes_t1', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# diabetes_t2
BMI_depression_summ_diabetes_t2 <- BMI_depression_DT%>%
  dplyr::group_by(diabetes_t2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t2) %>% 
  dplyr::mutate(variable = 'diabetes_t2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# learning_disability
BMI_depression_summ_learning_disability <- BMI_depression_DT%>%
  dplyr::group_by(learning_disability)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = learning_disability) %>% 
  dplyr::mutate(variable = 'learning_disability', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# hypertension
BMI_depression_summ_hypertension <- BMI_depression_DT%>%
  dplyr::group_by(hypertension)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = hypertension) %>% 
  dplyr::mutate(variable = 'hypertension', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# psychosis_schiz_bipolar
BMI_depression_summ_psychosis_schiz_bipolar <- BMI_depression_DT%>%
  dplyr::group_by(psychosis_schiz_bipolar)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = psychosis_schiz_bipolar) %>% 
  dplyr::mutate(variable = 'psychosis_schiz_bipolar', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# dementia
BMI_depression_summ_dementia <- BMI_depression_DT%>%
  dplyr::group_by(dementia)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = dementia) %>% 
  dplyr::mutate(variable = 'dementia', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))



# asthma
BMI_depression_summ_asthma <- BMI_depression_DT%>%
  dplyr::group_by(asthma)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = asthma) %>% 
  dplyr::mutate(variable = 'asthma', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# COPD
BMI_depression_summ_COPD <- BMI_depression_DT%>%
  dplyr::group_by(COPD)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = COPD) %>% 
  dplyr::mutate(variable = 'COPD', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# stroke_and_TIA
BMI_depression_summ_stroke_and_TIA <- BMI_depression_DT%>%
  dplyr::group_by(stroke_and_TIA)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = stroke_and_TIA) %>% 
  dplyr::mutate(variable = 'stroke_and_TIA', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# all_cancer
BMI_depression_summ_all_cancer <- BMI_depression_DT%>%
  dplyr::group_by(all_cancer)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = all_cancer) %>% 
  dplyr::mutate(variable = 'all_cancer', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# smoking_status
BMI_depression_summ_smoking_status <- BMI_depression_DT%>%
  dplyr::group_by(smoking_status)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = smoking_status) %>% 
  dplyr::mutate(variable = 'smoking_status', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# ethnic_no_miss
BMI_depression_summ_ethnic_no_miss <- BMI_depression_DT%>%
  dplyr::group_by(ethnic_no_miss)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = ethnic_no_miss) %>% 
  dplyr::mutate(variable = 'ethnic_no_miss', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# eth_group_16
BMI_depression_summ_eth_group_16 <- BMI_depression_DT%>%
  dplyr::group_by(eth_group_16)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = eth_group_16) %>% 
  dplyr::mutate(variable = 'eth_group_16', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# precovid_BMI_category
BMI_depression_summ_precovid_BMI_category <- BMI_depression_DT%>%
   dplyr::group_by(precovid_bmi_category)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = precovid_bmi_category) %>% 
  dplyr::mutate(variable = 'precovid_bmi_category', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# chronic_cardiac
BMI_depression_summ_chronic_cardiac <- BMI_depression_DT%>%
  dplyr::group_by(chronic_cardiac)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = chronic_cardiac) %>% 
  dplyr::mutate(variable = 'chronic_cardiac', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

BMI_depression_traj_change_summary <- BMI_depression_DT_summ %>% 
  bind_rows(BMI_depression_summ_age_group_2) %>% 
  bind_rows(BMI_depression_summ_sex) %>% 
  bind_rows(BMI_depression_summ_ethnic_no_miss) %>% 
  bind_rows(BMI_depression_summ_eth_group_16) %>% 
  bind_rows(BMI_depression_summ_imd) %>%
  bind_rows(BMI_depression_summ_region) %>%
  bind_rows(BMI_depression_summ_precovid_BMI_category) %>% 
  bind_rows(BMI_depression_summ_diabetes_t2) %>% 
  bind_rows(BMI_depression_summ_diabetes_t1) %>% 
  bind_rows(BMI_depression_summ_hypertension) %>% 
  bind_rows(BMI_depression_summ_learning_disability) %>%
  bind_rows(BMI_depression_summ_psychosis_schiz_bipolar) %>% 
  bind_rows(BMI_depression_summ_chronic_cardiac) %>% 
  bind_rows(BMI_depression_summ_COPD) %>%
  bind_rows(BMI_depression_summ_asthma)%>%
  bind_rows(BMI_depression_summ_dementia) %>% 
  bind_rows(BMI_depression_summ_all_cancer) %>% 
  bind_rows(BMI_depression_summ_stroke_and_TIA) %>% 
  bind_rows(BMI_depression_summ_smoking_status) %>% 
  dplyr::mutate(across(where(is.numeric), round, digits=2)) %>%
  dplyr::mutate(n = plyr::round_any(n, 5)) 


### LEARNING DISABILITY
## filter out missing + learning_disability
BMI_learning_disability_DT <- BMI_DT %>%
dplyr::filter(complete_bmi_data == "complete")  %>% 
dplyr::filter(learning_disability == TRUE)

BMI_learning_disability_DT %>%
tabyl(age_group_2, sex)

BMI_learning_disability_DT_summ <- BMI_learning_disability_DT %>% 
  dplyr::summarise( n = n(), mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::mutate(group = "all", .before=1) %>% 
  dplyr::mutate(variable = "all", .before =1)
  




## create summaries to assess for change in mean in sd per subgroup
 #age_group_2
BMI_learning_disability_summ_age_group_2 <- BMI_learning_disability_DT %>%
  dplyr::group_by(age_group_2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = age_group_2) %>% 
  dplyr::mutate(variable = 'age_group_2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))
 


# sex
BMI_learning_disability_summ_sex <- BMI_learning_disability_DT%>%
  dplyr::group_by(sex)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = sex) %>% 
  dplyr::mutate(variable = 'sex', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# region
BMI_learning_disability_summ_region <- BMI_learning_disability_DT%>%
  dplyr::group_by(region)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = region) %>% 
  dplyr::mutate(variable = 'region', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# imd
BMI_learning_disability_summ_imd <- BMI_learning_disability_DT%>%
  dplyr::group_by(imd)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = imd) %>% 
  dplyr::mutate(variable = 'imd', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))




# diabetes_t1
BMI_learning_disability_summ_diabetes_t1 <- BMI_learning_disability_DT%>%
  dplyr::group_by(diabetes_t1)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t1) %>% 
  dplyr::mutate(variable = 'diabetes_t1', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# diabetes_t2
BMI_learning_disability_summ_diabetes_t2 <- BMI_learning_disability_DT%>%
  dplyr::group_by(diabetes_t2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t2) %>% 
  dplyr::mutate(variable = 'diabetes_t2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# hypertension
BMI_learning_disability_summ_hypertension <- BMI_learning_disability_DT%>%
  dplyr::group_by(hypertension)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = hypertension) %>% 
  dplyr::mutate(variable = 'hypertension', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# depression
BMI_learning_disability_summ_depression <- BMI_learning_disability_DT%>%
  dplyr::group_by(depression)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = depression) %>% 
  dplyr::mutate(variable = 'depression', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# psychosis_schiz_bipolar
BMI_learning_disability_summ_psychosis_schiz_bipolar <- BMI_learning_disability_DT%>%
  dplyr::group_by(psychosis_schiz_bipolar)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = psychosis_schiz_bipolar) %>% 
  dplyr::mutate(variable = 'psychosis_schiz_bipolar', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# dementia
BMI_learning_disability_summ_dementia <- BMI_learning_disability_DT%>%
  dplyr::group_by(dementia)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = dementia) %>% 
  dplyr::mutate(variable = 'dementia', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))



# asthma
BMI_learning_disability_summ_asthma <- BMI_learning_disability_DT%>%
  dplyr::group_by(asthma)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = asthma) %>% 
  dplyr::mutate(variable = 'asthma', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# COPD
BMI_learning_disability_summ_COPD <- BMI_learning_disability_DT%>%
  dplyr::group_by(COPD)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = COPD) %>% 
  dplyr::mutate(variable = 'COPD', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# stroke_and_TIA
BMI_learning_disability_summ_stroke_and_TIA <- BMI_learning_disability_DT%>%
  dplyr::group_by(stroke_and_TIA)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = stroke_and_TIA) %>% 
  dplyr::mutate(variable = 'stroke_and_TIA', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# all_cancer
BMI_learning_disability_summ_all_cancer <- BMI_learning_disability_DT%>%
  dplyr::group_by(all_cancer)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = all_cancer) %>% 
  dplyr::mutate(variable = 'all_cancer', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# smoking_status
BMI_learning_disability_summ_smoking_status <- BMI_learning_disability_DT%>%
  dplyr::group_by(smoking_status)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = smoking_status) %>% 
  dplyr::mutate(variable = 'smoking_status', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# ethnic_no_miss
BMI_learning_disability_summ_ethnic_no_miss <- BMI_learning_disability_DT%>%
  dplyr::group_by(ethnic_no_miss)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = ethnic_no_miss) %>% 
  dplyr::mutate(variable = 'ethnic_no_miss', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# eth_group_16
BMI_learning_disability_summ_eth_group_16 <- BMI_learning_disability_DT%>%
  dplyr::group_by(eth_group_16)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = eth_group_16) %>% 
  dplyr::mutate(variable = 'eth_group_16', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# precovid_BMI_category
BMI_learning_disability_summ_precovid_BMI_category <- BMI_learning_disability_DT%>%
   dplyr::group_by(precovid_bmi_category)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = precovid_bmi_category) %>% 
  dplyr::mutate(variable = 'precovid_bmi_category', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# chronic_cardiac
BMI_learning_disability_summ_chronic_cardiac <- BMI_learning_disability_DT%>%
  dplyr::group_by(chronic_cardiac)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = chronic_cardiac) %>% 
  dplyr::mutate(variable = 'chronic_cardiac', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

BMI_learning_disability_traj_change_summary <- BMI_learning_disability_DT_summ %>% 
  bind_rows(BMI_learning_disability_summ_age_group_2) %>% 
  bind_rows(BMI_learning_disability_summ_sex) %>% 
  bind_rows(BMI_learning_disability_summ_ethnic_no_miss) %>% 
  bind_rows(BMI_learning_disability_summ_eth_group_16) %>% 
  bind_rows(BMI_learning_disability_summ_imd) %>%
  bind_rows(BMI_learning_disability_summ_region) %>%
  bind_rows(BMI_learning_disability_summ_precovid_BMI_category) %>% 
  bind_rows(BMI_learning_disability_summ_diabetes_t2) %>% 
  bind_rows(BMI_learning_disability_summ_diabetes_t1) %>% 
  bind_rows(BMI_learning_disability_summ_hypertension) %>% 
  bind_rows(BMI_learning_disability_summ_depression) %>%
  bind_rows(BMI_learning_disability_summ_psychosis_schiz_bipolar) %>% 
  bind_rows(BMI_learning_disability_summ_chronic_cardiac) %>% 
  bind_rows(BMI_learning_disability_summ_COPD) %>%
  bind_rows(BMI_learning_disability_summ_asthma)%>%
  bind_rows(BMI_learning_disability_summ_dementia) %>% 
  bind_rows(BMI_learning_disability_summ_all_cancer) %>% 
  bind_rows(BMI_learning_disability_summ_stroke_and_TIA) %>% 
  bind_rows(BMI_learning_disability_summ_smoking_status) %>% 
  dplyr::mutate(across(where(is.numeric), round, digits=2)) %>%
  dplyr::mutate(n = plyr::round_any(n, 5)) 








### LEARNING DISABILITY
## filter out missing + psychosis_schiz_bipolar
BMI_psychosis_schiz_bipolar_DT <- BMI_DT %>%
dplyr::filter(complete_bmi_data == "complete")  %>% 
dplyr::filter(psychosis_schiz_bipolar == TRUE)

BMI_psychosis_schiz_bipolar_DT %>%
tabyl(age_group_2, sex) 

BMI_psychosis_schiz_bipolar_DT %>%
tabyl(smoking_status) 

BMI_psychosis_schiz_bipolar_DT_summ <- BMI_psychosis_schiz_bipolar_DT %>% 
  dplyr::summarise( n = n(), mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::mutate(group = "all", .before=1) %>% 
  dplyr::mutate(variable = "all", .before =1)
  




## create summaries to assess for change in mean in sd per subgroup
 #age_group_2
BMI_psychosis_schiz_bipolar_summ_age_group_2 <- BMI_psychosis_schiz_bipolar_DT %>%
  dplyr::group_by(age_group_2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = age_group_2) %>% 
  dplyr::mutate(variable = 'age_group_2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))
 


# sex
BMI_psychosis_schiz_bipolar_summ_sex <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(sex)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = sex) %>% 
  dplyr::mutate(variable = 'sex', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# region
BMI_psychosis_schiz_bipolar_summ_region <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(region)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = region) %>% 
  dplyr::mutate(variable = 'region', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# imd
BMI_psychosis_schiz_bipolar_summ_imd <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(imd)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = imd) %>% 
  dplyr::mutate(variable = 'imd', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))




# diabetes_t1
BMI_psychosis_schiz_bipolar_summ_diabetes_t1 <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(diabetes_t1)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t1) %>% 
  dplyr::mutate(variable = 'diabetes_t1', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# diabetes_t2
BMI_psychosis_schiz_bipolar_summ_diabetes_t2 <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(diabetes_t2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t2) %>% 
  dplyr::mutate(variable = 'diabetes_t2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# hypertension
BMI_psychosis_schiz_bipolar_summ_hypertension <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(hypertension)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = hypertension) %>% 
  dplyr::mutate(variable = 'hypertension', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# depression
BMI_psychosis_schiz_bipolar_summ_depression <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(depression)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = depression) %>% 
  dplyr::mutate(variable = 'depression', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# learning_disability
BMI_psychosis_schiz_bipolar_summ_learning_disability <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(learning_disability)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = learning_disability) %>% 
  dplyr::mutate(variable = 'learning_disability', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# dementia
BMI_psychosis_schiz_bipolar_summ_dementia <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(dementia)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = dementia) %>% 
  dplyr::mutate(variable = 'dementia', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))



# asthma
BMI_psychosis_schiz_bipolar_summ_asthma <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(asthma)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = asthma) %>% 
  dplyr::mutate(variable = 'asthma', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# COPD
BMI_psychosis_schiz_bipolar_summ_COPD <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(COPD)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = COPD) %>% 
  dplyr::mutate(variable = 'COPD', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# stroke_and_TIA
BMI_psychosis_schiz_bipolar_summ_stroke_and_TIA <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(stroke_and_TIA)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = stroke_and_TIA) %>% 
  dplyr::mutate(variable = 'stroke_and_TIA', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# all_cancer
BMI_psychosis_schiz_bipolar_summ_all_cancer <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(all_cancer)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = all_cancer) %>% 
  dplyr::mutate(variable = 'all_cancer', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# smoking_status
BMI_psychosis_schiz_bipolar_summ_smoking_status <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(smoking_status)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = smoking_status) %>% 
  dplyr::mutate(variable = 'smoking_status', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# ethnic_no_miss
BMI_psychosis_schiz_bipolar_summ_ethnic_no_miss <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(ethnic_no_miss)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = ethnic_no_miss) %>% 
  dplyr::mutate(variable = 'ethnic_no_miss', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# eth_group_16
BMI_psychosis_schiz_bipolar_summ_eth_group_16 <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(eth_group_16)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = eth_group_16) %>% 
  dplyr::mutate(variable = 'eth_group_16', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# precovid_BMI_category
BMI_psychosis_schiz_bipolar_summ_precovid_BMI_category <- BMI_psychosis_schiz_bipolar_DT%>%
   dplyr::group_by(precovid_bmi_category)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = precovid_bmi_category) %>% 
  dplyr::mutate(variable = 'precovid_bmi_category', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# chronic_cardiac
BMI_psychosis_schiz_bipolar_summ_chronic_cardiac <- BMI_psychosis_schiz_bipolar_DT%>%
  dplyr::group_by(chronic_cardiac)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = chronic_cardiac) %>% 
  dplyr::mutate(variable = 'chronic_cardiac', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

BMI_psychosis_schiz_bipolar_traj_change_summary <- BMI_psychosis_schiz_bipolar_DT_summ %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_age_group_2) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_sex) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_ethnic_no_miss) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_eth_group_16) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_imd) %>%
  bind_rows(BMI_psychosis_schiz_bipolar_summ_region) %>%
  bind_rows(BMI_psychosis_schiz_bipolar_summ_precovid_BMI_category) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_diabetes_t2) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_diabetes_t1) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_hypertension) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_depression) %>%
  bind_rows(BMI_psychosis_schiz_bipolar_summ_learning_disability) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_chronic_cardiac) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_COPD) %>%
  bind_rows(BMI_psychosis_schiz_bipolar_summ_asthma)%>%
  bind_rows(BMI_psychosis_schiz_bipolar_summ_dementia) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_all_cancer) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_stroke_and_TIA) %>% 
  bind_rows(BMI_psychosis_schiz_bipolar_summ_smoking_status) %>% 
  dplyr::mutate(across(where(is.numeric), round, digits=2)) %>%
  dplyr::mutate(n = plyr::round_any(n, 5)) 






## Save outputs


write.csv (BMI_psychosis_schiz_bipolar_traj_change_summary, here::here ("output/data","mean_bmi_traj_change_psychosis_schiz_bipolar.csv"))

write.csv (BMI_learning_disability_traj_change_summary, here::here ("output/data","mean_bmi_traj_change_learning_disability.csv"))

write.csv (BMI_depression_traj_change_summary, here::here ("output/data","mean_bmi_traj_change_depression.csv"))