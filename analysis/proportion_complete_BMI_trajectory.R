


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
library(plyr)

## Read in files  >>> Change PATH!!

# check working directory:  getwd()

#BMI_complete_categories <- read_feather (here::here ("output/data", "BMI_all_2019.feather"))

BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories_final_demog.feather"))

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(complete_trajectories = complete_bmi_data)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(timechange1_check = time_change1)


## Due to way BMI is extracted 141 patients with a value recorded on 1st March 2018 were counted in two time windows
## This created a time difference of 0 and therefore an infinity value with BMI change/time
## create a flag to identify when a time difference between BMI measures is recorded as '0'  Then filter these out.
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(time_change_error = case_when(
    timechange1_check == 0 ~ 1, 
    timechange1_check != 0 ~ 0
  ))


BMI_trajectories %>% 
  tabyl(time_change_error)


########### CODE ADDED - to stop NAs from being excluded from analysis
## need to add this code to other trajectory analyses
BMI_trajectories <- BMI_trajectories %>% 
  replace_na(list(time_change_error = 0))

###########


BMI_trajectories <- BMI_trajectories %>% 
dplyr::filter(time_change_error == 0)

## Change to factors.  
BMI_trajectories$age_group <- factor(BMI_trajectories$age_group,      # Reordering group factor levels
                                       levels = c("18-39", "40-65", "65-80", "80+", "missing"))



BMI_trajectories$age_group_2 <- factor(BMI_trajectories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))






#BMI_trajectories <- BMI_trajectories %>% 
#dplyr:: mutate(
  #age_group = forcats::fct_relevel(age_group, "18-39", "40-65", "65-80", "80+", "missing"))



BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(complete_trajectories = case_when(
    complete_trajectories == 'complete' ~ "TRUE", 
    complete_trajectories == 'incomplete' ~ "FALSE"
  ))

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::select(patient_id, 
                sex, 
                age_group, 
                age_group_2, 
                region, 
                imd, 
                learning_disability,
                dementia, 
                depression, 
                psychosis_schiz_bipolar,
                diabetes_t1,
                diabetes_t2, 
                asthma, 
                COPD, 
                stroke_and_TIA, 
                chronic_cardiac, 
                hypertension, 
                all_cancer, 
                smoking_status, 
                ethnic_no_miss, 
                eth_group_16,
                complete_trajectories)
              
                
BMI_trajectories <-BMI_trajectories %>%
  dplyr::mutate(
    across(
      .cols = c(learning_disability,depression, dementia,psychosis_schiz_bipolar,  diabetes_t1, diabetes_t2, asthma, COPD, stroke_and_TIA, chronic_cardiac, hypertension, all_cancer), 
      .names = "comorbid_{col}"
    )
  )

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::select(-c(learning_disability,depression, dementia,psychosis_schiz_bipolar, diabetes_t1, diabetes_t2, asthma, COPD, stroke_and_TIA, chronic_cardiac, hypertension, all_cancer))

bmi_trajectories <- ungroup (BMI_trajectories)


########### Had complete_trajectories in whole analysis population

bmi_trajectories_population <- bmi_trajectories %>%
  dplyr:: summarise(N_total = n()) %>%
  ungroup()

bmi_trajectories_population_n <- bmi_trajectories %>%  
  dplyr::group_by(complete_trajectories) %>% 
  dplyr::summarise(n_complete_trajectories = n()) %>%
  dplyr::filter(complete_trajectories == 'TRUE')

bmi_trajectories_population <- bmi_trajectories_population %>%
  bind_cols(bmi_trajectories_population_n)  

bmi_trajectories_population <- bmi_trajectories_population %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "all", .before=1) %>%
  dplyr::mutate(group = 'all', before=1) %>% 
  dplyr::select(variable, group, n_complete_trajectories, N_total, percent_complete_trajectories)






## by age_group


complete_bmi_trajectories_age_group <- bmi_trajectories %>%
  tabyl(age_group, complete_trajectories) 

complete_bmi_trajectories_age_group <- complete_bmi_trajectories_age_group %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "age_group", .before=1) %>%
  dplyr::rename(group = age_group)



chisq_age_group <- chisq.test(bmi_trajectories$age_group, bmi_trajectories$complete_trajectories) 

chisq_age_group <- broom::tidy(chisq_age_group) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_age_group <- complete_bmi_trajectories_age_group %>%
  bind_cols(chisq_age_group)


### sex
complete_bmi_trajectories_sex <- bmi_trajectories %>%
  tabyl(sex, complete_trajectories) 

complete_bmi_trajectories_sex <- complete_bmi_trajectories_sex %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "sex", .before=1) %>%
  dplyr::rename(group = sex)



chisq_sex <- chisq.test(bmi_trajectories$sex, bmi_trajectories$complete_trajectories) 

chisq_sex <- broom::tidy(chisq_sex) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_sex <- complete_bmi_trajectories_sex %>%
  bind_cols(chisq_sex)

## region
complete_bmi_trajectories_region <- bmi_trajectories %>%
  tabyl(region, complete_trajectories) 

complete_bmi_trajectories_region <- complete_bmi_trajectories_region %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "region", .before=1) %>%
  dplyr::rename(group = region)



chisq_region <- chisq.test(bmi_trajectories$region, bmi_trajectories$complete_trajectories) 

chisq_region <- broom::tidy(chisq_region) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_region <- complete_bmi_trajectories_region %>%
  bind_cols(chisq_region)


## imd
complete_bmi_trajectories_imd <- bmi_trajectories %>%
  tabyl(imd, complete_trajectories) 

complete_bmi_trajectories_imd <- complete_bmi_trajectories_imd %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "imd", .before=1) %>%
  dplyr::rename(group = imd)



chisq_imd <- chisq.test(bmi_trajectories$imd, bmi_trajectories$complete_trajectories) 

chisq_imd <- broom::tidy(chisq_imd) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_imd <- complete_bmi_trajectories_imd %>%
  bind_cols(chisq_imd)

## 6 group eth

complete_bmi_trajectories_ethnic_no_miss <- bmi_trajectories %>%
  tabyl(ethnic_no_miss, complete_trajectories) 

complete_bmi_trajectories_ethnic_no_miss <- complete_bmi_trajectories_ethnic_no_miss %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "ethnic_no_miss", .before=1) %>%
  dplyr::rename(group = ethnic_no_miss)



chisq_ethnic_no_miss <- chisq.test(bmi_trajectories$ethnic_no_miss, bmi_trajectories$complete_trajectories) 

chisq_ethnic_no_miss <- broom::tidy(chisq_ethnic_no_miss) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_ethnic_no_miss <- complete_bmi_trajectories_ethnic_no_miss %>%
  bind_cols(chisq_ethnic_no_miss)


## 16 group eth
complete_bmi_trajectories_eth_group_16 <- bmi_trajectories %>%
  tabyl(eth_group_16, complete_trajectories) 

complete_bmi_trajectories_eth_group_16 <- complete_bmi_trajectories_eth_group_16 %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "eth_group_16", .before=1) %>%
  dplyr::rename(group = eth_group_16)



chisq_eth_group_16 <- chisq.test(bmi_trajectories$eth_group_16, bmi_trajectories$complete_trajectories) 

chisq_eth_group_16 <- broom::tidy(chisq_eth_group_16) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_eth_group_16 <- complete_bmi_trajectories_eth_group_16 %>%
  bind_cols(chisq_eth_group_16)


## comorbid_learning_disability

complete_bmi_trajectories_comorbid_learning_disability <- bmi_trajectories %>%
  tabyl(comorbid_learning_disability, complete_trajectories) 

complete_bmi_trajectories_comorbid_learning_disability <- complete_bmi_trajectories_comorbid_learning_disability %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_learning_disability", .before=1) %>%
  dplyr::rename(group = comorbid_learning_disability) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_learning_disability <- chisq.test(bmi_trajectories$comorbid_learning_disability, bmi_trajectories$complete_trajectories) 

chisq_comorbid_learning_disability <- broom::tidy(chisq_comorbid_learning_disability) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_learning_disability <- complete_bmi_trajectories_comorbid_learning_disability %>%
  bind_cols(chisq_comorbid_learning_disability)



## comorbid_depression

complete_bmi_trajectories_comorbid_depression <- bmi_trajectories %>%
  tabyl(comorbid_depression, complete_trajectories) 

complete_bmi_trajectories_comorbid_depression <- complete_bmi_trajectories_comorbid_depression %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_depression", .before=1) %>%
  dplyr::rename(group = comorbid_depression) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_depression <- chisq.test(bmi_trajectories$comorbid_depression, bmi_trajectories$complete_trajectories) 

chisq_comorbid_depression <- broom::tidy(chisq_comorbid_depression) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_depression <- complete_bmi_trajectories_comorbid_depression %>%
  bind_cols(chisq_comorbid_depression)


## comorbid_dementia

complete_bmi_trajectories_comorbid_dementia <- bmi_trajectories %>%
  tabyl(comorbid_dementia, complete_trajectories) 

complete_bmi_trajectories_comorbid_dementia <- complete_bmi_trajectories_comorbid_dementia %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_dementia", .before=1) %>%
  dplyr::rename(group = comorbid_dementia) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_dementia <- chisq.test(bmi_trajectories$comorbid_dementia, bmi_trajectories$complete_trajectories) 

chisq_comorbid_dementia <- broom::tidy(chisq_comorbid_dementia) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_dementia <- complete_bmi_trajectories_comorbid_dementia %>%
  bind_cols(chisq_comorbid_dementia)



## comorbid_psychosis_schiz_bipolar

complete_bmi_trajectories_comorbid_psychosis_schiz_bipolar <- bmi_trajectories %>%
  tabyl(comorbid_psychosis_schiz_bipolar, complete_trajectories) 

complete_bmi_trajectories_comorbid_psychosis_schiz_bipolar <- complete_bmi_trajectories_comorbid_psychosis_schiz_bipolar %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_psychosis_schiz_bipolar", .before=1) %>%
  dplyr::rename(group = comorbid_psychosis_schiz_bipolar) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_psychosis_schiz_bipolar <- chisq.test(bmi_trajectories$comorbid_psychosis_schiz_bipolar, bmi_trajectories$complete_trajectories) 

chisq_comorbid_psychosis_schiz_bipolar <- broom::tidy(chisq_comorbid_psychosis_schiz_bipolar) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_psychosis_schiz_bipolar <- complete_bmi_trajectories_comorbid_psychosis_schiz_bipolar %>%
  bind_cols(chisq_comorbid_psychosis_schiz_bipolar)



## comorbid_asthma

complete_bmi_trajectories_comorbid_asthma <- bmi_trajectories %>%
  tabyl(comorbid_asthma, complete_trajectories) 

complete_bmi_trajectories_comorbid_asthma <- complete_bmi_trajectories_comorbid_asthma %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_asthma", .before=1) %>%
  dplyr::rename(group = comorbid_asthma) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_asthma <- chisq.test(bmi_trajectories$comorbid_asthma, bmi_trajectories$complete_trajectories) 

chisq_comorbid_asthma <- broom::tidy(chisq_comorbid_asthma) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_asthma <- complete_bmi_trajectories_comorbid_asthma %>%
  bind_cols(chisq_comorbid_asthma)


## comorbid_COPD

complete_bmi_trajectories_comorbid_COPD <- bmi_trajectories %>%
  tabyl(comorbid_COPD, complete_trajectories) 

complete_bmi_trajectories_comorbid_COPD <- complete_bmi_trajectories_comorbid_COPD %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_COPD", .before=1) %>%
  dplyr::rename(group = comorbid_COPD) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_COPD <- chisq.test(bmi_trajectories$comorbid_COPD, bmi_trajectories$complete_trajectories) 

chisq_comorbid_COPD <- broom::tidy(chisq_comorbid_COPD) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_COPD <- complete_bmi_trajectories_comorbid_COPD %>%
  bind_cols(chisq_comorbid_COPD)

## comorbid_stroke_and_TIA

complete_bmi_trajectories_comorbid_stroke_and_TIA <- bmi_trajectories %>%
  tabyl(comorbid_stroke_and_TIA, complete_trajectories) 

complete_bmi_trajectories_comorbid_stroke_and_TIA <- complete_bmi_trajectories_comorbid_stroke_and_TIA %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_stroke_and_TIA", .before=1) %>%
  dplyr::rename(group = comorbid_stroke_and_TIA) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_stroke_and_TIA <- chisq.test(bmi_trajectories$comorbid_stroke_and_TIA, bmi_trajectories$complete_trajectories) 

chisq_comorbid_stroke_and_TIA <- broom::tidy(chisq_comorbid_stroke_and_TIA) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_stroke_and_TIA <- complete_bmi_trajectories_comorbid_stroke_and_TIA %>%
  bind_cols(chisq_comorbid_stroke_and_TIA)


## comorbid_chronic_cardiac

complete_bmi_trajectories_comorbid_chronic_cardiac <- bmi_trajectories %>%
  tabyl(comorbid_chronic_cardiac, complete_trajectories) 

complete_bmi_trajectories_comorbid_chronic_cardiac <- complete_bmi_trajectories_comorbid_chronic_cardiac %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_chronic_cardiac", .before=1) %>%
  dplyr::rename(group = comorbid_chronic_cardiac) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_chronic_cardiac <- chisq.test(bmi_trajectories$comorbid_chronic_cardiac, bmi_trajectories$complete_trajectories) 

chisq_comorbid_chronic_cardiac <- broom::tidy(chisq_comorbid_chronic_cardiac) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_chronic_cardiac <- complete_bmi_trajectories_comorbid_chronic_cardiac %>%
  bind_cols(chisq_comorbid_chronic_cardiac)


## comorbid_hypertension

complete_bmi_trajectories_comorbid_hypertension <- bmi_trajectories %>%
  tabyl(comorbid_hypertension, complete_trajectories) 

complete_bmi_trajectories_comorbid_hypertension <- complete_bmi_trajectories_comorbid_hypertension %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_hypertension", .before=1) %>%
  dplyr::rename(group = comorbid_hypertension) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_hypertension <- chisq.test(bmi_trajectories$comorbid_hypertension, bmi_trajectories$complete_trajectories) 

chisq_comorbid_hypertension <- broom::tidy(chisq_comorbid_hypertension) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_hypertension <- complete_bmi_trajectories_comorbid_hypertension %>%
  bind_cols(chisq_comorbid_hypertension)


## comorbid_all_cancer

complete_bmi_trajectories_comorbid_all_cancer <- bmi_trajectories %>%
  tabyl(comorbid_all_cancer, complete_trajectories) 

complete_bmi_trajectories_comorbid_all_cancer <- complete_bmi_trajectories_comorbid_all_cancer %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_all_cancer", .before=1) %>%
  dplyr::rename(group = comorbid_all_cancer) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_all_cancer <- chisq.test(bmi_trajectories$comorbid_all_cancer, bmi_trajectories$complete_trajectories) 

chisq_comorbid_all_cancer <- broom::tidy(chisq_comorbid_all_cancer) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_all_cancer <- complete_bmi_trajectories_comorbid_all_cancer %>%
  bind_cols(chisq_comorbid_all_cancer)
############### NEW CODE

## comorbid_diabetes_t1

complete_bmi_trajectories_comorbid_diabetes_t1 <- bmi_trajectories %>%
  tabyl(comorbid_diabetes_t1, complete_trajectories) 

complete_bmi_trajectories_comorbid_diabetes_t1 <- complete_bmi_trajectories_comorbid_diabetes_t1 %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_diabetes_t1", .before=1) %>%
  dplyr::rename(group = comorbid_diabetes_t1) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_diabetes_t1 <- chisq.test(bmi_trajectories$comorbid_diabetes_t1, bmi_trajectories$complete_trajectories) 

chisq_comorbid_diabetes_t1 <- broom::tidy(chisq_comorbid_diabetes_t1) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_diabetes_t1 <- complete_bmi_trajectories_comorbid_diabetes_t1 %>%
  bind_cols(chisq_comorbid_diabetes_t1)


## comorbid_diabetes_t2

complete_bmi_trajectories_comorbid_diabetes_t2 <- bmi_trajectories %>%
  tabyl(comorbid_diabetes_t2, complete_trajectories) 

complete_bmi_trajectories_comorbid_diabetes_t2 <- complete_bmi_trajectories_comorbid_diabetes_t2 %>%
  dplyr::rename(n_complete_trajectories = 'TRUE') %>% 
  dplyr::rename(n_no_complete_trajectories = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_complete_trajectories + n_complete_trajectories) %>%
  dplyr::select(-('n_no_complete_trajectories'))  %>%
  dplyr:: mutate(percent_complete_trajectories = ((n_complete_trajectories/N_total)*100)) %>%
  dplyr::mutate(percent_complete_trajectories = round(percent_complete_trajectories, 2)) %>%
  dplyr::mutate(variable = "comorbid_diabetes_t2", .before=1) %>%
  dplyr::rename(group = comorbid_diabetes_t2) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_diabetes_t2 <- chisq.test(bmi_trajectories$comorbid_diabetes_t2, bmi_trajectories$complete_trajectories) 

chisq_comorbid_diabetes_t2 <- broom::tidy(chisq_comorbid_diabetes_t2) %>%
  dplyr::select(p.value, method)

complete_bmi_trajectories_comorbid_diabetes_t2 <- complete_bmi_trajectories_comorbid_diabetes_t2 %>%
  bind_cols(chisq_comorbid_diabetes_t2)



######################  END OF NEW CODE:  two new rows added to below


## complete bmi trajectories


had_complete_bmi_trajectories <- bmi_trajectories_population %>% 
  bind_rows(complete_bmi_trajectories_age_group) %>%
  bind_rows(complete_bmi_trajectories_sex) %>%
  bind_rows(complete_bmi_trajectories_region) %>%
  bind_rows(complete_bmi_trajectories_imd) %>%
  bind_rows(complete_bmi_trajectories_ethnic_no_miss) %>% 
  bind_rows(complete_bmi_trajectories_eth_group_16) %>% 
  bind_rows(complete_bmi_trajectories_comorbid_hypertension) %>% 
  bind_rows (complete_bmi_trajectories_comorbid_diabetes_t2) %>%
  bind_rows (complete_bmi_trajectories_comorbid_diabetes_t1) %>%
  bind_rows(complete_bmi_trajectories_comorbid_learning_disability) %>% 
  bind_rows(complete_bmi_trajectories_comorbid_depression) %>% 
  bind_rows(complete_bmi_trajectories_comorbid_dementia) %>% 
  bind_rows(complete_bmi_trajectories_comorbid_psychosis_schiz_bipolar) %>% 
  bind_rows(complete_bmi_trajectories_comorbid_asthma) %>% 
  bind_rows(complete_bmi_trajectories_comorbid_COPD) %>% 
  bind_rows(complete_bmi_trajectories_comorbid_stroke_and_TIA) %>% 
  bind_rows(complete_bmi_trajectories_comorbid_chronic_cardiac) %>% 
  bind_rows(complete_bmi_trajectories_comorbid_all_cancer)

had_complete_bmi_trajectories <- had_complete_bmi_trajectories %>%
  dplyr::mutate('p.value' = round(p.value, 2))  %>%
  dplyr::filter(n_complete_trajectories >5) 

had_complete_bmi_trajectories <- as.data.frame(had_complete_bmi_trajectories) %>%
  dplyr::mutate(n_complete_trajectories = plyr::round_any(had_complete_bmi_trajectories$n_complete_trajectories, 5)) %>% 
  dplyr::mutate(N = plyr::round_any(had_complete_bmi_trajectories$N_total, 5)) 

write.csv (had_complete_bmi_trajectories, here::here ("output/data","complete_bmi_trajectories_proportions.csv"))                
                



