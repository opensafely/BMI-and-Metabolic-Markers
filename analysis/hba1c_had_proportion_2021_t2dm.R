## Author: M Samuel 
## Proportion of T2DM who have had an HbA1c



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

hba1c_summary <- read_feather (here::here ("output/data", "hba1c_2021_summary.feather"))
precovid_hba1c <- read_feather (here::here ("output/data","precovid_hba1c_control.feather"))

precovid_hba1c <- precovid_hba1c  %>%
    dplyr::select("patient_id", "precovid_control") %>%
    ungroup()

hba1c_summary <- hba1c_summary %>%
  ungroup()

hba1c_summary <- hba1c_summary %>% 
    dplyr::left_join(precovid_hba1c)

hba1c_summary %>% 
    tabyl(precovid_control)


## limit analysis to type 2 diabetics
hba1c_summary <- dplyr::filter(hba1c_summary, (diabetes_t2 == 'TRUE' & diabetes_t1=='FALSE'))
hba1c_summary <-hba1c_summary %>%
  dplyr::mutate(
    across(
      .cols = c(learning_disability,depression, dementia,psychosis_schiz_bipolar, diabetes_type, diabetes_t1, diabetes_t2, asthma, COPD, stroke_and_TIA, chronic_cardiac, hypertension, all_cancer), 
      .names = "comorbid_{col}"
    )
  )


## create a flag for whether they had any DM meds
hba1c_summary <- hba1c_summary %>%                   
  dplyr::mutate(dm_meds = oad_meds + insulin_meds)  ## combine oral meds and insulin



hba1c_summary <-   hba1c_summary %>%    
  dplyr::mutate(dm_meds = case_when(
    dm_meds == 0 ~ FALSE,
    dm_meds > 0 ~ TRUE
  ))  

hba1c_summary %>% 
  tabyl(dm_meds) 






########### Had Hba1c in whole analysis population

hba1c_population <- hba1c_summary %>%
  dplyr:: summarise(N_total = n()) %>%
  ungroup()

hba1c_population_n <- hba1c_summary %>%  
  dplyr::group_by(had_hba1c) %>% 
  dplyr::summarise(n_had_hba1c = n()) %>%
  dplyr::filter(had_hba1c == 'TRUE')

hba1c_population <- hba1c_population %>%
  bind_cols(hba1c_population_n)  

hba1c_population <- hba1c_population %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "all", .before=1) %>%
  dplyr::mutate(group = 'all', before=1) %>% 
  dplyr::select(variable, group, n_had_hba1c, N_total, percent_hba1c)






## by age_group


hba1c_2021_age_group <- hba1c_summary %>%
  tabyl(age_group, had_hba1c) 

hba1c_2021_age_group <- hba1c_2021_age_group %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "age_group", .before=1) %>%
  dplyr::rename(group = age_group)



chisq_age_group <- chisq.test(hba1c_summary$age_group, hba1c_summary$had_hba1c) 

chisq_age_group <- broom::tidy(chisq_age_group) %>%
  dplyr::select(p.value, method)

hba1c_2021_age_group <- hba1c_2021_age_group %>%
  bind_cols(chisq_age_group)


### sex
hba1c_2021_sex <- hba1c_summary %>%
  tabyl(sex, had_hba1c) 

hba1c_2021_sex <- hba1c_2021_sex %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "sex", .before=1) %>%
  dplyr::rename(group = sex)



chisq_sex <- chisq.test(hba1c_summary$sex, hba1c_summary$had_hba1c) 

chisq_sex <- broom::tidy(chisq_sex) %>%
  dplyr::select(p.value, method)

hba1c_2021_sex <- hba1c_2021_sex %>%
  bind_cols(chisq_sex)

## region
hba1c_2021_region <- hba1c_summary %>%
  tabyl(region, had_hba1c) 

hba1c_2021_region <- hba1c_2021_region %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "region", .before=1) %>%
  dplyr::rename(group = region)



chisq_region <- chisq.test(hba1c_summary$region, hba1c_summary$had_hba1c) 

chisq_region <- broom::tidy(chisq_region) %>%
  dplyr::select(p.value, method)

hba1c_2021_region <- hba1c_2021_region %>%
  bind_cols(chisq_region)


## imd
hba1c_2021_imd <- hba1c_summary %>%
  tabyl(imd, had_hba1c) 

hba1c_2021_imd <- hba1c_2021_imd %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "imd", .before=1) %>%
  dplyr::rename(group = imd)



chisq_imd <- chisq.test(hba1c_summary$imd, hba1c_summary$had_hba1c) 

chisq_imd <- broom::tidy(chisq_imd) %>%
  dplyr::select(p.value, method)

hba1c_2021_imd <- hba1c_2021_imd %>%
  bind_cols(chisq_imd)

## 6 group eth

hba1c_2021_ethnic_no_miss <- hba1c_summary %>%
  tabyl(ethnic_no_miss, had_hba1c) 

hba1c_2021_ethnic_no_miss <- hba1c_2021_ethnic_no_miss %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "ethnic_no_miss", .before=1) %>%
  dplyr::rename(group = ethnic_no_miss)



chisq_ethnic_no_miss <- chisq.test(hba1c_summary$ethnic_no_miss, hba1c_summary$had_hba1c) 

chisq_ethnic_no_miss <- broom::tidy(chisq_ethnic_no_miss) %>%
  dplyr::select(p.value, method)

hba1c_2021_ethnic_no_miss <- hba1c_2021_ethnic_no_miss %>%
  bind_cols(chisq_ethnic_no_miss)


## 16 group eth
hba1c_2021_eth_group_16 <- hba1c_summary %>%
  tabyl(eth_group_16, had_hba1c) 

hba1c_2021_eth_group_16 <- hba1c_2021_eth_group_16 %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "eth_group_16", .before=1) %>%
  dplyr::rename(group = eth_group_16)



chisq_eth_group_16 <- chisq.test(hba1c_summary$eth_group_16, hba1c_summary$had_hba1c) 

chisq_eth_group_16 <- broom::tidy(chisq_eth_group_16) %>%
  dplyr::select(p.value, method)

hba1c_2021_eth_group_16 <- hba1c_2021_eth_group_16 %>%
  bind_cols(chisq_eth_group_16)


## comorbid_learning_disability

hba1c_2021_comorbid_learning_disability <- hba1c_summary %>%
  tabyl(comorbid_learning_disability, had_hba1c) 

hba1c_2021_comorbid_learning_disability <- hba1c_2021_comorbid_learning_disability %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_learning_disability", .before=1) %>%
  dplyr::rename(group = comorbid_learning_disability) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_learning_disability <- chisq.test(hba1c_summary$comorbid_learning_disability, hba1c_summary$had_hba1c) 

chisq_comorbid_learning_disability <- broom::tidy(chisq_comorbid_learning_disability) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_learning_disability <- hba1c_2021_comorbid_learning_disability %>%
  bind_cols(chisq_comorbid_learning_disability)



## comorbid_depression

hba1c_2021_comorbid_depression <- hba1c_summary %>%
  tabyl(comorbid_depression, had_hba1c) 

hba1c_2021_comorbid_depression <- hba1c_2021_comorbid_depression %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_depression", .before=1) %>%
  dplyr::rename(group = comorbid_depression) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_depression <- chisq.test(hba1c_summary$comorbid_depression, hba1c_summary$had_hba1c) 

chisq_comorbid_depression <- broom::tidy(chisq_comorbid_depression) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_depression <- hba1c_2021_comorbid_depression %>%
  bind_cols(chisq_comorbid_depression)


## comorbid_dementia

hba1c_2021_comorbid_dementia <- hba1c_summary %>%
  tabyl(comorbid_dementia, had_hba1c) 

hba1c_2021_comorbid_dementia <- hba1c_2021_comorbid_dementia %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_dementia", .before=1) %>%
  dplyr::rename(group = comorbid_dementia) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_dementia <- chisq.test(hba1c_summary$comorbid_dementia, hba1c_summary$had_hba1c) 

chisq_comorbid_dementia <- broom::tidy(chisq_comorbid_dementia) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_dementia <- hba1c_2021_comorbid_dementia %>%
  bind_cols(chisq_comorbid_dementia)



## comorbid_psychosis_schiz_bipolar

hba1c_2021_comorbid_psychosis_schiz_bipolar <- hba1c_summary %>%
  tabyl(comorbid_psychosis_schiz_bipolar, had_hba1c) 

hba1c_2021_comorbid_psychosis_schiz_bipolar <- hba1c_2021_comorbid_psychosis_schiz_bipolar %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_psychosis_schiz_bipolar", .before=1) %>%
  dplyr::rename(group = comorbid_psychosis_schiz_bipolar) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_psychosis_schiz_bipolar <- chisq.test(hba1c_summary$comorbid_psychosis_schiz_bipolar, hba1c_summary$had_hba1c) 

chisq_comorbid_psychosis_schiz_bipolar <- broom::tidy(chisq_comorbid_psychosis_schiz_bipolar) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_psychosis_schiz_bipolar <- hba1c_2021_comorbid_psychosis_schiz_bipolar %>%
  bind_cols(chisq_comorbid_psychosis_schiz_bipolar)



## comorbid_COPD

hba1c_2021_comorbid_asthma <- hba1c_summary %>%
  tabyl(comorbid_asthma, had_hba1c) 

hba1c_2021_comorbid_asthma <- hba1c_2021_comorbid_asthma %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_asthma", .before=1) %>%
  dplyr::rename(group = comorbid_asthma) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_asthma <- chisq.test(hba1c_summary$comorbid_asthma, hba1c_summary$had_hba1c) 

chisq_comorbid_asthma <- broom::tidy(chisq_comorbid_asthma) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_asthma <- hba1c_2021_comorbid_asthma %>%
  bind_cols(chisq_comorbid_asthma)


## comorbid_COPD

hba1c_2021_comorbid_COPD <- hba1c_summary %>%
  tabyl(comorbid_COPD, had_hba1c) 

hba1c_2021_comorbid_COPD <- hba1c_2021_comorbid_COPD %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_COPD", .before=1) %>%
  dplyr::rename(group = comorbid_COPD) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_COPD <- chisq.test(hba1c_summary$comorbid_COPD, hba1c_summary$had_hba1c) 

chisq_comorbid_COPD <- broom::tidy(chisq_comorbid_COPD) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_COPD <- hba1c_2021_comorbid_COPD %>%
  bind_cols(chisq_comorbid_COPD)

## comorbid_stroke_and_TIA

hba1c_2021_comorbid_stroke_and_TIA <- hba1c_summary %>%
  tabyl(comorbid_stroke_and_TIA, had_hba1c) 

hba1c_2021_comorbid_stroke_and_TIA <- hba1c_2021_comorbid_stroke_and_TIA %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_stroke_and_TIA", .before=1) %>%
  dplyr::rename(group = comorbid_stroke_and_TIA) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_stroke_and_TIA <- chisq.test(hba1c_summary$comorbid_stroke_and_TIA, hba1c_summary$had_hba1c) 

chisq_comorbid_stroke_and_TIA <- broom::tidy(chisq_comorbid_stroke_and_TIA) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_stroke_and_TIA <- hba1c_2021_comorbid_stroke_and_TIA %>%
  bind_cols(chisq_comorbid_stroke_and_TIA)


## comorbid_chronic_cardiac

hba1c_2021_comorbid_chronic_cardiac <- hba1c_summary %>%
  tabyl(comorbid_chronic_cardiac, had_hba1c) 

hba1c_2021_comorbid_chronic_cardiac <- hba1c_2021_comorbid_chronic_cardiac %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_chronic_cardiac", .before=1) %>%
  dplyr::rename(group = comorbid_chronic_cardiac) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_chronic_cardiac <- chisq.test(hba1c_summary$comorbid_chronic_cardiac, hba1c_summary$had_hba1c) 

chisq_comorbid_chronic_cardiac <- broom::tidy(chisq_comorbid_chronic_cardiac) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_chronic_cardiac <- hba1c_2021_comorbid_chronic_cardiac %>%
  bind_cols(chisq_comorbid_chronic_cardiac)


## comorbid_hypertension

hba1c_2021_comorbid_hypertension <- hba1c_summary %>%
  tabyl(comorbid_hypertension, had_hba1c) 

hba1c_2021_comorbid_hypertension <- hba1c_2021_comorbid_hypertension %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_hypertension", .before=1) %>%
  dplyr::rename(group = comorbid_hypertension) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_hypertension <- chisq.test(hba1c_summary$comorbid_hypertension, hba1c_summary$had_hba1c) 

chisq_comorbid_hypertension <- broom::tidy(chisq_comorbid_hypertension) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_hypertension <- hba1c_2021_comorbid_hypertension %>%
  bind_cols(chisq_comorbid_hypertension)


## comorbid_all_cancer

hba1c_2021_comorbid_all_cancer <- hba1c_summary %>%
  tabyl(comorbid_all_cancer, had_hba1c) 

hba1c_2021_comorbid_all_cancer <- hba1c_2021_comorbid_all_cancer %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "comorbid_all_cancer", .before=1) %>%
  dplyr::rename(group = comorbid_all_cancer) %>%
  dplyr::mutate(group = as.character(group))



chisq_comorbid_all_cancer <- chisq.test(hba1c_summary$comorbid_all_cancer, hba1c_summary$had_hba1c) 

chisq_comorbid_all_cancer <- broom::tidy(chisq_comorbid_all_cancer) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_all_cancer <- hba1c_2021_comorbid_all_cancer %>%
  bind_cols(chisq_comorbid_all_cancer)


## Patients who are on diabetic meds (tablets or insulin)

hba1c_2021_comorbid_dm_meds  <- hba1c_summary %>%
  tabyl(dm_meds, had_hba1c)

hba1c_2021_comorbid_dm_meds <- hba1c_2021_comorbid_dm_meds %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "dm_meds", .before=1) %>%
  dplyr::rename(group = dm_meds) %>%
  dplyr::mutate(group = as.character(group))






chisq_comorbid_dm_meds <- chisq.test(hba1c_summary$dm_meds, hba1c_summary$had_hba1c) 

chisq_comorbid_dm_meds <- broom::tidy(chisq_comorbid_dm_meds) %>%
  dplyr::select(p.value, method)

hba1c_2021_comorbid_dm_meds <- hba1c_2021_comorbid_dm_meds %>%
  bind_cols(chisq_comorbid_dm_meds)



hba1c_2021_precovid_control <- hba1c_summary %>%
  tabyl(precovid_control, had_hba1c) 

hba1c_2021_precovid_control <- hba1c_2021_precovid_control %>%
  dplyr::rename(n_had_hba1c = 'TRUE') %>% 
  dplyr::rename(n_no_hba1c = 'FALSE') %>%
  dplyr::mutate(N_total = n_no_hba1c + n_had_hba1c) %>%
  dplyr::select(-('n_no_hba1c'))  %>%
  dplyr:: mutate(percent_hba1c = ((n_had_hba1c/N_total)*100)) %>%
  dplyr::mutate(percent_hba1c = round(percent_hba1c, 2)) %>%
  dplyr::mutate(variable = "precovid_control", .before=1) %>%
  dplyr::rename(group = precovid_control)



chisq_precovid_control <- chisq.test(hba1c_summary$precovid_control, hba1c_summary$had_hba1c) 

chisq_precovid_control <- broom::tidy(chisq_precovid_control) %>%
  dplyr::select(p.value, method)

hba1c_2021_precovid_control <- hba1c_2021_precovid_control %>%
  bind_cols(chisq_precovid_control)






had_hba1c_2021 <- hba1c_population %>% 
  bind_rows(hba1c_2021_age_group) %>%
  bind_rows(hba1c_2021_sex) %>%
  bind_rows(hba1c_2021_region) %>%
  bind_rows(hba1c_2021_imd) %>%
  bind_rows(hba1c_2021_ethnic_no_miss) %>% 
  bind_rows(hba1c_2021_eth_group_16) %>% 
  bind_rows(hba1c_2021_comorbid_dm_meds) %>%
  bind_rows(hba1c_2021_precovid_control) %>%
  bind_rows(hba1c_2021_comorbid_hypertension) %>% 
  bind_rows(hba1c_2021_comorbid_learning_disability) %>% 
  bind_rows(hba1c_2021_comorbid_depression) %>% 
  bind_rows(hba1c_2021_comorbid_dementia) %>% 
  bind_rows(hba1c_2021_comorbid_psychosis_schiz_bipolar) %>% 
  bind_rows(hba1c_2021_comorbid_asthma) %>% 
  bind_rows(hba1c_2021_comorbid_COPD) %>% 
  bind_rows(hba1c_2021_comorbid_stroke_and_TIA) %>% 
  bind_rows(hba1c_2021_comorbid_chronic_cardiac) %>% 
  bind_rows(hba1c_2021_comorbid_all_cancer)

had_hba1c_2021 <- had_hba1c_2021 %>%
dplyr::mutate('p.value' = round(p.value, 2)) 

write.csv (had_hba1c_2021, here::here ("output/data","T2DM_proportion_hba1c_2021.csv"))