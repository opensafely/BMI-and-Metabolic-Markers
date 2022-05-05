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




BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories_final.feather"))


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


BMI_trajectories <- BMI_trajectories %>% 
dplyr::filter(time_change_error == 0)



#BMI_trajectories <- BMI_trajectories %>% 
#dplyr:: mutate(
  #age_group = forcats::fct_relevel(age_group, "18-39", "40-65", "65-80", "80+", "missing"))


## Change to factors.  
BMI_trajectories$age_group <- factor(BMI_trajectories$age_group,      # Reordering group factor levels
                                       levels = c("18-39", "40-65", "65-80", "80+", "missing"))



BMI_trajectories$age_group_2 <- factor(BMI_trajectories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))





colnames(BMI_trajectories)

BMI_trajectories <- BMI_trajectories %>%
  dplyr::mutate(
    across(
      .cols = c(learning_disability,depression, dementia,psychosis_schiz_bipolar, diabetes_type, diabetes_t1, diabetes_t2, asthma, COPD, stroke_and_TIA, chronic_cardiac, hypertension, all_cancer), 
      .names = "comorbid_{col}"))





BMI_traj_DT <- setDT(BMI_trajectories)


N_all = (length(BMI_trajectories$yearly_bmi_change1) - sum(is.na(BMI_trajectories$yearly_bmi_change1)))
N_population = length(BMI_trajectories$yearly_bmi_change1)
mean_all = mean(BMI_trajectories$yearly_bmi_change1, na.rm = TRUE) 

bmi_change_all <- skimr::skim_without_charts(BMI_trajectories$yearly_bmi_change1)  %>% 
  dplyr::rename("mean_bmi_change" = "numeric.mean") %>% 
  dplyr::rename("sd_bmi_change" = "numeric.sd") %>% 
  dplyr::mutate ("N_population" = N_population) %>% 
  dplyr::mutate ("N" = N_all) %>% 
  dplyr::select("N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::mutate ('group' = 'all', .before = 1) %>%
  dplyr::mutate(variable="all", .before = 1 )



## age_group
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="age_group"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="age_group"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="age_group"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="age_group"]

prepandemic_BMI_age_group <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "age_group") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "age_group") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "age_group") %>%
  dplyr::select(age_group, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'age_group') %>%
  dplyr::mutate(variable="age_group", .before = 1 ) %>%
  dplyr::arrange(group)

## age_group_2
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="age_group_2"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="age_group_2"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="age_group_2"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="age_group_2"]

prepandemic_BMI_age_group_2 <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "age_group_2") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "age_group_2") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "age_group_2") %>%
  dplyr::select(age_group_2, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'age_group_2') %>%
  dplyr::mutate(variable="age_group_2", .before = 1 ) %>%
  dplyr::arrange(group)

## sex
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="sex"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="sex"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="sex"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="sex"]

prepandemic_BMI_sex <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "sex") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "sex") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "sex") %>%
  dplyr::select(sex, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'sex') %>%
  dplyr::mutate(variable="sex", .before = 1 ) %>%
  dplyr::arrange(group)

## region
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="region"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="region"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="region"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="region"]

prepandemic_BMI_region <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "region") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "region") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "region") %>%
  dplyr::select(region, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'region') %>%
  dplyr::mutate(variable="region", .before = 1 ) %>%
  dplyr::arrange(group)

## imd
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="imd"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="imd"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="imd"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="imd"]

prepandemic_BMI_imd <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "imd") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "imd") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "imd") %>%
  dplyr::select(imd, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'imd') %>%
  dplyr::mutate(variable="imd", .before = 1 ) %>%
  dplyr::arrange(group)

## ethnic_no_miss
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="ethnic_no_miss"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="ethnic_no_miss"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="ethnic_no_miss"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="ethnic_no_miss"]

prepandemic_BMI_ethnic_no_miss <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "ethnic_no_miss") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "ethnic_no_miss") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "ethnic_no_miss") %>%
  dplyr::select(ethnic_no_miss, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'ethnic_no_miss') %>%
  dplyr::mutate(variable="ethnic_no_miss", .before = 1 ) %>%
  dplyr::arrange(group)


## eth_group_16
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="eth_group_16"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="eth_group_16"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="eth_group_16"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="eth_group_16"]

prepandemic_BMI_eth_group_16 <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "eth_group_16") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "eth_group_16") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "eth_group_16") %>%
  dplyr::select(eth_group_16, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'eth_group_16') %>%
  dplyr::mutate(variable="eth_group_16", .before = 1 ) %>%
  dplyr::arrange(group)




## comorbid_hypertension
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="comorbid_hypertension"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="comorbid_hypertension"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="comorbid_hypertension"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="comorbid_hypertension"]

prepandemic_BMI_comorbid_hypertension <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "comorbid_hypertension") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "comorbid_hypertension") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "comorbid_hypertension") %>%
  dplyr::select(comorbid_hypertension, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'comorbid_hypertension') %>%
  dplyr::mutate(variable="comorbid_hypertension", .before = 1 ) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(group = as.character(group))

## comorbid_diabetes_t2
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="comorbid_diabetes_t2"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="comorbid_diabetes_t2"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="comorbid_diabetes_t2"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="comorbid_diabetes_t2"]

prepandemic_BMI_comorbid_diabetes_t2 <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "comorbid_diabetes_t2") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "comorbid_diabetes_t2") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "comorbid_diabetes_t2") %>%
  dplyr::select(comorbid_diabetes_t2, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'comorbid_diabetes_t2') %>%
  dplyr::mutate(variable="comorbid_diabetes_t2", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## comorbid_diabetes_t1
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="comorbid_diabetes_t1"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="comorbid_diabetes_t1"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="comorbid_diabetes_t1"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="comorbid_diabetes_t1"]

prepandemic_BMI_comorbid_diabetes_t1 <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "comorbid_diabetes_t1") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "comorbid_diabetes_t1") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "comorbid_diabetes_t1") %>%
  dplyr::select(comorbid_diabetes_t1, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'comorbid_diabetes_t1') %>%
  dplyr::mutate(variable="comorbid_diabetes_t1", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## learning_disability
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="learning_disability"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="learning_disability"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="learning_disability"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="learning_disability"]

prepandemic_BMI_learning_disability <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "learning_disability") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "learning_disability") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "learning_disability") %>%
  dplyr::select(learning_disability, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'learning_disability') %>%
  dplyr::mutate(variable="learning_disability", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## depression
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="depression"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="depression"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="depression"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="depression"]

prepandemic_BMI_depression <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "depression") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "depression") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "depression") %>%
  dplyr::select(depression, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'depression') %>%
  dplyr::mutate(variable="depression", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## dementia
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="dementia"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="dementia"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="dementia"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="dementia"]

prepandemic_BMI_dementia <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "dementia") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "dementia") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "dementia") %>%
  dplyr::select(dementia, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'dementia') %>%
  dplyr::mutate(variable="dementia", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## psychosis_schiz_bipolar
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="psychosis_schiz_bipolar"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="psychosis_schiz_bipolar"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="psychosis_schiz_bipolar"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="psychosis_schiz_bipolar"]

prepandemic_BMI_psychosis_schiz_bipolar <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "psychosis_schiz_bipolar") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "psychosis_schiz_bipolar") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "psychosis_schiz_bipolar") %>%
  dplyr::select(psychosis_schiz_bipolar, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'psychosis_schiz_bipolar') %>%
  dplyr::mutate(variable="psychosis_schiz_bipolar", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))


## COPD
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="COPD"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="COPD"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="COPD"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="COPD"]

prepandemic_BMI_COPD <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "COPD") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "COPD") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "COPD") %>%
  dplyr::select(COPD, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'COPD') %>%
  dplyr::mutate(variable="COPD", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))


## asthma
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="asthma"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="asthma"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="asthma"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="asthma"]

prepandemic_BMI_asthma <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "asthma") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "asthma") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "asthma") %>%
  dplyr::select(asthma, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'asthma') %>%
  dplyr::mutate(variable="asthma", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## chronic_cardiac
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="chronic_cardiac"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="chronic_cardiac"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="chronic_cardiac"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="chronic_cardiac"]

prepandemic_BMI_chronic_cardiac <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "chronic_cardiac") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "chronic_cardiac") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "chronic_cardiac") %>%
  dplyr::select(chronic_cardiac, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'chronic_cardiac') %>%
  dplyr::mutate(variable="chronic_cardiac", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## stroke_and_TIA
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="stroke_and_TIA"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="stroke_and_TIA"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="stroke_and_TIA"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="stroke_and_TIA"]

prepandemic_BMI_stroke_and_TIA <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "stroke_and_TIA") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "stroke_and_TIA") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "stroke_and_TIA") %>%
  dplyr::select(stroke_and_TIA, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'stroke_and_TIA') %>%
  dplyr::mutate(variable="stroke_and_TIA", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## all_cancer
N_prepandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change1)) , by="all_cancer"]
n_prepandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change1))) , by="all_cancer"]
mean_prepandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change1,  na.rm = TRUE))), by="all_cancer"]
sd_prepandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change1,  na.rm = TRUE))), by="all_cancer"]

prepandemic_BMI_all_cancer <- N_prepandemic_BMI %>%
  dplyr::left_join(n_prepandemic_BMI, by = "all_cancer") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_prepandemic_BMI, by = "all_cancer") %>%
  dplyr::left_join(sd_prepandemic_BMI, by = "all_cancer") %>%
  dplyr::select(all_cancer, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'all_cancer') %>%
  dplyr::mutate(variable="all_cancer", .before = 1 ) %>%
  dplyr::arrange(group)  %>% 
  dplyr::mutate(group = as.character(group))


prepandemic_bmi_change <- dplyr::bind_rows( bmi_change_all, 
                                            prepandemic_BMI_age_group,
                                            prepandemic_BMI_age_group_2,
                                            prepandemic_BMI_sex, 
                                            prepandemic_BMI_region, 
                                            prepandemic_BMI_imd, 
                                            prepandemic_BMI_ethnic_no_miss,
                                            prepandemic_BMI_eth_group_16,
                                            prepandemic_BMI_comorbid_hypertension, 
                                            prepandemic_BMI_comorbid_diabetes_t2,
                                            prepandemic_BMI_comorbid_diabetes_t1, 
                                            prepandemic_BMI_learning_disability,
                                            prepandemic_BMI_depression,
                                            prepandemic_BMI_dementia,
                                            prepandemic_BMI_psychosis_schiz_bipolar, 
                                            prepandemic_BMI_COPD, 
                                            prepandemic_BMI_asthma,
                                            prepandemic_BMI_chronic_cardiac, 
                                            prepandemic_BMI_stroke_and_TIA,
                                            prepandemic_BMI_all_cancer)

BMI_traj_DT <- setDT(BMI_trajectories)


N_all = (length(BMI_trajectories$yearly_bmi_change2) - sum(is.na(BMI_trajectories$yearly_bmi_change2)))
N_population = length(BMI_trajectories$yearly_bmi_change2)
mean_all = mean(BMI_trajectories$yearly_bmi_change2, na.rm = TRUE) 

bmi_change_all <- skimr::skim_without_charts(BMI_trajectories$yearly_bmi_change2)  %>% 
  dplyr::rename("mean_bmi_change" = "numeric.mean") %>% 
  dplyr::rename("sd_bmi_change" = "numeric.sd") %>% 
  dplyr::mutate ("N_population" = N_population) %>% 
  dplyr::mutate ("N" = N_all) %>% 
  dplyr::select("N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::mutate ('group' = 'all', .before = 1) %>%
  dplyr::mutate(variable="all", .before = 1 )



## age_group
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="age_group"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="age_group"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="age_group"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="age_group"]

postpandemic_BMI_age_group <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "age_group") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "age_group") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "age_group") %>%
  dplyr::select(age_group, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'age_group') %>%
  dplyr::mutate(variable="age_group", .before = 1 ) %>%
  dplyr::arrange(group)

## age_group_2
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="age_group_2"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="age_group_2"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="age_group_2"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="age_group_2"]

postpandemic_BMI_age_group_2 <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "age_group_2") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "age_group_2") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "age_group_2") %>%
  dplyr::select(age_group_2, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'age_group_2') %>%
  dplyr::mutate(variable="age_group_2", .before = 1 ) %>%
  dplyr::arrange(group)

## sex
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="sex"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="sex"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="sex"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="sex"]

postpandemic_BMI_sex <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "sex") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "sex") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "sex") %>%
  dplyr::select(sex, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'sex') %>%
  dplyr::mutate(variable="sex", .before = 1 ) %>%
  dplyr::arrange(group)

## region
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="region"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="region"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="region"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="region"]

postpandemic_BMI_region <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "region") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "region") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "region") %>%
  dplyr::select(region, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'region') %>%
  dplyr::mutate(variable="region", .before = 1 ) %>%
  dplyr::arrange(group)

## imd
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="imd"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="imd"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="imd"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="imd"]

postpandemic_BMI_imd <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "imd") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "imd") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "imd") %>%
  dplyr::select(imd, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'imd') %>%
  dplyr::mutate(variable="imd", .before = 1 ) %>%
  dplyr::arrange(group)

## ethnic_no_miss
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="ethnic_no_miss"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="ethnic_no_miss"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="ethnic_no_miss"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="ethnic_no_miss"]

postpandemic_BMI_ethnic_no_miss <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "ethnic_no_miss") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "ethnic_no_miss") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "ethnic_no_miss") %>%
  dplyr::select(ethnic_no_miss, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'ethnic_no_miss') %>%
  dplyr::mutate(variable="ethnic_no_miss", .before = 1 ) %>%
  dplyr::arrange(group)


## eth_group_16
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="eth_group_16"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="eth_group_16"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="eth_group_16"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="eth_group_16"]

postpandemic_BMI_eth_group_16 <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "eth_group_16") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "eth_group_16") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "eth_group_16") %>%
  dplyr::select(eth_group_16, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'eth_group_16') %>%
  dplyr::mutate(variable="eth_group_16", .before = 1 ) %>%
  dplyr::arrange(group)




## comorbid_hypertension
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="comorbid_hypertension"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="comorbid_hypertension"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="comorbid_hypertension"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="comorbid_hypertension"]

postpandemic_BMI_comorbid_hypertension <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "comorbid_hypertension") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "comorbid_hypertension") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "comorbid_hypertension") %>%
  dplyr::select(comorbid_hypertension, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'comorbid_hypertension') %>%
  dplyr::mutate(variable="comorbid_hypertension", .before = 1 ) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(group = as.character(group))

## comorbid_diabetes_t2
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="comorbid_diabetes_t2"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="comorbid_diabetes_t2"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="comorbid_diabetes_t2"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="comorbid_diabetes_t2"]

postpandemic_BMI_comorbid_diabetes_t2 <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "comorbid_diabetes_t2") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "comorbid_diabetes_t2") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "comorbid_diabetes_t2") %>%
  dplyr::select(comorbid_diabetes_t2, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'comorbid_diabetes_t2') %>%
  dplyr::mutate(variable="comorbid_diabetes_t2", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## comorbid_diabetes_t1
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="comorbid_diabetes_t1"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="comorbid_diabetes_t1"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="comorbid_diabetes_t1"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="comorbid_diabetes_t1"]

postpandemic_BMI_comorbid_diabetes_t1 <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "comorbid_diabetes_t1") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "comorbid_diabetes_t1") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "comorbid_diabetes_t1") %>%
  dplyr::select(comorbid_diabetes_t1, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'comorbid_diabetes_t1') %>%
  dplyr::mutate(variable="comorbid_diabetes_t1", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## learning_disability
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="learning_disability"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="learning_disability"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="learning_disability"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="learning_disability"]

postpandemic_BMI_learning_disability <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "learning_disability") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "learning_disability") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "learning_disability") %>%
  dplyr::select(learning_disability, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'learning_disability') %>%
  dplyr::mutate(variable="learning_disability", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## depression
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="depression"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="depression"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="depression"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="depression"]

postpandemic_BMI_depression <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "depression") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "depression") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "depression") %>%
  dplyr::select(depression, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'depression') %>%
  dplyr::mutate(variable="depression", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## dementia
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="dementia"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="dementia"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="dementia"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="dementia"]

postpandemic_BMI_dementia <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "dementia") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "dementia") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "dementia") %>%
  dplyr::select(dementia, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'dementia') %>%
  dplyr::mutate(variable="dementia", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## psychosis_schiz_bipolar
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="psychosis_schiz_bipolar"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="psychosis_schiz_bipolar"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="psychosis_schiz_bipolar"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="psychosis_schiz_bipolar"]

postpandemic_BMI_psychosis_schiz_bipolar <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "psychosis_schiz_bipolar") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "psychosis_schiz_bipolar") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "psychosis_schiz_bipolar") %>%
  dplyr::select(psychosis_schiz_bipolar, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'psychosis_schiz_bipolar') %>%
  dplyr::mutate(variable="psychosis_schiz_bipolar", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))


## COPD
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="COPD"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="COPD"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="COPD"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="COPD"]

postpandemic_BMI_COPD <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "COPD") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "COPD") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "COPD") %>%
  dplyr::select(COPD, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'COPD') %>%
  dplyr::mutate(variable="COPD", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))


## asthma
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="asthma"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="asthma"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="asthma"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="asthma"]

postpandemic_BMI_asthma <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "asthma") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "asthma") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "asthma") %>%
  dplyr::select(asthma, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'asthma') %>%
  dplyr::mutate(variable="asthma", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## chronic_cardiac
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="chronic_cardiac"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="chronic_cardiac"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="chronic_cardiac"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="chronic_cardiac"]

postpandemic_BMI_chronic_cardiac <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "chronic_cardiac") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "chronic_cardiac") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "chronic_cardiac") %>%
  dplyr::select(chronic_cardiac, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'chronic_cardiac') %>%
  dplyr::mutate(variable="chronic_cardiac", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## stroke_and_TIA
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="stroke_and_TIA"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="stroke_and_TIA"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="stroke_and_TIA"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="stroke_and_TIA"]

postpandemic_BMI_stroke_and_TIA <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "stroke_and_TIA") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "stroke_and_TIA") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "stroke_and_TIA") %>%
  dplyr::select(stroke_and_TIA, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'stroke_and_TIA') %>%
  dplyr::mutate(variable="stroke_and_TIA", .before = 1 ) %>%
  dplyr::arrange(group) %>% 
  dplyr::mutate(group = as.character(group))

## all_cancer
N_postpandemic_BMI <- BMI_traj_DT [, .(N_population = length(yearly_bmi_change2)) , by="all_cancer"]
n_postpandemic_BMI <- BMI_traj_DT [, .(N_missing = sum(is.na(yearly_bmi_change2))) , by="all_cancer"]
mean_postpandemic_BMI <- BMI_traj_DT[, .( mean_bmi_change = (mean(yearly_bmi_change2,  na.rm = TRUE))), by="all_cancer"]
sd_postpandemic_BMI <- BMI_traj_DT[, .( sd_bmi_change = (sd(yearly_bmi_change2,  na.rm = TRUE))), by="all_cancer"]

postpandemic_BMI_all_cancer <- N_postpandemic_BMI %>%
  dplyr::left_join(n_postpandemic_BMI, by = "all_cancer") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(mean_postpandemic_BMI, by = "all_cancer") %>%
  dplyr::left_join(sd_postpandemic_BMI, by = "all_cancer") %>%
  dplyr::select(all_cancer, "N", N_population, mean_bmi_change, sd_bmi_change) %>%
  dplyr::rename('group' = 'all_cancer') %>%
  dplyr::mutate(variable="all_cancer", .before = 1 ) %>%
  dplyr::arrange(group)  %>% 
  dplyr::mutate(group = as.character(group))


postpandemic_bmi_change <- dplyr::bind_rows( bmi_change_all, 
                                            postpandemic_BMI_age_group,
                                            postpandemic_BMI_age_group_2,
                                            postpandemic_BMI_sex, 
                                            postpandemic_BMI_region, 
                                            postpandemic_BMI_imd, 
                                            postpandemic_BMI_ethnic_no_miss,
                                            postpandemic_BMI_eth_group_16,
                                            postpandemic_BMI_comorbid_hypertension, 
                                            postpandemic_BMI_comorbid_diabetes_t2,
                                            postpandemic_BMI_comorbid_diabetes_t1, 
                                            postpandemic_BMI_learning_disability,
                                            postpandemic_BMI_depression,
                                            postpandemic_BMI_dementia,
                                            postpandemic_BMI_psychosis_schiz_bipolar, 
                                            postpandemic_BMI_COPD, 
                                            postpandemic_BMI_asthma,
                                            postpandemic_BMI_chronic_cardiac, 
                                            postpandemic_BMI_stroke_and_TIA,
                                            postpandemic_BMI_all_cancer)

write.csv (postpandemic_bmi_change, here::here ("output/data","postpandemic_bmi_change_per_year.csv"))

write.csv (prepandemic_bmi_change, here::here ("output/data","prepandemic_bmi_change_per_year.csv"))
