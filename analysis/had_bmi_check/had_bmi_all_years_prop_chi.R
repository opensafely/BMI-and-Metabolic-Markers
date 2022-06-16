#### Author: M Samuel
#### Date: 15th June
####  This script looks at the odds of having a BMI in different groups


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
library(skimr)

BMI_median_2 <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

BMI_complete_categories <- BMI_median_2 %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2019"|year == "2020"| year == "2021" ) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group_2 = as.factor(age_group_2)) %>%
  dplyr::mutate(age_group_2 = fct_relevel(age_group_2, "18-29", "30-39", "40-49", "50-59", "60-69", "70-79","80+")) %>% 
  dplyr::mutate(sex = fct_relevel(sex, 'F', "M"))



BMI_DT <- as.data.table(BMI_complete_categories)
## Analysis by sex

#1. look at proportion who have had BMI by year,  for each group of sex

prop_sex <- BMI_DT[, tabyl(.SD, year, had_bmi), by=sex] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "sex") %>% 
  dplyr::rename(group = sex)


#2. look at chi squared test of odds of difference between groups

test_sex <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=sex]  %>% 
  dplyr::mutate(variable="sex") %>% 
  dplyr::rename(group=sex)



## Analysis by age_group_2

#1. look at proportion who have had BMI by year,  for each group of age_group_2

prop_age_group_2 <- BMI_DT[, tabyl(.SD, year, had_bmi), by=age_group_2] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "age_group_2") %>% 
  dplyr::rename(group = age_group_2)


#2. look at chi squared test of odds of difference between groups

test_age_group_2 <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=age_group_2]  %>% 
  dplyr::mutate(variable="age_group_2") %>% 
  dplyr::rename(group=age_group_2)



## Analysis by imd

#1. look at proportion who have had BMI by year,  for each group of imd

prop_imd <- BMI_DT[, tabyl(.SD, year, had_bmi), by=imd] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "imd") %>% 
  dplyr::rename(group = imd)


#2. look at chi squared test of odds of difference between groups

test_imd <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=imd]  %>% 
  dplyr::mutate(variable="imd") %>% 
  dplyr::rename(group=imd)



## Analysis by eth_group_16

#1. look at proportion who have had BMI by year,  for each group of eth_group_16

prop_eth_group_16 <- BMI_DT[, tabyl(.SD, year, had_bmi), by=eth_group_16] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "eth_group_16") %>% 
  dplyr::rename(group = eth_group_16)


#2. look at chi squared test of odds of difference between groups

test_eth_group_16 <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=eth_group_16]  %>% 
  dplyr::mutate(variable="eth_group_16") %>% 
  dplyr::rename(group=eth_group_16)



## Analysis by region

#1. look at proportion who have had BMI by year,  for each group of region

prop_region <- BMI_DT[, tabyl(.SD, year, had_bmi), by=region] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "region") %>% 
  dplyr::rename(group = region)


#2. look at chi squared test of odds of difference between groups

test_region <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=region]  %>% 
  dplyr::mutate(variable="region") %>% 
  dplyr::rename(group=region)



## Analysis by comorbid_hypertension

#1. look at proportion who have had BMI by year,  for each group of comorbid_hypertension

prop_comorbid_hypertension <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_hypertension] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_hypertension") %>% 
  dplyr::rename(group = comorbid_hypertension) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_hypertension <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_hypertension]  %>% 
  dplyr::mutate(variable="comorbid_hypertension") %>% 
  dplyr::rename(group=comorbid_hypertension)



## Analysis by comorbid_diabetes_t2

#1. look at proportion who have had BMI by year,  for each group of comorbid_diabetes_t2

prop_comorbid_diabetes_t2 <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_diabetes_t2] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_diabetes_t2") %>% 
  dplyr::rename(group = comorbid_diabetes_t2) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_diabetes_t2 <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_diabetes_t2]  %>% 
  dplyr::mutate(variable="comorbid_diabetes_t2") %>% 
  dplyr::rename(group=comorbid_diabetes_t2)




## Analysis by comorbid_diabetes_t1

#1. look at proportion who have had BMI by year,  for each group of comorbid_diabetes_t1

prop_comorbid_diabetes_t1 <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_diabetes_t1] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_diabetes_t1") %>% 
  dplyr::rename(group = comorbid_diabetes_t1) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_diabetes_t1 <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_diabetes_t1]  %>% 
  dplyr::mutate(variable="comorbid_diabetes_t1") %>% 
  dplyr::rename(group=comorbid_diabetes_t1)




## Analysis by comorbid_chronic_cardiac

#1. look at proportion who have had BMI by year,  for each group of comorbid_chronic_cardiac

prop_comorbid_chronic_cardiac <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_chronic_cardiac] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_chronic_cardiac") %>% 
  dplyr::rename(group = comorbid_chronic_cardiac) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_chronic_cardiac <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_chronic_cardiac]  %>% 
  dplyr::mutate(variable="comorbid_chronic_cardiac") %>% 
  dplyr::rename(group=comorbid_chronic_cardiac)



## Analysis by comorbid_learning_disability

#1. look at proportion who have had BMI by year,  for each group of comorbid_learning_disability

prop_comorbid_learning_disability <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_learning_disability] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_learning_disability") %>% 
  dplyr::rename(group = comorbid_learning_disability) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_learning_disability <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_learning_disability]  %>% 
  dplyr::mutate(variable="comorbid_learning_disability") %>% 
  dplyr::rename(group=comorbid_learning_disability)


## Analysis by comorbid_psychosis_schiz_bipolar

#1. look at proportion who have had BMI by year,  for each group of comorbid_psychosis_schiz_bipolar

prop_comorbid_psychosis_schiz_bipolar <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_psychosis_schiz_bipolar] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_psychosis_schiz_bipolar") %>% 
  dplyr::rename(group = comorbid_psychosis_schiz_bipolar) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_psychosis_schiz_bipolar <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_psychosis_schiz_bipolar]  %>% 
  dplyr::mutate(variable="comorbid_psychosis_schiz_bipolar") %>% 
  dplyr::rename(group=comorbid_psychosis_schiz_bipolar)



## Analysis by comorbid_depression

#1. look at proportion who have had BMI by year,  for each group of comorbid_depression

prop_comorbid_depression <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_depression] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_depression") %>% 
  dplyr::rename(group = comorbid_depression) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_depression <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_depression]  %>% 
  dplyr::mutate(variable="comorbid_depression") %>% 
  dplyr::rename(group=comorbid_depression)



## Analysis by comorbid_asthma

#1. look at proportion who have had BMI by year,  for each group of comorbid_asthma

prop_comorbid_asthma <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_asthma] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_asthma") %>% 
  dplyr::rename(group = comorbid_asthma) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_asthma <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_asthma]  %>% 
  dplyr::mutate(variable="comorbid_asthma") %>% 
  dplyr::rename(group=comorbid_asthma)



## Analysis by comorbid_COPD

#1. look at proportion who have had BMI by year,  for each group of comorbid_COPD

prop_comorbid_COPD <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_COPD] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_COPD") %>% 
  dplyr::rename(group = comorbid_COPD) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_COPD <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_COPD]  %>% 
  dplyr::mutate(variable="comorbid_COPD") %>% 
  dplyr::rename(group=comorbid_COPD)





## Analysis by comorbid_dementia

#1. look at proportion who have had BMI by year,  for each group of comorbid_dementia

prop_comorbid_dementia <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_dementia] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_dementia") %>% 
  dplyr::rename(group = comorbid_dementia) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_dementia <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_dementia]  %>% 
  dplyr::mutate(variable="comorbid_dementia") %>% 
  dplyr::rename(group=comorbid_dementia)




## Analysis by comorbid_stroke_and_TIA

#1. look at proportion who have had BMI by year,  for each group of comorbid_stroke_and_TIA

prop_comorbid_stroke_and_TIA <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_stroke_and_TIA] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_stroke_and_TIA") %>% 
  dplyr::rename(group = comorbid_stroke_and_TIA) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_stroke_and_TIA <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_stroke_and_TIA]  %>% 
  dplyr::mutate(variable="comorbid_stroke_and_TIA") %>% 
  dplyr::rename(group=comorbid_stroke_and_TIA)



## Analysis by comorbid_all_cancer

#1. look at proportion who have had BMI by year,  for each group of comorbid_all_cancer

prop_comorbid_all_cancer <- BMI_DT[, tabyl(.SD, year, had_bmi), by=comorbid_all_cancer] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "comorbid_all_cancer") %>% 
  dplyr::rename(group = comorbid_all_cancer) %>%
  dplyr::mutate(group = as.character(group))


#2. look at chi squared test of odds of difference between groups

test_comorbid_all_cancer <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=comorbid_all_cancer]  %>% 
  dplyr::mutate(variable="comorbid_all_cancer") %>% 
  dplyr::rename(group=comorbid_all_cancer)


## WHOLE POPULATION

#1. look at proportion who have had BMI by year, 

prop_all <- BMI_DT[, tabyl(.SD, year, had_bmi)] %>% 
  dplyr::mutate(total = .[[2]] + .[[3]]) %>% 
  dplyr::mutate(proportion = .[[3]]/.[[4]])  %>% 
  dplyr::mutate(variable = "all") %>% 
  dplyr::mutate(group = "all")


#2. look at chi squared test of odds of difference between groups

test_all <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi)))]  %>% 
  dplyr::mutate(variable="all") %>% 
  dplyr::mutate(group= "all")


prop_all <- prop_all %>% 
  bind_rows(prop_age_group_2) %>%
  bind_rows(prop_sex) %>%
  bind_rows(prop_imd) %>%
  bind_rows(prop_eth_group_16) %>%
  bind_rows(prop_region) %>%
  bind_rows(prop_comorbid_diabetes_t2) %>%
  bind_rows(prop_comorbid_diabetes_t1) %>%
  bind_rows(prop_comorbid_hypertension) %>%
  bind_rows(prop_comorbid_chronic_cardiac) %>%
  bind_rows(prop_comorbid_learning_disability) %>%
  bind_rows(prop_comorbid_psychosis_schiz_bipolar) %>%
  bind_rows(prop_comorbid_depression) %>%
  bind_rows(prop_comorbid_asthma) %>%
  bind_rows(prop_comorbid_COPD) %>%
  bind_rows(prop_comorbid_dementia) %>%
  bind_rows(prop_comorbid_stroke_and_TIA) %>%
  bind_rows(prop_comorbid_all_cancer)  

prop_all <- prop_all %>% 
 dplyr::rename(had_bmi = "TRUE") %>% 
  dplyr::rename(no_bmi = "FALSE")  %>% 
  dplyr::rename(prop = proportion)

prop_all <- prop_all %>% 
  dplyr::mutate (stand_err = (sqrt(prop*(1-prop)/total)))


prop_all <- prop_all %>% 
  dplyr::mutate(had_bmi = plyr::round_any(prop_all$had_bmi, 5))  %>% 
  dplyr::mutate(no_bmi = plyr::round_any(prop_all$no_bmi, 5))  %>% 
  dplyr::mutate(total = plyr::round_any(prop_all$total, 5)) %>% 
  dplyr::mutate((across(where(is.numeric), round, 5))) 



test_all <- test_all %>% 
  rbind(test_age_group_2) %>%
  rbind(test_sex) %>%
  rbind(test_imd) %>%
  rbind(test_eth_group_16) %>%
  rbind(test_region) %>%
  rbind(test_comorbid_diabetes_t2) %>%
  rbind(test_comorbid_diabetes_t1) %>%
  rbind(test_comorbid_hypertension) %>%
  rbind(test_comorbid_chronic_cardiac) %>%
  rbind(test_comorbid_learning_disability) %>%
  rbind(test_comorbid_psychosis_schiz_bipolar) %>%
  rbind(test_comorbid_depression) %>%
  rbind(test_comorbid_asthma) %>%
  rbind(test_comorbid_COPD) %>%
  rbind(test_comorbid_dementia) %>%
  rbind(test_comorbid_stroke_and_TIA) %>%
  rbind(test_comorbid_all_cancer) 


## save outputs
prop_all
write.csv (prop_all, here::here ("output/data","proportion_had_bmi_2019_2021.csv"))
write.csv (test_all, here::here ("output/data","proportion_had_bmi_2019_2021_chisquare.csv"))




