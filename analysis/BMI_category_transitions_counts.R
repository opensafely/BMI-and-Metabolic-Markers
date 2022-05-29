## Load libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)
library(lubridate)
library(skimr)
library(dplyr)

data <- read_feather (here::here ("output/data", "BMI_category_transition.feather"))

data <- data %>%
   rename_all(~stringr::str_replace(.,"^comorbid_",""))


##  Add category for severely obese 

# 1. For 1st BMI
categories_change <- data %>% 
  dplyr::mutate(category_1a = bmi_1)

categories_change$category_1a <- cut(categories_change$category_1a, 
                                    breaks=c(0, 20,25,30,35,1000),
                                    labels= c("underweight", "healthy", "overweight", "obese", "severely obese"))


# 2. For 2nd BMI
categories_change <- categories_change %>% 
  dplyr::mutate(category_2a = bmi_2)

categories_change$category_2a <- cut(categories_change$category_2a, 
                                     breaks=c(0, 20,25,30,35,1000),
                                     labels= c("underweight", "healthy", "overweight", "obese", "severely obese"))



categories_change$age_group_2 <- factor(categories_change$age_group_2,      # Reordering group factor levels
                                                   levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))





### HEALTHY TO OVERWEIGHT/OBESE TRANSITIONS BY EXPOSURE GROUPS
normal_weight <- categories_change %>% 
  dplyr::filter(category_1 =="healthy")



## variable to show if normal became overweight/obese

normal_weight <- normal_weight %>% 
  dplyr::mutate(transition_bmi25 = healthy_cat_change)

normal_weight$transition_bmi25[normal_weight$transition_bmi25==1] <- 0
normal_weight$transition_bmi25[normal_weight$transition_bmi25==2] <- 1
normal_weight$transition_bmi25[normal_weight$transition_bmi25==3] <- 1

normal_weight %>% 
  tabyl(healthy_cat_change, transition_bmi25)


normal_weight_stats_function <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data %>%
    tabyl({{var}}, transition_bmi25) %>% 
    adorn_totals() %>%
    dplyr::rename(weightgain = 3) %>% 
    dplyr::rename(stable_weight = 2) %>% 
    dplyr::mutate(N_total = stable_weight + weightgain) %>% 
    dplyr::mutate(percent = weightgain/N_total) %>% 
    dplyr::select(-(stable_weight)) %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
    ungroup()%>%
    dplyr::mutate(weightgain = plyr::round_any(weightgain, 5)) %>% 
    dplyr::mutate(N_total = plyr::round_any(N_total, 5))  %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(variable = (v1))
}

age_group_2 <- normal_weight_stats_function (normal_weight, age_group_2)
sex <- normal_weight_stats_function(normal_weight, sex)
ethnic_no_miss <- normal_weight_stats_function(normal_weight, ethnic_no_miss)
eth_group_16 <- normal_weight_stats_function(normal_weight, eth_group_16)
imd <- normal_weight_stats_function(normal_weight, imd)
region <- normal_weight_stats_function(normal_weight, region)
hypertension <- normal_weight_stats_function(normal_weight, hypertension)
diabetes_t1 <- normal_weight_stats_function(normal_weight, diabetes_t1)
diabetes_t2 <- normal_weight_stats_function(normal_weight, diabetes_t2)
chronic_cardiac <- normal_weight_stats_function(normal_weight, chronic_cardiac)
learning_disability <- normal_weight_stats_function(normal_weight, learning_disability)
depression <- normal_weight_stats_function(normal_weight, depression)
dementia <- normal_weight_stats_function(normal_weight, dementia)
psychosis_schiz_bipolar <- normal_weight_stats_function(normal_weight, psychosis_schiz_bipolar)
asthma <- normal_weight_stats_function(normal_weight, asthma)
COPD <- normal_weight_stats_function(normal_weight, COPD)
stroke_and_TIA <- normal_weight_stats_function(normal_weight, stroke_and_TIA)
all_cancer <- normal_weight_stats_function(normal_weight, all_cancer)
smoking_status <- normal_weight_stats_function(normal_weight, smoking_status)
year <- normal_weight_stats_function(normal_weight, year)


counts_normal_weight_all <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  bind_rows(year)  %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "all", .before=1)


## For 1st BMI measured in  2017 and 2019 data

normal_weight_2017 <- normal_weight %>% 
  dplyr::filter(year == 2017)



age_group_2 <- normal_weight_stats_function (normal_weight_2017, age_group_2)
sex <- normal_weight_stats_function(normal_weight_2017, sex)
ethnic_no_miss <- normal_weight_stats_function(normal_weight_2017, ethnic_no_miss)
eth_group_16 <- normal_weight_stats_function(normal_weight_2017, eth_group_16)
imd <- normal_weight_stats_function(normal_weight_2017, imd)
region <- normal_weight_stats_function(normal_weight_2017, region)
hypertension <- normal_weight_stats_function(normal_weight_2017, hypertension)
diabetes_t1 <- normal_weight_stats_function(normal_weight_2017, diabetes_t1)
diabetes_t2 <- normal_weight_stats_function(normal_weight_2017, diabetes_t2)
chronic_cardiac <- normal_weight_stats_function(normal_weight_2017, chronic_cardiac)
learning_disability <- normal_weight_stats_function(normal_weight_2017, learning_disability)
depression <- normal_weight_stats_function(normal_weight_2017, depression)
dementia <- normal_weight_stats_function(normal_weight_2017, dementia)
psychosis_schiz_bipolar <- normal_weight_stats_function(normal_weight_2017, psychosis_schiz_bipolar)
asthma <- normal_weight_stats_function(normal_weight_2017, asthma)
COPD <- normal_weight_stats_function(normal_weight_2017, COPD)
stroke_and_TIA <- normal_weight_stats_function(normal_weight_2017, stroke_and_TIA)
all_cancer <- normal_weight_stats_function(normal_weight_2017, all_cancer)
smoking_status <- normal_weight_stats_function(normal_weight_2017, smoking_status)
year <- normal_weight_stats_function(normal_weight_2017, year)


counts_normal_weight_2017 <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>%
  dplyr::mutate(year= "2017", .before=1)


## For 1st BMI measured in  2017 and 2019 data

normal_weight_2019 <- normal_weight %>% 
  dplyr::filter(year == 2019)


age_group_2 <- normal_weight_stats_function (normal_weight_2019, age_group_2)
sex <- normal_weight_stats_function(normal_weight_2019, sex)
ethnic_no_miss <- normal_weight_stats_function(normal_weight_2019, ethnic_no_miss)
eth_group_16 <- normal_weight_stats_function(normal_weight_2019, eth_group_16)
imd <- normal_weight_stats_function(normal_weight_2019, imd)
region <- normal_weight_stats_function(normal_weight_2019, region)
hypertension <- normal_weight_stats_function(normal_weight_2019, hypertension)
diabetes_t1 <- normal_weight_stats_function(normal_weight_2019, diabetes_t1)
diabetes_t2 <- normal_weight_stats_function(normal_weight_2019, diabetes_t2)
chronic_cardiac <- normal_weight_stats_function(normal_weight_2019, chronic_cardiac)
learning_disability <- normal_weight_stats_function(normal_weight_2019, learning_disability)
depression <- normal_weight_stats_function(normal_weight_2019, depression)
dementia <- normal_weight_stats_function(normal_weight_2019, dementia)
psychosis_schiz_bipolar <- normal_weight_stats_function(normal_weight_2019, psychosis_schiz_bipolar)
asthma <- normal_weight_stats_function(normal_weight_2019, asthma)
COPD <- normal_weight_stats_function(normal_weight_2019, COPD)
stroke_and_TIA <- normal_weight_stats_function(normal_weight_2019, stroke_and_TIA)
all_cancer <- normal_weight_stats_function(normal_weight_2019, all_cancer)
smoking_status <- normal_weight_stats_function(normal_weight_2019, smoking_status)
year <- normal_weight_stats_function(normal_weight_2019, year)


counts_normal_weight_2019 <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>%
  dplyr::mutate(year= "2019", .before=1)


counts_normal_weight_all <- counts_normal_weight_all %>% 
  bind_rows(counts_normal_weight_2017) %>% 
  bind_rows(counts_normal_weight_2019)


### OVERWEIGHT TO OBESE TRANSITION
########### OVERWEIGHT TO OBESE
## models to predict variable association with transitions
overweight <- categories_change %>% 
  dplyr::filter(category_1 =="overweight")



## variable to show if 2nd BMI <25

overweight <- overweight %>% 
  dplyr::mutate(bmi2_under25= as.character(category_2))


overweight$bmi2_under25[overweight$bmi2_under25 == "obese"] <- 0
overweight$bmi2_under25[overweight$bmi2_under25 == "overweight" ] <- 0
overweight$bmi2_under25[overweight$bmi2_under25 == "healthy" ] <- 1
overweight$bmi2_under25[overweight$bmi2_under25 == "underweight" ] <- 1


overweight %>% 
  tabyl(category_2, bmi2_under25)





### function overweight to obese counts

overweight_to_obese_function <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data %>%
    tabyl({{var}}, become_obese) %>% 
    adorn_totals() %>%
    dplyr::rename(weightgain = 3) %>%       ## rename identified by column number in function
    dplyr::rename(stable_weight = 2) %>% 
    dplyr::mutate(N_total = stable_weight + weightgain) %>% 
    dplyr::mutate(percent = weightgain/N_total) %>% 
    dplyr::select(-(stable_weight)) %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
    ungroup()%>%
    dplyr::mutate(weightgain = plyr::round_any(weightgain, 5)) %>% 
    dplyr::mutate(N_total = plyr::round_any(N_total, 5))  %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(variable = (v1))
}

age_group_2 <- overweight_to_obese_function (overweight, age_group_2)
sex <- overweight_to_obese_function(overweight, sex)
ethnic_no_miss <- overweight_to_obese_function(overweight, ethnic_no_miss)
eth_group_16 <- overweight_to_obese_function(overweight, eth_group_16)
imd <- overweight_to_obese_function(overweight, imd)
region <- overweight_to_obese_function(overweight, region)
hypertension <- overweight_to_obese_function(overweight, hypertension)
diabetes_t1 <- overweight_to_obese_function(overweight, diabetes_t1)
diabetes_t2 <- overweight_to_obese_function(overweight, diabetes_t2)
chronic_cardiac <- overweight_to_obese_function(overweight, chronic_cardiac)
learning_disability <- overweight_to_obese_function(overweight, learning_disability)
depression <- overweight_to_obese_function(overweight, depression)
dementia <- overweight_to_obese_function(overweight, dementia)
psychosis_schiz_bipolar <- overweight_to_obese_function(overweight, psychosis_schiz_bipolar)
asthma <- overweight_to_obese_function(overweight, asthma)
COPD <- overweight_to_obese_function(overweight, COPD)
stroke_and_TIA <- overweight_to_obese_function(overweight, stroke_and_TIA)
all_cancer <- overweight_to_obese_function(overweight, all_cancer)
smoking_status <- overweight_to_obese_function(overweight, smoking_status)
year <- overweight_to_obese_function(overweight, year)


counts_overweight_obese_all <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  bind_rows(year)  %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "all", .before=1)


### overweight 2017 data
overweight_2017 <- overweight %>% 
  dplyr::filter(year == 2017)


age_group_2 <- overweight_to_obese_function (overweight_2017, age_group_2)
sex <- overweight_to_obese_function(overweight_2017, sex)
ethnic_no_miss <- overweight_to_obese_function(overweight_2017, ethnic_no_miss)
eth_group_16 <- overweight_to_obese_function(overweight_2017, eth_group_16)
imd <- overweight_to_obese_function(overweight_2017, imd)
region <- overweight_to_obese_function(overweight_2017, region)
hypertension <- overweight_to_obese_function(overweight_2017, hypertension)
diabetes_t1 <- overweight_to_obese_function(overweight_2017, diabetes_t1)
diabetes_t2 <- overweight_to_obese_function(overweight_2017, diabetes_t2)
chronic_cardiac <- overweight_to_obese_function(overweight_2017, chronic_cardiac)
learning_disability <- overweight_to_obese_function(overweight_2017, learning_disability)
depression <- overweight_to_obese_function(overweight_2017, depression)
dementia <- overweight_to_obese_function(overweight_2017, dementia)
psychosis_schiz_bipolar <- overweight_to_obese_function(overweight_2017, psychosis_schiz_bipolar)
asthma <- overweight_to_obese_function(overweight_2017, asthma)
COPD <- overweight_to_obese_function(overweight_2017, COPD)
stroke_and_TIA <- overweight_to_obese_function(overweight_2017, stroke_and_TIA)
all_cancer <- overweight_to_obese_function(overweight_2017, all_cancer)
smoking_status <- overweight_to_obese_function(overweight_2017, smoking_status)



counts_overweight_obese_2017 <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "2017", .before=1)

### overweight 2019 data
overweight_2019 <- overweight %>% 
  dplyr::filter(year == 2019)


age_group_2 <- overweight_to_obese_function (overweight_2019, age_group_2)
sex <- overweight_to_obese_function(overweight_2019, sex)
ethnic_no_miss <- overweight_to_obese_function(overweight_2019, ethnic_no_miss)
eth_group_16 <- overweight_to_obese_function(overweight_2019, eth_group_16)
imd <- overweight_to_obese_function(overweight_2019, imd)
region <- overweight_to_obese_function(overweight_2019, region)
hypertension <- overweight_to_obese_function(overweight_2019, hypertension)
diabetes_t1 <- overweight_to_obese_function(overweight_2019, diabetes_t1)
diabetes_t2 <- overweight_to_obese_function(overweight_2019, diabetes_t2)
chronic_cardiac <- overweight_to_obese_function(overweight_2019, chronic_cardiac)
learning_disability <- overweight_to_obese_function(overweight_2019, learning_disability)
depression <- overweight_to_obese_function(overweight_2019, depression)
dementia <- overweight_to_obese_function(overweight_2019, dementia)
psychosis_schiz_bipolar <- overweight_to_obese_function(overweight_2019, psychosis_schiz_bipolar)
asthma <- overweight_to_obese_function(overweight_2019, asthma)
COPD <- overweight_to_obese_function(overweight_2019, COPD)
stroke_and_TIA <- overweight_to_obese_function(overweight_2019, stroke_and_TIA)
all_cancer <- overweight_to_obese_function(overweight_2019, all_cancer)
smoking_status <- overweight_to_obese_function(overweight_2019, smoking_status)



counts_overweight_obese_2019 <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "2019", .before=1)

counts_overweight_obese_all <- counts_overweight_obese_all %>% 
  bind_rows(counts_overweight_obese_2017) %>% 
  bind_rows(counts_overweight_obese_2019)




### function overweight to weighloss

overweight_weightloss_function <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data %>%
    tabyl({{var}}, bmi2_under25) %>% 
    adorn_totals() %>%
    dplyr::rename(weightgain = 3) %>%       ## rename identified by column number in function
    dplyr::rename(stable_weight = 2) %>% 
    dplyr::mutate(N_total = stable_weight + weightgain) %>% 
    dplyr::mutate(percent = weightgain/N_total) %>% 
    dplyr::select(-(stable_weight)) %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
    ungroup()%>%
    dplyr::mutate(weightgain = plyr::round_any(weightgain, 5)) %>% 
    dplyr::mutate(N_total = plyr::round_any(N_total, 5))  %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(variable = (v1))
}

age_group_2 <- overweight_weightloss_function (overweight, age_group_2)
sex <- overweight_weightloss_function(overweight, sex)
ethnic_no_miss <- overweight_weightloss_function(overweight, ethnic_no_miss)
eth_group_16 <- overweight_weightloss_function(overweight, eth_group_16)
imd <- overweight_weightloss_function(overweight, imd)
region <- overweight_weightloss_function(overweight, region)
hypertension <- overweight_weightloss_function(overweight, hypertension)
diabetes_t1 <- overweight_weightloss_function(overweight, diabetes_t1)
diabetes_t2 <- overweight_weightloss_function(overweight, diabetes_t2)
chronic_cardiac <- overweight_weightloss_function(overweight, chronic_cardiac)
learning_disability <- overweight_weightloss_function(overweight, learning_disability)
depression <- overweight_weightloss_function(overweight, depression)
dementia <- overweight_weightloss_function(overweight, dementia)
psychosis_schiz_bipolar <- overweight_weightloss_function(overweight, psychosis_schiz_bipolar)
asthma <- overweight_weightloss_function(overweight, asthma)
COPD <- overweight_weightloss_function(overweight, COPD)
stroke_and_TIA <- overweight_weightloss_function(overweight, stroke_and_TIA)
all_cancer <- overweight_weightloss_function(overweight, all_cancer)
smoking_status <- overweight_weightloss_function(overweight, smoking_status)
year <- overweight_weightloss_function(overweight, year)


counts_overweight_weightloss_all <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  bind_rows(year)  %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "all", .before=1)


### overweight 2017 data
overweight_2017 <- overweight %>% 
  dplyr::filter(year == 2017)


age_group_2 <- overweight_weightloss_function (overweight_2017, age_group_2)
sex <- overweight_weightloss_function(overweight_2017, sex)
ethnic_no_miss <- overweight_weightloss_function(overweight_2017, ethnic_no_miss)
eth_group_16 <- overweight_weightloss_function(overweight_2017, eth_group_16)
imd <- overweight_weightloss_function(overweight_2017, imd)
region <- overweight_weightloss_function(overweight_2017, region)
hypertension <- overweight_weightloss_function(overweight_2017, hypertension)
diabetes_t1 <- overweight_weightloss_function(overweight_2017, diabetes_t1)
diabetes_t2 <- overweight_weightloss_function(overweight_2017, diabetes_t2)
chronic_cardiac <- overweight_weightloss_function(overweight_2017, chronic_cardiac)
learning_disability <- overweight_weightloss_function(overweight_2017, learning_disability)
depression <- overweight_weightloss_function(overweight_2017, depression)
dementia <- overweight_weightloss_function(overweight_2017, dementia)
psychosis_schiz_bipolar <- overweight_weightloss_function(overweight_2017, psychosis_schiz_bipolar)
asthma <- overweight_weightloss_function(overweight_2017, asthma)
COPD <- overweight_weightloss_function(overweight_2017, COPD)
stroke_and_TIA <- overweight_weightloss_function(overweight_2017, stroke_and_TIA)
all_cancer <- overweight_weightloss_function(overweight_2017, all_cancer)
smoking_status <- overweight_weightloss_function(overweight_2017, smoking_status)



counts_overweight_weightloss_2017 <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "2017", .before=1)

### overweight 2019 data
overweight_2019 <- overweight %>% 
  dplyr::filter(year == 2019)


age_group_2 <- overweight_weightloss_function (overweight_2019, age_group_2)
sex <- overweight_weightloss_function(overweight_2019, sex)
ethnic_no_miss <- overweight_weightloss_function(overweight_2019, ethnic_no_miss)
eth_group_16 <- overweight_weightloss_function(overweight_2019, eth_group_16)
imd <- overweight_weightloss_function(overweight_2019, imd)
region <- overweight_weightloss_function(overweight_2019, region)
hypertension <- overweight_weightloss_function(overweight_2019, hypertension)
diabetes_t1 <- overweight_weightloss_function(overweight_2019, diabetes_t1)
diabetes_t2 <- overweight_weightloss_function(overweight_2019, diabetes_t2)
chronic_cardiac <- overweight_weightloss_function(overweight_2019, chronic_cardiac)
learning_disability <- overweight_weightloss_function(overweight_2019, learning_disability)
depression <- overweight_weightloss_function(overweight_2019, depression)
dementia <- overweight_weightloss_function(overweight_2019, dementia)
psychosis_schiz_bipolar <- overweight_weightloss_function(overweight_2019, psychosis_schiz_bipolar)
asthma <- overweight_weightloss_function(overweight_2019, asthma)
COPD <- overweight_weightloss_function(overweight_2019, COPD)
stroke_and_TIA <- overweight_weightloss_function(overweight_2019, stroke_and_TIA)
all_cancer <- overweight_weightloss_function(overweight_2019, all_cancer)
smoking_status <- overweight_weightloss_function(overweight_2019, smoking_status)



counts_overweight_weightloss_2019 <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "2019", .before=1)

counts_overweight_weightloss_all <- counts_overweight_weightloss_all %>% 
  bind_rows(counts_overweight_weightloss_2017) %>% 
  bind_rows(counts_overweight_weightloss_2019)




### Obese weightloss counts
obese <- categories_change %>% 
  dplyr::filter(category_1 =="obese")


obese <- obese %>% 
  dplyr::mutate(weightloss= as.character(category_2))


obese$weightloss[obese$weightloss == "obese"] <- 0
obese$weightloss[obese$weightloss == "overweight" ] <- 1
obese$weightloss[obese$weightloss == "healthy" ] <- 1
obese$weightloss[obese$weightloss == "underweight" ] <- 1


obese %>% 
  tabyl(category_2, weightloss)


obese_weightloss_function <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data %>%
    tabyl({{var}}, weightloss) %>% 
    adorn_totals() %>%
    dplyr::rename(weightgain = 3) %>%       ## rename identified by column number in function
    dplyr::rename(stable_weight = 2) %>% 
    dplyr::mutate(N_total = stable_weight + weightgain) %>% 
    dplyr::mutate(percent = weightgain/N_total) %>% 
    dplyr::select(-(stable_weight)) %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
    ungroup()%>%
    dplyr::mutate(weightgain = plyr::round_any(weightgain, 5)) %>% 
    dplyr::mutate(N_total = plyr::round_any(N_total, 5))  %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(variable = (v1))
}

obese_weightloss_function(obese, sex)
age_group_2 <- obese_weightloss_function (obese, age_group_2)
sex <- obese_weightloss_function(obese, sex)
ethnic_no_miss <- obese_weightloss_function(obese, ethnic_no_miss)
eth_group_16 <- obese_weightloss_function(obese, eth_group_16)
imd <- obese_weightloss_function(obese, imd)
region <- obese_weightloss_function(obese, region)
hypertension <- obese_weightloss_function(obese, hypertension)
diabetes_t1 <- obese_weightloss_function(obese, diabetes_t1)
diabetes_t2 <- obese_weightloss_function(obese, diabetes_t2)
chronic_cardiac <- obese_weightloss_function(obese, chronic_cardiac)
learning_disability <- obese_weightloss_function(obese, learning_disability)
depression <- obese_weightloss_function(obese, depression)
dementia <- obese_weightloss_function(obese, dementia)
psychosis_schiz_bipolar <- obese_weightloss_function(obese, psychosis_schiz_bipolar)
asthma <- obese_weightloss_function(obese, asthma)
COPD <- obese_weightloss_function(obese, COPD)
stroke_and_TIA <- obese_weightloss_function(obese, stroke_and_TIA)
all_cancer <- obese_weightloss_function(obese, all_cancer)
smoking_status <- obese_weightloss_function(obese, smoking_status)
year <- obese_weightloss_function(obese, year)


counts_obese_weightloss_all <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  bind_rows(year)  %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "all", .before=1)


### obese 2017 data
obese_2017 <- obese %>% 
  dplyr::filter(year == 2017)


age_group_2 <- obese_weightloss_function (obese_2017, age_group_2)
sex <- obese_weightloss_function(obese_2017, sex)
ethnic_no_miss <- obese_weightloss_function(obese_2017, ethnic_no_miss)
eth_group_16 <- obese_weightloss_function(obese_2017, eth_group_16)
imd <- obese_weightloss_function(obese_2017, imd)
region <- obese_weightloss_function(obese_2017, region)
hypertension <- obese_weightloss_function(obese_2017, hypertension)
diabetes_t1 <- obese_weightloss_function(obese_2017, diabetes_t1)
diabetes_t2 <- obese_weightloss_function(obese_2017, diabetes_t2)
chronic_cardiac <- obese_weightloss_function(obese_2017, chronic_cardiac)
learning_disability <- obese_weightloss_function(obese_2017, learning_disability)
depression <- obese_weightloss_function(obese_2017, depression)
dementia <- obese_weightloss_function(obese_2017, dementia)
psychosis_schiz_bipolar <- obese_weightloss_function(obese_2017, psychosis_schiz_bipolar)
asthma <- obese_weightloss_function(obese_2017, asthma)
COPD <- obese_weightloss_function(obese_2017, COPD)
stroke_and_TIA <- obese_weightloss_function(obese_2017, stroke_and_TIA)
all_cancer <- obese_weightloss_function(obese_2017, all_cancer)
smoking_status <- obese_weightloss_function(obese_2017, smoking_status)



counts_obese_weightloss_2017 <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "2017", .before=1)

### obese 2019 data
obese_2019 <- obese %>% 
  dplyr::filter(year == 2019)


age_group_2 <- obese_weightloss_function (obese_2019, age_group_2)
sex <- obese_weightloss_function(obese_2019, sex)
ethnic_no_miss <- obese_weightloss_function(obese_2019, ethnic_no_miss)
eth_group_16 <- obese_weightloss_function(obese_2019, eth_group_16)
imd <- obese_weightloss_function(obese_2019, imd)
region <- obese_weightloss_function(obese_2019, region)
hypertension <- obese_weightloss_function(obese_2019, hypertension)
diabetes_t1 <- obese_weightloss_function(obese_2019, diabetes_t1)
diabetes_t2 <- obese_weightloss_function(obese_2019, diabetes_t2)
chronic_cardiac <- obese_weightloss_function(obese_2019, chronic_cardiac)
learning_disability <- obese_weightloss_function(obese_2019, learning_disability)
depression <- obese_weightloss_function(obese_2019, depression)
dementia <- obese_weightloss_function(obese_2019, dementia)
psychosis_schiz_bipolar <- obese_weightloss_function(obese_2019, psychosis_schiz_bipolar)
asthma <- obese_weightloss_function(obese_2019, asthma)
COPD <- obese_weightloss_function(obese_2019, COPD)
stroke_and_TIA <- obese_weightloss_function(obese_2019, stroke_and_TIA)
all_cancer <- obese_weightloss_function(obese_2019, all_cancer)
smoking_status <- obese_weightloss_function(obese_2019, smoking_status)



counts_obese_weightloss_2019 <- sex %>%
  bind_rows(age_group_2) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(ethnic_no_miss) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(dementia) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(smoking_status) %>%
  dplyr::select(variable, group, weightgain, N_total, percent) %>% 
  dplyr::mutate(year= "2019", .before=1)

counts_obese_weightloss_all <- counts_obese_weightloss_all %>% 
  bind_rows(counts_obese_weightloss_2017) %>% 
  bind_rows(counts_obese_weightloss_2019)






## Save Outputs

write.csv (counts_normal_weight_all, here::here ("output/data","healthy_transition_weightgain_counts.csv"))
write.csv (counts_overweight_obese_all, here::here ("output/data","overweight_transition_weightgain_counts.csv"))
write.csv (counts_overweight_weightloss_all, here::here ("output/data","overweight_transition_weightloss_counts.csv"))
write.csv (counts_obese_weightloss_all, here::here ("output/data","obese_transition_weightloss_counts.csv"))



