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
  dplyr::mutate(year= "2017", .before=1)


counts_normal_weight_all <- counts_normal_weight_all %>% 
  bind_rows(counts_normal_weight_2017) %>% 
  bind_rows(counts_normal_weight_2019)








## Save Outputs

write.csv (counts_normal_weight_all, here::here ("output/data","healthy_transition_weightgain_counts.csv"))
