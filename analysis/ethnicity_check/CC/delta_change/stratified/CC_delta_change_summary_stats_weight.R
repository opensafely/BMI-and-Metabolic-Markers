## M Samuel
## This R file defines the characteristics of the complete case delta change - with data extracted in March 2022
## The code can be applied to substrata population by filtering at the first stage.
## The total in each group, numbers with BMI data, mean and median BMI data are calculated


library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(purrr)
library(broom)
library(data.table)
library(janitor)
library(skimr)
library(ggplot2)
library(gtsummary)


#my_data <- read_csv("Documents/Academic GP/Open Safely/Dummy Data/CC_stratified_analysis_delta_change_data.csv")
my_data <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_change_data.csv"))




my_data <- my_data %>% 
  dplyr::mutate(change_90th = as.character(change_90th)) %>% 
  dplyr::mutate(change_90th = case_when(
    change_90th == 1 ~ "rapid_change", 
    change_90th == 0 ~ "not rapid"
  ))


## Write functions

function_1 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) 
}

function_1(my_data,sex)





function_2 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}, change_90th) %>%
    dplyr::rename(group = {{my_var}}) %>%
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))  %>% 
    dplyr::select(variable, group, rapid_change)
}

function_2(my_data,sex)



function_3 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  data %>%
    group_by({{my_var}}) %>%
    summarise(mean_d_change = mean(trajectory_change, na.rm = TRUE),
              sd_d_change = sd (trajectory_change, na.rm = TRUE), 
    ) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
  
}
function_3(my_data,sex)



function_4 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    group_by({{my_var}}) %>%
    summarise(Q1=quantile(trajectory_change,probs = 0.25, na.rm = TRUE),
              median=median(trajectory_change, na.rm = TRUE), 
              Q3=quantile(trajectory_change, probs = 0.75, na.rm = TRUE)) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
}

function_4(my_data,sex)



####################################
####################################
###################################
# healthy analysis
### FILTER

data_healthy <- my_data %>% 
  dplyr::filter(precovid_bmi_category  == "healthy")





###
sex <-function_1(data_healthy,  sex)
age <-function_1(data_healthy,  age_group_2)
ethnic <-function_1(data_healthy,  eth_collapsed)
imd <-function_1(data_healthy,  imd)
region <-function_1(data_healthy,  region)
hypertension <-function_1(data_healthy,   hypertension)
diabetes_t1 <-function_1(data_healthy,   diabetes_t1)
diabetes_t2 <-function_1(data_healthy,   diabetes_t2)
chronic_cardiac <-function_1(data_healthy,   chronic_cardiac)
learning_disability <-function_1(data_healthy,   learning_disability)
depression <-function_1(data_healthy,   depression)
dementia <-function_1(data_healthy,  dementia)
psychosis_schiz_bipolar <-function_1(data_healthy,   psychosis_schiz_bipolar)
asthma <-function_1(data_healthy,   asthma)
COPD <-function_1(data_healthy,   COPD)
stroke_and_TIA <-function_1(data_healthy,   stroke_and_TIA)
smoking_status <-function_1(data_healthy,  smoking_status)

complete <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status)

##

sex <-function_2(data_healthy,  sex)
age <-function_2(data_healthy,  age_group_2)
ethnic <-function_2(data_healthy,  eth_collapsed)
imd <-function_2(data_healthy,  imd)
region <-function_2(data_healthy,  region)
hypertension <-function_2(data_healthy,   hypertension)
diabetes_t1 <-function_2(data_healthy,   diabetes_t1)
diabetes_t2 <-function_2(data_healthy,   diabetes_t2)
chronic_cardiac <-function_2(data_healthy,   chronic_cardiac)
learning_disability <-function_2(data_healthy,   learning_disability)
depression <-function_2(data_healthy,   depression)
dementia <-function_2(data_healthy,  dementia)
psychosis_schiz_bipolar <-function_2(data_healthy,   psychosis_schiz_bipolar)
asthma <-function_2(data_healthy,   asthma)
COPD <-function_2(data_healthy,   COPD)
stroke_and_TIA <-function_2(data_healthy,   stroke_and_TIA)
smoking_status <-function_2(data_healthy,  smoking_status)

rapid <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status) 

##
sex <-function_3(data_healthy,  sex)
age <-function_3(data_healthy,  age_group_2)
ethnic <-function_3(data_healthy,  eth_collapsed)
imd <-function_3(data_healthy,  imd)
region <-function_3(data_healthy,  region)
hypertension <-function_3(data_healthy,   hypertension)
diabetes_t1 <-function_3(data_healthy,   diabetes_t1)
diabetes_t2 <-function_3(data_healthy,   diabetes_t2)
chronic_cardiac <-function_3(data_healthy,   chronic_cardiac)
learning_disability <-function_3(data_healthy,   learning_disability)
depression <-function_3(data_healthy,   depression)
dementia <-function_3(data_healthy,  dementia)
psychosis_schiz_bipolar <-function_3(data_healthy,   psychosis_schiz_bipolar)
asthma <-function_3(data_healthy,   asthma)
COPD <-function_3(data_healthy,   COPD)
stroke_and_TIA <-function_3(data_healthy,   stroke_and_TIA)
smoking_status <-function_3(data_healthy,  smoking_status)

mean_d_change <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status)


##
sex <-function_4(data_healthy,  sex)
age <-function_4(data_healthy,  age_group_2)
ethnic <-function_4(data_healthy,  eth_collapsed)
imd <-function_4(data_healthy,  imd)
region <-function_4(data_healthy,  region)
hypertension <-function_4(data_healthy,   hypertension)
diabetes_t1 <-function_4(data_healthy,   diabetes_t1)
diabetes_t2 <-function_4(data_healthy,   diabetes_t2)
chronic_cardiac <-function_4(data_healthy,   chronic_cardiac)
learning_disability <-function_4(data_healthy,   learning_disability)
depression <-function_4(data_healthy,   depression)
dementia <-function_4(data_healthy,  dementia)
psychosis_schiz_bipolar <-function_4(data_healthy,   psychosis_schiz_bipolar)
asthma <-function_4(data_healthy,   asthma)
COPD <-function_4(data_healthy,   COPD)
stroke_and_TIA <-function_4(data_healthy,   stroke_and_TIA)
smoking_status <-function_4(data_healthy,  smoking_status)

median_d_change <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status)




complete <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(mean_d_change) %>%  
  dplyr::left_join(median_d_change) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_healthy <- complete  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%
  dplyr::mutate(n_pop = plyr::round_any(complete$n_pop, 5)) %>% 
  dplyr::mutate(rapid_change = plyr::round_any(complete$rapid_change, 5)) 


##############



####################################
####################################
###################################
# overweight analysis
### FILTER

data_overweight <- my_data %>% 
  dplyr::filter(precovid_bmi_category  == "overweight")





###
sex <-function_1(data_overweight,  sex)
age <-function_1(data_overweight,  age_group_2)
ethnic <-function_1(data_overweight,  eth_collapsed)
imd <-function_1(data_overweight,  imd)
region <-function_1(data_overweight,  region)
hypertension <-function_1(data_overweight,   hypertension)
diabetes_t1 <-function_1(data_overweight,   diabetes_t1)
diabetes_t2 <-function_1(data_overweight,   diabetes_t2)
chronic_cardiac <-function_1(data_overweight,   chronic_cardiac)
learning_disability <-function_1(data_overweight,   learning_disability)
depression <-function_1(data_overweight,   depression)
dementia <-function_1(data_overweight,  dementia)
psychosis_schiz_bipolar <-function_1(data_overweight,   psychosis_schiz_bipolar)
asthma <-function_1(data_overweight,   asthma)
COPD <-function_1(data_overweight,   COPD)
stroke_and_TIA <-function_1(data_overweight,   stroke_and_TIA)
smoking_status <-function_1(data_overweight,  smoking_status)

complete <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status)

##

sex <-function_2(data_overweight,  sex)
age <-function_2(data_overweight,  age_group_2)
ethnic <-function_2(data_overweight,  eth_collapsed)
imd <-function_2(data_overweight,  imd)
region <-function_2(data_overweight,  region)
hypertension <-function_2(data_overweight,   hypertension)
diabetes_t1 <-function_2(data_overweight,   diabetes_t1)
diabetes_t2 <-function_2(data_overweight,   diabetes_t2)
chronic_cardiac <-function_2(data_overweight,   chronic_cardiac)
learning_disability <-function_2(data_overweight,   learning_disability)
depression <-function_2(data_overweight,   depression)
dementia <-function_2(data_overweight,  dementia)
psychosis_schiz_bipolar <-function_2(data_overweight,   psychosis_schiz_bipolar)
asthma <-function_2(data_overweight,   asthma)
COPD <-function_2(data_overweight,   COPD)
stroke_and_TIA <-function_2(data_overweight,   stroke_and_TIA)
smoking_status <-function_2(data_overweight,  smoking_status)

rapid <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status) 

##
sex <-function_3(data_overweight,  sex)
age <-function_3(data_overweight,  age_group_2)
ethnic <-function_3(data_overweight,  eth_collapsed)
imd <-function_3(data_overweight,  imd)
region <-function_3(data_overweight,  region)
hypertension <-function_3(data_overweight,   hypertension)
diabetes_t1 <-function_3(data_overweight,   diabetes_t1)
diabetes_t2 <-function_3(data_overweight,   diabetes_t2)
chronic_cardiac <-function_3(data_overweight,   chronic_cardiac)
learning_disability <-function_3(data_overweight,   learning_disability)
depression <-function_3(data_overweight,   depression)
dementia <-function_3(data_overweight,  dementia)
psychosis_schiz_bipolar <-function_3(data_overweight,   psychosis_schiz_bipolar)
asthma <-function_3(data_overweight,   asthma)
COPD <-function_3(data_overweight,   COPD)
stroke_and_TIA <-function_3(data_overweight,   stroke_and_TIA)
smoking_status <-function_3(data_overweight,  smoking_status)

mean_d_change <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status)


##
sex <-function_4(data_overweight,  sex)
age <-function_4(data_overweight,  age_group_2)
ethnic <-function_4(data_overweight,  eth_collapsed)
imd <-function_4(data_overweight,  imd)
region <-function_4(data_overweight,  region)
hypertension <-function_4(data_overweight,   hypertension)
diabetes_t1 <-function_4(data_overweight,   diabetes_t1)
diabetes_t2 <-function_4(data_overweight,   diabetes_t2)
chronic_cardiac <-function_4(data_overweight,   chronic_cardiac)
learning_disability <-function_4(data_overweight,   learning_disability)
depression <-function_4(data_overweight,   depression)
dementia <-function_4(data_overweight,  dementia)
psychosis_schiz_bipolar <-function_4(data_overweight,   psychosis_schiz_bipolar)
asthma <-function_4(data_overweight,   asthma)
COPD <-function_4(data_overweight,   COPD)
stroke_and_TIA <-function_4(data_overweight,   stroke_and_TIA)
smoking_status <-function_4(data_overweight,  smoking_status)

median_d_change <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status)




complete <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(mean_d_change) %>%  
  dplyr::left_join(median_d_change) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_overweight <- complete  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%
  dplyr::mutate(n_pop = plyr::round_any(complete$n_pop, 5)) %>% 
  dplyr::mutate(rapid_change = plyr::round_any(complete$rapid_change, 5)) 


##############


####################################
####################################
###################################
# obese analysis
### FILTER

data_obese <- my_data %>% 
  dplyr::filter(precovid_bmi_category  == "obese")





###
sex <-function_1(data_obese,  sex)
age <-function_1(data_obese,  age_group_2)
ethnic <-function_1(data_obese,  eth_collapsed)
imd <-function_1(data_obese,  imd)
region <-function_1(data_obese,  region)
hypertension <-function_1(data_obese,   hypertension)
diabetes_t1 <-function_1(data_obese,   diabetes_t1)
diabetes_t2 <-function_1(data_obese,   diabetes_t2)
chronic_cardiac <-function_1(data_obese,   chronic_cardiac)
learning_disability <-function_1(data_obese,   learning_disability)
depression <-function_1(data_obese,   depression)
dementia <-function_1(data_obese,  dementia)
psychosis_schiz_bipolar <-function_1(data_obese,   psychosis_schiz_bipolar)
asthma <-function_1(data_obese,   asthma)
COPD <-function_1(data_obese,   COPD)
stroke_and_TIA <-function_1(data_obese,   stroke_and_TIA)
smoking_status <-function_1(data_obese,  smoking_status)

complete <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status)

##

sex <-function_2(data_obese,  sex)
age <-function_2(data_obese,  age_group_2)
ethnic <-function_2(data_obese,  eth_collapsed)
imd <-function_2(data_obese,  imd)
region <-function_2(data_obese,  region)
hypertension <-function_2(data_obese,   hypertension)
diabetes_t1 <-function_2(data_obese,   diabetes_t1)
diabetes_t2 <-function_2(data_obese,   diabetes_t2)
chronic_cardiac <-function_2(data_obese,   chronic_cardiac)
learning_disability <-function_2(data_obese,   learning_disability)
depression <-function_2(data_obese,   depression)
dementia <-function_2(data_obese,  dementia)
psychosis_schiz_bipolar <-function_2(data_obese,   psychosis_schiz_bipolar)
asthma <-function_2(data_obese,   asthma)
COPD <-function_2(data_obese,   COPD)
stroke_and_TIA <-function_2(data_obese,   stroke_and_TIA)
smoking_status <-function_2(data_obese,  smoking_status)

rapid <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status) 

##
sex <-function_3(data_obese,  sex)
age <-function_3(data_obese,  age_group_2)
ethnic <-function_3(data_obese,  eth_collapsed)
imd <-function_3(data_obese,  imd)
region <-function_3(data_obese,  region)
hypertension <-function_3(data_obese,   hypertension)
diabetes_t1 <-function_3(data_obese,   diabetes_t1)
diabetes_t2 <-function_3(data_obese,   diabetes_t2)
chronic_cardiac <-function_3(data_obese,   chronic_cardiac)
learning_disability <-function_3(data_obese,   learning_disability)
depression <-function_3(data_obese,   depression)
dementia <-function_3(data_obese,  dementia)
psychosis_schiz_bipolar <-function_3(data_obese,   psychosis_schiz_bipolar)
asthma <-function_3(data_obese,   asthma)
COPD <-function_3(data_obese,   COPD)
stroke_and_TIA <-function_3(data_obese,   stroke_and_TIA)
smoking_status <-function_3(data_obese,  smoking_status)

mean_d_change <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status)


##
sex <-function_4(data_obese,  sex)
age <-function_4(data_obese,  age_group_2)
ethnic <-function_4(data_obese,  eth_collapsed)
imd <-function_4(data_obese,  imd)
region <-function_4(data_obese,  region)
hypertension <-function_4(data_obese,   hypertension)
diabetes_t1 <-function_4(data_obese,   diabetes_t1)
diabetes_t2 <-function_4(data_obese,   diabetes_t2)
chronic_cardiac <-function_4(data_obese,   chronic_cardiac)
learning_disability <-function_4(data_obese,   learning_disability)
depression <-function_4(data_obese,   depression)
dementia <-function_4(data_obese,  dementia)
psychosis_schiz_bipolar <-function_4(data_obese,   psychosis_schiz_bipolar)
asthma <-function_4(data_obese,   asthma)
COPD <-function_4(data_obese,   COPD)
stroke_and_TIA <-function_4(data_obese,   stroke_and_TIA)
smoking_status <-function_4(data_obese,  smoking_status)

median_d_change <- sex %>% 
  bind_rows(age) %>%
  bind_rows(ethnic) %>%
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
  bind_rows(smoking_status)




complete <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(mean_d_change) %>%  
  dplyr::left_join(median_d_change) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_obese <- complete  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%
  dplyr::mutate(n_pop = plyr::round_any(complete$n_pop, 5)) %>% 
  dplyr::mutate(rapid_change = plyr::round_any(complete$rapid_change, 5)) 


##############


write_csv (complete_obese, here::here ("output/data","CC_delta_change_summary_stats_obese.csv"))

write_csv (complete_overweight, here::here ("output/data","CC_delta_change_summary_stats_overweight.csv"))

write_csv (complete_healthy, here::here ("output/data","CC_delta_change_summary_stats_healthy.csv"))
