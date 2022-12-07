## M Samuel
## This R file defines the characteristics of the complete case delta pandemic and delta prepandemic - with data extracted in March 2022
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


my_data <- read_csv (here::here ("output/data", "CC_delta_change_data.csv"))


my_data <- my_data %>% 
  dplyr::select(-c(ethnic_no_miss, eth_group_16))

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




## Total Population

sex <-function_1(my_data,  sex)
age_group_2 <-function_1(my_data,  age_group_2)
eth_group_16 <-function_1(my_data,  eth_16_corrected)
imd <-function_1(my_data,  imd)
region <-function_1(my_data,  region)
hypertension <-function_1(my_data,   hypertension)
diabetes_t1 <-function_1(my_data,   diabetes_t1)
diabetes_t2 <-function_1(my_data,   diabetes_t2)
chronic_cardiac <-function_1(my_data,   chronic_cardiac)
learning_disability <-function_1(my_data,   learning_disability)
depression <-function_1(my_data,   depression)
dementia <-function_1(my_data,  dementia)
psychosis_schiz_bipolar <-function_1(my_data,   psychosis_schiz_bipolar)
asthma <-function_1(my_data,   asthma)
COPD <-function_1(my_data,   COPD)
stroke_and_TIA <-function_1(my_data,   stroke_and_TIA)
smoking_status <-function_1(my_data,  smoking_status)

complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
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

sex <-function_2(my_data,  sex)
age_group_2 <-function_2(my_data,  age_group_2)
eth_group_16 <-function_2(my_data,  eth_16_corrected)
imd <-function_2(my_data,  imd)
region <-function_2(my_data,  region)
hypertension <-function_2(my_data,   hypertension)
diabetes_t1 <-function_2(my_data,   diabetes_t1)
diabetes_t2 <-function_2(my_data,   diabetes_t2)
chronic_cardiac <-function_2(my_data,   chronic_cardiac)
learning_disability <-function_2(my_data,   learning_disability)
depression <-function_2(my_data,   depression)
dementia <-function_2(my_data,  dementia)
psychosis_schiz_bipolar <-function_2(my_data,   psychosis_schiz_bipolar)
asthma <-function_2(my_data,   asthma)
COPD <-function_2(my_data,   COPD)
stroke_and_TIA <-function_2(my_data,   stroke_and_TIA)
smoking_status <-function_2(my_data,  smoking_status)

rapid <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
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
sex <-function_3(my_data,  sex)
age_group_2 <-function_3(my_data,  age_group_2)
eth_group_16 <-function_3(my_data,  eth_16_corrected)
imd <-function_3(my_data,  imd)
region <-function_3(my_data,  region)
hypertension <-function_3(my_data,   hypertension)
diabetes_t1 <-function_3(my_data,   diabetes_t1)
diabetes_t2 <-function_3(my_data,   diabetes_t2)
chronic_cardiac <-function_3(my_data,   chronic_cardiac)
learning_disability <-function_3(my_data,   learning_disability)
depression <-function_3(my_data,   depression)
dementia <-function_3(my_data,  dementia)
psychosis_schiz_bipolar <-function_3(my_data,   psychosis_schiz_bipolar)
asthma <-function_3(my_data,   asthma)
COPD <-function_3(my_data,   COPD)
stroke_and_TIA <-function_3(my_data,   stroke_and_TIA)
smoking_status <-function_3(my_data,  smoking_status)

mean_d_change <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
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
sex <-function_4(my_data,  sex)
age_group_2 <-function_4(my_data,  age_group_2)
eth_group_16 <-function_4(my_data,  eth_16_corrected)
imd <-function_4(my_data,  imd)
region <-function_4(my_data,  region)
hypertension <-function_4(my_data,   hypertension)
diabetes_t1 <-function_4(my_data,   diabetes_t1)
diabetes_t2 <-function_4(my_data,   diabetes_t2)
chronic_cardiac <-function_4(my_data,   chronic_cardiac)
learning_disability <-function_4(my_data,   learning_disability)
depression <-function_4(my_data,   depression)
dementia <-function_4(my_data,  dementia)
psychosis_schiz_bipolar <-function_4(my_data,   psychosis_schiz_bipolar)
asthma <-function_4(my_data,   asthma)
COPD <-function_4(my_data,   COPD)
stroke_and_TIA <-function_4(my_data,   stroke_and_TIA)
smoking_status <-function_4(my_data,  smoking_status)

median_d_change <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
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


complete <- complete  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete$n_pop, 5)) %>% 
  dplyr::mutate(rapid_change = plyr::round_any(complete$rapid_change, 5)) 



write_csv (complete, here::here ("output/data","CC_delta_change_summary_stats.csv"))

