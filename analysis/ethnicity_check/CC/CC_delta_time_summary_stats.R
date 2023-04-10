## M Samuel 
## This script looks at the time between BMI measures in the delta analysis

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


my_data <- read_csv (here::here ("output/data", "CC_delta_data.csv"))


# remove error ethnicity variables
data <- my_data %>% 
  dplyr::select(-c(ethnic_no_miss, eth_group_16))

data <- data %>% 
  dplyr::mutate(rapid_bmi_change = as.character(rapid_bmi_change)) %>% 
  dplyr::mutate(rapid_bmi_change = case_when(
    rapid_bmi_change == 1 ~ "rapid", 
    rapid_bmi_change == 0 ~ "not rapid"
  ))




# remove error ethnicity variables
data <- my_data %>% 
  dplyr::select(-c(ethnic_no_miss, eth_group_16))

data <- data %>% 
  dplyr::mutate(rapid_bmi_change = as.character(rapid_bmi_change)) %>% 
  dplyr::mutate(rapid_bmi_change = case_when(
    rapid_bmi_change == 1 ~ "rapid", 
    rapid_bmi_change == 0 ~ "not rapid"
  ))


## Write functions

function_1 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) %>% 
    dplyr::select("variable", "group", "n")
}


function_2 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  data %>%
    group_by({{my_var}}) %>%
    summarise(mean_time_1 = mean(time_change1, na.rm = TRUE),
              sd_time_1 = sd (time_change1, na.rm = TRUE), 
    ) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
  
}


function_3 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  data %>%
    group_by({{my_var}}) %>%
    summarise(mean_time_2 = mean(time_change2, na.rm = TRUE),
              sd_time_2 = sd (time_change2, na.rm = TRUE), 
    ) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
  
}


function_1(data, sex)

function_2(data, sex)

function_3(data, sex)


### FILTER PREPANDEMIC DATA
prepandemic <- data %>% 
  dplyr::filter(pandemic_stage == "precovid")



## Prepandemic Analyses: Total Population

sex <-function_1(prepandemic,  sex)
age_group_2 <-function_1(prepandemic,  age_group_2)
eth_group_16 <-function_1(prepandemic,  eth_16_corrected)
imd <-function_1(prepandemic,  imd)
precovid_bmi_category <-function_1(prepandemic,  precovid_bmi_category)
hypertension <-function_1(prepandemic,   hypertension)
diabetes_t1 <-function_1(prepandemic,   diabetes_t1)
diabetes_t2 <-function_1(prepandemic,   diabetes_t2)
chronic_cardiac <-function_1(prepandemic,   chronic_cardiac)
learning_disability <-function_1(prepandemic,   learning_disability)
depression <-function_1(prepandemic,   depression)
dementia <-function_1(prepandemic,  dementia)
psychosis_schiz_bipolar <-function_1(prepandemic,   psychosis_schiz_bipolar)
asthma <-function_1(prepandemic,   asthma)
COPD <-function_1(prepandemic,   COPD)
stroke_and_TIA <-function_1(prepandemic,   stroke_and_TIA)
all <-function_1(prepandemic, pandemic_stage)

complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  bind_rows(all)

##

sex <-function_2(prepandemic,  sex)
age_group_2 <-function_2(prepandemic,  age_group_2)
eth_group_16 <-function_2(prepandemic,  eth_16_corrected)
imd <-function_2(prepandemic,  imd)
precovid_bmi_category <-function_2(prepandemic,  precovid_bmi_category)
hypertension <-function_2(prepandemic,   hypertension)
diabetes_t1 <-function_2(prepandemic,   diabetes_t1)
diabetes_t2 <-function_2(prepandemic,   diabetes_t2)
chronic_cardiac <-function_2(prepandemic,   chronic_cardiac)
learning_disability <-function_2(prepandemic,   learning_disability)
depression <-function_2(prepandemic,   depression)
dementia <-function_2(prepandemic,  dementia)
psychosis_schiz_bipolar <-function_2(prepandemic,   psychosis_schiz_bipolar)
asthma <-function_2(prepandemic,   asthma)
COPD <-function_2(prepandemic,   COPD)
stroke_and_TIA <-function_2(prepandemic,   stroke_and_TIA)
all <-function_2(prepandemic,  pandemic_stage)

time_1 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  bind_rows(all) 

##
sex <-function_3(prepandemic,  sex)
age_group_2 <-function_3(prepandemic,  age_group_2)
eth_group_16 <-function_3(prepandemic,  eth_16_corrected)
imd <-function_3(prepandemic,  imd)
precovid_bmi_category <-function_3(prepandemic,  precovid_bmi_category)
hypertension <-function_3(prepandemic,   hypertension)
diabetes_t1 <-function_3(prepandemic,   diabetes_t1)
diabetes_t2 <-function_3(prepandemic,   diabetes_t2)
chronic_cardiac <-function_3(prepandemic,   chronic_cardiac)
learning_disability <-function_3(prepandemic,   learning_disability)
depression <-function_3(prepandemic,   depression)
dementia <-function_3(prepandemic,  dementia)
psychosis_schiz_bipolar <-function_3(prepandemic,   psychosis_schiz_bipolar)
asthma <-function_3(prepandemic,   asthma)
COPD <-function_3(prepandemic,   COPD)
stroke_and_TIA <-function_3(prepandemic,   stroke_and_TIA)
all <-function_3(prepandemic,  pandemic_stage)

time_2 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  bind_rows(all) 




complete_prepandemic <- complete  %>%
  dplyr::left_join (time_1) %>%
  dplyr::left_join(time_2) %>% 
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_prepandemic <- complete_prepandemic  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_prepandemic$n_pop, 5)) %>% 
  dplyr::mutate(stage = "prepandemic", .before=1)





#######################################################################


####################################
####################################
### FILTER POSTPANDEMIC DATA
postpandemic <- data %>% 
  dplyr::filter(pandemic_stage == "postcovid")



## Postpandemic Analyses: Total Population

sex <-function_1(postpandemic,  sex)
age_group_2 <-function_1(postpandemic,  age_group_2)
eth_group_16 <-function_1(postpandemic,  eth_16_corrected)
imd <-function_1(postpandemic,  imd)
precovid_bmi_category <-function_1(postpandemic,  precovid_bmi_category)
hypertension <-function_1(postpandemic,   hypertension)
diabetes_t1 <-function_1(postpandemic,   diabetes_t1)
diabetes_t2 <-function_1(postpandemic,   diabetes_t2)
chronic_cardiac <-function_1(postpandemic,   chronic_cardiac)
learning_disability <-function_1(postpandemic,   learning_disability)
depression <-function_1(postpandemic,   depression)
dementia <-function_1(postpandemic,  dementia)
psychosis_schiz_bipolar <-function_1(postpandemic,   psychosis_schiz_bipolar)
asthma <-function_1(postpandemic,   asthma)
COPD <-function_1(postpandemic,   COPD)
stroke_and_TIA <-function_1(postpandemic,   stroke_and_TIA)
all <-function_1(postpandemic,  pandemic_stage)

complete_2 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  bind_rows(all)

##

sex <-function_2(postpandemic,  sex)
age_group_2 <-function_2(postpandemic,  age_group_2)
eth_group_16 <-function_2(postpandemic,  eth_16_corrected)
imd <-function_2(postpandemic,  imd)
precovid_bmi_category <-function_2(postpandemic,  precovid_bmi_category)
hypertension <-function_2(postpandemic,   hypertension)
diabetes_t1 <-function_2(postpandemic,   diabetes_t1)
diabetes_t2 <-function_2(postpandemic,   diabetes_t2)
chronic_cardiac <-function_2(postpandemic,   chronic_cardiac)
learning_disability <-function_2(postpandemic,   learning_disability)
depression <-function_2(postpandemic,   depression)
dementia <-function_2(postpandemic,  dementia)
psychosis_schiz_bipolar <-function_2(postpandemic,   psychosis_schiz_bipolar)
asthma <-function_2(postpandemic,   asthma)
COPD <-function_2(postpandemic,   COPD)
stroke_and_TIA <-function_2(postpandemic,   stroke_and_TIA)
all <-function_2(postpandemic,  pandemic_stage)

time_3 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  bind_rows(all) 

##
sex <-function_3(postpandemic,  sex)
age_group_2 <-function_3(postpandemic,  age_group_2)
eth_group_16 <-function_3(postpandemic,  eth_16_corrected)
imd <-function_3(postpandemic,  imd)
precovid_bmi_category <-function_3(postpandemic,  precovid_bmi_category)
hypertension <-function_3(postpandemic,   hypertension)
diabetes_t1 <-function_3(postpandemic,   diabetes_t1)
diabetes_t2 <-function_3(postpandemic,   diabetes_t2)
chronic_cardiac <-function_3(postpandemic,   chronic_cardiac)
learning_disability <-function_3(postpandemic,   learning_disability)
depression <-function_3(postpandemic,   depression)
dementia <-function_3(postpandemic,  dementia)
psychosis_schiz_bipolar <-function_3(postpandemic,   psychosis_schiz_bipolar)
asthma <-function_3(postpandemic,   asthma)
COPD <-function_3(postpandemic,   COPD)
stroke_and_TIA <-function_3(postpandemic,   stroke_and_TIA)
all <-function_3(postpandemic,  pandemic_stage)

time_4 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
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
  bind_rows(all) 



complete_postpandemic <- complete_2  %>%
  dplyr::left_join (time_3) %>%
  dplyr::left_join(time_4) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_postpandemic <- complete_postpandemic  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_postpandemic$n_pop, 5)) %>% 
  dplyr::mutate(stage = "pandemic", .before=1)

## - write data

complete <- complete_prepandemic %>% 
  dplyr::bind_rows(complete_postpandemic)



write_csv (complete, here::here ("output/data","CC_delta_time_summary_stats.csv"))



