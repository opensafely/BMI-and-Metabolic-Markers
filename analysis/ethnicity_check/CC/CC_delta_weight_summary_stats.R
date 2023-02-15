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


# my_data <- read_csv("Documents/Academic GP/Open Safely/Dummy Data/CC_delta_data.csv")
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



function_2 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}, bmi_change_cat) %>%
    adorn_percentages()   %>% 
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) 
}



function_3 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}, rapid_bmi_change) %>%
    dplyr::rename(group = {{my_var}}) %>%
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))  %>% 
    dplyr::select(variable, group, rapid)
}



function_4 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  data %>%
    group_by({{my_var}}) %>%
    summarise(mean_delta = mean(yearly_bmi_change, na.rm = TRUE),
              sd_delta = sd (yearly_bmi_change, na.rm = TRUE), 
    ) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
  
}

function_5 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    group_by({{my_var}}) %>%
    summarise(Q1=quantile(yearly_bmi_change,probs = 0.25, na.rm = TRUE),
              median=median(yearly_bmi_change, na.rm = TRUE), 
              Q3=quantile(yearly_bmi_change, probs = 0.75, na.rm = TRUE)) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
}




####################################
####################################
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

delta_categories <- sex %>% 
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

rapid <- sex %>% 
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
sex <-function_4(prepandemic,  sex)
age_group_2 <-function_4(prepandemic,  age_group_2)
eth_group_16 <-function_4(prepandemic,  eth_16_corrected)
imd <-function_4(prepandemic,  imd)
precovid_bmi_category <-function_4(prepandemic,  precovid_bmi_category)
hypertension <-function_4(prepandemic,   hypertension)
diabetes_t1 <-function_4(prepandemic,   diabetes_t1)
diabetes_t2 <-function_4(prepandemic,   diabetes_t2)
chronic_cardiac <-function_4(prepandemic,   chronic_cardiac)
learning_disability <-function_4(prepandemic,   learning_disability)
depression <-function_4(prepandemic,   depression)
dementia <-function_4(prepandemic,  dementia)
psychosis_schiz_bipolar <-function_4(prepandemic,   psychosis_schiz_bipolar)
asthma <-function_4(prepandemic,   asthma)
COPD <-function_4(prepandemic,   COPD)
stroke_and_TIA <-function_4(prepandemic,   stroke_and_TIA)
all <-function_4(prepandemic,  pandemic_stage)

delta_mean <- sex %>% 
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


## median/IQR
sex <-function_5(prepandemic,  sex)
age_group_2 <-function_5(prepandemic,  age_group_2)
eth_group_16 <-function_5(prepandemic,  eth_16_corrected)
imd <-function_5(prepandemic,  imd)
precovid_bmi_category <-function_5(prepandemic,  precovid_bmi_category)
hypertension <-function_5(prepandemic,   hypertension)
diabetes_t1 <-function_5(prepandemic,   diabetes_t1)
diabetes_t2 <-function_5(prepandemic,   diabetes_t2)
chronic_cardiac <-function_5(prepandemic,   chronic_cardiac)
learning_disability <-function_5(prepandemic,   learning_disability)
depression <-function_5(prepandemic,   depression)
dementia <-function_5(prepandemic,  dementia)
psychosis_schiz_bipolar <-function_5(prepandemic,   psychosis_schiz_bipolar)
asthma <-function_5(prepandemic,   asthma)
COPD <-function_5(prepandemic,   COPD)
stroke_and_TIA <-function_5(prepandemic,   stroke_and_TIA)
all <-function_5(prepandemic,  pandemic_stage)

delta_median <- sex %>% 
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
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_prepandemic <- complete_prepandemic  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_prepandemic$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_prepandemic$rapid, 5)) %>% 
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

delta_categories <- sex %>% 
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

rapid <- sex %>% 
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
sex <-function_4(postpandemic,  sex)
age_group_2 <-function_4(postpandemic,  age_group_2)
eth_group_16 <-function_4(postpandemic,  eth_16_corrected)
imd <-function_4(postpandemic,  imd)
precovid_bmi_category <-function_4(postpandemic,  precovid_bmi_category)
hypertension <-function_4(postpandemic,   hypertension)
diabetes_t1 <-function_4(postpandemic,   diabetes_t1)
diabetes_t2 <-function_4(postpandemic,   diabetes_t2)
chronic_cardiac <-function_4(postpandemic,   chronic_cardiac)
learning_disability <-function_4(postpandemic,   learning_disability)
depression <-function_4(postpandemic,   depression)
dementia <-function_4(postpandemic,  dementia)
psychosis_schiz_bipolar <-function_4(postpandemic,   psychosis_schiz_bipolar)
asthma <-function_4(postpandemic,   asthma)
COPD <-function_4(postpandemic,   COPD)
stroke_and_TIA <-function_4(postpandemic,   stroke_and_TIA)
all <-function_4(postpandemic,  pandemic_stage)

delta_mean <- sex %>% 
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


## Median IQR
sex <-function_5(postpandemic,  sex)
age_group_2 <-function_5(postpandemic,  age_group_2)
eth_group_16 <-function_5(postpandemic,  eth_16_corrected)
imd <-function_5(postpandemic,  imd)
precovid_bmi_category <-function_5(postpandemic,  precovid_bmi_category)
hypertension <-function_5(postpandemic,   hypertension)
diabetes_t1 <-function_5(postpandemic,   diabetes_t1)
diabetes_t2 <-function_5(postpandemic,   diabetes_t2)
chronic_cardiac <-function_5(postpandemic,   chronic_cardiac)
learning_disability <-function_5(postpandemic,   learning_disability)
depression <-function_5(postpandemic,   depression)
dementia <-function_5(postpandemic,  dementia)
psychosis_schiz_bipolar <-function_5(postpandemic,   psychosis_schiz_bipolar)
asthma <-function_5(postpandemic,   asthma)
COPD <-function_5(postpandemic,   COPD)
stroke_and_TIA <-function_5(postpandemic,   stroke_and_TIA)
all <-function_5(postpandemic,  pandemic_stage)

delta_median <- sex %>% 
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


complete_postpandemic <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>% 
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_postpandemic <- complete_postpandemic  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_postpandemic$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_postpandemic$rapid, 5)) %>% 
  dplyr::mutate(stage = "pandemic", .before=1)

## - write data

complete <- complete_prepandemic %>% 
  dplyr::bind_rows(complete_postpandemic) %>% 
  dplyr::select(-"percent", -"valid_percent")



write_csv (complete, here::here ("output/data","CC_delta_weight_summary_stats.csv"))



