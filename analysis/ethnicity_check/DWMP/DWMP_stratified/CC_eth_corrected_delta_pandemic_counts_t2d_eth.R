


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

#pandemic <- CC_stratified_analysis_delta_data_pandemic

pandemic <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_data_pandemic.csv"))

### FILTER for hypertensives

pandemic <- pandemic %>% 
  dplyr::filter(diabetes_t2 == TRUE)


## Flag rapid BMI change

pandemic <- pandemic %>% 
  dplyr::mutate(rapid_bmi_change = as.character(rapid_bmi_change)) %>% 
  dplyr::mutate(rapid_bmi_change = case_when(
    rapid_bmi_change == 1 ~ "rapid", 
    rapid_bmi_change == 0 ~ "not rapid"
  ))


## flag diabetes meds
pandemic <- pandemic %>% mutate_at(vars("insulin_meds", "oad_meds"), ~replace_na(.,0))


pandemic <- pandemic %>% dplyr::mutate(diabetes_med = case_when(
  insulin_meds == 1 ~ "insulin", 
  ((insulin_meds == 0) & (oad_meds == 1)) ~ "oad", 
  ((insulin_meds == 0) & (oad_meds == 0)) ~ "lifestyle"
))

###########################
## create stratified data
data_black <- pandemic %>% 
  dplyr::filter(eth_collapsed == "black")

data_south_asian <- pandemic %>% 
  dplyr::filter(eth_collapsed == "south_asian")



#######################
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
    #adorn_percentages()   %>% 
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

function_6 <- function(data, my_var) {
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


function_7 <- function(data, my_var) {
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



####################################
####################################

## pandemic Analyses:






sex <-function_1(data_black,  sex)
age_group_2 <-function_1(data_black,  age_group_2)
eth_group_16 <-function_1(data_black,  eth_collapsed)
imd <-function_1(data_black,  imd)
precovid_bmi_category <-function_1(data_black,  precovid_bmi_category)
hypertension <-function_1(data_black,   hypertension)
diabetes_med <-function_1(data_black,   diabetes_med)
diabetes_t2 <-function_1(data_black,   diabetes_t2)
chronic_cardiac <-function_1(data_black,   chronic_cardiac)
learning_disability <-function_1(data_black,   learning_disability)
depression <-function_1(data_black,   depression)
dementia <-function_1(data_black,  dementia)
psychosis_schiz_bipolar <-function_1(data_black,   psychosis_schiz_bipolar)
asthma <-function_1(data_black,   asthma)
COPD <-function_1(data_black,   COPD)
stroke_and_TIA <-function_1(data_black,   stroke_and_TIA)
region <-function_1(data_black,  region)
all <- function_1(data_black, pandemic_stage)

complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all) %>% 
  dplyr::select(variable, group, n)

##


##
sex <-function_3(data_black,  sex)
age_group_2 <-function_3(data_black,  age_group_2)
eth_group_16 <-function_3(data_black,  eth_collapsed)
imd <-function_3(data_black,  imd)
precovid_bmi_category <-function_3(data_black,  precovid_bmi_category)
hypertension <-function_3(data_black,   hypertension)
diabetes_med <-function_3(data_black,   diabetes_med)
diabetes_t2 <-function_3(data_black,   diabetes_t2)
chronic_cardiac <-function_3(data_black,   chronic_cardiac)
learning_disability <-function_3(data_black,   learning_disability)
depression <-function_3(data_black,   depression)
dementia <-function_3(data_black,  dementia)
psychosis_schiz_bipolar <-function_3(data_black,   psychosis_schiz_bipolar)
asthma <-function_3(data_black,   asthma)
COPD <-function_3(data_black,   COPD)
stroke_and_TIA <-function_3(data_black,   stroke_and_TIA)
region <-function_3(data_black,  region)
all <- function_3(data_black, pandemic_stage)

rapid <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all)


##
sex <-function_4(data_black,  sex)
age_group_2 <-function_4(data_black,  age_group_2)
eth_group_16 <-function_4(data_black,  eth_collapsed)
imd <-function_4(data_black,  imd)
precovid_bmi_category <-function_4(data_black,  precovid_bmi_category)
hypertension <-function_4(data_black,   hypertension)
diabetes_med <-function_4(data_black,   diabetes_med)
diabetes_t2 <-function_4(data_black,   diabetes_t2)
chronic_cardiac <-function_4(data_black,   chronic_cardiac)
learning_disability <-function_4(data_black,   learning_disability)
depression <-function_4(data_black,   depression)
dementia <-function_4(data_black,  dementia)
psychosis_schiz_bipolar <-function_4(data_black,   psychosis_schiz_bipolar)
asthma <-function_4(data_black,   asthma)
COPD <-function_4(data_black,   COPD)
stroke_and_TIA <-function_4(data_black,   stroke_and_TIA)
region <-function_4(data_black,  region)
all <- function_4(data_black, pandemic_stage)

delta_mean <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA)  %>%
  bind_rows(region) %>% 
  bind_rows(all)


## median/IQR
sex <-function_5(data_black,  sex)
age_group_2 <-function_5(data_black,  age_group_2)
eth_group_16 <-function_5(data_black,  eth_collapsed)
imd <-function_5(data_black,  imd)
precovid_bmi_category <-function_5(data_black,  precovid_bmi_category)
hypertension <-function_5(data_black,   hypertension)
diabetes_med <-function_5(data_black,   diabetes_med)
diabetes_t2 <-function_5(data_black,   diabetes_t2)
chronic_cardiac <-function_5(data_black,   chronic_cardiac)
learning_disability <-function_5(data_black,   learning_disability)
depression <-function_5(data_black,   depression)
dementia <-function_5(data_black,  dementia)
psychosis_schiz_bipolar <-function_5(data_black,   psychosis_schiz_bipolar)
asthma <-function_5(data_black,   asthma)
COPD <-function_5(data_black,   COPD)
stroke_and_TIA <-function_5(data_black,   stroke_and_TIA)
region <-function_5(data_black,  region)
all <- function_5(data_black, pandemic_stage)

delta_median <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all)

## time_1
sex <-function_6(data_black,  sex)
age_group_2 <-function_6(data_black,  age_group_2)
eth_group_16 <-function_6(data_black,  eth_collapsed)
imd <-function_6(data_black,  imd)
precovid_bmi_category <-function_6(data_black,  precovid_bmi_category)
hypertension <-function_6(data_black,   hypertension)
diabetes_med <-function_6(data_black,   diabetes_med)
diabetes_t2 <-function_6(data_black,   diabetes_t2)
chronic_cardiac <-function_6(data_black,   chronic_cardiac)
learning_disability <-function_6(data_black,   learning_disability)
depression <-function_6(data_black,   depression)
dementia <-function_6(data_black,  dementia)
psychosis_schiz_bipolar <-function_6(data_black,   psychosis_schiz_bipolar)
asthma <-function_6(data_black,   asthma)
COPD <-function_6(data_black,   COPD)
stroke_and_TIA <-function_6(data_black,   stroke_and_TIA)
region <-function_6(data_black,  region)
all <- function_6(data_black, pandemic_stage)

time_1 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all)


## time_2
sex <-function_7(data_black,  sex)
age_group_2 <-function_7(data_black,  age_group_2)
eth_group_16 <-function_7(data_black,  eth_collapsed)
imd <-function_7(data_black,  imd)
precovid_bmi_category <-function_7(data_black,  precovid_bmi_category)
hypertension <-function_7(data_black,   hypertension)
diabetes_med <-function_7(data_black,   diabetes_med)
diabetes_t2 <-function_7(data_black,   diabetes_t2)
chronic_cardiac <-function_7(data_black,   chronic_cardiac)
learning_disability <-function_7(data_black,   learning_disability)
depression <-function_7(data_black,   depression)
dementia <-function_7(data_black,  dementia)
psychosis_schiz_bipolar <-function_7(data_black,   psychosis_schiz_bipolar)
asthma <-function_7(data_black,   asthma)
COPD <-function_7(data_black,   COPD)
stroke_and_TIA <-function_7(data_black,   stroke_and_TIA)
region <-function_7(data_black,  region)
all <- function_7(data_black, pandemic_stage)

time_2 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all)



complete_data_black <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  # dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::left_join(time_1) %>%
  dplyr::left_join(time_2) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_black <- complete_data_black  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_data_black$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_black$rapid, 5)) 

#################################################
#################################################




sex <-function_1(data_south_asian,  sex)
age_group_2 <-function_1(data_south_asian,  age_group_2)
eth_group_16 <-function_1(data_south_asian,  eth_collapsed)
imd <-function_1(data_south_asian,  imd)
precovid_bmi_category <-function_1(data_south_asian,  precovid_bmi_category)
hypertension <-function_1(data_south_asian,   hypertension)
diabetes_med <-function_1(data_south_asian,   diabetes_med)
diabetes_t2 <-function_1(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_1(data_south_asian,   chronic_cardiac)
learning_disability <-function_1(data_south_asian,   learning_disability)
depression <-function_1(data_south_asian,   depression)
dementia <-function_1(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_1(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_1(data_south_asian,   asthma)
COPD <-function_1(data_south_asian,   COPD)
stroke_and_TIA <-function_1(data_south_asian,   stroke_and_TIA)
region <-function_1(data_south_asian,  region)
all <- function_1(data_south_asian, pandemic_stage)

complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all) %>% 
  dplyr::select(variable, group, n)

##


##
sex <-function_3(data_south_asian,  sex)
age_group_2 <-function_3(data_south_asian,  age_group_2)
eth_group_16 <-function_3(data_south_asian,  eth_collapsed)
imd <-function_3(data_south_asian,  imd)
precovid_bmi_category <-function_3(data_south_asian,  precovid_bmi_category)
hypertension <-function_3(data_south_asian,   hypertension)
diabetes_med <-function_3(data_south_asian,   diabetes_med)
diabetes_t2 <-function_3(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_3(data_south_asian,   chronic_cardiac)
learning_disability <-function_3(data_south_asian,   learning_disability)
depression <-function_3(data_south_asian,   depression)
dementia <-function_3(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_3(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_3(data_south_asian,   asthma)
COPD <-function_3(data_south_asian,   COPD)
stroke_and_TIA <-function_3(data_south_asian,   stroke_and_TIA)
region <-function_3(data_south_asian,  region)
all <- function_3(data_south_asian, pandemic_stage)

rapid <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all)


##
sex <-function_4(data_south_asian,  sex)
age_group_2 <-function_4(data_south_asian,  age_group_2)
eth_group_16 <-function_4(data_south_asian,  eth_collapsed)
imd <-function_4(data_south_asian,  imd)
precovid_bmi_category <-function_4(data_south_asian,  precovid_bmi_category)
hypertension <-function_4(data_south_asian,   hypertension)
diabetes_med <-function_4(data_south_asian,   diabetes_med)
diabetes_t2 <-function_4(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_4(data_south_asian,   chronic_cardiac)
learning_disability <-function_4(data_south_asian,   learning_disability)
depression <-function_4(data_south_asian,   depression)
dementia <-function_4(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_4(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_4(data_south_asian,   asthma)
COPD <-function_4(data_south_asian,   COPD)
stroke_and_TIA <-function_4(data_south_asian,   stroke_and_TIA)
region <-function_4(data_south_asian,  region)
all <- function_4(data_south_asian, pandemic_stage)

delta_mean <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA)  %>%
  bind_rows(region) %>% 
  bind_rows(all)


## median/IQR
sex <-function_5(data_south_asian,  sex)
age_group_2 <-function_5(data_south_asian,  age_group_2)
eth_group_16 <-function_5(data_south_asian,  eth_collapsed)
imd <-function_5(data_south_asian,  imd)
precovid_bmi_category <-function_5(data_south_asian,  precovid_bmi_category)
hypertension <-function_5(data_south_asian,   hypertension)
diabetes_med <-function_5(data_south_asian,   diabetes_med)
diabetes_t2 <-function_5(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_5(data_south_asian,   chronic_cardiac)
learning_disability <-function_5(data_south_asian,   learning_disability)
depression <-function_5(data_south_asian,   depression)
dementia <-function_5(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_5(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_5(data_south_asian,   asthma)
COPD <-function_5(data_south_asian,   COPD)
stroke_and_TIA <-function_5(data_south_asian,   stroke_and_TIA)
region <-function_5(data_south_asian,  region)
all <- function_5(data_south_asian, pandemic_stage)

delta_median <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all)

## time_1
sex <-function_6(data_south_asian,  sex)
age_group_2 <-function_6(data_south_asian,  age_group_2)
eth_group_16 <-function_6(data_south_asian,  eth_collapsed)
imd <-function_6(data_south_asian,  imd)
precovid_bmi_category <-function_6(data_south_asian,  precovid_bmi_category)
hypertension <-function_6(data_south_asian,   hypertension)
diabetes_med <-function_6(data_south_asian,   diabetes_med)
diabetes_t2 <-function_6(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_6(data_south_asian,   chronic_cardiac)
learning_disability <-function_6(data_south_asian,   learning_disability)
depression <-function_6(data_south_asian,   depression)
dementia <-function_6(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_6(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_6(data_south_asian,   asthma)
COPD <-function_6(data_south_asian,   COPD)
stroke_and_TIA <-function_6(data_south_asian,   stroke_and_TIA)
region <-function_6(data_south_asian,  region)
all <- function_6(data_south_asian, pandemic_stage)

time_1 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all)


## time_2
sex <-function_7(data_south_asian,  sex)
age_group_2 <-function_7(data_south_asian,  age_group_2)
eth_group_16 <-function_7(data_south_asian,  eth_collapsed)
imd <-function_7(data_south_asian,  imd)
precovid_bmi_category <-function_7(data_south_asian,  precovid_bmi_category)
hypertension <-function_7(data_south_asian,   hypertension)
diabetes_med <-function_7(data_south_asian,   diabetes_med)
diabetes_t2 <-function_7(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_7(data_south_asian,   chronic_cardiac)
learning_disability <-function_7(data_south_asian,   learning_disability)
depression <-function_7(data_south_asian,   depression)
dementia <-function_7(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_7(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_7(data_south_asian,   asthma)
COPD <-function_7(data_south_asian,   COPD)
stroke_and_TIA <-function_7(data_south_asian,   stroke_and_TIA)
region <-function_7(data_south_asian,  region)
all <- function_7(data_south_asian, pandemic_stage)

time_2 <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(precovid_bmi_category) %>%
  bind_rows(hypertension) %>%
  bind_rows(diabetes_med) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>% 
  bind_rows(region) %>% 
  bind_rows(all)



complete_data_south_asian <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  # dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::left_join(time_1) %>%
  dplyr::left_join(time_2) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_south_asian <- complete_data_south_asian  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_data_south_asian$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_south_asian$rapid, 5))



#########################
#########################



write_csv (complete_black, here::here ("output/data","CC_delta_pandemic_counts_t2d_black.csv"))
write_csv (complete_south_asian, here::here ("output/data","CC_delta_pandemic_counts_t2d_south_asian.csv"))


