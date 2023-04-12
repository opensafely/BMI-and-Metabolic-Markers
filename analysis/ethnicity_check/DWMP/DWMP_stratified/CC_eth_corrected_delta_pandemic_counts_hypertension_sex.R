


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
  dplyr::filter(hypertension == TRUE)


## Flag rapid BMI change

pandemic <- pandemic %>% 
  dplyr::mutate(rapid_bmi_change = as.character(rapid_bmi_change)) %>% 
  dplyr::mutate(rapid_bmi_change = case_when(
    rapid_bmi_change == 1 ~ "rapid", 
    rapid_bmi_change == 0 ~ "not rapid"
  ))


###########################
## create stratified data
data_male <- pandemic %>% 
  dplyr::filter(sex == "M")

data_female <- pandemic %>% 
  dplyr::filter(sex == "F")



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






sex <-function_1(data_male,  sex)
age_group_2 <-function_1(data_male,  age_group_2)
eth_group_16 <-function_1(data_male,  eth_collapsed)
imd <-function_1(data_male,  imd)
precovid_bmi_category <-function_1(data_male,  precovid_bmi_category)
hypertension <-function_1(data_male,   hypertension)
diabetes_t1 <-function_1(data_male,   diabetes_t1)
diabetes_t2 <-function_1(data_male,   diabetes_t2)
chronic_cardiac <-function_1(data_male,   chronic_cardiac)
learning_disability <-function_1(data_male,   learning_disability)
depression <-function_1(data_male,   depression)
dementia <-function_1(data_male,  dementia)
psychosis_schiz_bipolar <-function_1(data_male,   psychosis_schiz_bipolar)
asthma <-function_1(data_male,   asthma)
COPD <-function_1(data_male,   COPD)
stroke_and_TIA <-function_1(data_male,   stroke_and_TIA)
region <-function_1(data_male,  region)
all <- function_1(data_male, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all) %>% 
  dplyr::select(variable, group, n)

##


##
sex <-function_3(data_male,  sex)
age_group_2 <-function_3(data_male,  age_group_2)
eth_group_16 <-function_3(data_male,  eth_collapsed)
imd <-function_3(data_male,  imd)
precovid_bmi_category <-function_3(data_male,  precovid_bmi_category)
hypertension <-function_3(data_male,   hypertension)
diabetes_t1 <-function_3(data_male,   diabetes_t1)
diabetes_t2 <-function_3(data_male,   diabetes_t2)
chronic_cardiac <-function_3(data_male,   chronic_cardiac)
learning_disability <-function_3(data_male,   learning_disability)
depression <-function_3(data_male,   depression)
dementia <-function_3(data_male,  dementia)
psychosis_schiz_bipolar <-function_3(data_male,   psychosis_schiz_bipolar)
asthma <-function_3(data_male,   asthma)
COPD <-function_3(data_male,   COPD)
stroke_and_TIA <-function_3(data_male,   stroke_and_TIA)
region <-function_3(data_male,  region)
all <- function_3(data_male, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all)


##
sex <-function_4(data_male,  sex)
age_group_2 <-function_4(data_male,  age_group_2)
eth_group_16 <-function_4(data_male,  eth_collapsed)
imd <-function_4(data_male,  imd)
precovid_bmi_category <-function_4(data_male,  precovid_bmi_category)
hypertension <-function_4(data_male,   hypertension)
diabetes_t1 <-function_4(data_male,   diabetes_t1)
diabetes_t2 <-function_4(data_male,   diabetes_t2)
chronic_cardiac <-function_4(data_male,   chronic_cardiac)
learning_disability <-function_4(data_male,   learning_disability)
depression <-function_4(data_male,   depression)
dementia <-function_4(data_male,  dementia)
psychosis_schiz_bipolar <-function_4(data_male,   psychosis_schiz_bipolar)
asthma <-function_4(data_male,   asthma)
COPD <-function_4(data_male,   COPD)
stroke_and_TIA <-function_4(data_male,   stroke_and_TIA)
region <-function_4(data_male,  region)
all <- function_4(data_male, pandemic_stage)

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
  bind_rows(stroke_and_TIA)  %>%
  bind_rows(region) %>% 
  bind_rows(all)


## median/IQR
sex <-function_5(data_male,  sex)
age_group_2 <-function_5(data_male,  age_group_2)
eth_group_16 <-function_5(data_male,  eth_collapsed)
imd <-function_5(data_male,  imd)
precovid_bmi_category <-function_5(data_male,  precovid_bmi_category)
hypertension <-function_5(data_male,   hypertension)
diabetes_t1 <-function_5(data_male,   diabetes_t1)
diabetes_t2 <-function_5(data_male,   diabetes_t2)
chronic_cardiac <-function_5(data_male,   chronic_cardiac)
learning_disability <-function_5(data_male,   learning_disability)
depression <-function_5(data_male,   depression)
dementia <-function_5(data_male,  dementia)
psychosis_schiz_bipolar <-function_5(data_male,   psychosis_schiz_bipolar)
asthma <-function_5(data_male,   asthma)
COPD <-function_5(data_male,   COPD)
stroke_and_TIA <-function_5(data_male,   stroke_and_TIA)
region <-function_5(data_male,  region)
all <- function_5(data_male, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all)

## time_1
sex <-function_6(data_male,  sex)
age_group_2 <-function_6(data_male,  age_group_2)
eth_group_16 <-function_6(data_male,  eth_collapsed)
imd <-function_6(data_male,  imd)
precovid_bmi_category <-function_6(data_male,  precovid_bmi_category)
hypertension <-function_6(data_male,   hypertension)
diabetes_t1 <-function_6(data_male,   diabetes_t1)
diabetes_t2 <-function_6(data_male,   diabetes_t2)
chronic_cardiac <-function_6(data_male,   chronic_cardiac)
learning_disability <-function_6(data_male,   learning_disability)
depression <-function_6(data_male,   depression)
dementia <-function_6(data_male,  dementia)
psychosis_schiz_bipolar <-function_6(data_male,   psychosis_schiz_bipolar)
asthma <-function_6(data_male,   asthma)
COPD <-function_6(data_male,   COPD)
stroke_and_TIA <-function_6(data_male,   stroke_and_TIA)
region <-function_6(data_male,  region)
all <- function_6(data_male, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all)


## time_2
sex <-function_7(data_male,  sex)
age_group_2 <-function_7(data_male,  age_group_2)
eth_group_16 <-function_7(data_male,  eth_collapsed)
imd <-function_7(data_male,  imd)
precovid_bmi_category <-function_7(data_male,  precovid_bmi_category)
hypertension <-function_7(data_male,   hypertension)
diabetes_t1 <-function_7(data_male,   diabetes_t1)
diabetes_t2 <-function_7(data_male,   diabetes_t2)
chronic_cardiac <-function_7(data_male,   chronic_cardiac)
learning_disability <-function_7(data_male,   learning_disability)
depression <-function_7(data_male,   depression)
dementia <-function_7(data_male,  dementia)
psychosis_schiz_bipolar <-function_7(data_male,   psychosis_schiz_bipolar)
asthma <-function_7(data_male,   asthma)
COPD <-function_7(data_male,   COPD)
stroke_and_TIA <-function_7(data_male,   stroke_and_TIA)
region <-function_7(data_male,  region)
all <- function_7(data_male, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all)



complete_data_male <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  # dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::left_join(time_1) %>%
  dplyr::left_join(time_2) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_male <- complete_data_male  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_data_male$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_male$rapid, 5)) %>% 
  dplyr::mutate(stage = "data_male", .before=1)

#################################################
#################################################




sex <-function_1(data_female,  sex)
age_group_2 <-function_1(data_female,  age_group_2)
eth_group_16 <-function_1(data_female,  eth_collapsed)
imd <-function_1(data_female,  imd)
precovid_bmi_category <-function_1(data_female,  precovid_bmi_category)
hypertension <-function_1(data_female,   hypertension)
diabetes_t1 <-function_1(data_female,   diabetes_t1)
diabetes_t2 <-function_1(data_female,   diabetes_t2)
chronic_cardiac <-function_1(data_female,   chronic_cardiac)
learning_disability <-function_1(data_female,   learning_disability)
depression <-function_1(data_female,   depression)
dementia <-function_1(data_female,  dementia)
psychosis_schiz_bipolar <-function_1(data_female,   psychosis_schiz_bipolar)
asthma <-function_1(data_female,   asthma)
COPD <-function_1(data_female,   COPD)
stroke_and_TIA <-function_1(data_female,   stroke_and_TIA)
region <-function_1(data_female,  region)
all <- function_1(data_female, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all) %>% 
  dplyr::select(variable, group, n)

##


##
sex <-function_3(data_female,  sex)
age_group_2 <-function_3(data_female,  age_group_2)
eth_group_16 <-function_3(data_female,  eth_collapsed)
imd <-function_3(data_female,  imd)
precovid_bmi_category <-function_3(data_female,  precovid_bmi_category)
hypertension <-function_3(data_female,   hypertension)
diabetes_t1 <-function_3(data_female,   diabetes_t1)
diabetes_t2 <-function_3(data_female,   diabetes_t2)
chronic_cardiac <-function_3(data_female,   chronic_cardiac)
learning_disability <-function_3(data_female,   learning_disability)
depression <-function_3(data_female,   depression)
dementia <-function_3(data_female,  dementia)
psychosis_schiz_bipolar <-function_3(data_female,   psychosis_schiz_bipolar)
asthma <-function_3(data_female,   asthma)
COPD <-function_3(data_female,   COPD)
stroke_and_TIA <-function_3(data_female,   stroke_and_TIA)
region <-function_3(data_female,  region)
all <- function_3(data_female, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all)


##
sex <-function_4(data_female,  sex)
age_group_2 <-function_4(data_female,  age_group_2)
eth_group_16 <-function_4(data_female,  eth_collapsed)
imd <-function_4(data_female,  imd)
precovid_bmi_category <-function_4(data_female,  precovid_bmi_category)
hypertension <-function_4(data_female,   hypertension)
diabetes_t1 <-function_4(data_female,   diabetes_t1)
diabetes_t2 <-function_4(data_female,   diabetes_t2)
chronic_cardiac <-function_4(data_female,   chronic_cardiac)
learning_disability <-function_4(data_female,   learning_disability)
depression <-function_4(data_female,   depression)
dementia <-function_4(data_female,  dementia)
psychosis_schiz_bipolar <-function_4(data_female,   psychosis_schiz_bipolar)
asthma <-function_4(data_female,   asthma)
COPD <-function_4(data_female,   COPD)
stroke_and_TIA <-function_4(data_female,   stroke_and_TIA)
region <-function_4(data_female,  region)
all <- function_4(data_female, pandemic_stage)

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
  bind_rows(stroke_and_TIA)  %>%
  bind_rows(region) %>% 
  bind_rows(all)


## median/IQR
sex <-function_5(data_female,  sex)
age_group_2 <-function_5(data_female,  age_group_2)
eth_group_16 <-function_5(data_female,  eth_collapsed)
imd <-function_5(data_female,  imd)
precovid_bmi_category <-function_5(data_female,  precovid_bmi_category)
hypertension <-function_5(data_female,   hypertension)
diabetes_t1 <-function_5(data_female,   diabetes_t1)
diabetes_t2 <-function_5(data_female,   diabetes_t2)
chronic_cardiac <-function_5(data_female,   chronic_cardiac)
learning_disability <-function_5(data_female,   learning_disability)
depression <-function_5(data_female,   depression)
dementia <-function_5(data_female,  dementia)
psychosis_schiz_bipolar <-function_5(data_female,   psychosis_schiz_bipolar)
asthma <-function_5(data_female,   asthma)
COPD <-function_5(data_female,   COPD)
stroke_and_TIA <-function_5(data_female,   stroke_and_TIA)
region <-function_5(data_female,  region)
all <- function_5(data_female, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all)

## time_1
sex <-function_6(data_female,  sex)
age_group_2 <-function_6(data_female,  age_group_2)
eth_group_16 <-function_6(data_female,  eth_collapsed)
imd <-function_6(data_female,  imd)
precovid_bmi_category <-function_6(data_female,  precovid_bmi_category)
hypertension <-function_6(data_female,   hypertension)
diabetes_t1 <-function_6(data_female,   diabetes_t1)
diabetes_t2 <-function_6(data_female,   diabetes_t2)
chronic_cardiac <-function_6(data_female,   chronic_cardiac)
learning_disability <-function_6(data_female,   learning_disability)
depression <-function_6(data_female,   depression)
dementia <-function_6(data_female,  dementia)
psychosis_schiz_bipolar <-function_6(data_female,   psychosis_schiz_bipolar)
asthma <-function_6(data_female,   asthma)
COPD <-function_6(data_female,   COPD)
stroke_and_TIA <-function_6(data_female,   stroke_and_TIA)
region <-function_6(data_female,  region)
all <- function_6(data_female, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all)


## time_2
sex <-function_7(data_female,  sex)
age_group_2 <-function_7(data_female,  age_group_2)
eth_group_16 <-function_7(data_female,  eth_collapsed)
imd <-function_7(data_female,  imd)
precovid_bmi_category <-function_7(data_female,  precovid_bmi_category)
hypertension <-function_7(data_female,   hypertension)
diabetes_t1 <-function_7(data_female,   diabetes_t1)
diabetes_t2 <-function_7(data_female,   diabetes_t2)
chronic_cardiac <-function_7(data_female,   chronic_cardiac)
learning_disability <-function_7(data_female,   learning_disability)
depression <-function_7(data_female,   depression)
dementia <-function_7(data_female,  dementia)
psychosis_schiz_bipolar <-function_7(data_female,   psychosis_schiz_bipolar)
asthma <-function_7(data_female,   asthma)
COPD <-function_7(data_female,   COPD)
stroke_and_TIA <-function_7(data_female,   stroke_and_TIA)
region <-function_7(data_female,  region)
all <- function_7(data_female, pandemic_stage)

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
  bind_rows(region) %>% 
  bind_rows(all)



complete_data_female <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  # dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::left_join(time_1) %>%
  dplyr::left_join(time_2) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_female <- complete_data_female  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_data_female$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_female$rapid, 5)) %>% 
  dplyr::mutate(stage = "data_female", .before=1)



#########################
#########################

write_csv (complete_male, here::here ("output/data","CC_delta_pandemic_counts_hypertension_male.csv"))
write_csv (complete_female, here::here ("output/data","CC_delta_pandemic_counts_hypertension_female.csv"))



