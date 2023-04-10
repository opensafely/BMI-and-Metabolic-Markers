


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

sex <-function_1(pandemic,  sex)
age_group_2 <-function_1(pandemic,  age_group_2)
eth_group_16 <-function_1(pandemic,  eth_collapsed)
imd <-function_1(pandemic,  imd)
precovid_bmi_category <-function_1(pandemic,  precovid_bmi_category)
hypertension <-function_1(pandemic,   hypertension)
diabetes_t1 <-function_1(pandemic,   diabetes_t1)
diabetes_t2 <-function_1(pandemic,   diabetes_t2)
chronic_cardiac <-function_1(pandemic,   chronic_cardiac)
learning_disability <-function_1(pandemic,   learning_disability)
depression <-function_1(pandemic,   depression)
dementia <-function_1(pandemic,  dementia)
psychosis_schiz_bipolar <-function_1(pandemic,   psychosis_schiz_bipolar)
asthma <-function_1(pandemic,   asthma)
COPD <-function_1(pandemic,   COPD)
stroke_and_TIA <-function_1(pandemic,   stroke_and_TIA)
all <- function_1(pandemic, pandemic_stage)

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
  bind_rows(all) %>% 
  dplyr::select(variable, group, n)

##


##
sex <-function_3(pandemic,  sex)
age_group_2 <-function_3(pandemic,  age_group_2)
eth_group_16 <-function_3(pandemic,  eth_collapsed)
imd <-function_3(pandemic,  imd)
precovid_bmi_category <-function_3(pandemic,  precovid_bmi_category)
hypertension <-function_3(pandemic,   hypertension)
diabetes_t1 <-function_3(pandemic,   diabetes_t1)
diabetes_t2 <-function_3(pandemic,   diabetes_t2)
chronic_cardiac <-function_3(pandemic,   chronic_cardiac)
learning_disability <-function_3(pandemic,   learning_disability)
depression <-function_3(pandemic,   depression)
dementia <-function_3(pandemic,  dementia)
psychosis_schiz_bipolar <-function_3(pandemic,   psychosis_schiz_bipolar)
asthma <-function_3(pandemic,   asthma)
COPD <-function_3(pandemic,   COPD)
stroke_and_TIA <-function_3(pandemic,   stroke_and_TIA)
all <- function_3(pandemic, pandemic_stage)

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
sex <-function_4(pandemic,  sex)
age_group_2 <-function_4(pandemic,  age_group_2)
eth_group_16 <-function_4(pandemic,  eth_collapsed)
imd <-function_4(pandemic,  imd)
precovid_bmi_category <-function_4(pandemic,  precovid_bmi_category)
hypertension <-function_4(pandemic,   hypertension)
diabetes_t1 <-function_4(pandemic,   diabetes_t1)
diabetes_t2 <-function_4(pandemic,   diabetes_t2)
chronic_cardiac <-function_4(pandemic,   chronic_cardiac)
learning_disability <-function_4(pandemic,   learning_disability)
depression <-function_4(pandemic,   depression)
dementia <-function_4(pandemic,  dementia)
psychosis_schiz_bipolar <-function_4(pandemic,   psychosis_schiz_bipolar)
asthma <-function_4(pandemic,   asthma)
COPD <-function_4(pandemic,   COPD)
stroke_and_TIA <-function_4(pandemic,   stroke_and_TIA)
all <- function_4(pandemic, pandemic_stage)

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
  bind_rows(all)


## median/IQR
sex <-function_5(pandemic,  sex)
age_group_2 <-function_5(pandemic,  age_group_2)
eth_group_16 <-function_5(pandemic,  eth_collapsed)
imd <-function_5(pandemic,  imd)
precovid_bmi_category <-function_5(pandemic,  precovid_bmi_category)
hypertension <-function_5(pandemic,   hypertension)
diabetes_t1 <-function_5(pandemic,   diabetes_t1)
diabetes_t2 <-function_5(pandemic,   diabetes_t2)
chronic_cardiac <-function_5(pandemic,   chronic_cardiac)
learning_disability <-function_5(pandemic,   learning_disability)
depression <-function_5(pandemic,   depression)
dementia <-function_5(pandemic,  dementia)
psychosis_schiz_bipolar <-function_5(pandemic,   psychosis_schiz_bipolar)
asthma <-function_5(pandemic,   asthma)
COPD <-function_5(pandemic,   COPD)
stroke_and_TIA <-function_5(pandemic,   stroke_and_TIA)
all <- function_5(pandemic, pandemic_stage)

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

## time_1
sex <-function_6(pandemic,  sex)
age_group_2 <-function_6(pandemic,  age_group_2)
eth_group_16 <-function_6(pandemic,  eth_collapsed)
imd <-function_6(pandemic,  imd)
precovid_bmi_category <-function_6(pandemic,  precovid_bmi_category)
hypertension <-function_6(pandemic,   hypertension)
diabetes_t1 <-function_6(pandemic,   diabetes_t1)
diabetes_t2 <-function_6(pandemic,   diabetes_t2)
chronic_cardiac <-function_6(pandemic,   chronic_cardiac)
learning_disability <-function_6(pandemic,   learning_disability)
depression <-function_6(pandemic,   depression)
dementia <-function_6(pandemic,  dementia)
psychosis_schiz_bipolar <-function_6(pandemic,   psychosis_schiz_bipolar)
asthma <-function_6(pandemic,   asthma)
COPD <-function_6(pandemic,   COPD)
stroke_and_TIA <-function_6(pandemic,   stroke_and_TIA)
all <- function_6(pandemic, pandemic_stage)

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


## time_2
sex <-function_7(pandemic,  sex)
age_group_2 <-function_7(pandemic,  age_group_2)
eth_group_16 <-function_7(pandemic,  eth_collapsed)
imd <-function_7(pandemic,  imd)
precovid_bmi_category <-function_7(pandemic,  precovid_bmi_category)
hypertension <-function_7(pandemic,   hypertension)
diabetes_t1 <-function_7(pandemic,   diabetes_t1)
diabetes_t2 <-function_7(pandemic,   diabetes_t2)
chronic_cardiac <-function_7(pandemic,   chronic_cardiac)
learning_disability <-function_7(pandemic,   learning_disability)
depression <-function_7(pandemic,   depression)
dementia <-function_7(pandemic,  dementia)
psychosis_schiz_bipolar <-function_7(pandemic,   psychosis_schiz_bipolar)
asthma <-function_7(pandemic,   asthma)
COPD <-function_7(pandemic,   COPD)
stroke_and_TIA <-function_7(pandemic,   stroke_and_TIA)
all <- function_7(pandemic, pandemic_stage)

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



complete_pandemic <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  # dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::left_join(time_1) %>%
  dplyr::left_join(time_2) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_pandemic <- complete_pandemic  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_pandemic$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_pandemic$rapid, 5)) %>% 
  dplyr::mutate(stage = "pandemic", .before=1)


#############################################################
#####
#####


prepandemic <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_data_prepandemic.csv"))

### FILTER for hypertensives

prepandemic <- prepandemic %>% 
  dplyr::filter(diabetes_t2 == TRUE)


## Flag rapid BMI change

prepandemic <- prepandemic %>% 
  dplyr::mutate(rapid_bmi_change = as.character(rapid_bmi_change)) %>% 
  dplyr::mutate(rapid_bmi_change = case_when(
    rapid_bmi_change == 1 ~ "rapid", 
    rapid_bmi_change == 0 ~ "not rapid"
  ))





####################################
####################################

## prepandemic Analyses:

sex <-function_1(prepandemic,  sex)
age_group_2 <-function_1(prepandemic,  age_group_2)
eth_group_16 <-function_1(prepandemic,  eth_collapsed)
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
all <- function_1(prepandemic, pandemic_stage)


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
  bind_rows(all) %>% 
  dplyr::select(variable, group, n)

##


##
sex <-function_3(prepandemic,  sex)
age_group_2 <-function_3(prepandemic,  age_group_2)
eth_group_16 <-function_3(prepandemic,  eth_collapsed)
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
all <- function_3(prepandemic, pandemic_stage)


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
eth_group_16 <-function_4(prepandemic,  eth_collapsed)
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
all <- function_4(prepandemic, pandemic_stage)

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
eth_group_16 <-function_5(prepandemic,  eth_collapsed)
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
all <- function_5(prepandemic, pandemic_stage)

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

## time_1
sex <-function_6(prepandemic,  sex)
age_group_2 <-function_6(prepandemic,  age_group_2)
eth_group_16 <-function_6(prepandemic,  eth_collapsed)
imd <-function_6(prepandemic,  imd)
precovid_bmi_category <-function_6(prepandemic,  precovid_bmi_category)
hypertension <-function_6(prepandemic,   hypertension)
diabetes_t1 <-function_6(prepandemic,   diabetes_t1)
diabetes_t2 <-function_6(prepandemic,   diabetes_t2)
chronic_cardiac <-function_6(prepandemic,   chronic_cardiac)
learning_disability <-function_6(prepandemic,   learning_disability)
depression <-function_6(prepandemic,   depression)
dementia <-function_6(prepandemic,  dementia)
psychosis_schiz_bipolar <-function_6(prepandemic,   psychosis_schiz_bipolar)
asthma <-function_6(prepandemic,   asthma)
COPD <-function_6(prepandemic,   COPD)
stroke_and_TIA <-function_6(prepandemic,   stroke_and_TIA)
all <- function_6(prepandemic, pandemic_stage)

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
  bind_rows(stroke_and_TIA)%>% 
  bind_rows(all)


## time_2
sex <-function_7(prepandemic,  sex)
age_group_2 <-function_7(prepandemic,  age_group_2)
eth_group_16 <-function_7(prepandemic,  eth_collapsed)
imd <-function_7(prepandemic,  imd)
precovid_bmi_category <-function_7(prepandemic,  precovid_bmi_category)
hypertension <-function_7(prepandemic,   hypertension)
diabetes_t1 <-function_7(prepandemic,   diabetes_t1)
diabetes_t2 <-function_7(prepandemic,   diabetes_t2)
chronic_cardiac <-function_7(prepandemic,   chronic_cardiac)
learning_disability <-function_7(prepandemic,   learning_disability)
depression <-function_7(prepandemic,   depression)
dementia <-function_7(prepandemic,  dementia)
psychosis_schiz_bipolar <-function_7(prepandemic,   psychosis_schiz_bipolar)
asthma <-function_7(prepandemic,   asthma)
COPD <-function_7(prepandemic,   COPD)
stroke_and_TIA <-function_7(prepandemic,   stroke_and_TIA)
all <- function_7(prepandemic, pandemic_stage)

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
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  # dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::left_join(time_1) %>%
  dplyr::left_join(time_2) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_prepandemic <- complete_prepandemic  %>% 
  dplyr::mutate(n_pop = plyr::round_any(complete_prepandemic$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_prepandemic$rapid, 5)) %>% 
  dplyr::mutate(stage = "prepandemic", .before=1)



complete <- complete_pandemic %>% 
  bind_rows(complete_prepandemic)


write_csv (complete, here::here ("output/data","CC_delta_summary_stats_t2d.csv"))





