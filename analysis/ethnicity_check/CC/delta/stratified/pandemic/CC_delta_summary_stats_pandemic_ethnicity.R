
## M Samuel
## This R file looks at the stratified analyses of delta change
## Ethnicities are collapsed to prevent disclosure


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

#my_data <- read_csv("Documents/Academic GP/Open Safely/Dummy Data/CC_stratified_analysis_delta_data_pandemic.csv")

my_data <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_data_pandemic.csv"))


#########################
## Order ethnicity


my_data <- my_data %>%
  dplyr::mutate(eth_collapsed = factor(eth_collapsed, 
                                       levels = c("white",
                                                  "black", 
                                                  "south_asian", 
                                                  "mixed", 
                                                  "chinese_other")) ) 

my_data %>% 
  tabyl (eth_collapsed)

## IMD as factor
my_data <- my_data %>%
  dplyr::mutate(imd = as.factor(imd)) 


my_data <- my_data %>% 
  dplyr::mutate(rapid_bmi_change = as.character(rapid_bmi_change)) %>% 
  dplyr::mutate(rapid_bmi_change = case_when(
    rapid_bmi_change == 1 ~ "rapid", 
    rapid_bmi_change == 0 ~ "not rapid"
  ))

## change output to character to allow tabyl


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
### FILTER 
data_black <- my_data %>% 
  dplyr::filter(eth_collapsed == "black")

### FILTER 
data_south_asian <- my_data %>% 
  dplyr::filter(eth_collapsed == "south_asian")



########################################

####################################
## black ANALYSIS

sex <-function_1(data_black,  sex)
age_group_2 <-function_1(data_black,  age_group_2)
imd <-function_1(data_black,  imd)
region <-function_1(data_black,  region)
hypertension <-function_1(data_black,   hypertension)
diabetes_t1 <-function_1(data_black,   diabetes_t1)
diabetes_t2 <-function_1(data_black,   diabetes_t2)
chronic_cardiac <-function_1(data_black,   chronic_cardiac)
learning_disability <-function_1(data_black,   learning_disability)
depression <-function_1(data_black,   depression)
dementia <-function_1(data_black,  dementia)
psychosis_schiz_bipolar <-function_1(data_black,   psychosis_schiz_bipolar)
asthma <-function_1(data_black,   asthma)
COPD <-function_1(data_black,   COPD)
stroke_and_TIA <-function_1(data_black,   stroke_and_TIA)
smoking_status <-function_1(data_black,  smoking_status)

complete <- sex %>% 
  bind_rows(age_group_2) %>%
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

sex <-function_2(data_black,  sex)
age_group_2 <-function_2(data_black,  age_group_2)
imd <-function_2(data_black,  imd)
region <-function_2(data_black,  region)
hypertension <-function_2(data_black,   hypertension)
diabetes_t1 <-function_2(data_black,   diabetes_t1)
diabetes_t2 <-function_2(data_black,   diabetes_t2)
chronic_cardiac <-function_2(data_black,   chronic_cardiac)
learning_disability <-function_2(data_black,   learning_disability)
depression <-function_2(data_black,   depression)
dementia <-function_2(data_black,  dementia)
psychosis_schiz_bipolar <-function_2(data_black,   psychosis_schiz_bipolar)
asthma <-function_2(data_black,   asthma)
COPD <-function_2(data_black,   COPD)
stroke_and_TIA <-function_2(data_black,   stroke_and_TIA)
smoking_status <-function_2(data_black,  smoking_status)

delta_categories <- sex %>% 
  bind_rows(age_group_2) %>%
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
sex <-function_3(data_black,  sex)
age_group_2 <-function_3(data_black,  age_group_2)
imd <-function_3(data_black,  imd)
region <-function_3(data_black,  region)
hypertension <-function_3(data_black,   hypertension)
diabetes_t1 <-function_3(data_black,   diabetes_t1)
diabetes_t2 <-function_3(data_black,   diabetes_t2)
chronic_cardiac <-function_3(data_black,   chronic_cardiac)
learning_disability <-function_3(data_black,   learning_disability)
depression <-function_3(data_black,   depression)
dementia <-function_3(data_black,  dementia)
psychosis_schiz_bipolar <-function_3(data_black,   psychosis_schiz_bipolar)
asthma <-function_3(data_black,   asthma)
COPD <-function_3(data_black,   COPD)
stroke_and_TIA <-function_3(data_black,   stroke_and_TIA)
smoking_status <-function_3(data_black,  smoking_status)

rapid <- sex %>% 
  bind_rows(age_group_2) %>%
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
sex <-function_4(data_black,  sex)
age_group_2 <-function_4(data_black,  age_group_2)
imd <-function_4(data_black,  imd)
region <-function_4(data_black,  region)
hypertension <-function_4(data_black,   hypertension)
diabetes_t1 <-function_4(data_black,   diabetes_t1)
diabetes_t2 <-function_4(data_black,   diabetes_t2)
chronic_cardiac <-function_4(data_black,   chronic_cardiac)
learning_disability <-function_4(data_black,   learning_disability)
depression <-function_4(data_black,   depression)
dementia <-function_4(data_black,  dementia)
psychosis_schiz_bipolar <-function_4(data_black,   psychosis_schiz_bipolar)
asthma <-function_4(data_black,   asthma)
COPD <-function_4(data_black,   COPD)
stroke_and_TIA <-function_4(data_black,   stroke_and_TIA)
smoking_status <-function_4(data_black,  smoking_status)

delta_mean <- sex %>% 
  bind_rows(age_group_2) %>%
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


## median/IQR
sex <-function_5(data_black,  sex)
age_group_2 <-function_5(data_black,  age_group_2)
imd <-function_5(data_black,  imd)
region <-function_5(data_black,  region)
hypertension <-function_5(data_black,   hypertension)
diabetes_t1 <-function_5(data_black,   diabetes_t1)
diabetes_t2 <-function_5(data_black,   diabetes_t2)
chronic_cardiac <-function_5(data_black,   chronic_cardiac)
learning_disability <-function_5(data_black,   learning_disability)
depression <-function_5(data_black,   depression)
dementia <-function_5(data_black,  dementia)
psychosis_schiz_bipolar <-function_5(data_black,   psychosis_schiz_bipolar)
asthma <-function_5(data_black,   asthma)
COPD <-function_5(data_black,   COPD)
stroke_and_TIA <-function_5(data_black,   stroke_and_TIA)
smoking_status <-function_5(data_black,  smoking_status)

delta_median <- sex %>% 
  bind_rows(age_group_2) %>%
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



complete_data_black <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_data_black <- complete_data_black  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%
  dplyr::mutate(n_pop = plyr::round_any(complete_data_black$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_black$rapid, 5)) %>% 
  dplyr::mutate(stage = "data_black", .before=1)

#########################################################################################

####################################
## south_asian ANALYSIS

sex <-function_1(data_south_asian,  sex)
age_group_2 <-function_1(data_south_asian,  age_group_2)
imd <-function_1(data_south_asian,  imd)
region <-function_1(data_south_asian,  region)
hypertension <-function_1(data_south_asian,   hypertension)
diabetes_t1 <-function_1(data_south_asian,   diabetes_t1)
diabetes_t2 <-function_1(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_1(data_south_asian,   chronic_cardiac)
learning_disability <-function_1(data_south_asian,   learning_disability)
depression <-function_1(data_south_asian,   depression)
dementia <-function_1(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_1(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_1(data_south_asian,   asthma)
COPD <-function_1(data_south_asian,   COPD)
stroke_and_TIA <-function_1(data_south_asian,   stroke_and_TIA)
smoking_status <-function_1(data_south_asian,  smoking_status)

complete <- sex %>% 
  bind_rows(age_group_2) %>%
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

sex <-function_2(data_south_asian,  sex)
age_group_2 <-function_2(data_south_asian,  age_group_2)
imd <-function_2(data_south_asian,  imd)
region <-function_2(data_south_asian,  region)
hypertension <-function_2(data_south_asian,   hypertension)
diabetes_t1 <-function_2(data_south_asian,   diabetes_t1)
diabetes_t2 <-function_2(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_2(data_south_asian,   chronic_cardiac)
learning_disability <-function_2(data_south_asian,   learning_disability)
depression <-function_2(data_south_asian,   depression)
dementia <-function_2(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_2(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_2(data_south_asian,   asthma)
COPD <-function_2(data_south_asian,   COPD)
stroke_and_TIA <-function_2(data_south_asian,   stroke_and_TIA)
smoking_status <-function_2(data_south_asian,  smoking_status)

delta_categories <- sex %>% 
  bind_rows(age_group_2) %>%
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
sex <-function_3(data_south_asian,  sex)
age_group_2 <-function_3(data_south_asian,  age_group_2)
imd <-function_3(data_south_asian,  imd)
region <-function_3(data_south_asian,  region)
hypertension <-function_3(data_south_asian,   hypertension)
diabetes_t1 <-function_3(data_south_asian,   diabetes_t1)
diabetes_t2 <-function_3(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_3(data_south_asian,   chronic_cardiac)
learning_disability <-function_3(data_south_asian,   learning_disability)
depression <-function_3(data_south_asian,   depression)
dementia <-function_3(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_3(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_3(data_south_asian,   asthma)
COPD <-function_3(data_south_asian,   COPD)
stroke_and_TIA <-function_3(data_south_asian,   stroke_and_TIA)
smoking_status <-function_3(data_south_asian,  smoking_status)

rapid <- sex %>% 
  bind_rows(age_group_2) %>%
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
sex <-function_4(data_south_asian,  sex)
age_group_2 <-function_4(data_south_asian,  age_group_2)
imd <-function_4(data_south_asian,  imd)
region <-function_4(data_south_asian,  region)
hypertension <-function_4(data_south_asian,   hypertension)
diabetes_t1 <-function_4(data_south_asian,   diabetes_t1)
diabetes_t2 <-function_4(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_4(data_south_asian,   chronic_cardiac)
learning_disability <-function_4(data_south_asian,   learning_disability)
depression <-function_4(data_south_asian,   depression)
dementia <-function_4(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_4(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_4(data_south_asian,   asthma)
COPD <-function_4(data_south_asian,   COPD)
stroke_and_TIA <-function_4(data_south_asian,   stroke_and_TIA)
smoking_status <-function_4(data_south_asian,  smoking_status)

delta_mean <- sex %>% 
  bind_rows(age_group_2) %>%
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


## median/IQR
sex <-function_5(data_south_asian,  sex)
age_group_2 <-function_5(data_south_asian,  age_group_2)
imd <-function_5(data_south_asian,  imd)
region <-function_5(data_south_asian,  region)
hypertension <-function_5(data_south_asian,   hypertension)
diabetes_t1 <-function_5(data_south_asian,   diabetes_t1)
diabetes_t2 <-function_5(data_south_asian,   diabetes_t2)
chronic_cardiac <-function_5(data_south_asian,   chronic_cardiac)
learning_disability <-function_5(data_south_asian,   learning_disability)
depression <-function_5(data_south_asian,   depression)
dementia <-function_5(data_south_asian,  dementia)
psychosis_schiz_bipolar <-function_5(data_south_asian,   psychosis_schiz_bipolar)
asthma <-function_5(data_south_asian,   asthma)
COPD <-function_5(data_south_asian,   COPD)
stroke_and_TIA <-function_5(data_south_asian,   stroke_and_TIA)
smoking_status <-function_5(data_south_asian,  smoking_status)

delta_median <- sex %>% 
  bind_rows(age_group_2) %>%
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



complete_data_south_asian <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_data_south_asian <- complete_data_south_asian  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%
  dplyr::mutate(n_pop = plyr::round_any(complete_data_south_asian$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_south_asian$rapid, 5)) %>% 
  dplyr::mutate(stage = "data_south_asian", .before=1)


#######################################################################################

complete_data_black <- complete_data_black %>%
    dplyr::select ( "stage",       
                  "variable",     
                  "group",        
                  "n_pop",        
                  "rapid",        
                  "mean_delta",   
                  "sd_delta",     
                  "Q1",           
                  "median",      
                  "Q3"   )


complete_data_south_asian <- complete_data_south_asian %>%
    dplyr::select ( "stage",       
                  "variable",     
                  "group",        
                  "n_pop",        
                  "rapid",        
                  "mean_delta",   
                  "sd_delta",     
                  "Q1",           
                  "median",      
                  "Q3"   )

#########################################################################################

write_csv (complete_data_black, here::here ("output/data","CC_delta_summary_stats_black.csv"))
write_csv (complete_data_south_asian, here::here ("output/data","CC_delta_summary_stats_south_asian.csv"))








