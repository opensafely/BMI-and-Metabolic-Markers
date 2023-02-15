
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
  dplyr::mutate(eth_collapsed = as.factor(eth_collapsed)) 


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
data_IMD1 <- my_data %>% 
  dplyr::filter(imd == "1")

### FILTER 
data_IMD5 <- my_data %>% 
  dplyr::filter(imd == "5")



########################################

####################################
## black ANALYSIS

sex <-function_1(data_IMD1,  sex)
age_group_2 <-function_1(data_IMD1,  age_group_2)
eth_collapsed <-function_1(data_IMD1,  eth_collapsed)
region <-function_1(data_IMD1,  region)
hypertension <-function_1(data_IMD1,   hypertension)
diabetes_t1 <-function_1(data_IMD1,   diabetes_t1)
diabetes_t2 <-function_1(data_IMD1,   diabetes_t2)
chronic_cardiac <-function_1(data_IMD1,   chronic_cardiac)
learning_disability <-function_1(data_IMD1,   learning_disability)
depression <-function_1(data_IMD1,   depression)
dementia <-function_1(data_IMD1,  dementia)
psychosis_schiz_bipolar <-function_1(data_IMD1,   psychosis_schiz_bipolar)
asthma <-function_1(data_IMD1,   asthma)
COPD <-function_1(data_IMD1,   COPD)
stroke_and_TIA <-function_1(data_IMD1,   stroke_and_TIA)
smoking_status <-function_1(data_IMD1,  smoking_status)

complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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

sex <-function_2(data_IMD1,  sex)
age_group_2 <-function_2(data_IMD1,  age_group_2)
eth_collapsed <-function_2(data_IMD1,  eth_collapsed)
region <-function_2(data_IMD1,  region)
hypertension <-function_2(data_IMD1,   hypertension)
diabetes_t1 <-function_2(data_IMD1,   diabetes_t1)
diabetes_t2 <-function_2(data_IMD1,   diabetes_t2)
chronic_cardiac <-function_2(data_IMD1,   chronic_cardiac)
learning_disability <-function_2(data_IMD1,   learning_disability)
depression <-function_2(data_IMD1,   depression)
dementia <-function_2(data_IMD1,  dementia)
psychosis_schiz_bipolar <-function_2(data_IMD1,   psychosis_schiz_bipolar)
asthma <-function_2(data_IMD1,   asthma)
COPD <-function_2(data_IMD1,   COPD)
stroke_and_TIA <-function_2(data_IMD1,   stroke_and_TIA)
smoking_status <-function_2(data_IMD1,  smoking_status)

delta_categories <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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
sex <-function_3(data_IMD1,  sex)
age_group_2 <-function_3(data_IMD1,  age_group_2)
eth_collapsed <-function_3(data_IMD1,  eth_collapsed)
region <-function_3(data_IMD1,  region)
hypertension <-function_3(data_IMD1,   hypertension)
diabetes_t1 <-function_3(data_IMD1,   diabetes_t1)
diabetes_t2 <-function_3(data_IMD1,   diabetes_t2)
chronic_cardiac <-function_3(data_IMD1,   chronic_cardiac)
learning_disability <-function_3(data_IMD1,   learning_disability)
depression <-function_3(data_IMD1,   depression)
dementia <-function_3(data_IMD1,  dementia)
psychosis_schiz_bipolar <-function_3(data_IMD1,   psychosis_schiz_bipolar)
asthma <-function_3(data_IMD1,   asthma)
COPD <-function_3(data_IMD1,   COPD)
stroke_and_TIA <-function_3(data_IMD1,   stroke_and_TIA)
smoking_status <-function_3(data_IMD1,  smoking_status)

rapid <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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
sex <-function_4(data_IMD1,  sex)
age_group_2 <-function_4(data_IMD1,  age_group_2)
eth_collapsed <-function_4(data_IMD1,  eth_collapsed)
region <-function_4(data_IMD1,  region)
hypertension <-function_4(data_IMD1,   hypertension)
diabetes_t1 <-function_4(data_IMD1,   diabetes_t1)
diabetes_t2 <-function_4(data_IMD1,   diabetes_t2)
chronic_cardiac <-function_4(data_IMD1,   chronic_cardiac)
learning_disability <-function_4(data_IMD1,   learning_disability)
depression <-function_4(data_IMD1,   depression)
dementia <-function_4(data_IMD1,  dementia)
psychosis_schiz_bipolar <-function_4(data_IMD1,   psychosis_schiz_bipolar)
asthma <-function_4(data_IMD1,   asthma)
COPD <-function_4(data_IMD1,   COPD)
stroke_and_TIA <-function_4(data_IMD1,   stroke_and_TIA)
smoking_status <-function_4(data_IMD1,  smoking_status)

delta_mean <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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
sex <-function_5(data_IMD1,  sex)
age_group_2 <-function_5(data_IMD1,  age_group_2)
eth_collapsed <-function_5(data_IMD1,  eth_collapsed)
region <-function_5(data_IMD1,  region)
hypertension <-function_5(data_IMD1,   hypertension)
diabetes_t1 <-function_5(data_IMD1,   diabetes_t1)
diabetes_t2 <-function_5(data_IMD1,   diabetes_t2)
chronic_cardiac <-function_5(data_IMD1,   chronic_cardiac)
learning_disability <-function_5(data_IMD1,   learning_disability)
depression <-function_5(data_IMD1,   depression)
dementia <-function_5(data_IMD1,  dementia)
psychosis_schiz_bipolar <-function_5(data_IMD1,   psychosis_schiz_bipolar)
asthma <-function_5(data_IMD1,   asthma)
COPD <-function_5(data_IMD1,   COPD)
stroke_and_TIA <-function_5(data_IMD1,   stroke_and_TIA)
smoking_status <-function_5(data_IMD1,  smoking_status)

delta_median <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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



complete_data_IMD1 <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_data_IMD1 <- complete_data_IMD1  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%  
  dplyr::mutate(n_pop = plyr::round_any(complete_data_IMD1$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_IMD1$rapid, 5)) %>% 
  dplyr::mutate(stage = "data_IMD1", .before=1)

#########################################################################################

####################################
## south_asian ANALYSIS

sex <-function_1(data_IMD5,  sex)
age_group_2 <-function_1(data_IMD5,  age_group_2)
eth_collapsed <-function_1(data_IMD5,  eth_collapsed)
region <-function_1(data_IMD5,  region)
hypertension <-function_1(data_IMD5,   hypertension)
diabetes_t1 <-function_1(data_IMD5,   diabetes_t1)
diabetes_t2 <-function_1(data_IMD5,   diabetes_t2)
chronic_cardiac <-function_1(data_IMD5,   chronic_cardiac)
learning_disability <-function_1(data_IMD5,   learning_disability)
depression <-function_1(data_IMD5,   depression)
dementia <-function_1(data_IMD5,  dementia)
psychosis_schiz_bipolar <-function_1(data_IMD5,   psychosis_schiz_bipolar)
asthma <-function_1(data_IMD5,   asthma)
COPD <-function_1(data_IMD5,   COPD)
stroke_and_TIA <-function_1(data_IMD5,   stroke_and_TIA)
smoking_status <-function_1(data_IMD5,  smoking_status)

complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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

sex <-function_2(data_IMD5,  sex)
age_group_2 <-function_2(data_IMD5,  age_group_2)
eth_collapsed <-function_2(data_IMD5,  eth_collapsed)
region <-function_2(data_IMD5,  region)
hypertension <-function_2(data_IMD5,   hypertension)
diabetes_t1 <-function_2(data_IMD5,   diabetes_t1)
diabetes_t2 <-function_2(data_IMD5,   diabetes_t2)
chronic_cardiac <-function_2(data_IMD5,   chronic_cardiac)
learning_disability <-function_2(data_IMD5,   learning_disability)
depression <-function_2(data_IMD5,   depression)
dementia <-function_2(data_IMD5,  dementia)
psychosis_schiz_bipolar <-function_2(data_IMD5,   psychosis_schiz_bipolar)
asthma <-function_2(data_IMD5,   asthma)
COPD <-function_2(data_IMD5,   COPD)
stroke_and_TIA <-function_2(data_IMD5,   stroke_and_TIA)
smoking_status <-function_2(data_IMD5,  smoking_status)

delta_categories <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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
sex <-function_3(data_IMD5,  sex)
age_group_2 <-function_3(data_IMD5,  age_group_2)
eth_collapsed <-function_3(data_IMD5,  eth_collapsed)
region <-function_3(data_IMD5,  region)
hypertension <-function_3(data_IMD5,   hypertension)
diabetes_t1 <-function_3(data_IMD5,   diabetes_t1)
diabetes_t2 <-function_3(data_IMD5,   diabetes_t2)
chronic_cardiac <-function_3(data_IMD5,   chronic_cardiac)
learning_disability <-function_3(data_IMD5,   learning_disability)
depression <-function_3(data_IMD5,   depression)
dementia <-function_3(data_IMD5,  dementia)
psychosis_schiz_bipolar <-function_3(data_IMD5,   psychosis_schiz_bipolar)
asthma <-function_3(data_IMD5,   asthma)
COPD <-function_3(data_IMD5,   COPD)
stroke_and_TIA <-function_3(data_IMD5,   stroke_and_TIA)
smoking_status <-function_3(data_IMD5,  smoking_status)

rapid <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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
sex <-function_4(data_IMD5,  sex)
age_group_2 <-function_4(data_IMD5,  age_group_2)
eth_collapsed <-function_4(data_IMD5,  eth_collapsed)
region <-function_4(data_IMD5,  region)
hypertension <-function_4(data_IMD5,   hypertension)
diabetes_t1 <-function_4(data_IMD5,   diabetes_t1)
diabetes_t2 <-function_4(data_IMD5,   diabetes_t2)
chronic_cardiac <-function_4(data_IMD5,   chronic_cardiac)
learning_disability <-function_4(data_IMD5,   learning_disability)
depression <-function_4(data_IMD5,   depression)
dementia <-function_4(data_IMD5,  dementia)
psychosis_schiz_bipolar <-function_4(data_IMD5,   psychosis_schiz_bipolar)
asthma <-function_4(data_IMD5,   asthma)
COPD <-function_4(data_IMD5,   COPD)
stroke_and_TIA <-function_4(data_IMD5,   stroke_and_TIA)
smoking_status <-function_4(data_IMD5,  smoking_status)

delta_mean <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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
sex <-function_5(data_IMD5,  sex)
age_group_2 <-function_5(data_IMD5,  age_group_2)
eth_collapsed <-function_5(data_IMD5,  eth_collapsed)
region <-function_5(data_IMD5,  region)
hypertension <-function_5(data_IMD5,   hypertension)
diabetes_t1 <-function_5(data_IMD5,   diabetes_t1)
diabetes_t2 <-function_5(data_IMD5,   diabetes_t2)
chronic_cardiac <-function_5(data_IMD5,   chronic_cardiac)
learning_disability <-function_5(data_IMD5,   learning_disability)
depression <-function_5(data_IMD5,   depression)
dementia <-function_5(data_IMD5,  dementia)
psychosis_schiz_bipolar <-function_5(data_IMD5,   psychosis_schiz_bipolar)
asthma <-function_5(data_IMD5,   asthma)
COPD <-function_5(data_IMD5,   COPD)
stroke_and_TIA <-function_5(data_IMD5,   stroke_and_TIA)
smoking_status <-function_5(data_IMD5,  smoking_status)

delta_median <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_collapsed) %>%
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



complete_data_IMD5 <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_data_IMD5 <- complete_data_IMD5  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%
  dplyr::mutate(n_pop = plyr::round_any(complete_data_IMD5$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_IMD5$rapid, 5)) %>% 
  dplyr::mutate(stage = "data_IMD5", .before=1)


#######################################################################################



#########################################################################################

write_csv (complete_data_IMD1, here::here ("output/data","CC_delta_summary_stats_IMD1.csv"))
write_csv (complete_data_IMD5, here::here ("output/data","CC_delta_summary_stats_IMD5.csv"))








