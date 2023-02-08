
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

#my_data <- read_csv("~/Documents/Academic GP/Open Safely/Dummy Data/CC_stratified_analysis_delta_data_pandemic.csv")


my_data <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_data_pandemic.csv"))


## M Samuel


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


my_data %>% 
  tabyl(precovid_bmi_category)

####################################
####################################
### FILTER 
data_healthy <- my_data %>% 
  dplyr::filter(precovid_bmi_category == "healthy")

### FILTER 
data_overweight <- my_data %>% 
  dplyr::filter(precovid_bmi_category == "overweight")


### FILTER 
data_obese <- my_data %>% 
  dplyr::filter(precovid_bmi_category == "obese")



####################################
## healthy ANALYSIS

all <- function_1(data_healthy, precovid_bmi_category)
age <-function_1(data_healthy,  age_group_2)
sex <-function_1(data_healthy,  sex)
eth_collapsed <-function_1(data_healthy,  eth_collapsed)
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

complete <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_2(data_healthy, precovid_bmi_category)
age <-function_2(data_healthy,  age_group_2)
sex <-function_2(data_healthy,  sex)
eth_collapsed <-function_2(data_healthy,  eth_collapsed)
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

delta_categories <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_3(data_healthy, precovid_bmi_category)
age <-function_3(data_healthy,  age_group_2)
sex <-function_3(data_healthy,  sex)
eth_collapsed <-function_3(data_healthy,  eth_collapsed)
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

rapid <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_4(data_healthy, precovid_bmi_category)
age <-function_4(data_healthy,  age_group_2)
sex <-function_4(data_healthy,  sex)
eth_collapsed <-function_4(data_healthy,  eth_collapsed)
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

delta_mean <- all %>% 
  bind_rows(sex) %>% 
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_5(data_healthy, precovid_bmi_category)
age <-function_5(data_healthy,  age_group_2)
sex <-function_5(data_healthy,  sex)
eth_collapsed <-function_5(data_healthy,  eth_collapsed)
imd <-function_5(data_healthy,  imd)
region <-function_5(data_healthy,  region)
hypertension <-function_5(data_healthy,   hypertension)
diabetes_t1 <-function_5(data_healthy,   diabetes_t1)
diabetes_t2 <-function_5(data_healthy,   diabetes_t2)
chronic_cardiac <-function_5(data_healthy,   chronic_cardiac)
learning_disability <-function_5(data_healthy,   learning_disability)
depression <-function_5(data_healthy,   depression)
dementia <-function_5(data_healthy,  dementia)
psychosis_schiz_bipolar <-function_5(data_healthy,   psychosis_schiz_bipolar)
asthma <-function_5(data_healthy,   asthma)
COPD <-function_5(data_healthy,   COPD)
stroke_and_TIA <-function_5(data_healthy,   stroke_and_TIA)
smoking_status <-function_5(data_healthy,  smoking_status)

delta_median <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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



complete_data_healthy <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_data_healthy <- complete_data_healthy  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%
  dplyr::mutate(n_pop = plyr::round_any(complete_data_healthy$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_healthy$rapid, 5)) %>% 
  dplyr::mutate(stage = "data_healthy", .before=1)

#########################################################################################



####################################
## overweight ANALYSIS

all <- function_1(data_overweight, precovid_bmi_category)
age <-function_1(data_overweight,  age_group_2)
sex <-function_1(data_overweight,  sex)
eth_collapsed <-function_1(data_overweight,  eth_collapsed)
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

complete <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_2(data_overweight, precovid_bmi_category)
age <-function_2(data_overweight,  age_group_2)
sex <-function_2(data_overweight,  sex)
eth_collapsed <-function_2(data_overweight,  eth_collapsed)
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

delta_categories <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_3(data_overweight, precovid_bmi_category)
age <-function_3(data_overweight,  age_group_2)
sex <-function_3(data_overweight,  sex)
eth_collapsed <-function_3(data_overweight,  eth_collapsed)
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

rapid <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_4(data_overweight, precovid_bmi_category)
age <-function_4(data_overweight,  age_group_2)
sex <-function_4(data_overweight,  sex)
eth_collapsed <-function_4(data_overweight,  eth_collapsed)
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

delta_mean <- all %>% 
  bind_rows(sex) %>% 
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_5(data_overweight, precovid_bmi_category)
age <-function_5(data_overweight,  age_group_2)
sex <-function_5(data_overweight,  sex)
eth_collapsed <-function_5(data_overweight,  eth_collapsed)
imd <-function_5(data_overweight,  imd)
region <-function_5(data_overweight,  region)
hypertension <-function_5(data_overweight,   hypertension)
diabetes_t1 <-function_5(data_overweight,   diabetes_t1)
diabetes_t2 <-function_5(data_overweight,   diabetes_t2)
chronic_cardiac <-function_5(data_overweight,   chronic_cardiac)
learning_disability <-function_5(data_overweight,   learning_disability)
depression <-function_5(data_overweight,   depression)
dementia <-function_5(data_overweight,  dementia)
psychosis_schiz_bipolar <-function_5(data_overweight,   psychosis_schiz_bipolar)
asthma <-function_5(data_overweight,   asthma)
COPD <-function_5(data_overweight,   COPD)
stroke_and_TIA <-function_5(data_overweight,   stroke_and_TIA)
smoking_status <-function_5(data_overweight,  smoking_status)

delta_median <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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



complete_data_overweight <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_data_overweight <- complete_data_overweight  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%
  dplyr::mutate(n_pop = plyr::round_any(complete_data_overweight$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_overweight$rapid, 5)) %>% 
  dplyr::mutate(stage = "data_overweight", .before=1)

#########################################################################################


####################################
## obese ANALYSIS

all <- function_1(data_obese, precovid_bmi_category)
age <-function_1(data_obese,  age_group_2)
sex <-function_1(data_obese,  sex)
eth_collapsed <-function_1(data_obese,  eth_collapsed)
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

complete <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_2(data_obese, precovid_bmi_category)
age <-function_2(data_obese,  age_group_2)
sex <-function_2(data_obese,  sex)
eth_collapsed <-function_2(data_obese,  eth_collapsed)
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

delta_categories <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_3(data_obese, precovid_bmi_category)
age <-function_3(data_obese,  age_group_2)
sex <-function_3(data_obese,  sex)
eth_collapsed <-function_3(data_obese,  eth_collapsed)
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

rapid <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_4(data_obese, precovid_bmi_category)
age <-function_4(data_obese,  age_group_2)
sex <-function_4(data_obese,  sex)
eth_collapsed <-function_4(data_obese,  eth_collapsed)
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

delta_mean <- all %>% 
  bind_rows(sex) %>% 
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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
all <- function_5(data_obese, precovid_bmi_category)
age <-function_5(data_obese,  age_group_2)
sex <-function_5(data_obese,  sex)
eth_collapsed <-function_5(data_obese,  eth_collapsed)
imd <-function_5(data_obese,  imd)
region <-function_5(data_obese,  region)
hypertension <-function_5(data_obese,   hypertension)
diabetes_t1 <-function_5(data_obese,   diabetes_t1)
diabetes_t2 <-function_5(data_obese,   diabetes_t2)
chronic_cardiac <-function_5(data_obese,   chronic_cardiac)
learning_disability <-function_5(data_obese,   learning_disability)
depression <-function_5(data_obese,   depression)
dementia <-function_5(data_obese,  dementia)
psychosis_schiz_bipolar <-function_5(data_obese,   psychosis_schiz_bipolar)
asthma <-function_5(data_obese,   asthma)
COPD <-function_5(data_obese,   COPD)
stroke_and_TIA <-function_5(data_obese,   stroke_and_TIA)
smoking_status <-function_5(data_obese,  smoking_status)

delta_median <- all %>% 
  bind_rows(sex) %>%
  bind_rows(age) %>%
  bind_rows(eth_collapsed) %>%
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



complete_data_obese <- complete  %>%
  dplyr::left_join (rapid) %>%
  dplyr::left_join(delta_mean) %>% 
  dplyr::left_join(delta_categories) %>% 
  dplyr::left_join(delta_median) %>%
  dplyr::mutate(n_pop = n, .before="n")  %>% 
  dplyr::select(-("n"))


complete_data_obese <- complete_data_obese  %>% 
  dplyr::select(-"percent", -"valid_percent") %>%
  dplyr::mutate(n_pop = plyr::round_any(complete_data_obese$n_pop, 5)) %>% 
  dplyr::mutate(rapid = plyr::round_any(complete_data_obese$rapid, 5)) %>% 
  dplyr::mutate(stage = "data_obese", .before=1)









#########################################################################################

write_csv (complete_data_healthy, here::here ("output/data","CC_delta_summary_stats_healthy.csv"))
write_csv (complete_data_overweight, here::here ("output/data","CC_delta_summary_stats_overweight.csv"))
write_csv (complete_data_obese, here::here ("output/data","CC_delta_summary_stats_obese.csv"))






