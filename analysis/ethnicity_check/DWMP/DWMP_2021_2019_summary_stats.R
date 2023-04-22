#### Author: M Samuel
#### Date: March 2023
#### This script calculates the proportion of the population eligible for the DWMP
#### We are looking at this based on the 2019 weight of the 2021 population

## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(purrr)
library(broom)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)
library(skimr)






##################################
# READ IN FILES
##################################

dt <- read_csv(here::here("output/data", "BMI_complete_median_eth_corrected.csv"))

age_group <- read_feather(here::here ("output/data", "complete_meds_2021.feather"))





### create a joining file for age
age_group <- age_group %>% 
  dplyr::select("patient_id",
                "age_group_2")







dt <- dt %>% 
  dplyr::select(
    "patient_id", 
    "year",   
    "median_bmi", 
    "sex", 
    "region", 
    "imd", 
    "eth_16_corrected", 
    starts_with("comorbid"),
    "insulin_meds", 
    "oad_meds"             
  )             
  

dt <- dt %>% 
  left_join(age_group, by= 'patient_id')



dt %>% 
  tabyl(age_group_2)

# filter out those with problems with join
dt <- dt  %>% 
  drop_na(age_group_2)


## Filter out years with  missing BMI
dt <- dt %>% 
  drop_na(median_bmi)


dt <- dt %>% 
  dplyr::mutate(minority = case_when(
    (eth_16_corrected == "White_British" | eth_16_corrected == "White_Irish" | eth_16_corrected == "Other_White"|     eth_16_corrected == "None" ) ~ "white", 

  )) 

 dt <- dt %>%  mutate(
  minority  = as.character(minority),
  minority = ifelse(is.na(minority), "not_white", minority),
  minority = as.factor(minority)) 

 dt %>%
   tabyl(minority)
 





dt_2019 <- dt %>% 
  dplyr::filter(year <= 2019) %>%
  dplyr::mutate(stage = "2019")


dt_2019 <- dt_2019 %>% 
  dplyr::group_by(patient_id) %>% 
  arrange(desc(year), .by_group=TRUE)  %>% 
  slice_head %>% 
  ungroup()



######################################################
## Categorise weight_universal:  BMI < 18.5 underweight; BMI 18 < 25 healthy, BMI 25 < 30 overweight , BMI >= 30 obese
## Categorise weight_DWMP (ethnicity specific cut offs): obese:  BMI >= 30 in white, BMI >= 27.5 in minority ethnic groups
## minority variable flags individuals as white/not white:  None, not_white, white
######################################################

dt_2019 <- dt_2019 %>% 
  dplyr::mutate(weight_universal = case_when(
    median_bmi <18.5 ~ "underweight",
    median_bmi >= 18.5 & median_bmi < 25 ~ "healthy",
    median_bmi >= 25 & median_bmi < 30 ~ "overweight",
    median_bmi >= 30 ~ "obese",
  ))



 dt_2019 <- dt_2019 %>% 
  dplyr::mutate(weight_DWMP = case_when(
    ((median_bmi >= 30 & minority == "white") | (median_bmi >= 27.5 & minority == "not_white"))    ~  "DWMP_obese",
    ((median_bmi < 30 & minority == "white") | (median_bmi < 27.5 & minority == "not_white"))    ~  "DWMP_not_obese"
  )) 


 
 
 dt_2019 <- dt_2019 %>% 
   dplyr::mutate(dwmp_eligible = case_when(
     (weight_DWMP == "DWMP_obese" & (comorbid_hypertension == "TRUE")) |(weight_DWMP == "DWMP_obese" & (comorbid_diabetes_t1 == "TRUE"))|(weight_DWMP == "DWMP_obese" & (comorbid_diabetes_t2 == "TRUE")) ~ "DWMP_eligible", 
      (weight_DWMP == "DWMP_not_obese") ~ "not_dwmp", 
     (weight_DWMP == "DWMP_obese" & ((comorbid_hypertension != "TRUE") & (comorbid_diabetes_t1 != "TRUE") & (comorbid_diabetes_t2 != "TRUE")))~ "not_dwmp"
   ))
 
 
dt_2019 %>% 
  tabyl(dwmp_eligible)
 
dt_2019 %>% 
  tabyl(eth_16_corrected)





################################

dt_2021 <- dt %>% 
  dplyr::filter(year >= 2017) %>%
  dplyr::mutate(stage = "2021")


dt_2021 <- dt_2021 %>% 
  dplyr::group_by(patient_id) %>% 
  arrange(desc(year), .by_group=TRUE)  %>% 
  slice_head %>% 
  ungroup()



######################################################
## Categorise weight_universal:  BMI < 18.5 underweight; BMI 18 < 25 healthy, BMI 25 < 30 overweight , BMI >= 30 obese
## Categorise weight_DWMP (ethnicity specific cut offs): obese:  BMI >= 30 in white, BMI >= 27.5 in minority ethnic groups
## minority variable flags individuals as white/not white:  None, not_white, white
######################################################

dt_2021 <- dt_2021 %>% 
  dplyr::mutate(weight_universal = case_when(
    median_bmi <18.5 ~ "underweight",
    median_bmi >= 18.5 & median_bmi < 25 ~ "healthy",
    median_bmi >= 25 & median_bmi < 30 ~ "overweight",
    median_bmi >= 30 ~ "obese",
  ))



 dt_2021 <- dt_2021 %>% 
  dplyr::mutate(weight_DWMP = case_when(
    ((median_bmi >= 30 & minority == "white") | (median_bmi >= 27.5 & minority == "not_white"))    ~  "DWMP_obese",
    ((median_bmi < 30 & minority == "white") | (median_bmi < 27.5 & minority == "not_white"))    ~  "DWMP_not_obese"
  )) 


 
 
 dt_2021 <- dt_2021 %>% 
   dplyr::mutate(dwmp_eligible = case_when(
     (weight_DWMP == "DWMP_obese" & (comorbid_hypertension == "TRUE")) |(weight_DWMP == "DWMP_obese" & (comorbid_diabetes_t1 == "TRUE"))|(weight_DWMP == "DWMP_obese" & (comorbid_diabetes_t2 == "TRUE")) ~ "DWMP_eligible", 
      (weight_DWMP == "DWMP_not_obese") ~ "not_dwmp", 
     (weight_DWMP == "DWMP_obese" & ((comorbid_hypertension != "TRUE") & (comorbid_diabetes_t1 != "TRUE") & (comorbid_diabetes_t2 != "TRUE")))~ "not_dwmp"
   ))
 
 
dt_2021 %>% 
  tabyl(dwmp_eligible)
 
dt_2021 %>% 
  tabyl(eth_16_corrected)



##### 
dt_all <- dt_2019 %>%
bind_rows(dt_2021)



##############################
### WRITE FUNCTIONS

universal_weight_f <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}, weight_universal) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) 
}



DWMP_weight_f <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}, weight_DWMP) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) 
}



DWMP_eligible_f <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}, dwmp_eligible) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) 
}

## median BMI
function_3 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    group_by({{my_var}}) %>%
    summarise(Q1=quantile(median_bmi,probs = 0.25, na.rm = TRUE),
              median=median(median_bmi, na.rm = TRUE), 
              Q3=quantile(median_bmi, probs = 0.75, na.rm = TRUE)) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
}

#####

stage_1 <- universal_weight_f(dt_all, stage)
stage_2 <- DWMP_weight_f(dt_all, stage)
stage_3 <- DWMP_eligible_f (dt_all, stage)
stage_4 <- function_3 (dt_all, stage)


summary <- stage_1 %>% 
  left_join(stage_2, by = c("variable", "group"))%>% 
  left_join(stage_3, by = c("variable", "group"))%>% 
  left_join(stage_4, by = c("variable", "group"))

write_csv (summary, here::here ("output/data","DWMP_2019_2021_summary_stats.csv"))



