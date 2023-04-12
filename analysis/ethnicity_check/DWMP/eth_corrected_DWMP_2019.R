






#### Author: M Samuel
#### Date: March 2023
#### This script calculates the proportion of the population eligible for the DWMP

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

age_group <- read_feather(here::here ("output/data", "complete_meds_2019.feather"))





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
  dplyr::filter(year <= 2019) 


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


sex <-  universal_weight_f(dt_2019,sex) 
age <- universal_weight_f(dt_2019,age_group_2)
eth <- universal_weight_f(dt_2019,eth_16_corrected)
imd <- universal_weight_f(dt_2019,imd)
region <- universal_weight_f(dt_2019,region)

hypertension <-universal_weight_f(dt_2019,   comorbid_hypertension)
diabetes_t1 <-universal_weight_f(dt_2019,   comorbid_diabetes_t1)
diabetes_t2 <-universal_weight_f(dt_2019,   comorbid_diabetes_t2)
chronic_cardiac <-universal_weight_f(dt_2019,   comorbid_chronic_cardiac)
learning_disability <-universal_weight_f(dt_2019,   comorbid_learning_disability)
depression <-universal_weight_f(dt_2019,   comorbid_depression)
dementia <-universal_weight_f(dt_2019,  comorbid_dementia)
psychosis_schiz_bipolar <-universal_weight_f(dt_2019,   comorbid_psychosis_schiz_bipolar)
asthma <-universal_weight_f(dt_2019,   comorbid_asthma)
COPD <-universal_weight_f(dt_2019,   comorbid_COPD)
stroke_and_TIA <-universal_weight_f(dt_2019,   comorbid_stroke_and_TIA)

universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region,
  hypertension, 
  diabetes_t1, 
  diabetes_t2, 
  chronic_cardiac,
  learning_disability,
  depression,
  dementia,
  psychosis_schiz_bipolar,
  asthma, 
  COPD,
  stroke_and_TIA)

####





sex <-  DWMP_weight_f(dt_2019,sex) 
age <- DWMP_weight_f(dt_2019,age_group_2)
eth <- DWMP_weight_f(dt_2019,eth_16_corrected)
imd <- DWMP_weight_f(dt_2019,imd)
region <- DWMP_weight_f(dt_2019,region)
hypertension <-DWMP_weight_f(dt_2019,   comorbid_hypertension)
diabetes_t1 <-DWMP_weight_f(dt_2019,   comorbid_diabetes_t1)
diabetes_t2 <-DWMP_weight_f(dt_2019,   comorbid_diabetes_t2)
chronic_cardiac <-DWMP_weight_f(dt_2019,   comorbid_chronic_cardiac)
learning_disability <-DWMP_weight_f(dt_2019,   comorbid_learning_disability)
depression <- DWMP_weight_f(dt_2019,   comorbid_depression)
dementia <-DWMP_weight_f(dt_2019,  comorbid_dementia)
psychosis_schiz_bipolar <-DWMP_weight_f(dt_2019,   comorbid_psychosis_schiz_bipolar)
asthma <- DWMP_weight_f(dt_2019,   comorbid_asthma)
COPD <- DWMP_weight_f(dt_2019,   comorbid_COPD)
stroke_and_TIA <- DWMP_weight_f(dt_2019,   comorbid_stroke_and_TIA)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region,
  hypertension, 
  diabetes_t1, 
  diabetes_t2, 
  chronic_cardiac,
  learning_disability,
  depression,
  dementia,
  psychosis_schiz_bipolar,
  asthma, 
  COPD,
  stroke_and_TIA)




### 

sex <-  DWMP_eligible_f(dt_2019,sex)
age <- DWMP_eligible_f(dt_2019,age_group_2)
eth <- DWMP_eligible_f(dt_2019,eth_16_corrected)
imd <- DWMP_eligible_f(dt_2019,imd)
region <- DWMP_eligible_f(dt_2019,region)
hypertension <-DWMP_eligible_f(dt_2019,   comorbid_hypertension)
diabetes_t1 <-DWMP_eligible_f(dt_2019,   comorbid_diabetes_t1)
diabetes_t2 <-DWMP_eligible_f(dt_2019,   comorbid_diabetes_t2)
chronic_cardiac <- DWMP_eligible_f(dt_2019,   comorbid_chronic_cardiac)
learning_disability <- DWMP_eligible_f(dt_2019,   comorbid_learning_disability)
depression <- DWMP_eligible_f(dt_2019,   comorbid_depression)
dementia <- DWMP_eligible_f(dt_2019,  comorbid_dementia)
psychosis_schiz_bipolar <- DWMP_eligible_f(dt_2019,   comorbid_psychosis_schiz_bipolar)
asthma <- DWMP_eligible_f(dt_2019,   comorbid_asthma)
COPD <- DWMP_eligible_f(dt_2019,   comorbid_COPD)
stroke_and_TIA <- DWMP_eligible_f(dt_2019,   comorbid_stroke_and_TIA)


DWMP_eligible <- sex %>% 
  bind_rows(age, eth, imd, region,
  hypertension, 
  diabetes_t1, 
  diabetes_t2, 
  chronic_cardiac,
  learning_disability,
  depression,
  dementia,
  psychosis_schiz_bipolar,
  asthma, 
  COPD,
  stroke_and_TIA)


## median

sex <-function_3(dt_2019,  sex)
age <-function_3(dt_2019,  age_group_2)
eth <-function_3(dt_2019,  eth_16_corrected)
imd <-function_3(dt_2019,  imd)
region <-function_3(dt_2019,  region)
hypertension <-function_3(dt_2019,   comorbid_hypertension)
diabetes_t1 <-function_3(dt_2019,   comorbid_diabetes_t1)
diabetes_t2 <-function_3(dt_2019,   comorbid_diabetes_t2)
chronic_cardiac <-function_3(dt_2019,   comorbid_chronic_cardiac)
learning_disability <-function_3(dt_2019,   comorbid_learning_disability)
depression <-function_3(dt_2019,   comorbid_depression)
dementia <-function_3(dt_2019,   comorbid_dementia)
psychosis_schiz_bipolar <-function_3(dt_2019,   comorbid_psychosis_schiz_bipolar)
asthma <-function_3(dt_2019,   comorbid_asthma)
COPD <-function_3(dt_2019,   comorbid_COPD)
stroke_and_TIA <-function_3(dt_2019,   comorbid_stroke_and_TIA)


median_data <- sex %>% 
  bind_rows(age) %>%
  bind_rows(eth) %>%
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
  bind_rows(stroke_and_TIA) 


summary_2019 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))%>% 
  left_join(DWMP_eligible, by = c("variable", "group"))%>% 
  left_join(median_data, by = c("variable", "group"))



#################
#diabetes


### T2D 2019
#### 
T2D_2019 <- dt_2019 %>% 
  dplyr::filter(comorbid_diabetes_t2 == TRUE)

T2D_2019 <- T2D_2019 %>% mutate_at(vars("insulin_meds", "oad_meds"), ~replace_na(.,0))


T2D_2019 <- T2D_2019 %>% dplyr::mutate(diabetes_med = case_when(
  insulin_meds == 1 ~ "insulin", 
  ((insulin_meds == 0) & (oad_meds == 1)) ~ "oad", 
  ((insulin_meds == 0) & (oad_meds == 0)) ~ "lifestyle"
))

T2D_2019 %>% tabyl(diabetes_med)



sex <-  universal_weight_f(T2D_2019,sex) 
age <- universal_weight_f(T2D_2019,age_group_2)
eth <- universal_weight_f(T2D_2019,eth_16_corrected)
imd <- universal_weight_f(T2D_2019,imd)
region <- universal_weight_f(T2D_2019,region)

hypertension <-universal_weight_f(T2D_2019,   comorbid_hypertension)
diabetes_t1 <-universal_weight_f(T2D_2019,   comorbid_diabetes_t1)
diabetes_t2 <-universal_weight_f(T2D_2019,   comorbid_diabetes_t2)
chronic_cardiac <-universal_weight_f(T2D_2019,   comorbid_chronic_cardiac)
learning_disability <-universal_weight_f(T2D_2019,   comorbid_learning_disability)
depression <-universal_weight_f(T2D_2019,   comorbid_depression)
dementia <-universal_weight_f(T2D_2019,  comorbid_dementia)
psychosis_schiz_bipolar <-universal_weight_f(T2D_2019,   comorbid_psychosis_schiz_bipolar)
asthma <-universal_weight_f(T2D_2019,   comorbid_asthma)
COPD <-universal_weight_f(T2D_2019,   comorbid_COPD)
stroke_and_TIA <-universal_weight_f(T2D_2019,   comorbid_stroke_and_TIA)
diabetes_med <-universal_weight_f(T2D_2019,   diabetes_med)

universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region, 
  hypertension, 
  diabetes_t1, 
  diabetes_t2, 
  chronic_cardiac,
  learning_disability,
  depression,
  dementia,
  psychosis_schiz_bipolar,
  asthma, 
  COPD,
  stroke_and_TIA, 
  diabetes_med)


###
sex <-  DWMP_weight_f(T2D_2019,sex) 


age <- DWMP_weight_f(T2D_2019,age_group_2)

eth <- DWMP_weight_f(T2D_2019,eth_16_corrected)

imd <- DWMP_weight_f(T2D_2019,imd)


region <- DWMP_weight_f(T2D_2019,region)


hypertension <-DWMP_weight_f(T2D_2019,   comorbid_hypertension)
diabetes_t1 <-DWMP_weight_f(T2D_2019,   comorbid_diabetes_t1)
diabetes_t2 <-DWMP_weight_f(T2D_2019,   comorbid_diabetes_t2)
chronic_cardiac <-DWMP_weight_f(T2D_2019,   comorbid_chronic_cardiac)
learning_disability <-DWMP_weight_f(T2D_2019,   comorbid_learning_disability)
depression <- DWMP_weight_f(T2D_2019,   comorbid_depression)
dementia <-DWMP_weight_f(T2D_2019,  comorbid_dementia)
psychosis_schiz_bipolar <-DWMP_weight_f(T2D_2019,   comorbid_psychosis_schiz_bipolar)
asthma <- DWMP_weight_f(T2D_2019,   comorbid_asthma)
COPD <- DWMP_weight_f(T2D_2019,   comorbid_COPD)
stroke_and_TIA <- DWMP_weight_f(T2D_2019,   comorbid_stroke_and_TIA)
diabetes_med <- DWMP_weight_f(T2D_2019,   diabetes_med)

DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region,
  hypertension, 
  diabetes_t1, 
  diabetes_t2, 
  chronic_cardiac,
  learning_disability,
  depression,
  dementia,
  psychosis_schiz_bipolar,
  asthma, 
  COPD,
  stroke_and_TIA, 
  diabetes_med)

###
sex <-function_3(T2D_2019,  sex)
age <-function_3(T2D_2019,  age_group_2)
eth <-function_3(T2D_2019,  eth_16_corrected)
imd <-function_3(T2D_2019,  imd)
region <-function_3(T2D_2019,  region)
hypertension <-function_3(T2D_2019,   comorbid_hypertension)
diabetes_t1 <-function_3(T2D_2019,   comorbid_diabetes_t1)
diabetes_t2 <-function_3(T2D_2019,   comorbid_diabetes_t2)
chronic_cardiac <-function_3(T2D_2019,   comorbid_chronic_cardiac)
learning_disability <-function_3(T2D_2019,   comorbid_learning_disability)
depression <-function_3(T2D_2019,   comorbid_depression)
dementia <-function_3(T2D_2019,   comorbid_dementia)
psychosis_schiz_bipolar <-function_3(T2D_2019,   comorbid_psychosis_schiz_bipolar)
asthma <-function_3(T2D_2019,   comorbid_asthma)
COPD <-function_3(T2D_2019,   comorbid_COPD)
stroke_and_TIA <-function_3(T2D_2019,   comorbid_stroke_and_TIA)
diabetes_med <- function_3(T2D_2019,   diabetes_med)


median_data <- sex %>% 
  bind_rows(age) %>%
  bind_rows(eth) %>%
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
  bind_rows(diabetes_med) 



T2D_2019 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group")) %>% 
  left_join(median_data, by = c("variable", "group"))


#########################
## HYPERTENSIVE


### Hypertension_2019

BP_2019 <- dt_2019 %>% 
  dplyr::filter(comorbid_hypertension == TRUE)

sex <-  universal_weight_f(BP_2019,sex) 
age <- universal_weight_f(BP_2019,age_group_2)
eth <- universal_weight_f(BP_2019,eth_16_corrected)
imd <- universal_weight_f(BP_2019,imd)
region <- universal_weight_f(BP_2019,region)

hypertension <-universal_weight_f(BP_2019,   comorbid_hypertension)
diabetes_t1 <-universal_weight_f(BP_2019,   comorbid_diabetes_t1)
diabetes_t2 <-universal_weight_f(BP_2019,   comorbid_diabetes_t2)
chronic_cardiac <-universal_weight_f(BP_2019,   comorbid_chronic_cardiac)
learning_disability <-universal_weight_f(BP_2019,   comorbid_learning_disability)
depression <-universal_weight_f(BP_2019,   comorbid_depression)
dementia <-universal_weight_f(BP_2019,  comorbid_dementia)
psychosis_schiz_bipolar <-universal_weight_f(BP_2019,   comorbid_psychosis_schiz_bipolar)
asthma <-universal_weight_f(BP_2019,   comorbid_asthma)
COPD <-universal_weight_f(BP_2019,   comorbid_COPD)
stroke_and_TIA <-universal_weight_f(BP_2019,   comorbid_stroke_and_TIA)

universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region, 
  hypertension, 
  diabetes_t1, 
  diabetes_t2, 
  chronic_cardiac,
  learning_disability,
  depression,
  dementia,
  psychosis_schiz_bipolar,
  asthma, 
  COPD,
  stroke_and_TIA)






sex <-  DWMP_weight_f(BP_2019,sex) 
age <- DWMP_weight_f(BP_2019,age_group_2)
eth <- DWMP_weight_f(BP_2019,eth_16_corrected)
imd <- DWMP_weight_f(BP_2019,imd)
region <- DWMP_weight_f(BP_2019,region)
hypertension <-DWMP_weight_f(BP_2019,   comorbid_hypertension)
diabetes_t1 <-DWMP_weight_f(BP_2019,   comorbid_diabetes_t1)
diabetes_t2 <-DWMP_weight_f(BP_2019,   comorbid_diabetes_t2)
chronic_cardiac <-DWMP_weight_f(BP_2019,   comorbid_chronic_cardiac)
learning_disability <-DWMP_weight_f(BP_2019,   comorbid_learning_disability)
depression <- DWMP_weight_f(BP_2019,   comorbid_depression)
dementia <-DWMP_weight_f(BP_2019,  comorbid_dementia)
psychosis_schiz_bipolar <-DWMP_weight_f(BP_2019,   comorbid_psychosis_schiz_bipolar)
asthma <- DWMP_weight_f(BP_2019,   comorbid_asthma)
COPD <- DWMP_weight_f(BP_2019,   comorbid_COPD)
stroke_and_TIA <- DWMP_weight_f(BP_2019,   comorbid_stroke_and_TIA)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region,
  hypertension, 
  diabetes_t1, 
  diabetes_t2, 
  chronic_cardiac,
  learning_disability,
  depression,
  dementia,
  psychosis_schiz_bipolar,
  asthma, 
  COPD,
  stroke_and_TIA)

## median

sex <-function_3(BP_2019,  sex)
age <-function_3(BP_2019,  age_group_2)
eth <-function_3(BP_2019,  eth_16_corrected)
imd <-function_3(BP_2019,  imd)
region <-function_3(BP_2019,  region)
hypertension <-function_3(BP_2019,   comorbid_hypertension)
diabetes_t1 <-function_3(BP_2019,   comorbid_diabetes_t1)
diabetes_t2 <-function_3(BP_2019,   comorbid_diabetes_t2)
chronic_cardiac <-function_3(BP_2019,   comorbid_chronic_cardiac)
learning_disability <-function_3(BP_2019,   comorbid_learning_disability)
depression <-function_3(BP_2019,   comorbid_depression)
dementia <-function_3(BP_2019,   comorbid_dementia)
psychosis_schiz_bipolar <-function_3(BP_2019,   comorbid_psychosis_schiz_bipolar)
asthma <-function_3(BP_2019,   comorbid_asthma)
COPD <-function_3(BP_2019,   comorbid_COPD)
stroke_and_TIA <-function_3(BP_2019,   comorbid_stroke_and_TIA)


median_data <- sex %>% 
  bind_rows(age) %>%
  bind_rows(eth) %>%
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
  bind_rows(stroke_and_TIA) 



BP_2019 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group")) %>% 
  left_join(median_data, by = c("variable", "group"))



########### Join into single table

summary_2019 <- summary_2019 %>% 
  dplyr::mutate (category = "all", .before=1)

BP_2019 <- BP_2019 %>% 
  dplyr::mutate (category = "Hypertensive", .before=1)


T2D_2019 <- T2D_2019 %>% 
  dplyr::mutate (category = "Type_2_Diabetes", .before=1)


summary_2019 <- summary_2019 %>% 
  bind_rows(BP_2019, 
            T2D_2019)

summary_2019 <- summary_2019 %>% 
    dplyr::select(-c("DWMP_not_obese", "not_dwmp"))

summary_2019 <- summary_2019  %>% 
  dplyr::mutate(healthy = plyr::round_any(summary_2019$healthy, 5)) %>% 
  dplyr::mutate(obese = plyr::round_any(summary_2019$obese, 5))%>% 
  dplyr::mutate(overweight = plyr::round_any(summary_2019$overweight, 5))%>% 
  dplyr::mutate(underweight = plyr::round_any(summary_2019$underweight, 5))%>% 
  #dplyr::mutate(DWMP_not_obese = plyr::round_any(summary_2019$DWMP_not_obese, 5))%>% 
  dplyr::mutate(DWMP_obese = plyr::round_any(summary_2019$DWMP_obese, 5))%>% 
  #dplyr::mutate(not_dwmp = plyr::round_any(summary_2019$not_dwmp, 5))%>% 
  dplyr::mutate(DWMP_eligible = plyr::round_any(summary_2019$DWMP_eligible, 5))



write_csv (summary_2019, here::here ("output/data","Ethnicity_corrected_DWMP_2019.csv"))




