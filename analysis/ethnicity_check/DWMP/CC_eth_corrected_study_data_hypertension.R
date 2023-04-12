
## M Samuel
## This R file defines the characteristics of the complete case study population - with data extracted in March 2022
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


# study_data <- read_csv (here::here ("output/data", "CC_study_population_data.csv"))

BMI_2021 <- read_feather (here::here ("output/data", "BMI_complete_median_2021.feather"))

BMI_2021 <- BMI_2021 %>% 
dplyr::filter(comorbid_hypertension == TRUE)

##############   BMI 2021 data
## Check demographics of study population in 2021

# recode ethnicity
BMI_2021 <- BMI_2021 %>%  mutate(
  eth_group_16 = as.character(eth_group_16),
  eth_group_16 = ifelse(is.na(eth_group_16), "None", eth_group_16),
  eth_group_16 = as.factor(eth_group_16))


BMI_2021 %>% 
  tabyl(eth_group_16)


BMI_2021 <- BMI_2021 %>% 
  dplyr::mutate(eth_16_corrected = case_when(
    eth_group_16 == "White_British" ~ "White_British",
    eth_group_16 == "White_Irish" ~ "None",
    eth_group_16 == "Other_White" ~  "White_Black_Carib",
    eth_group_16 ==  "White_Black_Carib" ~ "Other_White",
    eth_group_16 ==  "White_Black_African" ~ "Pakistani",
    eth_group_16 ==  "White_Asian" ~ "Other_Black",
    eth_group_16 ==  "Other_Mixed" ~ "Indian",
    eth_group_16 ==  "Indian" ~ "White_Asian",
    eth_group_16 ==  "Pakistani" ~ "Bangladeshi",
    eth_group_16 == "Bangladeshi" ~ "Caribbean",
    eth_group_16 == "Other_Asian" ~ "Other_Asian",
    eth_group_16 == "Caribbean" ~ "Other_Mixed",
    eth_group_16 == "African" ~ "Chinese",
    eth_group_16 == "Other_Black" ~ "Other",
    eth_group_16 ==  "Chinese" ~ "White_Irish",
    eth_group_16 == "Other" ~ "White_Black_African",
    eth_group_16 ==  "None" ~ "African")) 

print("check ethnicity replace none with NA")
BMI_2021 %>%
  tabyl(eth_group_16, eth_16_corrected)

## ****  NEW CODE
BMI_2021 <- BMI_2021 %>% 
mutate(eth_16_corrected = na_if(eth_16_corrected, "None"))

print("check ethnicity replace none with NA")
BMI_2021 %>%
  tabyl(eth_group_16, eth_16_corrected)


print("check missing counts in all data BMI_2021")
BMI_2021 %>% 
  dplyr::select(age_group_2, sex, imd, eth_16_corrected, eth_group_16)  %>% 
  describe()

# check ethnicity - error counts
BMI_2021 %>% 
  tabyl(eth_group_16)

# check ethnicity - recoded
BMI_2021 %>% 
  tabyl(eth_16_corrected)



### Complete case - filter out missing
# filter NA for complete case data set
BMI_2021_cc <- BMI_2021 %>% 
  drop_na (imd) %>% 
  drop_na (eth_16_corrected)


print("COMPLETE CASE:check missing counts in complete case data BMI_2021")
 BMI_2021_cc %>% 
  dplyr::select(age_group_2, sex, imd, eth_16_corrected, eth_group_16)  %>% 
  describe()

 # check ethnicity - error counts
 BMI_2021_cc %>% 
   tabyl(eth_group_16)
 
 # check ethnicity - recoded
 BMI_2021_cc %>% 
   tabyl(eth_16_corrected)
 
print("BMI_2021_cc - check smoking status")
 BMI_2021_cc %>% 
   tabyl(smoking_status)


 
 BMI_2021_cc <- BMI_2021_cc %>% 
   mutate(smoking_status = na_if(smoking_status, "M")) %>% 
   mutate(smoking_status = factor(smoking_status, levels = c("N","S","E"))) 

 print("BMI_2021_cc - check smoking status: after recoding M as missing")
BMI_2021_cc %>% 
  tabyl(smoking_status)

######
study_data <- BMI_2021_cc

## drop incorrect ethnicity data
study_data <- study_data %>% 
  dplyr::select(-c(eth_group_16, ethnic_no_miss))

study_data <- study_data %>%
  dplyr::mutate(eth_16_corrected = factor(eth_16_corrected, 
                                          levels = c("White_British",
                                                     "White_Irish",
                                                     "Other_White",
                                                     "White_Black_Carib",
                                                     "White_Black_African",
                                                     "White_Asian",
                                                     "Other_Mixed",
                                                     "Indian",
                                                     "Pakistani",
                                                     "Bangladeshi",
                                                     "Other_Asian",
                                                     "Chinese",
                                                     "Caribbean",
                                                     "African",
                                                     "Other_Black",
                                                     "Other")) ) 


## collapse ethnicity
study_data <- study_data %>%
dplyr::mutate(eth_collapsed = case_when(
    eth_16_corrected ==   "White_British" ~ "white",
    eth_16_corrected ==  "White_Irish" ~ "white",
    eth_16_corrected ==  "Other_White"  ~ "white",
    eth_16_corrected == "White_Black_Carib" ~ "mixed",
    eth_16_corrected == "White_Black_African" ~ "mixed",
    eth_16_corrected == "White_Asian" ~ "mixed",
    eth_16_corrected == "Other_Mixed" ~ "mixed",
    eth_16_corrected == "Indian" ~ "south_asian",
    eth_16_corrected == "Pakistani" ~ "south_asian",
    eth_16_corrected == "Bangladeshi" ~ "south_asian",
    eth_16_corrected == "Other_Asian" ~ "chinese_other",
    eth_16_corrected == "Chinese" ~ "chinese_other",
    eth_16_corrected == "Caribbean" ~ "black",
    eth_16_corrected == "African" ~ "black",
    eth_16_corrected == "Other_Black" ~ "black",
    eth_16_corrected == "Other" ~ "chinese_other"
    ))  



## create a flag of whether a median BMI is available 
study_data <- study_data  %>%
  dplyr::mutate(bmi_data = as.character(median_bmi))  %>%
  dplyr::mutate(bmi_data = replace_na(bmi_data,"none")) %>%
  dplyr::mutate(bmi_data = case_when(
    bmi_data == "none" ~ "no_bmi_data", 
    bmi_data != "none" ~ "bmi_data"
  )) 


function_1 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) 
}
function_1(study_data, sex)



sex <-function_1(study_data,  sex)
age_group_2 <-function_1(study_data,  age_group_2)
eth_group_16 <-function_1(study_data,  eth_16_corrected)
imd <-function_1(study_data,  imd)
region <-function_1(study_data,  region)
hypertension <-function_1(study_data,   comorbid_hypertension)
diabetes_t1 <-function_1(study_data,   comorbid_diabetes_t1)
diabetes_t2 <-function_1(study_data,   comorbid_diabetes_t2)
chronic_cardiac <-function_1(study_data,   comorbid_chronic_cardiac)
learning_disability <-function_1(study_data,   comorbid_learning_disability)
depression <-function_1(study_data,   comorbid_depression)
dementia <-function_1(study_data,   comorbid_dementia)
psychosis_schiz_bipolar <-function_1(study_data,   comorbid_psychosis_schiz_bipolar)
asthma <-function_1(study_data,   comorbid_asthma)
COPD <-function_1(study_data,   comorbid_COPD)
stroke_and_TIA <-function_1(study_data,   comorbid_stroke_and_TIA)
eth_collapsed <-function_1(study_data,  eth_collapsed)

complete <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
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
  bind_rows(eth_collapsed) %>% 
  dplyr::mutate(population = "all", .before=1)




####### BMI_data_available

## proportion with BMI data available
function_2 <- function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    tabyl({{my_var}}, bmi_data) %>%
    dplyr::mutate(prop_bmi_data = bmi_data/(bmi_data+no_bmi_data)) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group)) 
}

function_2(study_data, sex)



sex <-function_2(study_data,  sex)
age_group_2 <-function_2(study_data,  age_group_2)
eth_group_16 <-function_2(study_data,  eth_16_corrected)
imd <-function_2(study_data,  imd)
region <-function_2(study_data,  region)
hypertension <-function_2(study_data,   comorbid_hypertension)
diabetes_t1 <-function_2(study_data,   comorbid_diabetes_t1)
diabetes_t2 <-function_2(study_data,   comorbid_diabetes_t2)
chronic_cardiac <-function_2(study_data,   comorbid_chronic_cardiac)
learning_disability <-function_2(study_data,   comorbid_learning_disability)
depression <-function_2(study_data,   comorbid_depression)
dementia <-function_2(study_data,   comorbid_dementia)
psychosis_schiz_bipolar <-function_2(study_data,   comorbid_psychosis_schiz_bipolar)
asthma <-function_2(study_data,   comorbid_asthma)
COPD <-function_2(study_data,   comorbid_COPD)
stroke_and_TIA <-function_2(study_data,   comorbid_stroke_and_TIA)
eth_collapsed <-function_2(study_data,  eth_collapsed)

bmi_data <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
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
  bind_rows(eth_collapsed)





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


sex <-function_3(study_data,  sex)
age_group_2 <-function_3(study_data,  age_group_2)
eth_group_16 <-function_3(study_data,  eth_16_corrected)
imd <-function_3(study_data,  imd)
region <-function_3(study_data,  region)
hypertension <-function_3(study_data,   comorbid_hypertension)
diabetes_t1 <-function_3(study_data,   comorbid_diabetes_t1)
diabetes_t2 <-function_3(study_data,   comorbid_diabetes_t2)
chronic_cardiac <-function_3(study_data,   comorbid_chronic_cardiac)
learning_disability <-function_3(study_data,   comorbid_learning_disability)
depression <-function_3(study_data,   comorbid_depression)
dementia <-function_3(study_data,   comorbid_dementia)
psychosis_schiz_bipolar <-function_3(study_data,   comorbid_psychosis_schiz_bipolar)
asthma <-function_3(study_data,   comorbid_asthma)
COPD <-function_3(study_data,   comorbid_COPD)
stroke_and_TIA <-function_3(study_data,   comorbid_stroke_and_TIA)
eth_collapsed <-function_3(study_data,  eth_collapsed)

median_data <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
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
  bind_rows(eth_collapsed)





### Mean BMI data

function_4 <-  function(data, my_var) {
  v1 <- deparse(substitute(my_var))
  
  data %>%
    group_by({{my_var}}) %>%
    summarise(mean= mean(median_bmi, na.rm = TRUE), sd = sd(median_bmi, na.rm=TRUE)) %>%
    dplyr::rename(group = {{my_var}}) %>% 
    dplyr::mutate(variable = (v1), .before=1)  %>%   
    dplyr::mutate(across(where(is.numeric), round, 5)) %>% 
    dplyr::mutate(group = as.character(group))
}

sex <-function_4(study_data,  sex)
age_group_2 <-function_4(study_data,  age_group_2)
eth_group_16 <-function_4(study_data,  eth_16_corrected)
imd <-function_4(study_data,  imd)
region <-function_4(study_data,  region)
hypertension <-function_4(study_data,   comorbid_hypertension)
diabetes_t1 <-function_4(study_data,   comorbid_diabetes_t1)
diabetes_t2 <-function_4(study_data,   comorbid_diabetes_t2)
chronic_cardiac <-function_4(study_data,   comorbid_chronic_cardiac)
learning_disability <-function_4(study_data,   comorbid_learning_disability)
depression <-function_4(study_data,   comorbid_depression)
dementia <-function_4(study_data,   comorbid_dementia)
psychosis_schiz_bipolar <-function_4(study_data,   comorbid_psychosis_schiz_bipolar)
asthma <-function_4(study_data,   comorbid_asthma)
COPD <-function_4(study_data,   comorbid_COPD)
stroke_and_TIA <-function_4(study_data,   comorbid_stroke_and_TIA)
eth_collapsed <-function_4(study_data,  eth_collapsed)

mean_data <- sex %>% 
  bind_rows(age_group_2) %>%
  bind_rows(eth_group_16) %>%
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
  bind_rows(eth_collapsed) 



complete_data <- complete %>% 
  dplyr::left_join(bmi_data) %>%
  dplyr::left_join(median_data) %>%
  dplyr::left_join(mean_data) 


complete_data <- complete_data %>%
  dplyr::rename(n_total = n)





complete_data <- complete_data  %>% 
  dplyr::mutate(bmi_data = plyr::round_any(complete_data$bmi_data, 5)) %>% 
  dplyr::mutate(no_bmi_data = plyr::round_any(complete_data$no_bmi_data, 5)) %>% 
  dplyr::mutate(n_total = plyr::round_any(complete_data$n_total, 5))

complete_data <- complete_data %>% 
dplyr::select( population,
                variable,
                group,
                n_total,
                bmi_data,
                Q1,
                median,
                Q3,
                mean,
                sd)



write_csv (complete_data, here::here ("output/data","CC_study_population_characteristics_hypertension.csv"))