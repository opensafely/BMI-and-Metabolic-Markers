
## M Samuel 21st October 2022
## This file looks at the proportion of patients who had a BMI recorded in their GP notes


## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)

## Read in files


BMI_complete_categories <- read.csv (here::here ("output/data", "BMI_complete_categories.csv"))

#names(BMI_complete_categories_2)

##### Change all exposure variables to characters to allow appending of summary tables

BMI_complete_categories_2 <- BMI_complete_categories

BMI_complete_categories_2 %>%
  dplyr::mutate(
    sex = as.character(sex), 
    age_group = as.character(age_group),
    ethnic_no_miss = as.character(ethnic_no_miss),
    imd = as.character(imd), 
    region = as.character(region),
    precovid_obese_flag = as.character(precovid_obese_flag)) %>%
  dplyr::mutate(across(starts_with("comorbid_"), as.character))





## Yearly summary  who had a BMI

bmi_recorded_all <- BMI_complete_categories_2 %>%
  dplyr::group_by(year) %>%
    dplyr::summarise(
      n_patient = n(),  #number of patients per group
      n_had_bmi = sum(had_bmi == '1')) %>%  # number with a flag to say they had BMI in the year
      dplyr::mutate(percent_had_bmi = n_had_bmi/n_patient*100) %>%
      dplyr::select(year, percent_had_bmi)%>%
      pivot_wider(names_from = year, values_from = percent_had_bmi)

bmi_recorded_all <- bmi_recorded_all %>%
  dplyr::mutate(covariate = "all", category = "all")

# View(bmi_recorded_all)



### Function to identify proportion who had a BMI (bmi_flag)

bmi_flag_function <- function (data, var) {
  tab1 <- data %>%
    dplyr::group_by(year, {{var}}) %>%
    dplyr::summarise(
      n_patient = n(),  #number of patients per group
      n_had_bmi = sum(had_bmi == '1')) %>%  # number with a flag to say they had BMI in the year
    dplyr::mutate(percent_had_bmi = n_had_bmi/n_patient*100) %>%
    dplyr::select({{var}}, year, percent_had_bmi)%>%
    pivot_wider(names_from = year, values_from = percent_had_bmi)
  
  tab1 %>%
    dplyr::rename( "category" = {{var}}) 
}




bmi_recorded_age_group <- bmi_flag_function(BMI_complete_categories_2, age_group) %>%
  dplyr::mutate(covariate="age_group")

bmi_recorded_imd <- bmi_flag_function(BMI_complete_categories_2, imd) %>%
  dplyr::mutate(covariate="imd")


bmi_recorded_sex <- bmi_flag_function(BMI_complete_categories_2, sex) %>%
  dplyr::mutate(covariate="sex")

bmi_recorded_region <- bmi_flag_function(BMI_complete_categories_2, region) %>%
  dplyr::mutate(covariate="region")

bmi_recorded_ethnic_no_miss <- bmi_flag_function(BMI_complete_categories_2, ethnic_no_miss) %>%
  dplyr::mutate(covariate="ethnic_no_miss")

bmi_recorded_comorbid_learning_disability <- bmi_flag_function(BMI_complete_categories_2, comorbid_learning_disability) %>%
  dplyr::mutate(covariate="comorbid_learning_disability")

bmi_recorded_comorbid_depression <- bmi_flag_function(BMI_complete_categories_2, comorbid_depression) %>%
  dplyr::mutate(covariate="comorbid_depression")

bmi_recorded_comorbid_dementia <- bmi_flag_function(BMI_complete_categories_2, comorbid_dementia) %>%
  dplyr::mutate(covariate="comorbid_dementia")

bmi_recorded_comorbid_psychosis_schiz_bipolar  <- bmi_flag_function(BMI_complete_categories_2, comorbid_psychosis_schiz_bipolar ) %>%
  dplyr::mutate(covariate="comorbid_psychosis_schiz_bipolar ")

bmi_recorded_comorbid_diabetes_type <- bmi_flag_function(BMI_complete_categories_2, comorbid_diabetes_type) %>%
  dplyr::mutate(covariate="comorbid_diabetes_type")

bmi_recorded_comorbid_diabetes_t1 <- bmi_flag_function(BMI_complete_categories_2, comorbid_diabetes_t1) %>%
  dplyr::mutate(covariate="comorbid_diabetes_t1")

bmi_recorded_comorbid_diabetes_t2 <- bmi_flag_function(BMI_complete_categories_2, comorbid_diabetes_t2) %>%
  dplyr::mutate(covariate="comorbid_diabetes_t2")

bmi_recorded_comorbid_asthma <- bmi_flag_function(BMI_complete_categories_2, comorbid_asthma) %>%
  dplyr::mutate(covariate="comorbid_asthma")

bmi_recorded_comorbid_COPD <- bmi_flag_function(BMI_complete_categories_2, comorbid_COPD) %>%
  dplyr::mutate(covariate="comorbid_COPD")

bmi_recorded_comorbid_stroke_and_TIA <- bmi_flag_function(BMI_complete_categories_2, comorbid_stroke_and_TIA) %>%
  dplyr::mutate(covariate="comorbid_stroke_and_TIA")


bmi_recorded_comorbid_chronic_cardiac <- bmi_flag_function(BMI_complete_categories_2, comorbid_chronic_cardiac) %>%
  dplyr::mutate(covariate="comorbid_chronic_cardiac")

bmi_recorded_comorbid_hypertension <- bmi_flag_function(BMI_complete_categories_2, comorbid_hypertension) %>%
  dplyr::mutate(covariate="comorbid_hypertension")

bmi_recorded_comorbid_all_cancer <- bmi_flag_function(BMI_complete_categories_2, comorbid_all_cancer) %>%
  dplyr::mutate(covariate="comorbid_all_cancer")


bmi_recorded_precovid_obese_flag <- bmi_flag_function(BMI_complete_categories_2, precovid_obese_flag) %>%
  dplyr::mutate(covariate="precovid_obese_flag")
###############

## append the data sets to create a single summary table


# Summary of covariates
bmi_recorded_summary_covariates <- bmi_recorded_all %>%
  dplyr::bind_rows(
    bmi_recorded_precovid_obese_flag,    
    bmi_recorded_age_group, 
    bmi_recorded_sex, 
    bmi_recorded_ethnic_no_miss, 
    bmi_recorded_imd, 
    bmi_recorded_region,   
    bmi_recorded_comorbid_learning_disability, 
    bmi_recorded_comorbid_depression, 
    bmi_recorded_comorbid_dementia,
    bmi_recorded_comorbid_psychosis_schiz_bipolar, 
    bmi_recorded_comorbid_diabetes_t2,
    bmi_recorded_comorbid_diabetes_t1,
    bmi_recorded_comorbid_asthma, 
    bmi_recorded_comorbid_COPD, 
    bmi_recorded_comorbid_stroke_and_TIA, 
    bmi_recorded_comorbid_chronic_cardiac, 
    bmi_recorded_comorbid_hypertension, 
    bmi_recorded_comorbid_all_cancer 
  )%>%
  dplyr::select(covariate,category, "2015","2016","2017","2018","2019","2020", "2021")

write.csv (bmi_recorded_summary_covariates, here::here ("output/data","BMI_recorded_summary_covariates.csv"))


