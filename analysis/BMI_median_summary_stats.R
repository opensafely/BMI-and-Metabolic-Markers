##########################################################
# Author:  Miriam Samuel
# Input:  BMI_complete_median.feather
# Output:
# Last modified:  9th March 2022
###########################################################


# >> Read in packages

##  packages
library(broom)
library(purrr)
library(dplyr)
library(janitor)
library(tidyverse)
library(here)
library(arrow)

#  >>  Read in file
BMI_complete_categories <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

# >> Start analysis

BMI_complete_categories_2 <- BMI_complete_categories

# names(BMI_complete_categories_2)

##### Change all exposure variables to characters to allow appending of summary tables

BMI_complete_categories_2 <- BMI_complete_categories_2 %>%
  dplyr::mutate(
    sex = as.character(sex), 
    age_group = as.character(age_group),
    ethnic_no_miss = as.character(ethnic_no_miss),
    eth_group_16 = as.character(eth_group_16),
    imd = as.character(imd), 
    region = as.character (region),
  ) %>%
  dplyr::mutate(across(starts_with("comorbid_"), as.character))
  
BMI_complete_categories_2

## Have changed to characters- need to bind the reframed data sets







## Yearly summary statistics of median BMI

median_bmi_all <- BMI_complete_categories_2 %>%
  dplyr::group_by(year) %>%
  summarise(
    average_bmi = median (median_bmi, na.rm = TRUE)) %>%
    pivot_wider(names_from = year, values_from = average_bmi)
median_bmi_all <- median_bmi_all %>%
  dplyr::mutate(covariate = "all", category = "all")
    
  


## Function to calculate average BMI
average_bmi_function   <- function(data, var) {
  tab1 <- data  %>%
    dplyr::group_by(year, {{var}}) %>%
    summarise(
      average_bmi = median(median_bmi, na.rm=TRUE)
    ) %>%
    pivot_wider(names_from = year, values_from = average_bmi)
  
   tab1 %>%
     dplyr::rename( "category" = {{var}})
}


median_bmi_age_group <- average_bmi_function(BMI_complete_categories_2, age_group) %>%
  dplyr::mutate(covariate="age_group")

median_bmi_imd <- average_bmi_function(BMI_complete_categories_2, imd) %>%
  dplyr::mutate(covariate="imd")


median_bmi_sex <- average_bmi_function(BMI_complete_categories_2, sex) %>%
  dplyr::mutate(covariate="sex")

median_bmi_region <- average_bmi_function(BMI_complete_categories_2, region) %>%
  dplyr::mutate(covariate="region")

median_bmi_ethnic_no_miss <- average_bmi_function(BMI_complete_categories_2, ethnic_no_miss) %>%
  dplyr::mutate(covariate="ethnic_no_miss")
  
median_bmi_eth_group_16 <- average_bmi_function(BMI_complete_categories_2, eth_group_16) %>%
  dplyr::mutate(covariate="eth_group_16")

median_bmi_comorbid_learning_disability <- average_bmi_function(BMI_complete_categories_2, comorbid_learning_disability) %>%
  dplyr::mutate(covariate="comorbid_learning_disability")

median_bmi_comorbid_depression <- average_bmi_function(BMI_complete_categories_2, comorbid_depression) %>%
  dplyr::mutate(covariate="comorbid_depression")

median_bmi_comorbid_dementia <- average_bmi_function(BMI_complete_categories_2, comorbid_dementia) %>%
  dplyr::mutate(covariate="comorbid_dementia")

median_bmi_comorbid_psychosis_schiz_bipolar  <- average_bmi_function(BMI_complete_categories_2, comorbid_psychosis_schiz_bipolar ) %>%
  dplyr::mutate(covariate="comorbid_psychosis_schiz_bipolar ")

median_bmi_comorbid_diabetes_type <- average_bmi_function(BMI_complete_categories_2, comorbid_diabetes_type) %>%
  dplyr::mutate(covariate="comorbid_diabetes_type")

median_bmi_comorbid_diabetes_t1 <- average_bmi_function(BMI_complete_categories_2, comorbid_diabetes_t1) %>%
  dplyr::mutate(covariate="comorbid_diabetes_t1")

median_bmi_comorbid_diabetes_t2 <- average_bmi_function(BMI_complete_categories_2, comorbid_diabetes_t2) %>%
  dplyr::mutate(covariate="comorbid_diabetes_t2")

median_bmi_comorbid_asthma <- average_bmi_function(BMI_complete_categories_2, comorbid_asthma) %>%
  dplyr::mutate(covariate="comorbid_asthma")

median_bmi_comorbid_COPD <- average_bmi_function(BMI_complete_categories_2, comorbid_COPD) %>%
  dplyr::mutate(covariate="comorbid_COPD")

median_bmi_comorbid_stroke_and_TIA <- average_bmi_function(BMI_complete_categories_2, comorbid_stroke_and_TIA) %>%
  dplyr::mutate(covariate="comorbid_stroke_and_TIA")


median_bmi_comorbid_chronic_cardiac <- average_bmi_function(BMI_complete_categories_2, comorbid_chronic_cardiac) %>%
  dplyr::mutate(covariate="comorbid_chronic_cardiac")

median_bmi_comorbid_hypertension <- average_bmi_function(BMI_complete_categories_2, comorbid_hypertension) %>%
  dplyr::mutate(covariate="comorbid_hypertension")

median_bmi_comorbid_all_cancer <- average_bmi_function(BMI_complete_categories_2, comorbid_all_cancer) %>%
  dplyr::mutate(covariate="comorbid_all_cancer")



###############

## append the data sets to create a single summary table


median_bmi_summary_demographic <- median_bmi_all %>%
  dplyr::bind_rows(median_bmi_age_group, 
                   median_bmi_sex, 
                   median_bmi_ethnic_no_miss, 
                   median_bmi_eth_group_16,
                   median_bmi_imd, 
                   median_bmi_region) %>%
  dplyr::select(covariate, category, "2015","2016","2017","2018","2019","2020", "2021")



# Summary of covariates
median_bmi_summary_covariates <- median_bmi_all %>%
  dplyr::bind_rows(
    median_bmi_comorbid_learning_disability, 
    median_bmi_comorbid_depression, 
    median_bmi_comorbid_dementia,
    median_bmi_comorbid_psychosis_schiz_bipolar, 
    median_bmi_comorbid_diabetes_t2,
    median_bmi_comorbid_diabetes_t1,
    median_bmi_comorbid_asthma, 
    median_bmi_comorbid_COPD, 
    median_bmi_comorbid_stroke_and_TIA, 
    median_bmi_comorbid_chronic_cardiac, 
    median_bmi_comorbid_hypertension, 
    median_bmi_comorbid_all_cancer 
    )%>%
  dplyr::select(covariate,category, "2015","2016","2017","2018","2019","2020", "2021")



#####################################################################################
# define outputs

write.csv (median_bmi_summary_demographic, here::here ("output/data","median_bmi_summary_table_demographic.csv"))
write.csv (median_bmi_summary_covariates, here::here ("output/data","median_bmi_summary_table_covariates.csv"))
