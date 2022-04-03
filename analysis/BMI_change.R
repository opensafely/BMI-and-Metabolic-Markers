## Author:  Miriam Samuel
## Date: 30th March
## Developing a script to look at individual level changes in BMI



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


BMI_complete_2019 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "BMI_all_2019.feather"))

BMI_complete_2020 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "BMI_all_2020.feather"))

BMI_complete_2021 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "BMI_all_2021.feather"))




BMI_complete_2019 <- BMI_complete_2019 %>%
  dplyr::filter(comorbid_diabetes_t2 == TRUE)  %>%
  dplyr::mutate(year="2019") 


BMI_complete_2020 <- BMI_complete_2020 %>%
  dplyr::filter(comorbid_diabetes_t2 == TRUE) %>%
  dplyr::select(-'precovid_obese_flag') %>%
  dplyr::mutate(year="2020") 

BMI_complete_2021 <- BMI_complete_2021 %>%
  dplyr::filter(comorbid_diabetes_t2 == TRUE) %>%
  dplyr::select(-'precovid_obese_flag') %>%
  dplyr::mutate(year="2021") 





BMI_complete_demographics <- dplyr::bind_rows (
                                  BMI_complete_2019, 
                                  BMI_complete_2020,
                                  BMI_complete_2021)


# create a demographics data set to reattach after pivot

BMI_complete_demographics <- BMI_complete_demographics %>%
  dplyr::group_by(patient_id) %>%
  dplyr::slice_head() %>%
  dplyr::select(-'median_bmi') %>%
  ungroup


## bind yearly data and select relevant variables
BMI_complete_data <- dplyr::bind_rows (
  BMI_complete_2019, 
  BMI_complete_2020,
  BMI_complete_2021)  %>%
  dplyr::select(patient_id, year, median_bmi)  %>%
  dplyr::mutate(median_bmi = as.numeric(median_bmi))

## pivot wider to allow across row calculations - 1 patient, 1 row
BMI_complete_data <- BMI_complete_data %>% 
  tidyr::pivot_wider(names_from = year, values_from = median_bmi)

BMI_complete_data <-  left_join(BMI_complete_data, BMI_complete_demographics, by='patient_id')

BMI_complete_data
###  



BMI_complete_data <- BMI_complete_data %>%
  dplyr::rename('Y2019' = '2019')  %>%
  dplyr::rename('Y2020' = '2020')  %>%
  dplyr::rename('Y2021' = '2021')  

BMI_complete_data

BMI_complete <- BMI_complete_data %>%
  dplyr::mutate(change_year1 = (Y2020 - Y2019)) %>%
  dplyr::mutate(change_year2 = (Y2021 - Y2020)) %>%
  dplyr::mutate(change_2y = (Y2021 - Y2019))
  



