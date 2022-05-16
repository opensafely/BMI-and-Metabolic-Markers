
## This file generates data for calculations around change in BMI categories

## Author: M Samuel 
## Date: 12th May

## load libraries
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
library(lubridate)
library(skimr)
library(ggplot2)



data <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

 

colnames(data)



median_bmi <- data %>%
  dplyr::select(patient_id, 
                median_bmi,
                BMI_categories, 
                BMI_over27.5,
                year,
                sex,
                age_group, 
                age_group_2, 
                region, imd, 
                ethnic_no_miss, 
                eth_group_16, 
                starts_with("comorbid_"), 
                smoking_status, 
                oad_meds, 
                insulin_meds)

bmi_2015 <- median_bmi %>% 
  dplyr::filter(year == '2015') %>% 
  select(patient_id, median_bmi, BMI_categories, BMI_over27.5) %>% 
  dplyr::rename(bmi_2015 = median_bmi,
                categories_2015 = BMI_categories, 
                over27.5_2015 = BMI_over27.5)



bmi_2017 <- median_bmi %>% 
  dplyr::filter(year == '2017') %>% 
  select(patient_id, median_bmi, BMI_categories, BMI_over27.5) %>% 
  dplyr::rename(bmi_2017 = median_bmi,
                categories_2017 = BMI_categories, 
                over27.5_2017 = BMI_over27.5)

bmi_2019 <- median_bmi %>% 
  dplyr::filter(year == '2019') %>% 
  select(patient_id, median_bmi, BMI_categories, BMI_over27.5) %>% 
  dplyr::rename(bmi_2019 = median_bmi,
                categories_2019 = BMI_categories, 
                over27.5_2019 = BMI_over27.5)

bmi_2021 <- median_bmi %>% 
  dplyr::filter(year == '2021') %>% 
  select(patient_id, median_bmi, BMI_categories, BMI_over27.5) %>% 
  dplyr::rename(bmi_2021 = median_bmi,
                categories_2021 = BMI_categories, 
                over27.5_2021 = BMI_over27.5)
  

#####################################################

period_1 <- bmi_2017 %>% 
  left_join(bmi_2019)


period_1 <- period_1 %>% 
  drop_na()

period_1 <- period_1 %>% 
  dplyr::mutate(healthy_cat_change = case_when(
    categories_2017 == 'healthy' & categories_2019 == "underweight" ~ 0,
    categories_2017 == 'healthy' & categories_2019 == "healthy" ~ 1,
    categories_2017 == 'healthy' & categories_2019 == "overweight" ~ 2, 
    categories_2017 == 'healthy' & categories_2019 == "obese" ~ 3 )) %>%
  dplyr::mutate(overweight_cat_change = case_when(
    categories_2017 == 'overweight' & categories_2019 == "underweight" ~ 0,
    categories_2017 == 'overweight' & categories_2019 == "healthy" ~ 1,
    categories_2017 == 'overweight' & categories_2019 == "overweight" ~ 2, 
    categories_2017 == 'overweight' & categories_2019 == "obese" ~ 3 )) %>%
  dplyr::mutate(obese_cat_change = case_when(
    categories_2017 == 'obese' & categories_2019 == "underweight" ~ 0,
    categories_2017 == 'obese' & categories_2019 == "healthy" ~ 1,
    categories_2017 == 'obese' & categories_2019 == "overweight" ~ 2, 
    categories_2017 == 'obese' & categories_2019 == "obese" ~ 3)) %>% 
  dplyr::rename( bmi_1 = bmi_2017, 
                 category_1 = categories_2017,
                 over27.5_1 = over27.5_2017,
                 bmi_2 = bmi_2019, 
                 category_2 = categories_2019,
                 over27.5_2 = over27.5_2019)





period_1 <- period_1 



demog_2017 <- median_bmi %>% 
  dplyr::filter(year == 2017) %>% 
  dplyr::select (-c(median_bmi,
                    BMI_categories, 
                    BMI_over27.5
  ))

period_1 <- period_1 %>% 
  left_join(demog_2017)




####
  
period_2 <- bmi_2019 %>% 
  dplyr::left_join(bmi_2021)

period_2 <- period_2 %>% 
  drop_na()

period_2 <- period_2 %>% 
  dplyr::mutate(healthy_cat_change = case_when(
    categories_2019 == 'healthy' & categories_2021 == "underweight" ~ 0,
    categories_2019 == 'healthy' & categories_2021 == "healthy" ~ 1,
    categories_2019 == 'healthy' & categories_2021 == "overweight" ~ 2, 
    categories_2019 == 'healthy' & categories_2021 == "obese" ~ 3 )) %>%
  dplyr::mutate(overweight_cat_change = case_when(
    categories_2019 == 'overweight' & categories_2021 == "underweight" ~ 0,
    categories_2019 == 'overweight' & categories_2021 == "healthy" ~ 1,
    categories_2019 == 'overweight' & categories_2021 == "overweight" ~ 2, 
    categories_2019 == 'overweight' & categories_2021 == "obese" ~ 3 )) %>%
  dplyr::mutate(obese_cat_change = case_when(
    categories_2019 == 'obese' & categories_2021 == "underweight" ~ 0,
    categories_2019 == 'obese' & categories_2021 == "healthy" ~ 1,
    categories_2019 == 'obese' & categories_2021 == "overweight" ~ 2, 
    categories_2019 == 'obese' & categories_2021 == "obese" ~ 3)) %>% 
  dplyr::rename( bmi_1 = bmi_2019, 
                 category_1 = categories_2019,
                 over27.5_1 = over27.5_2019,
                 bmi_2 = bmi_2021, 
                 category_2 = categories_2021,
                 over27.5_2 = over27.5_2021)

demog_2019 <- median_bmi %>% 
  dplyr::filter(year == 2019) %>% 
  dplyr::select (-c(median_bmi,
                    BMI_categories, 
                    BMI_over27.5
  ))

period_2 <- period_2 %>% 
  left_join(demog_2019)

############
base_period <- bmi_2015 %>% 
  left_join(bmi_2017)


base_period <- base_period %>% 
  drop_na()

base_period <- base_period %>% 
  dplyr::mutate(healthy_cat_change = case_when(
    categories_2015 == 'healthy' & categories_2017 == "underweight" ~ 0,
    categories_2015 == 'healthy' & categories_2017 == "healthy" ~ 1,
    categories_2015 == 'healthy' & categories_2017 == "overweight" ~ 2, 
    categories_2015 == 'healthy' & categories_2017 == "obese" ~ 3 )) %>%
  dplyr::mutate(overweight_cat_change = case_when(
    categories_2015 == 'overweight' & categories_2017 == "underweight" ~ 0,
    categories_2015 == 'overweight' & categories_2017 == "healthy" ~ 1,
    categories_2015 == 'overweight' & categories_2017 == "overweight" ~ 2, 
    categories_2015 == 'overweight' & categories_2017 == "obese" ~ 3 )) %>%
  dplyr::mutate(obese_cat_change = case_when(
    categories_2015 == 'obese' & categories_2017 == "underweight" ~ 0,
    categories_2015 == 'obese' & categories_2017 == "healthy" ~ 1,
    categories_2015 == 'obese' & categories_2017 == "overweight" ~ 2, 
    categories_2015 == 'obese' & categories_2017 == "obese" ~ 3)) %>% 
  dplyr::rename( bmi_1 = bmi_2015, 
                 category_1 = categories_2015,
                 over27.5_1 = over27.5_2015,
                 bmi_2 = bmi_2017, 
                 category_2 = categories_2017,
                 over27.5_2 = over27.5_2017)




demog_2015 <- median_bmi %>% 
  dplyr::filter(year == 2015) %>% 
  dplyr::select (-c(median_bmi,
                    BMI_categories, 
                    BMI_over27.5
  ))

base_period <- base_period %>% 
  left_join(demog_2015)





 ######## final data set
bmi_transitions <- period_1 %>% 
  dplyr::bind_rows(period_2) %>%
  dplyr::bind_rows(base_period)


bmi_transitions <- bmi_transitions %>% 
dplyr::mutate(become_obese = case_when(
  category_2 == "obese"  & category_1 !="obese" ~ 1, 
  category_2 !="obese" ~ 0,  
  category_2 == "obese"  & category_1 =="obese"  ~ 0
  ))   %>% 
  dplyr::mutate(become_eth_obese = case_when(
    over27.5_1 == "<27.5" & over27.5_2 == "27.5+" ~ 1, 
    over27.5_1 == "<27.5" & over27.5_2 == "<27.5" ~ 0
  )) %>% 
  dplyr::mutate(become_overweight = case_when(
    healthy_cat_change == 2 | healthy_cat_change == 3 ~ 1, 
    healthy_cat_change ==0 | healthy_cat_change == 1 ~0
  ))



#########
## broad outputs



## ethnic obese
bmi_transitions %>% 
  dplyr::filter(ethnic_no_miss == "Asian" | ethnic_no_miss == "Black" | ethnic_no_miss == "Mixed") %>%
  tabyl(year, become_eth_obese, show_na = FALSE) %>% 
  adorn_percentages("row")

## become obese
bmi_transitions %>% 
  tabyl(year, become_obese, show_na = FALSE) %>% 
  adorn_percentages("row")

## become overweight
bmi_transitions %>% 
  tabyl(year, become_overweight, show_na = FALSE) %>% 
  adorn_percentages("row")


## save outputs
write_feather (bmi_transitions, here::here ("output/data","BMI_category_transition.feather"))  



