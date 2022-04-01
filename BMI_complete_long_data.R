###  Demographic variables
## Link all the data sets: 


##  packages
library(broom)
library(purrr)
library(dplyr)
library(janitor)
library(tidyverse)
library(arrow)

### Read in data sets  <<<  CHANGE PATH!!

bmi_long_2015 <- read_feather (here::here ("output/data", "BMI_complete_long_2015.feather"))
bmi_long_2016 <- read_feather (here::here ("output/data", "BMI_complete_long_2016.feather"))
bmi_long_2017 <- read_feather (here::here ("output/data", "BMI_complete_long_2017.feather"))
bmi_long_2018 <- read_feather (here::here ("output/data", "BMI_complete_long_2018.feather"))
bmi_long_2019 <- read_feather (here::here ("output/data", "BMI_complete_long_2019.feather"))
bmi_long_2020 <- read_feather (here::here ("output/data", "BMI_complete_long_2020.feather"))
bmi_long_2021 <- read_feather (here::here ("output/data", "BMI_complete_long_2021.feather"))


patient_imd <- read_feather (here::here ("/data/home/hmy926/BMI Open Safely", "patient_imd.feather"))


all_long <- bmi_long_2015 %>%
  dplyr::bind_rows(bmi_long_2016) %>%
  dplyr::bind_rows(bmi_long_2017) %>%
  dplyr::bind_rows(bmi_long_2018) %>%
  dplyr::bind_rows(bmi_long_2019) %>%
  dplyr::bind_rows(bmi_long_2020) %>%
  dplyr::bind_rows(bmi_long_2021) 



## Recode IMD as error in previous code
all_long <- all_long %>%
  dplyr::ungroup() %>%
  dplyr::select(-(imd)) %>% 
  dplyr::left_join(patient_imd)

all_long <- all_long %>%
  dplyr::mutate(bmi_date = as.Date(bmi_measured_date)) %>%
  dplyr::select(-('bmi_measured_date'), -('date'))
  
write_feather (all_long, here::here ("output/data","all_bmi_long.feather"))  
