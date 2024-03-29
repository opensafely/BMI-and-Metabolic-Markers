### Author: Miriam Samuel
### 7th March 2022
### This file appends the data sets from each of the years to permit analysis on changes in yearly trends




### JOBS to do:  1)  put year flags on long data set.  2)  get rid of pre-covid obese flag... will need to generate in this file after data tables are appended.


## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(skimr)


## read in data files

BMI_2015_median <- read_feather (here::here ("output/data", "BMI_complete_median_2015.feather"))
BMI_2016_median <- read_feather (here::here ("output/data", "BMI_complete_median_2016.feather"))
BMI_2017_median <- read_feather (here::here ("output/data", "BMI_complete_median_2017.feather"))
BMI_2018_median <- read_feather (here::here ("output/data", "BMI_complete_median_2018.feather"))
BMI_2019_median <- read_feather (here::here ("output/data", "BMI_complete_median_2019.feather"))
BMI_2020_median <- read_feather (here::here ("output/data", "BMI_complete_median_2020.feather"))
BMI_2021_median <- read_feather (here::here ("output/data", "BMI_complete_median_2021.feather"))



# append all the years

BMI_complete_median <- bind_rows(BMI_2015_median, 
                               BMI_2016_median, 
                               BMI_2017_median, 
                               BMI_2018_median, 
                               BMI_2019_median, 
                               BMI_2020_median,
                               BMI_2021_median)


# create a flag for it they were obese pre-covid

BMI_complete_median <- BMI_complete_median %>%
  dplyr::mutate(patient_id = as.numeric(patient_id)) %>%
  arrange(patient_id) %>%
  dplyr::group_by(patient_id) %>%
  dplyr::mutate(
    precovid_obese = (((median_bmi >=30) & ((year=="2015")| (year=="2016")| (year=="2017")| (year=="2018") | (year=="2019")))) , 
    .after = "patient_id") %>%
  dplyr::mutate(
    precovid_obese_flag = (any(precovid_obese == "TRUE")),
    .after = "precovid_obese"
  )


BMI_complete_median <- BMI_complete_median %>% 
ungroup()

BMI_complete_median %>%
skim_without_charts

write_feather (BMI_complete_median, here::here ("output/data","BMI_complete_median.feather"))
