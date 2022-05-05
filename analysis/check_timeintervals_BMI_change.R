## Author: M Samuel 
## Date: 5th May 2022

### Data script to check if any of the time-changes are 0
### This could occur due to the way OS extracts the last BMI value per month. 
### Possible overlap in the 2019/2020 data set and the 2020/2021 data set
### May be the cause of the infinty values disrupting the calculations. 
## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)
library(skimr)


## Read in data

BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories_final.feather"))


BMI_trajectories_check <- BMI_trajectories %>% 
  dplyr::mutate(timechange1_check = time_change1)


## choose relevant variable for check

BMI_trajectories_check <- BMI_trajectories_check %>% 
  dplyr::select(patient_id, base_bmi_date, precovid_bmi_date, postcovid_bmi_date, timechange1_check)


## create a flag to identify when a time difference between BMI measures is recorded as '0'
BMI_trajectories_check <- BMI_trajectories_check %>% 
  dplyr::mutate(time_change_error = case_when(
    timechange1_check == 0 ~ 1, 
    timechange1_check != 0 ~ 0
  ))

time1_error <- BMI_trajectories_check %>% 
    tabyl(time_change_error)

## Filter a data set only containing data on rows affected by a '0' time difference
BMI_trajectories_check <- BMI_trajectories_check %>% 
  dplyr::filter (time_change_error == 1)

## create a brief table to see how many patients this affects.  If very few, will not need to go back and change code
BMI_trajectories_check <- skimr::skim_without_charts(BMI_trajectories_check)


## save table as output
write.csv (BMI_trajectories_check, here::here ("output/data","bmi_timeinterval_check.csv"))
write.csv (time1_error, here::here ("output/data","time1_error.csv"))
