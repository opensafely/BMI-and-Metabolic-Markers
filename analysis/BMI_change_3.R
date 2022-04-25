## Author:  Miriam Samuel
##: 25th April 2022
##: Split data prep for trajectory analysis... to reduce memory. 
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
library(janitor)
library(lubridate)
library(skimr)

BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories.feather"))
BMI_demog <- read_feather (here::here ("output/data", "BMI_demog.feather"))



## calculate BMI change for each period

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(bmi_change1 = (precovid_bmi - base_bmi)) %>% 
  dplyr::mutate(bmi_change2 = (postcovid_bmi - precovid_bmi))

## create a flag to see if missing values in each time period



BMI_trajectories_2 <- BMI_trajectories %>%       # flag rows with missing values
  dplyr::mutate(bmi1_missing = is.na(base_bmi), 
                bmi2_missing = is.na(precovid_bmi), 
                bmi3_missing = is.na(postcovid_bmi))


BMI_trajectories_2 <- BMI_trajectories_2 %>%                      # identify if values required for time change analysis complete
  dplyr::mutate(period1_complete = case_when(
    bmi1_missing == FALSE & bmi2_missing == FALSE ~ "complete",
    bmi1_missing == TRUE | bmi2_missing == TRUE ~ "incomplete"
  ))


BMI_trajectories_3 <- BMI_trajectories_2 %>%                       # filter date to perform time change analysis only on patients with complete data
  dplyr::filter(period1_complete == "complete") %>%
  dplyr::mutate(time_change1 = difftime(precovid_bmi_date, base_bmi_date, units = "days" )/365)


BMI_trajectories_3 <- BMI_trajectories_3 %>%                      # calculate change in BMI per year
  dplyr::mutate(time_change1 = as.numeric(time_change1)) %>%
  dplyr::mutate(yearly_bmi_change1 = bmi_change1/time_change1) 

BMI_trajectories_3 <- BMI_trajectories_3 %>% 
  dplyr::select(patient_id, time_change1, yearly_bmi_change1)


 ##  repeat to identify cahnge per year in period 2
BMI_trajectories_4 <- BMI_trajectories_2 %>%
  dplyr::mutate(period2_complete = case_when(
    bmi2_missing == FALSE & bmi3_missing == FALSE ~ "complete",
    bmi2_missing == TRUE | bmi3_missing == TRUE ~ "incomplete"
  ))


BMI_trajectories_4 <- BMI_trajectories_4 %>%                       # filter date to perform time change analysis only on patients with complete data
  dplyr::filter(period2_complete == "complete") %>%
  dplyr::mutate(time_change2 = difftime(postcovid_bmi_date, precovid_bmi_date, units = "days" )/365)


BMI_trajectories_4 <- BMI_trajectories_4 %>%                      # calculate change in BMI per year
  dplyr::mutate(time_change2 = as.numeric(time_change2)) %>%
  dplyr::mutate(yearly_bmi_change2 = bmi_change2/time_change2) 

BMI_trajectories_4 <- BMI_trajectories_4 %>% 
  dplyr::select(patient_id, time_change2, yearly_bmi_change2)


############################################
## Link time change data to main data set

BMI_trajectories_complete <- BMI_demog %>% 
  dplyr::left_join(BMI_trajectories) %>% 
  dplyr::left_join(BMI_trajectories_3) %>% 
  dplyr::left_join(BMI_trajectories_4)

BMI_trajectories_complete <- BMI_trajectories_complete %>% 
  dplyr::mutate(period1_missing=is.na(yearly_bmi_change1), 
                period2_missing=is.na(yearly_bmi_change2))

BMI_trajectories_complete <- BMI_trajectories_complete %>% 
  dplyr::mutate(complete_bmi_data = case_when(
    period1_missing==FALSE & period2_missing==FALSE ~ "complete", 
    period1_missing==TRUE | period2_missing==TRUE ~ "incomplete"
  ))


BMI_trajectories_complete %>% 
  tabyl(complete_bmi_data)
  
write_feather (BMI_trajectories_complete, here::here ("output/data","BMI_trajectories_final.feather"))
