## Author:  Miriam Samuel
## Date:  3rd April 2022
##:  R.script -  create data set required for trajectory analysis



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
library(lubridate)

## Read in files  >>> Change PATH!!

# check working directory:  getwd()

#BMI_complete_categories <- read_feather (here::here ("output/data", "BMI_all_2019.feather"))

BMI_all_long <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "all_bmi_long.feather"))


#################################################### remove to run main analysis
## Analysis just for dummy data to allow years to be extracted.  
# creates a year variable that correlates to the date BMI measured in dummy data.  (That does not correlate with year due to return expectations)
BMI_all <- BMI_all_long %>%
  dplyr::select(-("year"))

BMI_all$year <- lubridate::year(ymd(BMI_all$bmi_date))

#############################################
## Real analysis starts here


#BMI_all <- read_feather (here::here ("output/data", "all_bmi_long.feather"))
BMI_demog <- BMI_all %>% 
  dplyr::group_by(patient_id) %>% 
  dplyr::slice_head() %>%
  dplyr::select(-(monthly_bmi), -(bmi_date))


## reduce the number of columns for data manipulation - join to demographic data at end
all_long_date_cat <- BMI_all %>%
  drop_na(monthly_bmi) %>% 
  dplyr::select("patient_id", "monthly_bmi", "bmi_date", "year")

all_long_date_cat <- all_long_date_cat %>%                                        # split into year categories
  dplyr::mutate(time_frame = cut(
    year,
    breaks = c(2015, 2016, 2017, 2019, 2020, 2021),
    include.lowest = TRUE,
    labels = c("Y2015-2016", "Y2017", "Y2018-2019", "Y2020", "Y2021")   ## Note: years not correlating with date in dummy data due to return expectations
  ))

## generate a code to pick at random a bmi from each patient for each time frame  
all_long_sample <- all_long_date_cat %>%       
  group_by(patient_id, time_frame) %>%
  dplyr::slice_sample(n=1)




##  Analysis logic
# 1.  Time frame 1:  Use random BMI from 2015/16 if available, 2017 if not available
# 2.  Time frame 2:  Use random BMI from 2017/2018
# 3.  Time frame 3:  Use random BMI from 2021 if available, if not 2020.  
# 4.  calculate trajectories by time frame.   change per year for pre-pandemic and post pandemic measures. 



###  NEED TO CREATE A FLAG IF ANY EVENT IN THE PATIENT GROUP HAD A BMI in the 1st time period
sample_flag <- all_long_sample %>%     # create a logical column - was the BMI measure in 2015/2016
  dplyr::group_by(patient_id) %>%
  dplyr::mutate(tf1_2015_16 = case_when(
    time_frame == 'Y2015-2016' ~ 1,
    time_frame != 'Y2015 - 2016' ~ 0
  ))

sample_flag_2 <- sample_flag %>%  # create a flag within patient groups:  did they have a BMI in 2015/2016
  dplyr::group_by(patient_id) %>%
  dplyr::summarise (                         # summarise command is performed on the grouped objects.  Output just patient_id and presence of
    tf1_flag = max(tf1_2015_16))

sample_flag <- sample_flag  %>%
  left_join(sample_flag_2) 


sample_flag %>%
  tabyl(time_frame, tf1_flag)




sample_flag <- sample_flag %>%   ## create a flag to highlight patients with a BMI measure in 2017 but non in 2015/2016
  dplyr::mutate(tf1_2017 = case_when(
    time_frame== 'Y2017' & tf1_flag == 0 ~ 1,
    time_frame != 'Y2017' | tf1_flag ==1 ~ 0
  ))

sample_flag %>%
  tabyl (time_frame, tf1_2017)

sample_flag <- sample_flag %>%
  dplyr:: select(patient_id, time_frame, tf1_2015_16, tf1_flag, tf1_2017)



all_long_sample <- all_long_sample %>%   
  dplyr::left_join(sample_flag)  ## join by patient_id and time frame


## next step: create a code that takes keeps bmi from 2015/2016 first, then 2017 (if no 2015/16 data). 
## this reduces the chance of selecting data from time frame 1 and time frame2 which are too close together... without ignoring BMI data from 2017 if there is none in 2015/2016. 


sample_3 <- all_long_sample %>%
  dplyr::mutate(keep_2017 = case_when(
    year==2017  & tf1_2017==1 ~ 1,   # keep data from 2017 if none from 2015/16
    year==2017  & tf1_flag==0 ~ 0,   # drop data from 2017 if there is a result from 2015/16
    year !=2017 ~ 1                  # keep data from all other years
  ))



sample_3 <- sample_3 %>%
  dplyr::filter(keep_2017 ==1)   # only keep 2017 identified by previous flags



## select relevant columns
sample_3 <- sample_3 %>% 
  dplyr::select("patient_id", "monthly_bmi", "bmi_date", "year")

##########################

## Code to flag if BMI measure is in 2021
flag_2021 <- sample_3 %>%             
  dplyr::mutate (bmi_2021 = case_when(
    year==2021 ~ 1, 
    year!=2021 ~ 0
  ))


##  create a flag to identify patients who had a BMI in 2021
had_2021_flag <- flag_2021 %>%
  group_by(patient_id) %>%
  dplyr::summarise(
    had_2021 = max(bmi_2021)
  )

flag_2021 <- flag_2021 %>%
  dplyr::left_join(had_2021_flag)


## create code to identify BMIs from 2021 preferentially or 2020 if not available
flag_2021 <- flag_2021 %>%
  dplyr::mutate(keep_2020 = case_when(        
    year==2020 & had_2021==1 ~ 0,               # drop 2020 data if had BMI in 2021
    year==2020 & had_2021== 0 ~ 1,              # keep 2020 data if no BMI in 2021
    year!=2020 ~ 1                              # keep all other years
  ))


flag_2021 <- flag_2021 %>%
  dplyr::filter(keep_2020 == 1)



flag_2021 <- flag_2021 %>%
  dplyr::mutate(time_period = case_when(
    time_frame == 'Y2020'| time_frame == 'Y2021' ~ 'post_pandemic' ,
    time_frame == 'Y2018-2019' ~ 'pre_pandemic',
    time_frame=='Y2015-2016'| time_frame=='Y2017' ~ 'base',
  ))


flag_2021 %>%
  tabyl(time_frame, time_period)
###############################################
################################################

## Will need to pivot wider to allow across column calculations.  
## will have to split into three data sets first to keep BMI date data

## 1.  base bmi
base_bmi <- flag_2021 %>% 
  dplyr::filter(time_period == 'base')

base_bmi <- base_bmi %>% 
  dplyr::mutate (base_bmi = monthly_bmi) %>% 
  dplyr::mutate (base_bmi_date = bmi_date) 

base_bmi <- base_bmi %>% 
  dplyr::ungroup() %>%
  dplyr::select(patient_id, base_bmi, base_bmi_date)
 

## 2.  pre-pandemic bmi
precovid_bmi <- flag_2021 %>% 
  dplyr::filter(time_period == 'pre_pandemic')

precovid_bmi <- precovid_bmi %>% 
  dplyr::mutate (precovid_bmi = monthly_bmi) %>% 
  dplyr::mutate (precovid_bmi_date = bmi_date) 

precovid_bmi <- precovid_bmi %>% 
  dplyr::ungroup() %>%
  dplyr::select(patient_id, precovid_bmi, precovid_bmi_date) 


## 2.  post-pandemic bmi
postcovid_bmi <- flag_2021 %>% 
  dplyr::filter(time_period == 'post_pandemic')

postcovid_bmi <- postcovid_bmi %>% 
  dplyr::mutate (postcovid_bmi = monthly_bmi) %>% 
  dplyr::mutate (postcovid_bmi_date = bmi_date) 

postcovid_bmi <- postcovid_bmi %>% 
  dplyr::ungroup() %>%
  dplyr::select(patient_id, postcovid_bmi, postcovid_bmi_date) 

#####
## join data sets with demographic data to get full data set

BMI_trajectories <- BMI_demog %>% 
  dplyr::left_join(base_bmi) %>% 
  dplyr::left_join(precovid_bmi) %>% 
  dplyr::left_join(postcovid_bmi)


## calculate BMI change for each period

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(bmi_change1 = (precovid_bmi - base_bmi)) %>% 
  dplyr::mutate(bmi_change2 = (postcovid_bmi - precovid_bmi))
