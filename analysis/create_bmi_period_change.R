## Author:  Miriam Samuel
## Date:  3rd April 2022
##:  R.script -  create data set required for trajectory analysis


library(dplyr)
library(janitor)
library(tidyverse)
library(arrow)


## Read in data

all_long <- read_feather (here::here ("output/data", "all_bmi_long.feather"))

all_long_date_cat <- all_long %>%                                        # split into year categories
  dplyr::mutate(time_frame = cut(
    year,
    breaks = c(2015, 2016, 2017, 2019, 2020, 2021),
    include.lowest = TRUE,
    labels = c("Y2015-2016", "Y2017", "Y2018-2019", "Y2020", "Y2021")   ## Note: years not correlating with date in dummy data due to return expectations
  ))
  
  
# all_long_date_cat %>% tabyl(year, time_frame)     ##  check correlation


## generate a code to pick at random a bmi from each patient for each time frame  
all_long_sample <- all_long_date_cat %>%       
  group_by(patient_id, time_frame) %>%
  dplyr::slice_sample(n=1)





##  Analysis logic
# 1.  Time frame 1:  Use BMI from 2015/16 if available, 2017 if not available
# 2.  Time frame 2:  Use random BMI from 2017/2018
# 3.  Time frame 3:  Use BMI from 2021 if available, if not 2020.  
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

##


## next step: create a code that takes a random sample of bmi from 2015/2016 first, then 2017 if that was not present. 
## this reduces the chance of selecting data from time frame 1 and time frame2 which are too close together... without ignoring BMI data from 2017 if there is none in 2015/2016. 


sample_3 <- all_long_sample %>%
  dplyr::mutate(keep_2017 = case_when(
    year==2017  & tf1_flag==0 ~ 1,   # keep data from 2017 if none from 2015/16
    year==2017  & tf1_flag==1 ~ 0,   # drop data from 2017 if there is a result from 2015/16
    year !=2017 ~ 1                  # keep data from all other years
  ))




sample_3 <- sample_3 %>%
  dplyr::filter(keep_2017 ==1)   # only keep 2017 identified by previous flags





##########################


## Code to flag if BMI measure is in 2021
flag_2021 <- sample_3 %>%             
  dplyr::mutate (bmi_2021 = case_when(
    year==2021 ~ 1, 
    year!=2021 ~ 0
  ))


##  flag by group patients who had a BMI in 2021
had_2021_flag <- flag_2021 %>%
  group_by(patient_id) %>%
  dplyr::summarise(
    had_2021 = max(bmi_2021)
  )

flag_2021 <- flag_2021 %>%
  dplyr::left_join(had_2021_flag)



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

## Reshape wide to allow across row calculations

flag_2021_wide <- flag_2021 %>%
  tidyr::spread('time_period', 'monthly_bmi')
  
flag_2021_wide <- flag_2021_wide %>%
  dplyr::mutate(base = replace_na(base, 0)) %>%
  dplyr::mutate(post_pandemic = replace_na(post_pandemic, 0)) %>%
  dplyr::mutate(pre_pandemic = replace_na(pre_pandemic, 0))


flag_2021_wide %>%
  ungroup %>%
  group_by(patient_id)

## create column for to indicate by patient groups each of the recorded BMIs in the time frames
time_frame_1 <- flag_2021_wide %>%
  group_by(patient_id) %>%
  dplyr::summarise(
    TF1 = max(base))

time_frame_2 <- flag_2021_wide %>%
  group_by(patient_id) %>%
  dplyr::summarise(
    TF2 = max(pre_pandemic))

time_frame_3 <- flag_2021_wide %>%
  group_by(patient_id) %>%
  dplyr::summarise(
    TF3 = max(post_pandemic))

##  add the columns to the base data set so each patient event has all recorded BMIs
flag_2021_wide <- flag_2021_wide %>%
  left_join(time_frame_1) %>%
  left_join(time_frame_2) %>%
  left_join(time_frame_3)

## add the dates of the BMI taken
base_bmi_date <- flag_2021 %>%
  ungroup %>%
  dplyr::filter(time_period == 'base') %>%
  dplyr::mutate(TF1_bmi_date = bmi_date) %>%
  dplyr::select(patient_id, TF1_bmi_date)

pre_pandemic_bmi_date <- flag_2021 %>%
  ungroup %>%
  dplyr::filter(time_period == 'pre_pandemic') %>%
  dplyr::mutate(TF2_bmi_date = bmi_date) %>%
  dplyr::select(patient_id, TF2_bmi_date)

post_pandemic_bmi_date <- flag_2021 %>%
  ungroup %>%
  dplyr::filter(time_period == 'post_pandemic') %>%
  dplyr::mutate(TF3_bmi_date = bmi_date) %>%
  dplyr::select(patient_id, TF3_bmi_date)


flag_2021_wide <- flag_2021_wide %>%
  left_join(base_bmi_date) %>%
  left_join(pre_pandemic_bmi_date) %>%
  left_join(post_pandemic_bmi_date)

bmi_trajectories <- flag_2021_wide %>%
  ungroup %>%
  dplyr::arrange(patient_id, desc(year)) %>%
  group_by (patient_id) %>%
  dplyr::slice_head()


bmi_trajectories <- bmi_trajectories %>%
 dplyr::mutate(TF1 = na_if(TF1, '0')) %>%
 dplyr::mutate(TF2 = na_if(TF2, '0')) %>% 
 dplyr::mutate(TF3 = na_if(TF3, '0')) 



##  BMI_change: absolute

bmi_trajectories <- bmi_trajectories %>%
  dplyr::mutate(bmi_change1 = (TF2-TF1))


bmi_trajectories <- bmi_trajectories %>%
  dplyr::mutate(time_change1 = (TF2_bmi_date - TF1_bmi_date)/365.25)


bmi_trajectories <- bmi_trajectories %>%
  dplyr::mutate(time_change1 = as.numeric(time_change1))%>%
  dplyr::mutate(yearly_bmi_change_base = (bmi_change1/time_change1))
  



## time period 2

bmi_trajectories <- bmi_trajectories %>%
  dplyr::mutate(bmi_change2 = (TF3-TF2))


bmi_trajectories <- bmi_trajectories %>%
  dplyr::mutate(time_change2 = (TF3_bmi_date - TF2_bmi_date)/365.25)


bmi_trajectories <- bmi_trajectories %>%
  dplyr::mutate(time_change2 = as.numeric(time_change2))%>%
  dplyr::mutate(yearly_bmi_change_covid= (bmi_change2/time_change2))




##  change in percentage bmi change

bmi_trajectories <- bmi_trajectories %>%
  dplyr::mutate(diff_bmi_change_py = (yearly_bmi_change_covid - yearly_bmi_change_base))

bmi_trajectories <- bmi_trajectories %>%
  dplyr::select(-("time_frame"), -("tf1_2015_16"), -("tf1_2015_16"), -("tf1_flag"), -("tf1_2017"),  -("keep_2017"),    -("bmi_2021"),  -("had_2021"), -("keep_2020")) 
                 

                                              
write_feather (bmi_trajectories, here::here ("output/data","bmi_period_change.feather")
                                               
                      


##  Export file for analysis
