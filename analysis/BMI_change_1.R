



## Author:  Miriam Samuel
## Date:  25th April 2022
##:  R.script -  create data set required for trajectory analysis. 
##: long sript - split to reduce memory for each job. 



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

## Read in files  >>> Change PATH!!



BMI_all <- read_feather (here::here ("output/data", "all_bmi_long.feather"))

# BMI_all_long <- read_feather (here::here ("/data/home/hmy926/BMI Open Safely", "all_bmi_long_new.feather"))


#################################################### remove to run main analysis
## Analysis just for dummy data to allow years to be extracted.  
# creates a year variable that correlates to the date BMI measured in dummy data.  (That does not correlate with year due to return expectations)
# BMI_all <- BMI_all_long %>%
  # dplyr::select(-("year"))

# BMI_all$year <- lubridate::year(ymd(BMI_all$bmi_date))

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



write_feather (BMI_demog, here::here ("output/data","BMI_demog.feather"))
write_feather (all_long_sample, here::here ("output/data","BMI_all_long_sample.feather"))  
