## This script looks at sex  adjusted predictors rapid bmi change 
## Author: M Samuel
## Date: 4th May 2022



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
library(skimr)
library(ggplot2)



# BMI_trajectories <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "BMI_trajectory_data_long.feather"))


BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))

colnames(BMI_trajectories)




check <- BMI_trajectories %>%
  tabyl(eth_group_16)



check

# check <- BMI_trajectories %>% mutate(
  # eth_group_16_na = as.character(eth_group_16_na),
 # eth_group_16_na = ifelse(is.na(eth_group_16_na), "None", eth_group_16_na),
 # eth_group_16_na = as.factor(eth_group_16_na))



write_csv (check, here::here ("output/data","check_ethnicity_categories.csv"))