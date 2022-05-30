## This file redacts low numbers

## M Samuel 
##30th May 2022



## Load libraries
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




mean <- read_csv (here::here ("output/data", "mean_bmi_traj_change_t1dm.csv"))
univariate <- read_csv (here::here ("output/data", "univariate_bmi_trajectory_change_T1DM.csv"))
age_adjusted <- read_csv (here::here ("output/data", "age_adjusted_bmi_trajectory_change_T1DM.csv"))

mean <- mean %>% 
  dplyr::filter(n>0)
  
univariate <- univariate %>%
  dplyr::filter(term != "smoking_statusM")
  
age_adjusted <- age_adjusted %>%
  dplyr::filter(term != "smoking_statusM")
  
  
write.csv (mean, here::here ("output/data","mean_bmi_traj_change_t1dm_redacted.csv"))
write.csv (univariate, here::here ("output/data","univariate_bmi_traj_change_t1dm_redacted.csv"))
write.csv (age_adjusted, here::here ("output/data","age_adj_bmi_traj_change_t1dm_redacted.csv"))
  
