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
library(lubridate)
library(skimr)
library(ggplot2)
library(gtsummary)


## read file
BMI_data <- read_feather (here::here ("/Users/miriamsamuel/Documents/Academic GP/Open Safely/Dummy Data", "BMI_trajectory_data_long.feather"))

## Filter for data that had BMI_trajectory data in the precovid period.  This is my population
trajectory_population <- BMI_data %>% 
  dplyr::filter(period1_missing == FALSE)  ## include data with a precovid trajectory (period 1)

## Filter out patients who were underweight precovid and those with cancer

trajectory_population <- trajectory_population %>%
  dplyr::filter(all_cancer == "FALSE") %>% 
  dplyr::filter(precovid_bmi_category != "underweight")


## Create a flag for COVID
trajectory_population <- trajectory_population %>% 
  dplyr::mutate(complete = case_when(
    complete_bmi_data == "complete" ~ 1, 
    complete_bmi_data == "incomplete" ~ 0)
  )



## create a data set dropping NA of predictors so the prediction function will work
traj_pop_complete <- trajectory_population %>% 
  drop_na(age_group_2) %>% 
  drop_na(imd) %>% 
  drop_na(eth_group_16)


## create model adjusted for covariates that are likely to influence the odds of having complete data
models_1 <- glm(complete~sex + age_group_2 + imd + eth_group_16 + diabetes_t2, family = binomial, data = traj_pop_complete)

## ALSO the following variables have quality outcomes attached to yearly review of BP
# learning_disability, psychosis_schiz_bipolar


fitted <- (broom::augment(models_1, traj_pop_complete))

prob_weights <- fitted %>% 
  dplyr::mutate(weight = 1/.fitted)
  

write_feather (prob_weight, here::here ("output/data","IPW_weight_added.feather"))
