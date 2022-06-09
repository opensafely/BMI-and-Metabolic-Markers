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


## create model adjusted for covariates that are likely to influence the odds of having complete data
models_1 <- glm(complete~sex + age_group_2 + imd + eth_group_16 + diabetes_t2, family = binomial, data = traj_pop_complete)

## ALSO the following variables have quality outcomes attached to yearly review of BP
# learning_disability, psychosis_schiz_bipolar


fitted <- (broom::augment(models_1, traj_pop_complete))

prob_weights <- fitted %>% 
  dplyr::mutate(weight = 1/.fitted)
  

write_feather (prob_weight, here::here ("output/data","IPW_weight_added.feather"))
