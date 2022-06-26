#### Author: M Samuel
#### 
####  Round outputs


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
library(skimr)

BMI_data<- read_csv (here::here ("output/data", "had_bmi_2019_21_models_1_demographics.csv "))

BMI_data <- BMI_data %>% 
  dplyr::mutate(n = plyr::round_any(BMI_data$n, 5))  
  
  
write.csv (BMI_data, here::here ("output/data","had_bmi_2019_21_models_1_demographics_round.csv"))
