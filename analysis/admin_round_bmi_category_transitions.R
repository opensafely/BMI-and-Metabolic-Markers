## This script rounds and redacts tables to ensure no counts <5 and all values rounded to 5
## 12th May
## Author: M Samuel


## Add libraries
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
library(stringr)


## Add data





## ACTION: bmi_change_univariate
## DATA:  rapid_bmi_change_popcharac.csv
data <- read_csv (here::here ("output/data", "bmi_category_transitions_round.csv"))

data <- data %>% 
 dplyr::rename('n_transition' = 'n')


 

data <- data %>% 
  dplyr::mutate(n_transition = plyr::round_any(data$n_transition, 5)) 

rapid_change <- data

 write.csv (rapid_change, here::here ("output/data","bmi_category_transitions_round_2.csv"))

