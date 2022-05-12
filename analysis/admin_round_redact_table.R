## This script rounds and redacts tables to ensure no counts <5 and all values rounded to 5
## 12th May
## Author: M Samuel


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
data <- read_csv (here::here ("output/data", "rapid_bmi_change_popcharac.csv"))

data <- data %>% 
 dplyr::rename('not_rapid' = '0') %>% 
 dplyr::rename('rapid' = '1')


data <- data %>% 
  dplyr::mutate(percent_not_rapid = stringr::str_extract(data$not_rapid, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_not_rapid = stringr::str_extract(string = data$not_rapid,
                                           pattern = "(?<=\\().*(?=\\))")) %>% 
  dplyr::mutate(N_not_rapid = as.numeric(N_not_rapid)) %>% 
  dplyr::mutate(percent_rapid = stringr::str_extract(data$rapid, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_rapid = stringr::str_extract(string = data$rapid,
                                           pattern = "(?<=\\().*(?=\\))")) %>% 
  dplyr::mutate(N_rapid = as.numeric(N_rapid)) %>% 
  dplyr::select(-("rapid"), -("not_rapid")) %>%     ## add group and variable
  dplyr::filter(N_rapid >5) %>%
  dplyr::filter(N_not_rapid >5) 

data <- data %>% 
  dplyr::mutate(N_rapid = plyr::round_any(data$N_rapid, 5)) %>% 
  dplyr::mutate(N_not_rapid = plyr::round_any(data$N_not_rapid, 5))

rapid_change <- data



 write.csv (rapid_change, here::here ("output/data","rapid_bmi_change_popcharac_round.csv"))



