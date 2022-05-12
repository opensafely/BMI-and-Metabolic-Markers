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



## precovid BMI change categories

data2 <- read_csv (here::here ("output/data", "precovid_bmi_trajectories.csv"))

colnames(data)

data <- data2 %>% 
  dplyr::rename(weightloss=  ">0.1 loss" ) %>%
  dplyr::rename(stable =  "-0.1 to <0.1" ) %>%
  dplyr::rename(slow_gain =  "0.1 to <0.3" ) %>%
  dplyr::rename(mod_gain =  "0.3 to <0.5" ) %>%
  dplyr::rename(rapid_gain =  "over 0.5" )
 
  
  
data <- data %>% 
  ## extract strings:: weightloss
  dplyr::mutate(percent_weightloss = stringr::str_extract(data$weightloss, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_weightloss = stringr::str_extract(string = data$weightloss,
                                                    pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_weightloss = as.numeric(N_weightloss)) %>%
  ## extract strings:: stable
  dplyr::mutate(percent_stable = stringr::str_extract(data$stable, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_stable = stringr::str_extract(string = data$stable,
                                                pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_stable = as.numeric(N_stable)) %>%
  ## extract strings:: slow_gain
  dplyr::mutate(percent_slow_gain = stringr::str_extract(data$slow_gain, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_slow_gain = stringr::str_extract(string = data$slow_gain,
                                                   pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_slow_gain = as.numeric(N_slow_gain)) %>%
  ## extract strings:: mod_gain
  dplyr::mutate(percent_mod_gain = stringr::str_extract(data$mod_gain, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_mod_gain = stringr::str_extract(string = data$mod_gain,
                                                  pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_mod_gain = as.numeric(N_mod_gain)) %>%
  ## extract strings:: rapid_gain
  dplyr::mutate(percent_rapid_gain = stringr::str_extract(data$rapid_gain, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_rapid_gain = stringr::str_extract(string = data$rapid_gain,
                                                    pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_rapid_gain = as.numeric(N_rapid_gain)) 
  
  data$N_weightloss[data$N_weightloss<6] <- NA
  data$N_stable[data$N_stable<6] <- NA
  data$N_slow_gain[data$N_slow_gain<6] <- NA
  data$N_mod_gain[data$N_mod_gain<6] <- NA
  data$N_rapid_gain[data$N_rapid_gain<6] <- NA
  
  data <- data %>% 
    dplyr::select(-c("weightloss", "stable", "slow_gain", "mod_gain", "rapid_gain"))
  


data <- data %>% 
  dplyr::mutate(N_weightloss = plyr::round_any(data$N_weightloss, 5)) %>% 
  dplyr::mutate(N_stable = plyr::round_any(data$N_stable, 5)) %>%
  dplyr::mutate(N_slow_gain = plyr::round_any(data$N_slow_gain, 5)) %>%
  dplyr::mutate(N_mod_gain = plyr::round_any(data$N_mod_gain, 5)) %>%
  dplyr::mutate(N_rapid_gain = plyr::round_any(data$N_rapid_gain, 5)) 


data$N_weightloss[data$N_weightloss == 5] <- ">5"
data$N_stable[data$N_stable == 5] <- ">5"
data$N_slow_gain[data$N_slow_gain == 5] <- ">5"
data$N_mod_gain[data$N_mod_gain == 5] <- ">5"
data$N_rapid_gain[data$N_rapid_gain == 5] <- ">5"


precovid_change_categories <- data

write.csv (precovid_change_categories, here::here ("output/data","precovid_bmi_trajectories.round.csv"))



## Postcovid change categories


data2 <- read_csv (here::here ("output/data", "postcovid_bmi_trajectories.csv"))

colnames(data)

data <- data2 %>% 
  dplyr::rename(weightloss=  ">0.1 loss" ) %>%
  dplyr::rename(stable =  "-0.1 to <0.1" ) %>%
  dplyr::rename(slow_gain =  "0.1 to <0.3" ) %>%
  dplyr::rename(mod_gain =  "0.3 to <0.5" ) %>%
  dplyr::rename(rapid_gain =  "over 0.5" )
 
  
  
data <- data %>% 
  ## extract strings:: weightloss
  dplyr::mutate(percent_weightloss = stringr::str_extract(data$weightloss, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_weightloss = stringr::str_extract(string = data$weightloss,
                                                    pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_weightloss = as.numeric(N_weightloss)) %>%
  ## extract strings:: stable
  dplyr::mutate(percent_stable = stringr::str_extract(data$stable, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_stable = stringr::str_extract(string = data$stable,
                                                pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_stable = as.numeric(N_stable)) %>%
  ## extract strings:: slow_gain
  dplyr::mutate(percent_slow_gain = stringr::str_extract(data$slow_gain, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_slow_gain = stringr::str_extract(string = data$slow_gain,
                                                   pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_slow_gain = as.numeric(N_slow_gain)) %>%
  ## extract strings:: mod_gain
  dplyr::mutate(percent_mod_gain = stringr::str_extract(data$mod_gain, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_mod_gain = stringr::str_extract(string = data$mod_gain,
                                                  pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_mod_gain = as.numeric(N_mod_gain)) %>%
  ## extract strings:: rapid_gain
  dplyr::mutate(percent_rapid_gain = stringr::str_extract(data$rapid_gain, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(N_rapid_gain = stringr::str_extract(string = data$rapid_gain,
                                                    pattern = "[0-9]{0}[0-9]+")) %>% 
  dplyr::mutate(N_rapid_gain = as.numeric(N_rapid_gain)) 
  
  data$N_weightloss[data$N_weightloss<6] <- NA
  data$N_stable[data$N_stable<6] <- NA
  data$N_slow_gain[data$N_slow_gain<6] <- NA
  data$N_mod_gain[data$N_mod_gain<6] <- NA
  data$N_rapid_gain[data$N_rapid_gain<6] <- NA
  
  data <- data %>% 
    dplyr::select(-c("weightloss", "stable", "slow_gain", "mod_gain", "rapid_gain"))
  


data <- data %>% 
  dplyr::mutate(N_weightloss = plyr::round_any(data$N_weightloss, 5)) %>% 
  dplyr::mutate(N_stable = plyr::round_any(data$N_stable, 5)) %>%
  dplyr::mutate(N_slow_gain = plyr::round_any(data$N_slow_gain, 5)) %>%
  dplyr::mutate(N_mod_gain = plyr::round_any(data$N_mod_gain, 5)) %>%
  dplyr::mutate(N_rapid_gain = plyr::round_any(data$N_rapid_gain, 5)) 


data$N_weightloss[data$N_weightloss == 5] <- ">5"
data$N_stable[data$N_stable == 5] <- ">5"
data$N_slow_gain[data$N_slow_gain == 5] <- ">5"
data$N_mod_gain[data$N_mod_gain == 5] <- ">5"
data$N_rapid_gain[data$N_rapid_gain == 5] <- ">5"


postcovid_change_categories <- data

write.csv (postcovid_change_categories, here::here ("output/data","postcovid_bmi_trajectories.round.csv"))


## Mean Change in BMI trajectories
data2 <- read_csv (here::here ("output/data", "mean_bmi_traj_change.csv"))

data <- as.data.frame(data2)
  
data <- data %>% 
  dplyr::filter(n>5)

data <- data %>% 
  dplyr::mutate(n = plyr::round_any(data$n, 5))

data$n[data$n == 5] <- ">5"

trajectory_change <- data

write.csv (trajectory_change, here::here ("output/data", "mean_bmi_traj_change_round.csv"))
