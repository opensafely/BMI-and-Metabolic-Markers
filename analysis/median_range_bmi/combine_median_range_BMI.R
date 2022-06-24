## M Samuel 
## June 2022
## This script ensures that the demographic data used is the most recent recorded for the patient (e.g. age)


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



## Median range BMI
data_2015 <- read_csv (here::here ("output/data", "median_range_bmi_2015.csv"))
data_2016 <- read_csv (here::here ("output/data", "median_range_bmi_2016.csv"))
data_2017 <- read_csv (here::here ("output/data", "median_range_bmi_2017.csv"))
data_2018 <- read_csv (here::here ("output/data", "median_range_bmi_2018.csv"))


data_2019 <- read_csv (here::here ("output/data", "median_range_bmi_2019.csv"))
data_2020 <- read_csv (here::here ("output/data", "median_range_bmi_2020.csv"))
data_2021 <- read_csv (here::here ("output/data", "median_range_bmi_2021.csv"))

data_2015 <- data_2015 %>% 
  dplyr::mutate(year = 2015) 

  data_2016 <- data_2016 %>% 
  dplyr::mutate(year = 2016) 

  data_2017 <- data_2017 %>% 
  dplyr::mutate(year = 2017) 

  data_2018 <- data_2018 %>% 
  dplyr::mutate(year = 2018) 




data_2019 <- data_2019 %>% 
  dplyr::mutate(year = 2019) 


data_2020 <- data_2020 %>% 
  dplyr::mutate(year = 2020) 


data_2021 <- data_2021 %>% 
  dplyr::mutate(year = 2021) 


data_all <- data_2015 %>% 
    bind_rows(data_2016, data_2017, data_2018, data_2019, data_2020, data_2021) %>% 
    dplyr::mutate(across(where(is.numeric), round, digits = 5)) %>%
    dplyr::filter(N >5) 

data_all <- as.data.frame(data_all) %>%
  dplyr::mutate(N = plyr::round_any(data_all$N, 5)) %>% 
  dplyr::mutate(N_population = plyr::round_any(data_all$N_population, 5)) 

# values of 5 or below have already been filtered out
data_all$N[data_all$N==5] <- "6-10"

write.csv (data_all, here::here ("output/data","median_range_bmi_2015_2021.csv"))
