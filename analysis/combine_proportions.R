## M Samuel 
## 5th May 2022
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
library(lubridate)
library(skimr)
library(ggplot2)
library(gtsummary)

# webshot::install_phantomjs()



## Had BMI
had_bmi_2019 <- read_csv (here::here ("output/data", "proportion_had_bmi_2019.csv"))
had_bmi_2020 <- read_csv (here::here ("output/data", "proportion_had_bmi_2020.csv"))
had_bmi_2021 <- read_csv (here::here ("output/data", "proportion_had_bmi_2021.csv"))


had_bmi_2019 <- had_bmi_2019 %>% 
  dplyr::mutate(year = 2019) 


had_bmi_2020 <- had_bmi_2020 %>% 
  dplyr::mutate(year = 2020)


had_bmi_2021 <- had_bmi_2021 %>% 
  dplyr::mutate(year = 2021)


had_bmi <- had_bmi_2019 %>% 
  bind_rows(had_bmi_2020) %>%
  bind_rows( had_bmi_2021) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) 


proportion_obese_2019 <- read_csv (here::here ("output/data", "proportion_obese_2019.csv"))
proportion_obese_2020 <- read_csv (here::here ("output/data", "proportion_obese_2020.csv"))
proportion_obese_2021 <- read_csv (here::here ("output/data", "proportion_obese_2021.csv"))




### Proportion Obese
proportion_obese_2019 <- proportion_obese_2019 %>% 
  dplyr::mutate(year = 2019) 


proportion_obese_2020 <- proportion_obese_2020 %>% 
  dplyr::mutate(year = 2020)


proportion_obese_2021 <- proportion_obese_2021 %>% 
  dplyr::mutate(year = 2021)


proportion_obese <- proportion_obese_2019 %>% 
  bind_rows(proportion_obese_2020) %>%
  bind_rows( proportion_obese_2021) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2))



proportion_had_sbp_2019 <- read_csv (here::here ("output/data", "proportion_had_sbp_2019.csv"))
proportion_had_sbp_2020 <- read_csv (here::here ("output/data", "proportion_had_sbp_2020.csv"))
proportion_had_sbp_2021 <- read_csv (here::here ("output/data", "proportion_had_sbp_2021.csv"))


proportion_had_sbp_2019 <- proportion_had_sbp_2019 %>% 
  dplyr::mutate(year = 2019) 


proportion_had_sbp_2020 <- proportion_had_sbp_2020 %>% 
  dplyr::mutate(year = 2020)


proportion_had_sbp_2021 <- proportion_had_sbp_2021 %>% 
  dplyr::mutate(year = 2021)


proportion_had_sbp <- proportion_had_sbp_2019 %>% 
  bind_rows(proportion_had_sbp_2020) %>%
  bind_rows( proportion_had_sbp_2021) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) 


## DWMP eligible

proportion_DWMP_eligible_2019 <- read_csv (here::here ("output/data", "proportion_DWMP_eligible_2019.csv"))
proportion_DWMP_eligible_2020 <- read_csv (here::here ("output/data", "proportion_DWMP_eligible_2020.csv"))
proportion_DWMP_eligible_2021 <- read_csv (here::here ("output/data", "proportion_DWMP_eligible_2021.csv"))


proportion_DWMP_eligible_2019 <- proportion_DWMP_eligible_2019 %>% 
  dplyr::mutate(year = 2019) 


proportion_DWMP_eligible_2020 <- proportion_DWMP_eligible_2020 %>% 
  dplyr::mutate(year = 2020)


proportion_DWMP_eligible_2021 <- proportion_DWMP_eligible_2021 %>% 
  dplyr::mutate(year = 2021)


proportion_DWMP_eligible <- proportion_DWMP_eligible_2019 %>% 
  bind_rows(proportion_DWMP_eligible_2020) %>%
  bind_rows( proportion_DWMP_eligible_2021) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) 


## DWMP hypertension
proportion_DWMP_hypertension_2019 <- read_csv (here::here ("output/data", "proportion_DWMP_hypertension_2019.csv"))
proportion_DWMP_hypertension_2020 <- read_csv (here::here ("output/data", "proportion_DWMP_hypertension_2020.csv"))
proportion_DWMP_hypertension_2021 <- read_csv (here::here ("output/data", "proportion_DWMP_hypertension_2021.csv"))


proportion_DWMP_hypertension_2019 <- proportion_DWMP_hypertension_2019 %>% 
  dplyr::mutate(year = 2019) 


proportion_DWMP_hypertension_2020 <- proportion_DWMP_hypertension_2020 %>% 
  dplyr::mutate(year = 2020)


proportion_DWMP_hypertension_2021 <- proportion_DWMP_hypertension_2021 %>% 
  dplyr::mutate(year = 2021)


proportion_DWMP_hypertension <- proportion_DWMP_hypertension_2019 %>% 
  bind_rows(proportion_DWMP_hypertension_2020) %>%
  bind_rows( proportion_DWMP_hypertension_2021) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) 


## DWMP T2DM
proportion_DWMP_T2DM_2019 <- read_csv (here::here ("output/data", "proportion_DWMP_T2DM_2019.csv"))
proportion_DWMP_T2DM_2020 <- read_csv (here::here ("output/data", "proportion_DWMP_T2DM_2020.csv"))
proportion_DWMP_T2DM_2021 <- read_csv (here::here ("output/data", "proportion_DWMP_T2DM_2021.csv"))


proportion_DWMP_T2DM_2019 <- proportion_DWMP_T2DM_2019 %>% 
  dplyr::mutate(year = 2019) 


proportion_DWMP_T2DM_2020 <- proportion_DWMP_T2DM_2020 %>% 
  dplyr::mutate(year = 2020)


proportion_DWMP_T2DM_2021 <- proportion_DWMP_T2DM_2021 %>% 
  dplyr::mutate(year = 2021)


proportion_DWMP_T2DM <- proportion_DWMP_T2DM_2019 %>% 
  bind_rows(proportion_DWMP_T2DM_2020) %>%
  bind_rows( proportion_DWMP_T2DM_2021) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) 


## T1DM DWMP

proportion_DWMP_T1DM_2019 <- read_csv (here::here ("output/data", "proportion_DWMP_T1DM_2019.csv"))
proportion_DWMP_T1DM_2020 <- read_csv (here::here ("output/data", "proportion_DWMP_T1DM_2020.csv"))
proportion_DWMP_T1DM_2021 <- read_csv (here::here ("output/data", "proportion_DWMP_T1DM_2021.csv"))


proportion_DWMP_T1DM_2019 <- proportion_DWMP_T1DM_2019 %>% 
  dplyr::mutate(year = 2019) 


proportion_DWMP_T1DM_2020 <- proportion_DWMP_T1DM_2020 %>% 
  dplyr::mutate(year = 2020)


proportion_DWMP_T1DM_2021 <- proportion_DWMP_T1DM_2021 %>% 
  dplyr::mutate(year = 2021)


proportion_DWMP_T1DM <- proportion_DWMP_T1DM_2019 %>% 
  bind_rows(proportion_DWMP_T1DM_2020) %>%
  bind_rows( proportion_DWMP_T1DM_2021) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) 


## Median range BMI
median_range_bmi_2019 <- read_csv (here::here ("output/data", "median_range_bmi_2019.csv"))
median_range_bmi_2020 <- read_csv (here::here ("output/data", "median_range_bmi_2020.csv"))
median_range_bmi_2021 <- read_csv (here::here ("output/data", "median_range_bmi_2021.csv"))


median_range_bmi_2019 <- median_range_bmi_2019 %>% 
  dplyr::mutate(year = 2019) 


median_range_bmi_2020 <- median_range_bmi_2020 %>% 
  dplyr::mutate(year = 2020)


median_range_bmi_2021 <- median_range_bmi_2021 %>% 
  dplyr::mutate(year = 2021)


median_range_bmi <- median_range_bmi_2019 %>% 
  bind_rows(median_range_bmi_2020) %>%
  bind_rows( median_range_bmi_2021) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) 



write.csv (proportion_obese, here::here ("output/data","proportion_obese_all.csv"))
write.csv (had_bmi, here::here ("output/data","had_bmi_all.csv"))
write.csv (proportion_had_sbp, here::here ("output/data","had_sbp_all.csv"))
write.csv (proportion_DWMP_eligible, here::here ("output/data","DWMP_all.csv"))
write.csv (proportion_DWMP_hypertension, here::here ("output/data","DWMP_hypertension_all.csv"))
write.csv (proportion_DWMP_T2DM, here::here ("output/data","DWMP_T2DM_all.csv"))
write.csv (proportion_DWMP_T1DM, here::here ("output/data","DWMP_T1DM_all.csv"))
write.csv (median_range_bmi, here::here ("output/data","BMI_median_range_all.csv"))

