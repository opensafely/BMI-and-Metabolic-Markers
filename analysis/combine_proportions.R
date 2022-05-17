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
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
  dplyr::filter(n_had_bmi >5) 

had_bmi <- as.data.frame(had_bmi) %>%
  dplyr::mutate(n_had_bmi = plyr::round_any(had_bmi$n_had_bmi, 5)) %>% 
  dplyr::mutate(N = plyr::round_any(had_bmi$N, 5)) 

# values of 5 or below have already been filtered out
had_bmi$n_had_bmi[had_bmi$n_had_bmi==5] <- "6-10"







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
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
  dplyr::filter(n_obese >5) 

proportion_obese <- as.data.frame(proportion_obese) %>%
  dplyr::mutate(n_obese = plyr::round_any(proportion_obese$n_obese, 5)) %>% 
  dplyr::mutate(N = plyr::round_any(proportion_obese$N, 5)) 

# values of 5 or below have already been filtered out
proportion_obese$n_obese[proportion_obese$n_obese==5] <- "6-10"







proportion_had_sbp_2019 <- read_csv (here::here ("output/data", "proportion_had_sbp_2019.csv"))
proportion_had_sbp_2020 <- read_csv (here::here ("output/data", "proportion_had_sbp_2020.csv"))
proportion_had_sbp_2021 <- read_csv (here::here ("output/data", "proportion_had_sbp_2021.csv"))


proportion_had_sbp_2019 <- proportion_had_sbp_2019 %>% 
  dplyr::mutate(year = 2019) 


proportion_had_sbp_2020 <- proportion_had_sbp_2020 %>% 
  dplyr::mutate(year = 2020)


proportion_had_sbp_2021 <- proportion_had_sbp_2021 %>% 
  dplyr::mutate(year = 2021)


had_sbp <- proportion_had_sbp_2019 %>% 
  bind_rows(proportion_had_sbp_2020) %>%
  bind_rows( proportion_had_sbp_2021) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
  dplyr::filter(n_had_sbp >5) 

had_sbp <- as.data.frame(had_sbp) %>%
  dplyr::mutate(n_had_sbp = plyr::round_any(had_sbp$n_had_sbp, 5)) %>% 
  dplyr::mutate(N = plyr::round_any(had_sbp$N, 5)) 

# values of 5 or below have already been filtered out
had_sbp$n_had_sbp[had_sbp$n_had_sbp==5] <- "6-10"


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
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
  dplyr::filter(n_DWMP_eligible >5) 

proportion_DWMP_eligible <- as.data.frame(proportion_DWMP_eligible) %>%
  dplyr::mutate(n_DWMP_eligible = plyr::round_any(proportion_DWMP_eligible$n_DWMP_eligible, 5)) %>% 
  dplyr::mutate(N = plyr::round_any(proportion_DWMP_eligible$N, 5)) 

# values of 5 or below have already been filtered out
proportion_DWMP_eligible$n_DWMP_eligible[proportion_DWMP_eligible$n_DWMP_eligible==5] <- "6-10"






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
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
  dplyr::filter(n_DWMP_eligible >5) 

proportion_DWMP_hypertension <- as.data.frame(proportion_DWMP_hypertension) %>%
  dplyr::mutate(n_DWMP_eligible = plyr::round_any(proportion_DWMP_hypertension$n_DWMP_eligible, 5)) %>% 
  dplyr::mutate(N = plyr::round_any(proportion_DWMP_hypertension$N, 5)) 

# values of 5 or below have already been filtered out
proportion_DWMP_hypertension$n_DWMP_eligible[proportion_DWMP_hypertension$n_DWMP_eligible==5] <- "6-10"

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
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
  dplyr::filter(n_DWMP_eligible >5) 

proportion_DWMP_T2DM <- as.data.frame(proportion_DWMP_T2DM) %>%
  dplyr::mutate(n_DWMP_eligible = plyr::round_any(proportion_DWMP_T2DM$n_DWMP_eligible, 5)) %>% 
  dplyr::mutate(N = plyr::round_any(proportion_DWMP_T2DM$N, 5)) 

# values of 5 or below have already been filtered out
proportion_DWMP_T2DM$n_DWMP_eligible[proportion_DWMP_T2DM$n_DWMP_eligible==5] <- "6-10"

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
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
  dplyr::filter(n_DWMP_eligible >5) 

proportion_DWMP_T1DM <- as.data.frame(proportion_DWMP_T1DM) %>%
  dplyr::mutate(n_DWMP_eligible = plyr::round_any(proportion_DWMP_T1DM$n_DWMP_eligible, 5)) %>% 
  dplyr::mutate(N = plyr::round_any(proportion_DWMP_T1DM$N, 5)) 

# values of 5 or below have already been filtered out
proportion_DWMP_T1DM$n_DWMP_eligible[proportion_DWMP_T1DM$n_DWMP_eligible==5] <- "6-10"




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
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) %>%
  dplyr::filter(N >5) 

median_range_bmi <- as.data.frame(median_range_bmi) %>%
  dplyr::mutate(N = plyr::round_any(median_range_bmi$N, 5)) %>% 
  dplyr::mutate(N_population = plyr::round_any(median_range_bmi$N_population, 5)) 

# values of 5 or below have already been filtered out
median_range_bmi$N[median_range_bmi$N==5] <- "6-10"


write.csv (proportion_obese, here::here ("output/data","proportion_obese_all.csv"))
write.csv (had_bmi, here::here ("output/data","had_bmi_all.csv"))
write.csv (had_sbp, here::here ("output/data","had_sbp_all.csv"))
write.csv (proportion_DWMP_eligible, here::here ("output/data","DWMP_all.csv"))
write.csv (proportion_DWMP_hypertension, here::here ("output/data","DWMP_hypertension_all.csv"))
write.csv (proportion_DWMP_T2DM, here::here ("output/data","DWMP_T2DM_all.csv"))
write.csv (proportion_DWMP_T1DM, here::here ("output/data","DWMP_T1DM_all.csv"))
write.csv (median_range_bmi, here::here ("output/data","BMI_median_range_all.csv"))

