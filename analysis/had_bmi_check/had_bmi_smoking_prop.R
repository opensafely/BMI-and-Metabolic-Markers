#### Author: M Samuel
#### Date:  Oct 2022
####  This script looks at the odds of having a BMI in different groups


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

BMI_median_2 <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

BMI_complete_categories <- BMI_median_2 %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2019"|year == "2020"| year == "2021" ) 


BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(smoking_status= 'M'))


BMI_DT <- as.data.table(BMI_complete_categories)
## Analysis by smoking_status

#1. look at proportion who have had BMI by year,  for each group of smoking_status

prop_all <- BMI_DT[, tabyl(.SD, year, had_bmi), by=smoking_status] %>% 
  dplyr::mutate(total = .[[3]] + .[[4]]) %>% 
  dplyr::mutate(proportion = .[[4]]/.[[5]])  %>% 
  dplyr::mutate(variable = "smoking_status") %>% 
  dplyr::rename(group = smoking_status)


#2. look at chi squared test of odds of difference between groups

test_all <- BMI_DT[, .(chisq.test(table(.SD$year, .SD$had_bmi))), by=smoking_status]  %>% 
  dplyr::mutate(variable="smoking_status") %>% 
  dplyr::rename(group=smoking_status)


prop_all <- prop_all %>% 
 dplyr::rename(had_bmi = "TRUE") %>% 
  dplyr::rename(no_bmi = "FALSE")  %>% 
  dplyr::rename(prop = proportion)

prop_all <- prop_all %>% 
  dplyr::mutate (stand_err = (sqrt(prop*(1-prop)/total)))


prop_all <- prop_all %>% 
  dplyr::mutate(had_bmi = plyr::round_any(prop_all$had_bmi, 5))  %>% 
  dplyr::mutate(no_bmi = plyr::round_any(prop_all$no_bmi, 5))  %>% 
  dplyr::mutate(total = plyr::round_any(prop_all$total, 5)) %>% 
  dplyr::mutate((across(where(is.numeric), round, 5))) 






## save outputs
prop_all
write.csv (prop_all, here::here ("output/data","proportion_had_bmi_2019_2021_smoking.csv"))
write.csv (test_all, here::here ("output/data","proportion_had_bmi_2019_2021_chisquare_smoking.csv"))



