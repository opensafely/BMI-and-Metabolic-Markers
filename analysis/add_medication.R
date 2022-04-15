## Author:  M Samuel
##  This R script add medication data to the main data base.  Less than 14 days between extracting the two data sets.  So, although there will be patient mismatch, it is likely minimal. 


## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(broom)
library(dplyr)
library(janitor)



####################################
###################################
#####  read in files

main_2015 <- read_feather (here::here ("output/data", "input_all_2015-03-01.feather"))
main_2016 <- read_feather (here::here ("output/data", "input_all_2016-03-01.feather"))
main_2017 <- read_feather (here::here ("output/data", "input_all_2017-03-01.feather"))
main_2018 <- read_feather (here::here ("output/data", "input_all_2018-03-01.feather"))
main_2019 <- read_feather (here::here ("output/data", "input_all_2019-03-01.feather"))
main_2020 <- read_feather (here::here ("output/data", "input_all_2020-03-01.feather"))
main_2021 <- read_feather (here::here ("output/data", "input_all_2021-03-01.feather"))


dm_meds_2015 <- read.csv (here::here ("output/data", "input_dm_meds_2015-03-01.csv))
dm_meds_2016 <- read.csv (here::here ("output/data", "input_dm_meds_2016-03-01.csv))
dm_meds_2017 <- read.csv (here::here ("output/data", "input_dm_meds_2017-03-01.csv))
dm_meds_2018 <- read.csv (here::here ("output/data", "input_dm_meds_2018-03-01.csv))
dm_meds_2019 <- read.csv (here::here ("output/data", "input_dm_meds_2019-03-01.csv))
dm_meds_2020 <- read.csv (here::here ("output/data", "input_dm_meds_2020-03-01.csv))
dm_meds_2021 <- read.csv (here::here ("output/data", "input_dm_meds_2021-03-01.csv))

## join 2015 data

main_2015 <- main_2015 %>%
dplyr::left_join(dm_meds_2015)

main_2015 %>%
tabyl(diabetes_type, oad_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
  
main_2015 %>%
tabyl(diabetes_type, insulin_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


## join 2016 data

main_2016 <- main_2016 %>%
dplyr::left_join(dm_meds_2016) 

main_2016 %>%
tabyl(diabetes_type, oad_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

main_2016 %>%
tabyl(diabetes_type, insulin_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()




## join 2017 data

main_2017 <- main_2017 %>%
dplyr::left_join(dm_meds_2017)

main_2017 %>%
tabyl(diabetes_type, oad_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

main_2017 %>%
tabyl(diabetes_type, insulin_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()



## join 2018 data

main_2018 <- main_2018 %>%
dplyr::left_join(dm_meds_2018)

main_2018 %>%
tabyl(diabetes_type, oad_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

main_2018 %>%
tabyl(diabetes_type, insulin_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


## join 2019 data

main_2019 <- main_2019 %>%
dplyr::left_join(dm_meds_2019)

main_2019 %>%
tabyl(diabetes_type, oad_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

main_2019 %>%
tabyl(diabetes_type, insulin_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


## join 2020 data

main_2020 <- main_2020 %>%
dplyr::left_join(dm_meds_2020)

main_2020 %>%
tabyl(diabetes_type, oad_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

main_2020 %>%
tabyl(diabetes_type, insulin_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

## join 2021 data



main_2021 <- main_2021 %>%
dplyr::left_join(dm_meds_2021)

main_2021 %>%
tabyl(diabetes_type, oad_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

main_2021 %>%
tabyl(diabetes_type, insulin_meds) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
  
  
  
