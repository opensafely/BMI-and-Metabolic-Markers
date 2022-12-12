## M Samuel
## This R file looks at the stratified analyses of delta 
## Ethnicities are collapsed to prevent disclosure


library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(purrr)
library(broom)
library(data.table)
library(janitor)
library(skimr)
library(ggplot2)
library(gtsummary)


my_data <- read_csv (here::here ("output/data", "CC_delta_data.csv"))




my_data <- my_data %>% 
  dplyr::select(-c(ethnic_no_miss, eth_group_16))



my_data <- my_data  %>% 
  dplyr::mutate(eth_collapsed = case_when(
    eth_16_corrected ==   "White_British" ~ "white",
    eth_16_corrected ==  "White_Irish" ~ "white",
    eth_16_corrected ==  "Other_White"  ~ "white",
    eth_16_corrected == "White_Black_Carib" ~ "mixed",
    eth_16_corrected == "White_Black_African" ~ "mixed",
    eth_16_corrected == "White_Asian" ~ "mixed",
    eth_16_corrected == "Other_Mixed" ~ "mixed",
    eth_16_corrected == "Indian" ~ "south_asian",
    eth_16_corrected == "Pakistani" ~ "south_asian",
    eth_16_corrected == "Bangladeshi" ~ "south_asian",
    eth_16_corrected == "Other_Asian" ~ "chinese_other",
    eth_16_corrected == "Chinese" ~ "chinese_other",
    eth_16_corrected == "Caribbean" ~ "black",
    eth_16_corrected == "African" ~ "black",
    eth_16_corrected == "Other_Black" ~ "black",
    eth_16_corrected == "Other" ~ "chinese_other"
    ))  

print("check ethnicity collapsed correctly")
my_data %>%
  tabyl(eth_16_corrected, eth_collapsed)

my_data %>%
  tabyl(age_group_2) 

my_data <- my_data %>% 
  dplyr::mutate(age_collapsed = case_when(
    age_group_2 == "18-29" ~ "18-39", 
    age_group_2 ==  "30-39" ~ "18-39", 
    age_group_2 ==  "40-49" ~ "40-59", 
    age_group_2 == "50-59" ~ "40-59", 
    age_group_2 == "60-69" ~ "60- 79",
    age_group_2 ==  "70-79" ~ "60- 79",
  ))

print("check age collapsed correctly")
my_data %>%
  tabyl(age_group_2, age_collapsed)

## PANDEMIC DATA
pandemic_data <- my_data %>% 
 dplyr::filter(pandemic_stage == "postcovid")

print("check pandemic filter")
pandemic_data %>% 
    tabyl(pandemic_stage)



## PRE PANDEMIC DATA
prepandemic_data <- my_data %>% 
 dplyr::filter(pandemic_stage == "precovid")

print("check prepandemic filter")
prepandemic_data %>% 
    tabyl(pandemic_stage)


write_csv (pandemic_data, here::here ("output/data", "CC_stratified_analysis_delta_data_pandemic.csv"))
write_csv (prepandemic_data, here::here ("output/data", "CC_stratified_analysis_delta_data_prepandemic.csv"))

