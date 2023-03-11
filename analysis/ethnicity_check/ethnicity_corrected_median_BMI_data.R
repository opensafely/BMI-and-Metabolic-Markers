#### Author: M Samuel
#### Date: March 2023
####  This script corrects the ethnicity of the median BMI dataset.  Recoding is for the systematic miscoding previously identified. 
####  Correct coding is required to identify patients who are eligible for DWMP intervention as BMI cut offs are ethnicity based. 

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

BMI_data <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

BMI_data <- BMI_data %>% 
  dplyr::mutate(eth_16_corrected = case_when(
    eth_group_16 == "White_British" ~ "White_British",
    eth_group_16 == "White_Irish" ~ "None",
    eth_group_16 == "Other_White" ~  "White_Black_Carib",
    eth_group_16 ==  "White_Black_Carib" ~ "Other_White",
    eth_group_16 ==  "White_Black_African" ~ "Pakistani",
    eth_group_16 ==  "White_Asian" ~ "Other_Black",
    eth_group_16 ==  "Other_Mixed" ~ "Indian",
    eth_group_16 ==  "Indian" ~ "White_Asian",
    eth_group_16 ==  "Pakistani" ~ "Bangladeshi",
    eth_group_16 == "Bangladeshi" ~ "Caribbean",
    eth_group_16 == "Other_Asian" ~ "Other_Asian",
    eth_group_16 == "Caribbean" ~ "Other_Mixed",
    eth_group_16 == "African" ~ "Chinese",
    eth_group_16 == "Other_Black" ~ "Other",
    eth_group_16 ==  "Chinese" ~ "White_Irish",
    eth_group_16 == "Other" ~ "White_Black_African",
    eth_group_16 ==  "Missing" ~ "African")) 


BMI_data_2 <- BMI_data %>% 
    tabyl(eth_16_corrected, eth_group_16)

write_csv (BMI_data, here::here ("output/data","BMI_complete_median_eth_corrected.csv"))
write_csv (BMI_data_2, here::here ("output/data","BMI_complete_median_eth_checks.csv"))