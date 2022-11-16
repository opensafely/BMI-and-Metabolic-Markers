## This script recodes the ethnic 16 cataegories based on a systematic miscoding error
## 16th Nov 2022
## Miriam Samuel



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



# BMI_trajectories <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "BMI_trajectory_data_long.feather"))


BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))

colnames(BMI_trajectories)








BMI_2 <- BMI_trajectories %>%  mutate(
  eth_group_16 = as.character(eth_group_16),
  eth_group_16 = ifelse(is.na(eth_group_16), "None", eth_group_16),
  eth_group_16 = as.factor(eth_group_16))


BMI_2 %>% 
  tabyl(eth_group_16)



BMI_2 <- BMI_2 %>% 
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
    eth_group_16 ==  "None" ~ "African")) 



BMI_2 %>% 
  tabyl(eth_16_corrected, eth_group_16)

BMI_2 %>% 
  tabyl(eth_16_corrected)


BMI_2 <- BMI_2 %>% 
  mutate ( eth_16_corrected = na_if(eth_16_corrected, "None")) 

BMI_2 %>% 
  tabyl(eth_16_corrected)

BMI_2%>%
  tabyl(eth_16_corrected, eth_group_16)











write_feather (BMI_2, here::here ("output/data","BMI_trajectory_data_long_eth_corrected.feather"))