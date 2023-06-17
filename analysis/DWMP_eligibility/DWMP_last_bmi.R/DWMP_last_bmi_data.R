
## Author:  Miriam Samuel

## calculate median BMI predictions based on most recent BMI in 5 years
## create data set



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
library(janitor)
library(lubridate)
library(skimr)

## Read in files  >>> Change PATH!!



BMI_all <- read_feather (here::here ("output/data", "all_bmi_long.feather"))

#BMI_all <- read_feather (here::here ("/Users/miriamsamuel/Documents/Academic GP/Open Safely/Dummy Data", "all_bmi_long.feather"))


data <- BMI_all


## drop rows with missing BMI Data
data <- data %>% 
  drop_na(monthly_bmi)




### Filter out values before 2017

data <- data %>% 
  dplyr::filter(year > 2016)



##  Group by patient and create data set with most recent value
data <- data %>% 
  dplyr::group_by(patient_id)

data %>%
  tabyl(year)

data_2 <- data %>% arrange(desc(year), .by_group = TRUE) %>% 
  slice_head()

data_2 %>% tabyl(year)
  
### Correct ethnicity


data_2 <- data_2 %>% 
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




data_2 %>% tabyl(eth_group_16, eth_16_corrected) 
## 
  

write_feather (data_2, here::here ("output/data","DWMP_most_recent_BMI.feather")) 
  