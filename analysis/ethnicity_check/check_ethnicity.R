## To check ethnicity data based on values derived


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

print("a")

BMI_data <- read_feather (here::here ("output/data", "input_all_2021-03-01.feather"))

colnames(BMI_data)

eth_16 <- BMI_data %>% 
    tabyl(ethnicity_16) %>% 
    dplyr::mutate (group = "eth_16")

eth_6 <- BMI_data %>% 
 tabyl(ethnicity) %>%
 dplyr::mutate (group = "eth_6")

 eth_16 <- bind_rows(eth_6, eth_16)

 eth_16 <-  eth_16 %>%
 dplyr::select(group, ethnicity, ethnicity_16, n, percent, valid_percent)




write_csv (eth_16, here::here ("output/data","ethnicity_checks.csv"))