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


meds_data <- read_feather (here::here ("output/data", "complete_meds_2021.feather"))

median_data <- read_feather (here::here ("output/data", "BMI_complete_median_2021.feather"))

colnames(BMI_data)

BMI_data %>% 
    janitor::tabyl(ethnicity_16) %>% 
    dplyr::mutate (group = "eth_16")

BMI_data %>% 
 janitor::tabyl(ethnicity) %>%
 dplyr::mutate (group = "eth_6")

eth_16 <- BMI_data %>% 
    janitor::tabyl(ethnicity_16) %>% 
    dplyr::mutate (group = "eth_16") %>% 
    dplyr::mutate (data = "all_data") 

eth_6 <- BMI_data %>% 
 janitor::tabyl(ethnicity) %>%
 dplyr::mutate (group = "eth_6") %>% 
    dplyr::mutate (data = "all_data") 

print("meds")

meds_data %>% 
    janitor::tabyl(ethnicity_16) %>% 
    dplyr::mutate (group = "eth_16")

meds_data %>% 
 janitor::tabyl(ethnicity) %>%
 dplyr::mutate (group = "eth_6") 

meds_eth_16 <- meds_data %>% 
    janitor::tabyl(ethnicity_16) %>% 
    dplyr::mutate (group = "eth_16")%>% 
    dplyr::mutate (data = "meds_join")

meds_eth_6 <- meds_data %>% 
 janitor::tabyl(ethnicity) %>%
 dplyr::mutate (group = "eth_6")%>% 
 dplyr::mutate (data = "meds_join") 

print("median")

median_data %>% 
 janitor::tabyl(ethnic_no_miss) %>%
 dplyr::mutate (group = "eth_6")%>% 
 dplyr::mutate (data = "median") 


median_data %>% 
 janitor::tabyl(eth_group_16) %>%
 dplyr::mutate (group = "eth_16")%>% 
 dplyr::mutate (data = "median")

median_eth_6 <- median_data %>% 
 janitor::tabyl(ethnic_no_miss) %>%
 dplyr::mutate (group = "eth_6")%>% 
 dplyr::mutate (data = "median") 


median_eth_16 <- median_data %>% 
 janitor::tabyl(eth_group_16) %>%
 dplyr::mutate (group = "eth_16")%>% 
 dplyr::mutate (data = "median") 


 eth_16 <- bind_rows(eth_6, eth_16, meds_eth_6, meds_eth_16, median_eth_6, median_eth_16)

eth_16

eth_16 <-  eth_16 %>%
 dplyr::select(data, group, ethnicity, ethnicity_16, n, percent, ethnic_no_miss, eth_group_16)




write_csv (eth_16, here::here ("output/data","ethnicity_checks.csv"))