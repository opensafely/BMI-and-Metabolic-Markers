
## Population limited to those with T2DM
## THis script redacts
# M Samuel 
# 16th Nov




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

## from: bmi_trajectory_change_summary

# data <- read_csv (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "change_90th_counts_lowbmiexc_eth_corrected_t2dm.csv"))

# models <- read_csv("~/Documents/Academic GP/Open Safely/Dummy Data/change_90th_t2dm_models_eth_corrected.csv")

data <- read_csv (here::here ("output/data", "change_90th_counts_lowbmiexc_eth_corrected_t2dm.csv"))
models <- read_csv (here::here ("output/data", "change_90th_t2dm_models_eth_corrected.csv"))

## redact < 5 values in the counts data

data <- data %>% 
  dplyr::mutate(top_decile = as.character(top_decile)) 
  
 data <- data %>%
   dplyr::mutate(top_decile = case_when(
    (variable == "smoking_status" & group == "M") ~ "<5", 
    (variable != "smoking status" | group != "M") ~ top_decile
  ))


 data <- data %>%
   dplyr::mutate(percent = case_when(
     (variable == "smoking_status" & group == "M") ~ "na", 
     (variable != "smoking status" | group != "M") ~ percent
   ))
 

## 

## redacting smoking_missing from models data
 
models_2 <- models %>%
  dplyr::filter( term != "smoking_statusM")




### Write outputs


write_csv (data, here::here ("output/data","lowbmiexc_eth_corrected_change_90th_counts_t2dm_redact.csv"))
write_csv (models_2, here::here ("output/data","lowbmiexc_eth_corrected_change_90th_models_t2dm_redact.csv"))
