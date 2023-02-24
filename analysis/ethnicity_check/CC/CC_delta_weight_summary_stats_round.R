## M Samuel
## This R file defines the characteristics of the complete case delta pandemic and delta prepandemic - with data extracted in March 2022
## The code can be applied to substrata population by filtering at the first stage.
## The total in each group, numbers with BMI data, mean and median BMI data are calculated


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

data <- read_csv (here::here ("output/data", "CC_delta_weight_summary_stats.csv"))



data <- data %>% 
  dplyr::rename(
    weight_loss = ">0.1 loss", 
    stable = "-0.1 to <0.1", 
    slow_gain =  "0.1 to <0.3", 
    mod_gain =   "0.3 to <0.5", 
    rapid_gain =   "over 0.5")

data <- data %>% 
  dplyr::mutate(
  weight_loss = weight_loss*n_pop, 
  stable = stable*n_pop, 
  slow_gain = slow_gain*n_pop, 
  mod_gain = mod_gain * n_pop, 
  rapid_gain = rapid_gain * n_pop)


data <- data %>% 
  dplyr::mutate(weight_loss = plyr::round_any(data$weight_loss, 5)) %>% 
  dplyr::mutate(stable = plyr::round_any(data$stable, 5)) %>% 
  dplyr::mutate(slow_gain = plyr::round_any(data$slow_gain, 5)) %>% 
  dplyr::mutate(mod_gain = plyr::round_any(data$mod_gain, 5)) 

data <- data %>% 
  dplyr::select (-"rapid_gain")

## re-order
data <- data %>% 
  dplyr::select("stage", 
  "variable",    
  "group",       
  "n_pop",        
  "rapid",  
  "stable", 
  "weight_loss", 
  "slow_gain",   
  "mod_gain",        
  "mean_delta",  
  "sd_delta",      
  "Q1",          
  "median",      
  "Q3")  


write_csv (data, here::here ("output/data","CC_delta_weight_summary_stats_round.csv"))
