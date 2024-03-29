## Checking for ethnicity misclassification in regression models.  Due to systematic coding error. 

# Nov 16th 2022
# M Samuel

##   6 class ethnicity; can just map results as NA was coded as '6'
##   16 class ethnicity:  need to recode and rerun models as NA was dropped. However the NAs represented an ethnicity due to miscoding. 

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




check_1 <- BMI_trajectories %>%
  tabyl(eth_group_16)

check_1



check <- BMI_trajectories %>%  mutate(
  eth_group_16 = as.character(eth_group_16),
  eth_group_16 = ifelse(is.na(eth_group_16), "None", eth_group_16),
  eth_group_16 = as.factor(eth_group_16))


check_2 <- check %>%
  tabyl(eth_group_16)

check_2

write_csv (check_2, here::here ("output/data","check_ethnicity_categories.csv"))
