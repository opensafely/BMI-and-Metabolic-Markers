## M Samuel 
## Complete Case Analysis with corrected ethnicity data
## This script filters data for complete case analysis


## Calculate numbers of individuals with data in 2021


## M Samuel 
## Complete Case Analysis with corrected ethnicity data
## This script filters data for complete case analysis


## Calculate numbers of individuals with data in 2021


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
library(janitor)
library(skimr)
library(ggplot2)
library(gtsummary)




#) 1.  Check files used for a) total population, b) delta, c) delta_change
#) 2.  Check counts before and after complete case analysis




## Read in files


#BMI_2021 <- read_feather (here::here ( "Documents/Academic GP/Open Safely/Dummy Data","BMI_complete_median_2021.feather"))
#BMI_traj <- read_feather (here::here ( "Documents/Academic GP/Open Safely/Dummy Data","BMI_trajectory_data_long_eth_corrected.feather"))
#BMI_delta_change <- read_feather (here::here ( "Documents/Academic GP/Open Safely/Dummy Data","BMI_trajectory_models_eth_corrected_data.feather"))

  



BMI_traj <- read_feather (here::here ("output/data", "BMI_trajectory_data_long_eth_corrected.feather"))
BMI_2021 <- read_feather (here::here ("output/data", "BMI_complete_median_2021.feather"))
BMI_delta_change <- read_feather (here::here ( "output/data","BMI_trajectory_models_eth_corrected_data.feather"))




##############   BMI 2021 data
## Check demographics of study population in 2021

# recode ethnicity
BMI_2021 <- BMI_2021 %>% 
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

print("check ethnicity replace none with NA")
BMI_2021 %>%
  tabyl(eth_group_16, eth_16_corrected)

## ****  NEW CODE
BMI_2021 <- BMI_2021 %>% 
mutate(eth_16_corrected = na_if(eth_16_corrected, "None"))

print("check ethnicity replace none with NA")
BMI_2021 %>%
  tabyl(eth_group_16, eth_16_corrected)


print("check missing counts in all data BMI_2021")
BMI_2021 %>% 
  dplyr::select(age_group_2, sex, imd, eth_16_corrected, eth_group_16)  %>% 
  describe()

print("tabyl ethnicity in all data BMI_2021")
# check ethnicity - error counts
BMI_2021 %>% 
  tabyl(eth_group_16)

# check ethnicity - recoded
BMI_2021 %>% 
  tabyl(eth_16_corrected)



### Complete case - filter out missing
# filter NA for complete case data set
BMI_2021_cc <- BMI_2021 %>% 
  drop_na (imd) %>% 
  drop_na (eth_16_corrected)


print("COMPLETE CASE:check missing counts in complete case data BMI_2021")
 BMI_2021_cc %>% 
  dplyr::select(age_group_2, sex, imd, eth_16_corrected, eth_group_16)  %>% 
  describe()

 # check ethnicity - error counts
 BMI_2021_cc %>% 
   tabyl(eth_group_16)
 
 # check ethnicity - recoded
 BMI_2021_cc %>% 
   tabyl(eth_16_corrected)

############################################
############################################
##############  BMI Trajectory data
## Check counts of demographics of full data 

print("describe missing data for delta prepandemic and delta pandemic")
BMI_traj %>% 
  dplyr::select(age_group_2, sex, imd, eth_16_corrected, pandemic_stage) %>%
  describe()


## Check counts of complete case data
BMI_traj_cc <- BMI_traj %>% 
  drop_na (imd) %>% 
  drop_na (eth_16_corrected)

print("COMPLETE CASE:  describe missing data for delta prepandemic and delta pandemic in complete case")
BMI_traj_cc %>% 
  dplyr::select(age_group_2, sex, imd, eth_16_corrected, pandemic_stage) %>%
  describe()


#########################################
#########################################
### BMI delta change data

print("describe missing data for delta change analysis")
 BMI_delta_change %>% 
  dplyr::select(age_group_2, sex, imd, eth_16_corrected) %>% 
  describe()



BMI_delta_change_cc <- BMI_delta_change %>% 
  drop_na (imd) %>% 
  drop_na (eth_16_corrected)

print("COMPLETE CASE: describe missing data for delta change complete case analysis")
BMI_delta_change_cc %>% 
  dplyr::select(age_group_2, sex, imd, eth_16_corrected) %>% 
  describe()



write_csv (BMI_2021_cc, here::here ("output/data","CC_study_population_data.csv"))
write_csv (BMI_delta_change_cc, here::here ("output/data","CC_delta_change_data.csv"))
write_csv (BMI_traj_cc, here::here ("output/data","CC_delta_data.csv"))
