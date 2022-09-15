##  This R script develops the imputed data frame

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
library(mice)


BMI_trajectories <- read_csv (here::here ("output/data", "imputation_DF_for_impute.csv"))
BMI_imp_long <- read_csv (here::here ("output/data", "imputation_dataframe.csv"))






BMI_trajectories


BMI_trajectories$imd <- factor(BMI_trajectories$imd, 
                                levels = c('1','2','3','4','5'))


BMI_imp_long$imd <- factor(BMI_imp_long$imd, 
                                levels = c('1','2','3','4','5'))


##
BMI_trajectories$smoking_status <- factor(BMI_trajectories$smoking_status, 
                                          levels = c('N',"S", "E", "M"))


BMI_imp_long$smoking_status <- factor(BMI_imp_long$smoking_status, 
                                          levels = c('N',"S", "E", "M"))


BMI_trajectories$eth_group_16 <- factor(BMI_trajectories$eth_group_16, 
                                    levels = c(
                                     "White_British",
                                     "White_Irish",
                                     "Other_White",
                                     "Indian",
                                     "Pakistani",
                                     "Bangladeshi",
                                     "Other_Asian",
                                     "Caribbean",
                                     "African",
                                     "Other_Black",
                                     "Chinese",
                                     "White_Asian",
                                     "White_Black_Carib",
                                     "White_Black_African",
                                     "Other_Mixed",
                                     "Other",
                                     "Missing"))

BMI_imp_long$eth_group_16 <- factor(BMI_imp_long$eth_group_16, 
                                    levels = c(
                                     "White_British",
                                     "White_Irish",
                                     "Other_White",
                                     "Indian",
                                     "Pakistani",
                                     "Bangladeshi",
                                     "Other_Asian",
                                     "Caribbean",
                                     "African",
                                     "Other_Black",
                                     "Chinese",
                                     "White_Asian",
                                     "White_Black_Carib",
                                     "White_Black_African",
                                     "Other_Mixed",
                                     "Other",
                                     "Missing"))


## classify pre and post covid BMI change as rapid in sample data set

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(postcovid_change = precovid_change + trajectory_change)   %>% 
  dplyr::mutate(postcovid_rapid = case_when(
    postcovid_change >=0.5 ~ TRUE, 
    postcovid_change <0.5 ~ FALSE
  )) %>% 
  dplyr::mutate(precovid_rapid = case_when(
    precovid_change >=0.5 ~ TRUE, 
    precovid_change <0.5 ~ FALSE
  ))



## classify top decile of BMI trajjectory change

BMI_trajectories$decile_change <- ntile(BMI_trajectories$trajectory_change, 10)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate (top_decile_change = case_when(
    decile_change == 10 ~ TRUE, 
    decile_change < 10 ~ FALSE
  ))






### create  mutated variables in imputed data set


BMI_imp_long <- BMI_imp_long %>% 
  dplyr::mutate(postcovid_change = precovid_change + trajectory_change)   %>% 
  dplyr::mutate(postcovid_rapid = case_when(
    postcovid_change >=0.5 ~ TRUE, 
    postcovid_change <0.5 ~ FALSE
  )) %>% 
  dplyr::mutate(precovid_rapid = case_when(
    precovid_change >=0.5 ~ TRUE, 
    precovid_change <0.5 ~ FALSE
  ))




#check_1 <- BMI_imp_long %>% 
 # group_by(.imp) %>%
  #slice_sample(n=20) %>% 
 # dplyr::select(.imp, trajectory_change) 

#check_1 <- check_1 %>% 
 # group_by(.imp) %>%
  #dplyr::mutate(decile_change = ntile(trajectory_change, 10))

## small sample confirms that ntile in group_by has calculated deciles for each imputation group seperately
  
  
BMI_imp_long <- BMI_imp_long %>%
  group_by(.imp) %>%
  dplyr::mutate(decile_change = ntile(trajectory_change, 10)) %>% 
  dplyr::mutate (top_decile_change = case_when(
    decile_change == 10 ~ TRUE, 
    decile_change < 10 ~ FALSE
  ))


write.csv (BMI_imp_long, here::here ("output/data", "imputation_dataframe_rapid_change.csv"))
write.csv (BMI_trajectories, here::here ("output/data", "imputation_sample_data_rapid_change.csv"))
