
## Redact T1DM smoking == missing as counts <5

## load libraries
## Load libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)


mean_data <- read_csv (here::here ("output/data", "mean_bmi_traj_change_cancerandlowbmi_removed_t1dm.csv"))

univariate <- read_csv (here::here ("output/data", "univariate_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))

sex <- read_csv (here::here ("output/data", "sex_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))
age <- read_csv (here::here ("output/data", "age_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))
ethnicity <- read_csv (here::here ("output/data", "ethnic_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))
imd <- read_csv (here::here ("output/data", "imd_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))


sexage <- read_csv (here::here ("output/data", "sexage_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))
sexageimd <- read_csv (here::here ("output/data", "sexageimd_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))
sexageeth <- read_csv (here::here ("output/data", "sexageethnic_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))
sexageethimd <- read_csv (here::here ("output/data", "sexageimdethnic_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM.csv"))


mean_data <- mean_data %>% 
dplyr::filter(group != "M")

univariate <- univariate %>% 
dplyr::filter(term != "smoking_statusM")


sex <- sex %>% 
dplyr::filter(term != "smoking_statusM")

age <- age %>% 
dplyr::filter(term != "smoking_statusM")

imd <- imd %>% 
dplyr::filter(term != "smoking_statusM")

sexage <- sexage %>% 
dplyr::filter(term != "smoking_statusM")

sexageimd <- sexageimd %>% 
dplyr::filter(term != "smoking_statusM")

sexageeth <- sexageeth %>% 
dplyr::filter(term != "smoking_statusM")

sexageethimd <- sexageethimd %>% 
dplyr::filter(term != "smoking_statusM")

ethnicity <- ethnicity %>% 
dplyr::filter(term != "smoking_statusM")


write.csv(ethnicity, here::here ("output/data","ethnic_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM_redact.csv")) 


write.csv(univariate, here::here ("output/data","univariate_lowbmi_excluded_bmi_trajectory_change_T1DM_redact.csv")) 

write.csv(sex, here::here ("output/data","sex_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM_redact.csv")) 

write.csv(sexageethimd, here::here ("output/data","sexageimdethnic_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM_redact.csv")) 

write.csv(sexageeth, here::here ("output/data","sexageethnic_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM_redact.csv")) 

write.csv(sexageimd, here::here ("output/data","sexageimd_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM_redact.csv")) 

write.csv(sexage, here::here ("output/data","sexage_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM_redact.csv")) 

write.csv(imd, here::here ("output/data","imd_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM_redact.csv")) 

write.csv(age, here::here ("output/data","age_adjusted_lowbmi_excluded_bmi_trajectory_change_T1DM_redact.csv")) 

write.csv(mean_data, here::here ("output/data","mean_bmi_traj_change_cancerandlowbmi_removed_T1DM_redact.csv")) 









 




























