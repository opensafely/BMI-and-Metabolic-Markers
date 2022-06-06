
## Redact SMI smoking == missing as counts <5

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





univariate <- read_csv (here::here ("output/data", "SMI_lowbmi_excluded_bmi_trajectory_change_univariate.csv"))

sex <- read_csv (here::here ("output/data", "SMI_lowbmi_excluded_bmi_trajectory_change_sex_adj.csv"))
age <- read_csv (here::here ("output/data", "SMI_lowbmi_excluded_bmi_trajectory_change_age_adj.csv"))
ethnicity <- read_csv (here::here ("output/data", "SMI_lowbmi_excluded_bmi_trajectory_change_ethnicity_adj.csv"))
imd <- read_csv (here::here ("output/data", "SMI_lowbmi_excluded_bmi_trajectory_change_imd_adj.csv"))


sexage <- read_csv (here::here ("output/data", "SMI_lowbmi_excluded_bmi_trajectory_change_sexage_adj.csv"))
sexageimd <- read_csv (here::here ("output/data", "SMI_lowbmi_excluded_bmi_trajectory_change_sexageimd_adj.csv"))
sexageeth <- read_csv (here::here ("output/data", "SMI_lowbmi_excluded_bmi_trajectory_change_sexageeth_adj.csv"))
sexageethimd <- read_csv (here::here ("output/data", "SMI_lowbmi_excluded_bmi_trajectory_change_sexageethimd_adj.csv"))

mean_data <- read_csv (here::here ("output/data", "mean_bmi_traj_change_psychosis_schiz_bipolar.csv"))



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


write.csv(ethnicity, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_ethnicity_adj_redact.csv")) 


write.csv(univariate, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_univariate_redact.csv")) 

write.csv(sex, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sex_adj_redact.csv")) 

write.csv(sexageethimd, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sexageethimd_adj_redact.csv")) 

write.csv(sexageeth, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sexageeth_adj_redact.csv")) 

write.csv(sexageimd, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sexageimd_adj_redact.csv")) 

write.csv(sexage, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_sexage_adj_redact.csv")) 

write.csv(imd, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_imd_adj_redact.csv")) 

write.csv(age, here::here ("output/data","SMI_lowbmi_excluded_bmi_trajectory_change_age_adj_redact.csv")) 

write.csv(mean_data, here::here ("output/data","mean_bmi_traj_change_psychosis_schiz_bipolar_redact.csv")) 



