
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


BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories_final_demog.feather"))


## Remove infinity time change values


## Due to way BMI is extracted 141 patients with a value recorded on 1st March 2018 were counted in two time windows
## This created a time difference of 0 and therefore an infinity value with BMI change/time
## create a flag to identify when a time difference between BMI measures is recorded as '0'  Then filter these out.
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(timechange1_check = time_change1)



BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(time_change_error = timechange1_check)

BMI_trajectories$time_change_error[BMI_trajectories$time_change_error == 0] <- 1
BMI_trajectories$time_change_error[BMI_trajectories$time_change_error != 0] <- 0



BMI_trajectories %>% 
  tabyl(time_change_error)


########### IMPORTANT 
## need to add this code to other trajectory analyses or NA will be filtered out
## Actually just want to filter out the infinity values
BMI_trajectories <- BMI_trajectories %>% 
  replace_na(list(time_change_error = 0))

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(time_change_error == 0)




## order the age-groups for ordered plots

BMI_trajectories$age_group_2 <- factor(BMI_trajectories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))

BMI_trajectories$age_group <- factor(BMI_trajectories$age_group,      # Reordering group factor levels
                                     levels = c("18-39", "40-65", "65-80", "80+"))


BMI_trajectories$smoking_status <- factor(BMI_trajectories$smoking_status, 
                                          levels = c('N',"S", "E", "M"))


## selected the variables for analysis


## remove redundant columns
BMI_trajectories <- BMI_trajectories  %>% 
  dplyr::select(-c(type1_diabetes,          
                   type2_diabetes,         
                   unknown_diabetes,  
                   sbp_date_measured,       
                   dbp_date_measured,       
                   diabetes_type,  
                   year, 
                   had_bmi, 
                   age_group, 
                   ethnic_no_miss, 
                   cholesterol_test,
                   insulin_meds, 
                   oad_meds,
                   sbp,
                   dbp,
                   ends_with("_date"))) %>%
  dplyr::rename(precovid_change = yearly_bmi_change1) %>% 
  dplyr::rename(postcovid_change = yearly_bmi_change2) 

colnames(BMI_trajectories)





## filter out extreme BMI Change values.  Likely to be error entries.  Limits cover >95.5% of population
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(post_covid_missing = postcovid_change) %>% 
  dplyr::mutate(post_covid_missing = replace_na(post_covid_missing, "missing"))

BMI_trajectories <- BMI_trajectories %>%
  dplyr::filter(precovid_change>-6 & precovid_change<6) %>% 
  dplyr::filter((postcovid_change>-6 & postcovid_change<6) | post_covid_missing == "missing" ) 




## develop a variable for change in BMI_trajectory

BMI_trajectories <- BMI_trajectories %>% 
  ungroup()



BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(trajectory_change = postcovid_change - precovid_change)


## Categorise BMIs at different stages
BMI_trajectories <- BMI_trajectories %>%
  dplyr:: mutate (base_bmi_category = base_bmi) %>% 
  dplyr::mutate(precovid_bmi_category = precovid_bmi) %>% 
  dplyr::mutate(postcovid_bmi_category = postcovid_bmi)



## categorise patients based on BMI at base, precovid and postcovid. 
## done in base R for increased efficiency

BMI_trajectories$base_bmi_category[BMI_trajectories$base_bmi_category < 18.5] <- "underweight"
BMI_trajectories$base_bmi_category[BMI_trajectories$base_bmi_category >= 18.5 & BMI_trajectories$base_category <25] <- "healthy"
BMI_trajectories$base_bmi_category[BMI_trajectories$base_bmi_category >= 25 & BMI_trajectories$base_category <30] <- "overweight"
BMI_trajectories$base_bmi_category[BMI_trajectories$base_bmi_category >= 30 & BMI_trajectories$base_category <99] <- "obese"


BMI_trajectories$precovid_bmi_category[BMI_trajectories$precovid_bmi_category < 18.5] <- "underweight"
BMI_trajectories$precovid_bmi_category[BMI_trajectories$precovid_bmi_category >= 18.5 & BMI_trajectories$precovid_bmi_category <25] <- "healthy"
BMI_trajectories$precovid_bmi_category[BMI_trajectories$precovid_bmi_category >= 25 & BMI_trajectories$precovid_bmi_category <30] <- "overweight"
BMI_trajectories$precovid_bmi_category[BMI_trajectories$precovid_bmi_category >= 30 & BMI_trajectories$precovid_bmi_category <99] <- "obese"

BMI_trajectories$postcovid_bmi_category[BMI_trajectories$postcovid_bmi_category < 18.5] <- "underweight"
BMI_trajectories$postcovid_bmi_category[BMI_trajectories$postcovid_bmi_category >= 18.5 & BMI_trajectories$postcovid_bmi_category <25] <- "healthy"
BMI_trajectories$postcovid_bmi_category[BMI_trajectories$postcovid_bmi_category >= 25 & BMI_trajectories$postcovid_bmi_category <30] <- "overweight"
BMI_trajectories$postcovid_bmi_category[BMI_trajectories$postcovid_bmi_category >= 30 & BMI_trajectories$postcovid_bmi_category <99] <- "obese"




## Select only variable that could influence the imputation and used in models: ## can calculate postcovid_change from trajectory change and precvid change
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::select(-c("bmi_change1", "bmi_change2",  "time_change1", "time_change2", 
                   "period1_missing", "period2_missing", "complete_bmi_data", "timechange1_check", "time_change_error","post_covid_missing", "postcovid_change"))








## Save 
BMI_trajectories


write.csv (BMI_trajectories, here::here ("output/data", "imputation_data_long.csv"))
