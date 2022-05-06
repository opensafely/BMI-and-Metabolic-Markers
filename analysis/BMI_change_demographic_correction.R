## M Samuel 
## 5th May 2022
## This script ensures that the demographic data used is the most recent recorded for the patient (e.g. age)


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
library(gt)  
library(webshot)
# webshot::install_phantomjs()



## Read in files
BMI_all <- read_feather (here::here ("output/feather", "all_bmi_long.feather"))
BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories_final.feather"))



### Remove demographic data from change data set
BMI_change <- BMI_trajectories  %>% 
  dplyr::select("patient_id", 
                "base_bmi", 
                "base_bmi_date", 
                "precovid_bmi", 
                "precovid_bmi_date", 
                "postcovid_bmi", 
                "postcovid_bmi_date",      
                "bmi_change1",             
                "bmi_change2" ,         
                "time_change1",            
                "yearly_bmi_change1",     
                "time_change2",
                "yearly_bmi_change2",
                "period1_missing",         
                "period2_missing",
                "complete_bmi_data")    


## Remove BMI change data from demographic data set

BMI_change_demog <- BMI_all %>% 
  dplyr::select(-(monthly_bmi), -(bmi_date))


### Group by patient and slice head, but first sort by year!
BMI_change_demog <- BMI_change_demog %>% 
  dplyr::group_by(patient_id) %>% 
  dplyr::arrange(desc(year), .by_group = TRUE) %>% 
  dplyr::slice_head()


## Join change data to demographic data

BMI_trajectories_final_demog <- BMI_change_demog %>% 
  dplyr::left_join(BMI_change)

BMI_trajectories_final_demog$age_group_2 <- factor(BMI_trajectories_final_demog$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))
# colnames(BMI_trajectories_final_demog)

BMI_traj_demog_table <- BMI_trajectories_final_demog %>% 
  dplyr::ungroup() %>%
  dplyr::select("sex",
                "age_group_2", 
                "region",                
                "imd",
                "hypertension",
                "diabetes_t1", 
                "diabetes_t2",
                "learning_disability", 
                "depression",               
                "psychosis_schiz_bipolar", 
                "dementia", 
                "asthma",
                "COPD",
                "stroke_and_TIA",          
                "chronic_cardiac",                 
                "all_cancer",                           
                "smoking_status", 
                "ethnic_no_miss",         
                "eth_group_16",           
                "complete_bmi_data")

BMI_traj_demog_table <- BMI_traj_demog_table %>%
  tidyr::replace_na(list(complete_bmi_data = "incomplete")) 


BMI_demog_table <- BMI_traj_demog_table %>% 
  tbl_summary(
       by= complete_bmi_data,                                     # stratify entire table by outcome
      statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),   # stats and format for categorical columns
      digits = all_continuous() ~ 1,                              # rounding for continuous columns
      type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
      label  = list(                                            # display labels for column names
        sex ~ "Sex",
        age_group_2 ~ "Age Group",
        region ~ "Region",         
        imd ~ "Index of Multiple Deprivation",
        ethnic_no_miss ~ "Ethnicity (5 Categories)",  
        eth_group_16 ~ "Ethnicity (16 Categories)",
        hypertension ~ "Hypertension",
        diabetes_t1 ~ "Type 1 Diabetes",
        diabetes_t2 ~ "Type 2 Diabetes",
        learning_disability ~ "Learning Disability",
        depression ~ "Depression", 
        psychosis_schiz_bipolar ~ "Psychosis, Schizophrenia and Bipolar",
        dementia ~ "Dementia", 
        asthma ~ "Asthma",
        COPD ~ "COPD",
        stroke_and_TIA ~ "Stroke and TIA",     
        chronic_cardiac ~ "Chronic Cardiac Disease",                
        all_cancer ~ "Cancer",                      
        smoking_status ~ "Smoking Status"),
      missing_text = "Missing") 

BMI_demog_table <- BMI_demog_table %>%
  as_gt() %>%  
  tab_header(
    title = "Demographic characteristics of patients with and without complete BMI trajectory data"
  )

summary_extract <- BMI_demog_table
  extract_summary()
  
summary_extract

BMI_demog_table %>% gtsave("BMI_traj_demog.png", expand = 10, path=here::here("output/data"))        

BMI_demog_table %>% gtsave("BMI_traj_demog.html", path=here::here("output/data"))

write_feather (BMI_trajectories_final_demog, here::here ("output/data","BMI_trajectories_final_demog.feather"))
