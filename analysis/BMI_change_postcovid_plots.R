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




## Read in files
BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories_final_demog.feather"))


## Change age group to factors and arrange 
BMI_trajectories$age_group <- factor(BMI_trajectories$age_group,      # Reordering group factor levels
                                     levels = c("18-39", "40-65", "65-80", "80+", "missing"))



BMI_trajectories$age_group_2 <- factor(BMI_trajectories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))





## Due to way BMI is extracted 141 patients with a value recorded on 1st March 2018 were counted in two time windows
## This created a time difference of 0 and therefore an infinity value with BMI change/time
## create a flag to identify when a time difference between BMI measures is recorded as '0'  Then filter these out.

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(timechange1_check = time_change1)


BMI_trajectories <- BMI_trajectories %>%            ## create a flag to identify where difference in time is 0
  dplyr::mutate(time_change_error = case_when(
    timechange1_check == 0 ~ 1, 
    timechange1_check != 0 ~ 0
  ))


BMI_trajectories %>% 
  tabyl(time_change_error)


########### IMPORTANT 
## need to add this code to other trajectory analyses
BMI_trajectories <- BMI_trajectories %>% 
  replace_na(list(time_change_error = 0))


BMI_trajectory_analysis <- BMI_trajectories %>% 
  dplyr::filter(time_change_error != 1)

####T######## IMPORTANT


## remove redundant columns
BMI_trajectory_analysis <- BMI_trajectory_analysis  %>% 
  dplyr::select(-c(type1_diabetes,          
                  type2_diabetes,         
                  unknown_diabetes,  
                  sbp_date_measured,       
                  dbp_date_measured,       
                  diabetes_type,  
                  year))

BMI_trajectory_data <- BMI_trajectory_analysis


## FILTER FOR ANALYSIS
## Filter out patients with cancer:::  disease associated with weightloss
BMI_trajectory_analysis <- BMI_trajectory_analysis %>% 
  dplyr::filter(all_cancer == FALSE) %>%                ## filter out cancer as associated with weight loss
  dplyr::filter(complete_bmi_data == "complete")      ## only keep thosse with complete data





## classify rate of change of bmi - pre and post pandemic

BMI_trajectory_analysis <- BMI_trajectory_analysis  %>%
  dplyr:: mutate (base_category = base_bmi) %>% 
  dplyr::mutate(precovid_bmi_category = precovid_bmi) %>% 
  dplyr::mutate(postcovid_bmi_category = postcovid_bmi)


## categorise patients based on BMI at base, precovid and postcovid. 
## done in base R for increased efficiency

BMI_trajectory_analysis$base_category[BMI_trajectory_analysis$base_category < 18.5] <- "underweight"
BMI_trajectory_analysis$base_category[BMI_trajectory_analysis$base_category >= 18.5 & BMI_trajectory_analysis$base_category <25] <- "healthy"
BMI_trajectory_analysis$base_category[BMI_trajectory_analysis$base_category >= 25 & BMI_trajectory_analysis$base_category <30] <- "overweight"
BMI_trajectory_analysis$base_category[BMI_trajectory_analysis$base_category >= 30 & BMI_trajectory_analysis$base_category <99] <- "obese"


BMI_trajectory_analysis$precovid_bmi_category[BMI_trajectory_analysis$precovid_bmi_category < 18.5] <- "underweight"
BMI_trajectory_analysis$precovid_bmi_category[BMI_trajectory_analysis$precovid_bmi_category >= 18.5 & BMI_trajectory_analysis$precovid_bmi_category <25] <- "healthy"
BMI_trajectory_analysis$precovid_bmi_category[BMI_trajectory_analysis$precovid_bmi_category >= 25 & BMI_trajectory_analysis$precovid_bmi_category <30] <- "overweight"
BMI_trajectory_analysis$precovid_bmi_category[BMI_trajectory_analysis$precovid_bmi_category >= 30 & BMI_trajectory_analysis$precovid_bmi_category <99] <- "obese"

BMI_trajectory_analysis$postcovid_bmi_category[BMI_trajectory_analysis$postcovid_bmi_category < 18.5] <- "underweight"
BMI_trajectory_analysis$postcovid_bmi_category[BMI_trajectory_analysis$postcovid_bmi_category >= 18.5 & BMI_trajectory_analysis$postcovid_bmi_category <25] <- "healthy"
BMI_trajectory_analysis$postcovid_bmi_category[BMI_trajectory_analysis$postcovid_bmi_category >= 25 & BMI_trajectory_analysis$postcovid_bmi_category <30] <- "overweight"
BMI_trajectory_analysis$postcovid_bmi_category[BMI_trajectory_analysis$postcovid_bmi_category >= 30 & BMI_trajectory_analysis$postcovid_bmi_category <99] <- "obese"



#### Categorise BMI change

BMI_trajectory_analysis <- BMI_trajectory_analysis %>% 
  dplyr::mutate(precovid_bmi_change = yearly_bmi_change1)

BMI_trajectory_analysis <- BMI_trajectory_analysis %>% 
dplyr::mutate ( precovid_bmi_change = cut(
      yearly_bmi_change1, 
      breaks = c(-999,-0.1, 0.1, 0.3, 0.5, 999), 
      labels = c(">0.1 loss", "-0.1 to <0.1", "0.1 to <0.3", "0.3 to <0.5", "over 0.5"),
      include.lowest = TRUE))



BMI_trajectory_analysis <- BMI_trajectory_analysis %>% 
  dplyr::mutate ( postcovid_bmi_change = cut(
    yearly_bmi_change2, 
    breaks = c(-999,-0.1, 0.1, 0.3, 0.5, 999), 
    labels = c(">0.1 loss", "-0.1 to <0.1", "0.1 to <0.3", "0.3 to <0.5", "over 0.5"),
    include.lowest = TRUE)) 



## Consider by age group
postcovid_BMI_traject_stable <- BMI_trajectory_analysis %>% 
  dplyr::filter(precovid_bmi_category != "underweight") %>% 
  dplyr::filter(precovid_bmi_change  =="-0.1 to <0.1")

stable_bmi_plot <- ggplot(data = postcovid_BMI_traject_stable, 
       mapping = aes(x=postcovid_bmi_change)) +
      geom_bar()+
      facet_wrap(~age_group_2)+
      theme(axis.text.x = element_text(angle = 90)) +
      labs(
        title = "Body Mass Index (BMI) change/year after the onset of the COVID-19 pandemic", 
        subtitle = "Amongst adults with a recorded yearly BMI change of less than +/- 0.1 kg/m^2 prior to 1st March 2020",
        x = "Rate of BMI (kg/m^2) change per year", 
        caption="BMI data captured in routinely collected GP electronic health records") 



## Consider by age group
postcovid_BMI_traject_slow <- BMI_trajectory_analysis %>% 
  dplyr::filter(precovid_bmi_category != "underweight") %>% 
  dplyr::filter(precovid_bmi_change  =="0.1 to <0.3")

slow_bmi_increase_plot <- ggplot(data = postcovid_BMI_traject_slow, 
       mapping = aes(x=postcovid_bmi_change)) +
  geom_bar()+
  facet_wrap(~age_group_2)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    title = "Body Mass Index (BMI) change/year after the onset of the COVID-19 pandemic", 
    subtitle = "Amongst adults with a slow yearly BMI increase of 0.1-0.3 kg/m^2 prior to 1st March 2020",
    x = "Rate of BMI (kg/m^2) change per year", 
    caption="BMI data captured in routinely collected GP electronic health records") 


## Consider by age group
postcovid_BMI_traject_mod <- BMI_trajectory_analysis %>% 
  dplyr::filter(precovid_bmi_category != "underweight") %>% 
  dplyr::filter(precovid_bmi_change  =="0.3 to <0.5")

mod_bmi_increase_plot <- ggplot(data = postcovid_BMI_traject_mod, 
       mapping = aes(x=postcovid_bmi_change)) +
  geom_bar()+
  facet_wrap(~age_group_2)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    title = "Body Mass Index (BMI) change/year after the onset of the COVID-19 pandemic", 
    subtitle = "Amongst adults with a moderate yearly BMI increase of  0.3 to <0.5 kg/m^2 prior to 1st March 2020",
    x = "Rate of BMI (kg/m^2) change per year", 
    caption="BMI data captured in routinely collected GP electronic health records") 



## Consider by age group
postcovid_BMI_traject_rapid <- BMI_trajectory_analysis %>% 
  dplyr::filter(precovid_bmi_category != "underweight") %>% 
  dplyr::filter(precovid_bmi_change  =="over 0.5")

rapid_bmi_increase_plot <- ggplot(data = postcovid_BMI_traject_rapid, 
       mapping = aes(x=postcovid_bmi_change)) +
  geom_bar()+
  facet_wrap(~age_group_2)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    title = "Patterns of BMI change/year after the onset of the COVID-19 pandemic", 
    subtitle = "Amongst adults with a rapid yearly BMI increase, of 0.5 kg/m^2 or more, prior to 1st March 2020",
    x = "Rate of BMI (kg/m^2) change per year", 
    caption="BMI data captured in routinely collected GP electronic health records") 

ggsave(
  plot = stable_bmi_plot,
  filename = "precovid_stable_bmi.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = slow_bmi_increase_plot,
  filename = "precovid_slow_inc_bmi.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = mod_bmi_increase_plot,
  filename = "precovid_mod_inc_bmi.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = rapid_bmi_increase_plot,
  filename = "precovid_rapid_inc_bmi.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

write_feather (BMI_trajectory_data, here::here ("output/data","BMI_trajectory_data.feather"))
write_feather (BMI_trajectory_analysis, here::here ("output/data","BMI_trajectory_data_filtered.feather"))