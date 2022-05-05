## This script will do summary plots of pre and post pandemic BMI changes
## Author: Miriam Samuel 
## Date: 5th May


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
library(lubridate)
library(skimr)
library(ggplot2)


BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories_final.feather"))



## order the age-groups for ordered plots
BMI_trajectories <- BMI_trajectories# Replicate data
BMI_trajectories$age_group_2 <- factor(BMI_trajectories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



## selected the variables for analysis
BMI_trajectories_long <- BMI_trajectories %>% 
  dplyr::select(patient_id, age_group_2, yearly_bmi_change1, yearly_bmi_change2) %>% 
  dplyr::rename(precovid = yearly_bmi_change1) %>% 
  dplyr::rename(postcovid = yearly_bmi_change2) 
  

BMi_trajectories_long_DT <- as.data.table(BMI_trajectories_long)

## reshape long in data table format

BMi_trajectories_long_DT = melt(BMi_trajectories_long_DT, 
                                id.vars = c("patient_id", "age_group_2"), 
                                measure.vars = c("precovid", 'postcovid'), 
                                variable.name = "pandemic_stage", 
                                value.name = "yearly_bmi_change")
  


  

## histograms with pre and post pandemic data


bmi_change_plot <-  ggplot( data = BMi_trajectories_long_DT, 
                               mapping = aes( x = yearly_bmi_change, color=pandemic_stage)) + 
                                  labs(title = "Rate of BMI change per year in whole population", 
                                  subtitle = "Data on BMI collected through routine primary care electronic health records between March 2015 and March 2022") + 
                                  geom_histogram() +
                                  xlim (-10, 10) 


bmi_change_plot_age <- ggplot( data = BMi_trajectories_long_DT, 
                                    mapping = aes( x = yearly_bmi_change, color=pandemic_stage)) + 
                                    labs(title = "Rate of BMI change per year by Age Group", 
                                    subtitle = "Data on BMI collected through routine primary care electronic health records between March 2015 and March 2022") + 
                                    geom_histogram() +
                                    xlim (-10, 10) +
                                    facet_wrap(~age_group_2)



## create a short summary table to check
BMI_change_summary <- BMI_trajectories_long %>% 
  ungroup() %>%
  dplyr::select(precovid, postcovid) 

BMI_change_summary <-  skimr::skim_without_charts(BMI_change_summary)


## saveoutputs
ggsave(
    plot = bmi_change_plot,
    filename = "bmi_change_plot.png", 
    path = here::here("output"),
    dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
    plot = bmi_change_plot_age,
    filename = "bmi_change_plot_age.png", 
    path = here::here("output"),
    dpi=600, width = 30, height = 30, units = "cm"
)


write.csv (BMI_change_summary, here::here ("output/data","short_bmi_change_summary.csv"))


