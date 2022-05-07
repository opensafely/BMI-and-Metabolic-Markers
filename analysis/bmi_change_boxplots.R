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


BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories_final_demog.feather"))


## Remove infinity time change values


## Due to way BMI is extracted 141 patients with a value recorded on 1st March 2018 were counted in two time windows
## This created a time difference of 0 and therefore an infinity value with BMI change/time
## create a flag to identify when a time difference between BMI measures is recorded as '0'  Then filter these out.
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(timechange1_check = time_change1)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(time_change_error = case_when(
    timechange1_check == 0 ~ 1, 
    timechange1_check != 0 ~ 0
  ))


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
BMI_trajectories <- BMI_trajectories# Replicate data
BMI_trajectories$age_group_2 <- factor(BMI_trajectories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



## selected the variables for analysis


## remove redundant columns
BMI_trajectory_analysis <- BMI_trajectories  %>% 
  dplyr::select(-c(type1_diabetes,          
                   type2_diabetes,         
                   unknown_diabetes,  
                   sbp_date_measured,       
                   dbp_date_measured,       
                   diabetes_type,  
                   year, 
                   had_bmi, 
                   ends_with("_date"))) %>%
  dplyr::rename(precovid = yearly_bmi_change1) %>% 
  dplyr::rename(postcovid = yearly_bmi_change2) 
  



colnames(BMI_trajectory_analysis)



BMI_trajectories_long_DT <- as.data.table(BMI_trajectory_analysis)

## reshape long in data table format

BMI_trajectories_long_DT = melt(BMI_trajectories_long_DT, 
                                measure.vars = c("precovid", 'postcovid'), 
                                variable.name = "pandemic_stage", 
                                value.name = "yearly_bmi_change")


BMI_trajectories_long_DT <- BMI_trajectories_long_DT %>%
  dplyr::filter(yearly_bmi_change>-6 & yearly_bmi_change<6)

# Change box plot colors by groups
sex_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ sex) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = "Sex") +
    theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


imd_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ imd) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = "Index of Multiple Deprivation") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ethnicity_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ ethnic_no_miss) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = "Ethnicity") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



region_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ region) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = "Region") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



hypertension_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ hypertension) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = "Comorbid Hypertension") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


t2dm_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ diabetes_t2) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = " Comorbid Type 2 Diabetes") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


t1dm_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ diabetes_t1) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = " Comorbid Type 1 Diabetes") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ld_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ learning_disability) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = "Learning Disability Recorded") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

depression_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ depression) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = " Comorbid Depression") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


smi_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ psychosis_schiz_bipolar) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = "Serious Mental Illness (schizophrenia, psychosis and bipolar disorder") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


chronic_cardiac_plot <- ggplot(BMI_trajectories_long_DT, aes(x=pandemic_stage, y=yearly_bmi_change, fill=pandemic_stage)) +
  geom_boxplot() +
  facet_grid(age_group_2 ~ chronic_cardiac) +
  labs(
    title = "Body Mass Index (BMI) change per year amongst adults living in the UK", 
    subtitle = "Data from routinely collected GP electronic Health Records", 
    y = "BMI Change Per Year",
    x = "Chronic Cardiac Disease") +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))







## saveoutputs
ggsave(
  plot = chronic_cardiac_plot,
  filename = "covid_bmi_change_cardiac.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = depression_plot,
  filename = "covid_bmi_change_depression.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = ethnicity_plot,
  filename = "covid_bmi_change_ethnicity.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = hypertension_plot,
  filename = "covid_bmi_change_hypertension.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = imd_plot,
  filename = "covid_bmi_change_imd.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = ld_plot,
  filename = "covid_bmi_change_learning_diff.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = region_plot,
  filename = "covid_bmi_change_region.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)


ggsave(
  plot = sex_plot,
  filename = "covid_bmi_change_sex.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)


ggsave(
  plot = smi_plot,
  filename = "covid_bmi_change_serious_mental.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = t1dm_plot,
  filename = "covid_bmi_change_t1dm.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = t2dm_plot,
  filename = "covid_bmi_change_t2dm.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)



