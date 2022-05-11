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

BMI_trajectories$age_group <- factor(BMI_trajectories$age_group,      # Reordering group factor levels
                                       levels = c("18-39", "40-65", "65-80", "80+"))



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


## filter out extreme BMI Change values.  Likely to be error entries.  Limits cover >95.5% of population

BMI_trajectory_analysis <- BMI_trajectories_long_DT %>%
  dplyr::filter(yearly_bmi_change>-6 & yearly_bmi_change<6)



## classify rate of change of bmi - pre and post pandemic

BMI_trajectory_analysis <- BMI_trajectory_analysis  %>%
  dplyr:: mutate (base_bmi_category = base_bmi) %>% 
  dplyr::mutate(precovid_bmi_category = precovid_bmi) %>% 
  dplyr::mutate(postcovid_bmi_category = postcovid_bmi)


## categorise patients based on BMI at base, precovid and postcovid. 
## done in base R for increased efficiency

BMI_trajectory_analysis$base_bmi_category[BMI_trajectory_analysis$base_bmi_category < 18.5] <- "underweight"
BMI_trajectory_analysis$base_bmi_category[BMI_trajectory_analysis$base_bmi_category >= 18.5 & BMI_trajectory_analysis$base_category <25] <- "healthy"
BMI_trajectory_analysis$base_bmi_category[BMI_trajectory_analysis$base_bmi_category >= 25 & BMI_trajectory_analysis$base_category <30] <- "overweight"
BMI_trajectory_analysis$base_bmi_category[BMI_trajectory_analysis$base_bmi_category >= 30 & BMI_trajectory_analysis$base_category <99] <- "obese"


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
  dplyr::mutate ( bmi_change_cat = cut(
    yearly_bmi_change, 
    breaks = c(-999,-0.1, 0.1, 0.3, 0.5, 999), 
    labels = c(">0.1 loss", "-0.1 to <0.1", "0.1 to <0.3", "0.3 to <0.5", "over 0.5"),
    include.lowest = TRUE))




## Create plots

imd_plot <- BMI_trajectory_analysis %>% 
  ggplot(                                                       
    mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and Index of Multiple Deprivation (IMD)",
    fill = "BMI change/year",
    x = "IMD",
    y = "Proportion") +
  facet_grid(age_group_2 ~ imd)

sex_plot <- BMI_trajectory_analysis %>% 
  ggplot(                                                       
    mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and sex",
    fill = "BMI change/year",
    x = "Sex",
    y = "Proportion") +
  facet_grid(age_group_2 ~ sex)



ethnicity_plot <- BMI_trajectory_analysis %>% 
  ggplot(                                                       
    mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and ethnicity",
    fill = "BMI change/year",
    x = "Ethnicity",
    y = "Proportion") +
  facet_grid(age_group_2 ~ ethnic_no_miss)



region_plot <- BMI_trajectory_analysis %>% 
  ggplot(                                                       
    mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and region",
    x = "Region",
    y = "Proportion",
    fill = "BMI change/year") +
  facet_grid(age_group_2 ~ region)



t2dm_plot <- BMI_trajectory_analysis %>% 
  ggplot(                                                       
    mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and comorbid Type 2 Diabetes",
    x = "Type 2 Diabetes",
    y = "Proportion",
    fill = "BMI change/year") +
  facet_grid(age_group_2 ~ diabetes_t2)



t1dm_plot <- BMI_trajectory_analysis %>% 
  ggplot(                                                       
  mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and comorbid Type 1 Diabetes",
    x = "Type 1 Diabetes",
    y = "Proportion",
    fill = "BMI change/year") +
  facet_grid(age_group_2 ~ diabetes_t1)



hypertension_plot <- BMI_trajectory_analysis %>% 
ggplot(                                                       
  mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and comorbid hypertension",
    x = "Hypertension",
    y = "Proportion",
    fill = "BMI change/year") +
  facet_grid(age_group_2 ~ hypertension)



ld_plot <- BMI_trajectory_analysis %>% 
  ggplot(                                                       
    mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and recorded learning difficulties",
    x = "Learning Difficulties",
    y = "Proportion",
    fill = "BMI change/year") +
  facet_grid(age_group_2 ~ learning_disability)




depression_plot <- BMI_trajectory_analysis %>% 
  ggplot(                                                       
    mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and comorbid depression",
    x = "Depression",
    y = "Proportion",
    fill = "BMI change/year") +
  facet_grid(age_group_2 ~ depression)



smi_plot <- BMI_trajectory_analysis %>% 
  ggplot(                                                       
    mapping = aes(x = pandemic_stage, fill = bmi_change_cat))+
  geom_bar(position = "fill", col = "black") +                    
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")+
  labs(
    title = "Rate of Body Mass index change/year amongst adults living in the UK",
    subtitle = "Stratified by age group and comorbid schizophrenia, psychosis or bipolar disorder",
    x = "Serious Mental Illness",
    y = "Proportion",
    fill = "BMI change/year") +
  facet_grid(age_group_2 ~ psychosis_schiz_bipolar)



## saveoutputs


ggsave(
  plot = depression_plot,
  filename = "category_bmi_change_depression.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = ethnicity_plot,
  filename = "category_bmi_change_ethnicity.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = hypertension_plot,
  filename = "category_bmi_change_hypertension.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = imd_plot,
  filename = "category_bmi_change_imd.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = ld_plot,
  filename = "category_bmi_change_learning_diff.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = region_plot,
  filename = "category_bmi_change_region.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)


ggsave(
  plot = sex_plot,
  filename = "category_bmi_change_sex.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)


ggsave(
  plot = smi_plot,
  filename = "category_bmi_change_serious_mental.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = t1dm_plot,
  filename = "category_bmi_change_t1dm.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = t2dm_plot,
  filename = "category_bmi_change_t2dm.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)


 
write_feather (BMI_trajectory_analysis, here::here ("output/data","BMI_trajectory_data_long.feather"))
