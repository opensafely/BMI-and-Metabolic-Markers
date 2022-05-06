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
BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_data_filtered.feather"))

BMI_trajectories_change <- BMI_trajectories %>% 
  dplyr::mutate(trajectory_change = (yearly_bmi_change1 - yearly_bmi_change2))




 age_plot <- ggplot(BMI_trajectories_change, 
       aes(x=age_group_2, y=yearly_bmi_change2)) +
       geom_violin() +
        theme(axis.text.x = element_text(angle = 90))+
      stat_summary(fun=mean, geom="point", size=1, color="red") +
      ylim(-10, 10) +
  labs(
    title = "Distribution of BMI* change/year amongst adults living in the UK", 
    subtitle = "BMI data from routinely collected GP electronic health recordis",
    x = "Age Group", 
    y = "BMI change/year",
    caption="*BMI: Body Mass Index")



imd_plot <- ggplot(BMI_trajectories_change, 
       aes(x=imd, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
  labs(
    title = "Distribution of BMI change/year amongst adults living in the UK", 
    subtitle = "Analyses stratified by Age Group",
    x = "Index of Multiple Deprivation", 
    y = "BMI change/year",
    caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")


ethnic_plot <- ggplot(BMI_trajectories_change, 
       aes(x=ethnic_no_miss, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
  labs(
    title = "Distribution of BMI change/year amongst adults living in the UK", 
    subtitle = "Analyses stratified by Age Group",
    x = "Ethnicity", 
    y = "BMI change/year",
    caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")


precovid_bmi_plot <- ggplot(BMI_trajectories_change, 
       aes(x=precovid_bmi_category, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
labs(
  title = "Distribution of BMI change/year amongst adults living in the UK", 
  subtitle = "Analyses stratified by Age Group",
  x = "BMI category recorded prior to 1st March 2020", 
  y = "BMI change/year",
  caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")



precovid_traj_plot <- ggplot(BMI_trajectories_change, 
       aes(x=precovid_bmi_change, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
labs(
  title = "Distribution of BMI change/year amongst adults living in the UK", 
  subtitle = "Analyses stratified by Age Group",
  x = "BMI change/year recorded prior to 1st March 2020", 
  y = "BMI change/year",
  caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")



region_plot <- ggplot(BMI_trajectories_change, 
       aes(x=region, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
labs(
  title = "Distribution of BMI change/year amongst adults living in the UK", 
  subtitle = "Analyses stratified by Age Group",
  x = "Region", 
  y = "BMI change/year",
  caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")


learning_disability_plot <- ggplot(BMI_trajectories_change, 
       aes(x=learning_disability, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
labs(
  title = "Distribution of BMI change/year amongst adults living in the UK", 
  subtitle = "Analyses stratified by Age Group",
  x = "Comorbid Learning Difficulties", 
  y = "BMI change/year",
  caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")


depression_plot <- ggplot(BMI_trajectories_change, 
       aes(x=depression, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
labs(
  title = "Distribution of BMI change/year amongst adults living in the UK", 
  subtitle = "Analyses stratified by Age Group",
  x = "Comorbid Depression", 
  y = "BMI change/year",
  caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")


diabetes2_plot <- ggplot(BMI_trajectories_change, 
       aes(x=diabetes_t2, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) + 
  labs(
    title = "Distribution of BMI change/year amongst adults living in the UK", 
    subtitle = "Analyses stratified by Age Group",
    x = "Comorbid Type 2 Diabetes Mellitus", 
    y = "BMI change/year",
    caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")




diabetes1_plot <- ggplot(BMI_trajectories_change, 
       aes(x=diabetes_t1, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
  labs(
    title = "Distribution of BMI change/year amongst adults living in the UK", 
    subtitle = "Analyses stratified by Age Group",
    x = "Comorbid Type 1 Diabetes Mellitus", 
    y = "BMI change/year",
    caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")




serious_mental_plot <- ggplot(BMI_trajectories_change, 
       aes(x=psychosis_schiz_bipolar, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
  labs(
    title = "Distribution of BMI change/year amongst adults living in the UK", 
    subtitle = "Analyses stratified by Age Group",
    x = "Comorbid Serious Mental Illness", 
    y = "BMI change/year",
    caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")



hypertension_plot <- ggplot(BMI_trajectories_change, 
       aes(x=hypertension, y=yearly_bmi_change2)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom="point", size=1, color="red") +
  ylim(-10, 10) +
  facet_wrap(~age_group_2) +
  labs(
    title = "Distribution of BMI change/year amongst adults living in the UK", 
    subtitle = "Analyses stratified by Age Group",
    x = "Comorbid Hypertension", 
    y = "BMI change/year",
    caption="Body Mass Index (BMI) data from routinely collected GP electronic health records")

### save plots

ggsave(
  plot = age_plot,
  filename = "age_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)
 
ggsave(
  plot = depression_plot,
  filename = "depression_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
) 

ggsave(
  plot = diabetes1_plot,
  filename = "diabetes1_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = diabetes2_plot,
  filename = "diabetes2_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = ethnic_plot,
  filename = "ethnic_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = hypertension_plot,
  filename = "hypertension_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = imd_plot,
  filename = "imd_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)


ggsave(
  plot = learning_disability_plot,
  filename = "LD_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = precovid_bmi_plot,
  filename = "bmi_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = precovid_traj_plot,
  filename = "precovid_traj_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = region_plot,
  filename = "region_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
  plot = serious_mental_plot,
  filename = "SMI_postcovid_trajetory.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)





