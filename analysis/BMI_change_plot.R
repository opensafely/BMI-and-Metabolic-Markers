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
BMI_trajectories_long <- BMI_trajectories %>% 
  dplyr::select(patient_id, age_group_2, yearly_bmi_change1, yearly_bmi_change2) %>% 
  dplyr::rename(precovid = yearly_bmi_change1) %>% 
  dplyr::rename(postcovid = yearly_bmi_change2) 


BMI_trajectories_long_DT <- as.data.table(BMI_trajectories_long)

## reshape long in data table format

BMI_trajectories_long_DT = melt(BMI_trajectories_long_DT, 
                                id.vars = c("patient_id", "age_group_2"), 
                                measure.vars = c("precovid", 'postcovid'), 
                                variable.name = "pandemic_stage", 
                                value.name = "yearly_bmi_change")





## histograms with pre and post pandemic data


bmi_change_plot <-  ggplot( data = BMI_trajectories_long_DT, 
                            mapping = aes( x = yearly_bmi_change, color=pandemic_stage)) + 
  labs(title = "Rate of BMI change per year in whole population", 
       subtitle = "Data on BMI collected through routine primary care electronic health records between March 2015 and March 2022") + 
  geom_histogram(bins = 10) +
  xlim (-7, 7)  +
  stat_bin(bins =10, geom="text", aes(label=..count..), vjust = 1)




bmi_change_plot_age <- ggplot( data = BMI_trajectories_long_DT, 
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

BMI_change_summary<-  skimr::skim_without_charts(BMI_change_summary) %>% 
  dplyr::mutate(across(where(is.numeric), round, 2))

BMI_change_summary <- BMI_change_summary %>% 
  dplyr::select(skim_variable, n_missing, complete_rate) %>% 
  dplyr::rename(pandemic_stage = skim_variable)

BMI_change_summary_2 <- BMI_trajectories_long_DT[, 
                                                 list(mean = mean(yearly_bmi_change, na.rm=TRUE),
                                                      sd = sd(yearly_bmi_change, na.rm=TRUE),
                                                      median = median(yearly_bmi_change, na.rm=TRUE),
                                                      '0.1st' = quantile(yearly_bmi_change, probs = c(.001), na.rm = TRUE),
                                                      '0.5th' = quantile(yearly_bmi_change, probs = c(.005), na.rm = TRUE),
                                                      '5th' = quantile(yearly_bmi_change, probs = c(.05), na.rm = TRUE), 
                                                      '10th' = quantile(yearly_bmi_change, probs = c(.10), na.rm = TRUE),
                                                      '25th' = quantile(yearly_bmi_change, probs = c(.25), na.rm = TRUE),
                                                      '75th' = quantile(yearly_bmi_change, probs = c(.75), na.rm = TRUE), 
                                                      '90th' = quantile(yearly_bmi_change, probs = c(.90), na.rm = TRUE), 
                                                      '95th' = quantile(yearly_bmi_change, probs = c(.95), na.rm = TRUE),
                                                      '99.5th' = quantile(yearly_bmi_change, probs = c(.995), na.rm = TRUE),
                                                      '99.9th' = quantile(yearly_bmi_change, probs = c(.999), na.rm = TRUE)),
                                                 by = .(pandemic_stage)]


extreme_low <- BMI_trajectories_long_DT %>% 
  dplyr::arrange(yearly_bmi_change) %>%
  dplyr::group_by(pandemic_stage) %>%
  dplyr::slice_head(n=10) %>%
  dplyr::mutate(n10_0th = mean(yearly_bmi_change)) %>% 
  slice_head() %>% 
  dplyr::select("n10_0th", "pandemic_stage" )

extreme_high <- BMI_trajectories_long_DT %>% 
  dplyr::filter(yearly_bmi_change < 999999999) %>%   ## filter out NA
  dplyr::arrange(yearly_bmi_change, na.rm=TRUE) %>%
  dplyr::group_by(pandemic_stage) %>%
  dplyr::slice_tail(n=10) %>%
  dplyr::mutate(n10_100th = mean(yearly_bmi_change)) %>% 
  slice_head() %>% 
  dplyr::select("n10_100th", "pandemic_stage" )

BMI_change_summary_2 <- BMI_change_summary_2 %>% 
  dplyr::left_join(extreme_low) %>% 
  dplyr::left_join(extreme_high)






BMI_change_summary <- BMI_change_summary %>% 
  dplyr::left_join(BMI_change_summary_2) %>%
  dplyr::mutate(across(where(is.numeric), round, 2))



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

