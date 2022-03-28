## Author: Miriam Samuel
## Date:  28th March
## This R script plots the median BMI to visualise changes


## Specify libraries

library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(ggplot2)

BMI_plots <- read_feather (here::here ("output/data", "BMI_all_2019.feather"))
BMI_plots <- BMI_plots %>%
  dplyr::select(patient_id, median_bmi, age_group, sex, ethnic_no_miss, comorbid_diabetes_t2, comorbid_hypertension)


median_bmi_plot_all <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram()

bmi_age <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_wrap (~age_group)
bmi_ethnicity <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_wrap (~ethnic_no_miss)
bmi_T2DM  <-  ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_wrap (~comorbid_diabetes_t2)
bmi_hypertension <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_wrap (~comorbid_hypertension)

bmi_age_T2DM <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_grid (age_group ~comorbid_diabetes_t2)
bmi_age_hypertension <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_grid (age_group ~comorbid_hypertension)



ggsave(filename=here::here("output", "plots","median_bmi_all.png")), median_bmi_plot_all, dpi=600, width = 30, height = 30, units = "cm")
