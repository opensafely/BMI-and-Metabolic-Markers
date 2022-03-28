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

BMI_plots$comorbid_diabetes_t2[BMI_plots$comorbid_diabetes_t2==TRUE] <- "T2DM - Present"
BMI_plots$comorbid_diabetes_t2[BMI_plots$comorbid_diabetes_t2==FALSE] <- "T2DM - Absent"

BMI_plots$comorbid_hypertension[BMI_plots$comorbid_hypertension==TRUE] <- "Hypertension - Present"
BMI_plots$comorbid_hypertension[BMI_plots$comorbid_hypertension==FALSE] <- "Hypertension - Absent"


median_bmi_plot_all <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + labs(title = "Median recorded BMI distribution of patients in 2019", subtitle = "Data from routine primary care electronic health records") + geom_histogram()

bmi_age <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_wrap (~age_group)
bmi_ethnicity <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_wrap (~ethnic_no_miss)
bmi_T2DM  <-  ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_wrap (~comorbid_diabetes_t2)
bmi_hypertension <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + geom_histogram() + facet_wrap (~comorbid_hypertension)

bmi_age_T2DM <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) +  labs(title = "Median recorded BMI distribution of patients in 2019", subtitle = "Data from routine primary care electronic health records") + geom_histogram() + facet_grid (age_group ~comorbid_diabetes_t2)
bmi_age_hypertension <- ggplot( data = BMI_plots, mapping = aes( x = median_bmi)) + labs(title = "Median recorded BMI distribution of patients in 2019", subtitle = "Data from routine primary care electronic health records") +  geom_histogram() + facet_grid (age_group ~comorbid_hypertension)


ggsave(
    plot = median_bmi_plot_all,
    filename = "median_bmi_all_2019.png", 
    path = here::here("output"),
    dpi=600, width = 15, height = 15, units = "cm"
)

ggsave(
    plot = bmi_age_T2DM,
    filename = "median_bmi_age_T2DM_2019.png", 
    path = here::here("output"),
    dpi=600, width = 30, height = 30, units = "cm"
)

ggsave(
    plot = bmi_age_hypertension,
    filename = "median_bmi_age_hypertension_2019.png", 
    path = here::here("output"),
    dpi=600, width = 30, height = 30, units = "cm"
)

# ggsave(filename=here::here("output/plots","median_bmi_all.png")), median_bmi_plot_all, dpi=600, width = 30, height = 30, units = "cm")

# write_feather (BMI_complete_categories_DWMP, here::here ("output/data","BMI_complete_median_2015.feather"))  ..  example file path

