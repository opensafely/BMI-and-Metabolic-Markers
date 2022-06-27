## This script looks at prepandemic rate of BMI change is associated with the change in trajectory
## patient underweight prepandemic and with a cancer code have been excluded
## Author: M Samuel
## Date: 4th May 2022



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
library(data.table)

BMI_DT <- read_feather (here::here ("output/data", "BMI_trajectory_models_data.feather"))





BMI_DT  %>% 
  tabyl(precovid_bmi_category)

## *** Change to code complete

## filter out missing
BMI_DT <- BMI_DT %>%
  dplyr::filter(complete_bmi_data == "complete") 


BMI_DT <- BMI_DT %>% 
  dplyr::mutate ( precovid_change_cat = cut(
    precovid_change, 
    breaks = c(-999,-0.1, 0.1, 0.3, 0.5, 999), 
    labels = c(">0.1 loss", "-0.1 to <0.1", "0.1 to <0.3", "0.3 to <0.5", "over 0.5"),
    include.lowest = TRUE))

BMI_DT <- as.data.table(BMI_DT)

BMI_DT %>%
  tabyl(age_group_2, sex)



### seperate 

BMI_hypertension <- BMI_DT %>% 
  dplyr::filter(hypertension == TRUE)

BMI_T1DM <- BMI_DT %>% 
  dplyr::filter(diabetes_t1 == TRUE)

BMI_T2DM <- BMI_DT %>% 
  dplyr::filter(diabetes_t2 == TRUE)


## create summaries to assess for change in mean in sd per subgroup
#age_group_2
mean_change <- BMI_DT %>%
  dplyr::group_by(precovid_change_cat)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = precovid_change_cat) %>% 
  dplyr::mutate(variable = 'precovid_change_cat', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))  %>% 
  dplyr::mutate(population = "all", .before=1)


mean_hypertension <- BMI_hypertension %>%
  dplyr::group_by(precovid_change_cat)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = precovid_change_cat) %>% 
  dplyr::mutate(variable = 'precovid_change_cat', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))  %>% 
  dplyr::mutate(population = "hypertension", .before=1)


mean_T1DM<- BMI_T1DM %>%
  dplyr::group_by(precovid_change_cat)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = precovid_change_cat) %>% 
  dplyr::mutate(variable = 'precovid_change_cat', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))  %>% 
  dplyr::mutate(population = "T1DM", .before=1)

mean_T2DM <- BMI_T2DM %>%
  dplyr::group_by(precovid_change_cat)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = precovid_change_cat) %>% 
  dplyr::mutate(variable = 'precovid_change_cat', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))  %>% 
  dplyr::mutate(population = "T2DM", .before=1)


mean_change <- mean_change %>% 
  dplyr::bind_rows(mean_hypertension, mean_T2DM, mean_T1DM)


mean_change <- mean_change %>% 
  dplyr::mutate(n = plyr::round_any(mean_change$n, 5)) 

## models



univariate_model <- BMI_DT[, lm(trajectory_change ~ precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)


age_model <- BMI_DT[, lm(trajectory_change ~ age_group_2 + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

sex_model <- BMI_DT[, lm(trajectory_change ~ sex + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

agesex_model <- BMI_DT[, lm(trajectory_change ~ age_group_2 + sex + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)


models <- univariate_model %>% 
  bind_rows(age_model, sex_model, agesex_model) %>% 
  dplyr::mutate(population = "all", .before=1)


## For hypertensives



univariate_model <- BMI_hypertension[, lm(trajectory_change ~ precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)


age_model <- BMI_hypertension[, lm(trajectory_change ~ age_group_2 + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

sex_model <- BMI_hypertension[, lm(trajectory_change ~ sex + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

agesex_model <- BMI_hypertension[, lm(trajectory_change ~ age_group_2 + sex + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)


models_hypertension <- univariate_model %>% 
  bind_rows(age_model, sex_model, agesex_model) %>% 
  dplyr::mutate(population = "hypertension", .before=1)



## For T2DM



univariate_model <- BMI_T2DM[, lm(trajectory_change ~ precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)


age_model <- BMI_T2DM[, lm(trajectory_change ~ age_group_2 + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

sex_model <- BMI_T2DM[, lm(trajectory_change ~ sex + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

agesex_model <- BMI_T2DM[, lm(trajectory_change ~ age_group_2 + sex + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)


models_T2DM <- univariate_model %>% 
  bind_rows(age_model, sex_model, agesex_model) %>% 
  dplyr::mutate(population = "T2DM", .before=1)



## For T1DM



univariate_model <- BMI_T1DM[, lm(trajectory_change ~ precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)


age_model <- BMI_T1DM[, lm(trajectory_change ~ age_group_2 + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

sex_model <- BMI_T1DM[, lm(trajectory_change ~ sex + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)

agesex_model <- BMI_T1DM[, lm(trajectory_change ~ age_group_2 + sex + precovid_change_cat)] %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)



models_T1DM <- univariate_model %>% 
  bind_rows(age_model, sex_model, agesex_model) %>% 
  dplyr::mutate(population = "T1DM", .before=1)


## models
models <- models %>% 
  dplyr::bind_rows(models_hypertension, models_T2DM, models_T1DM)


## save models and mean_change
write.csv (mean_change, here::here ("output/data","mean_traj_change_by_precovid_change.csv"))
write.csv (models, here::here ("output/data","models_traj_change_by_precovid_change.csv"))