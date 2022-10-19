
#### Author: M Samuel
#### Date: 24th March 2022
#### This script calculates median BMI and (univariate) evidence of differences between groups 


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

## Read in files  >>> Change PATH!!

BMI_complete_categories_main <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))


## 2015

BMI_complete_categories <- BMI_complete_categories_main %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2015") %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", "40-65", "65-80", "80+"))


BMI_complete_categories$age_group_2 <- factor(BMI_complete_categories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(precovid_obese_flag=FALSE))


## FIX to have NA as missing

BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(smoking_status= 'M'))


BMI_complete_categories_DT <- data.table(BMI_complete_categories)



## Smoking

N_median_smoking_status <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="smoking_status"]
n_median_smoking_status <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="smoking_status"]
quartile1_median_smoking_status <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="smoking_status"]
median_median_smoking_status <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="smoking_status"]
quartile3_median_smoking_status <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="smoking_status"]

KWRS_smoking_status <- kruskal.test(smoking_status ~ median_bmi, BMI_complete_categories)
significance_smoking_status <- data.frame(t(sapply(KWRS_smoking_status, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_smoking_status <- significance_smoking_status$p.value

median_2015 <- N_median_smoking_status %>%
  dplyr::left_join(n_median_smoking_status, by = "smoking_status") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(median_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(quartile3_median_smoking_status, by = "smoking_status") %>%
  dplyr::select(smoking_status, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_smoking_status) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'smoking_status') %>%
  dplyr::mutate(variable="smoking_status", .before = 1 ) %>%
  dplyr::mutate(year="2015", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


################
### 2016
################




BMI_complete_categories <- BMI_complete_categories_main %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2016") %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", "40-65", "65-80", "80+"))


BMI_complete_categories$age_group_2 <- factor(BMI_complete_categories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(precovid_obese_flag=FALSE))


## FIX to have NA as missing

BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(smoking_status= 'M'))


BMI_complete_categories_DT <- data.table(BMI_complete_categories)



## Smoking

N_median_smoking_status <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="smoking_status"]
n_median_smoking_status <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="smoking_status"]
quartile1_median_smoking_status <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="smoking_status"]
median_median_smoking_status <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="smoking_status"]
quartile3_median_smoking_status <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="smoking_status"]

KWRS_smoking_status <- kruskal.test(smoking_status ~ median_bmi, BMI_complete_categories)
significance_smoking_status <- data.frame(t(sapply(KWRS_smoking_status, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_smoking_status <- significance_smoking_status$p.value

median_2016 <- N_median_smoking_status %>%
  dplyr::left_join(n_median_smoking_status, by = "smoking_status") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(median_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(quartile3_median_smoking_status, by = "smoking_status") %>%
  dplyr::select(smoking_status, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_smoking_status) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'smoking_status') %>%
  dplyr::mutate(variable="smoking_status", .before = 1 ) %>%
  dplyr::mutate(year="2016", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## 2017


BMI_complete_categories <- BMI_complete_categories_main %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2017") %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", "40-65", "65-80", "80+"))


BMI_complete_categories$age_group_2 <- factor(BMI_complete_categories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(precovid_obese_flag=FALSE))


## FIX to have NA as missing

BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(smoking_status= 'M'))


BMI_complete_categories_DT <- data.table(BMI_complete_categories)



## Smoking

N_median_smoking_status <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="smoking_status"]
n_median_smoking_status <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="smoking_status"]
quartile1_median_smoking_status <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="smoking_status"]
median_median_smoking_status <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="smoking_status"]
quartile3_median_smoking_status <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="smoking_status"]

KWRS_smoking_status <- kruskal.test(smoking_status ~ median_bmi, BMI_complete_categories)
significance_smoking_status <- data.frame(t(sapply(KWRS_smoking_status, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_smoking_status <- significance_smoking_status$p.value

median_2017 <- N_median_smoking_status %>%
  dplyr::left_join(n_median_smoking_status, by = "smoking_status") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(median_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(quartile3_median_smoking_status, by = "smoking_status") %>%
  dplyr::select(smoking_status, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_smoking_status) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'smoking_status') %>%
  dplyr::mutate(variable="smoking_status", .before = 1 ) %>%
  dplyr::mutate(year="2017", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))



## 2018


BMI_complete_categories <- BMI_complete_categories_main %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2018") %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", "40-65", "65-80", "80+"))


BMI_complete_categories$age_group_2 <- factor(BMI_complete_categories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(precovid_obese_flag=FALSE))


## FIX to have NA as missing

BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(smoking_status= 'M'))


BMI_complete_categories_DT <- data.table(BMI_complete_categories)



## Smoking

N_median_smoking_status <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="smoking_status"]
n_median_smoking_status <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="smoking_status"]
quartile1_median_smoking_status <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="smoking_status"]
median_median_smoking_status <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="smoking_status"]
quartile3_median_smoking_status <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="smoking_status"]

KWRS_smoking_status <- kruskal.test(smoking_status ~ median_bmi, BMI_complete_categories)
significance_smoking_status <- data.frame(t(sapply(KWRS_smoking_status, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_smoking_status <- significance_smoking_status$p.value

median_2018 <- N_median_smoking_status %>%
  dplyr::left_join(n_median_smoking_status, by = "smoking_status") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(median_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(quartile3_median_smoking_status, by = "smoking_status") %>%
  dplyr::select(smoking_status, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_smoking_status) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'smoking_status') %>%
  dplyr::mutate(variable="smoking_status", .before = 1 ) %>%
  dplyr::mutate(year="2018", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))



##2019


BMI_complete_categories <- BMI_complete_categories_main %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2019") %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", "40-65", "65-80", "80+"))


BMI_complete_categories$age_group_2 <- factor(BMI_complete_categories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(precovid_obese_flag=FALSE))


## FIX to have NA as missing

BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(smoking_status= 'M'))


BMI_complete_categories_DT <- data.table(BMI_complete_categories)



## Smoking

N_median_smoking_status <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="smoking_status"]
n_median_smoking_status <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="smoking_status"]
quartile1_median_smoking_status <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="smoking_status"]
median_median_smoking_status <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="smoking_status"]
quartile3_median_smoking_status <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="smoking_status"]

KWRS_smoking_status <- kruskal.test(smoking_status ~ median_bmi, BMI_complete_categories)
significance_smoking_status <- data.frame(t(sapply(KWRS_smoking_status, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_smoking_status <- significance_smoking_status$p.value

median_2019 <- N_median_smoking_status %>%
  dplyr::left_join(n_median_smoking_status, by = "smoking_status") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(median_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(quartile3_median_smoking_status, by = "smoking_status") %>%
  dplyr::select(smoking_status, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_smoking_status) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'smoking_status') %>%
  dplyr::mutate(variable="smoking_status", .before = 1 ) %>%
  dplyr::mutate(year="2019", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))




## 2020

BMI_complete_categories <- BMI_complete_categories_main %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2020") %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", "40-65", "65-80", "80+"))


BMI_complete_categories$age_group_2 <- factor(BMI_complete_categories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(precovid_obese_flag=FALSE))


## FIX to have NA as missing

BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(smoking_status= 'M'))


BMI_complete_categories_DT <- data.table(BMI_complete_categories)



## Smoking

N_median_smoking_status <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="smoking_status"]
n_median_smoking_status <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="smoking_status"]
quartile1_median_smoking_status <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="smoking_status"]
median_median_smoking_status <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="smoking_status"]
quartile3_median_smoking_status <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="smoking_status"]

KWRS_smoking_status <- kruskal.test(smoking_status ~ median_bmi, BMI_complete_categories)
significance_smoking_status <- data.frame(t(sapply(KWRS_smoking_status, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_smoking_status <- significance_smoking_status$p.value

median_2020 <- N_median_smoking_status %>%
  dplyr::left_join(n_median_smoking_status, by = "smoking_status") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(median_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(quartile3_median_smoking_status, by = "smoking_status") %>%
  dplyr::select(smoking_status, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_smoking_status) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'smoking_status') %>%
  dplyr::mutate(variable="smoking_status", .before = 1 ) %>%
  dplyr::mutate(year="2020", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))




### 2021


BMI_complete_categories <- BMI_complete_categories_main %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2021") %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", "40-65", "65-80", "80+"))


BMI_complete_categories$age_group_2 <- factor(BMI_complete_categories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(precovid_obese_flag=FALSE))


## FIX to have NA as missing

BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(smoking_status= 'M'))


BMI_complete_categories_DT <- data.table(BMI_complete_categories)



## Smoking

N_median_smoking_status <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="smoking_status"]
n_median_smoking_status <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="smoking_status"]
quartile1_median_smoking_status <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="smoking_status"]
median_median_smoking_status <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="smoking_status"]
quartile3_median_smoking_status <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="smoking_status"]

KWRS_smoking_status <- kruskal.test(smoking_status ~ median_bmi, BMI_complete_categories)
significance_smoking_status <- data.frame(t(sapply(KWRS_smoking_status, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_smoking_status <- significance_smoking_status$p.value

median_2021 <- N_median_smoking_status %>%
  dplyr::left_join(n_median_smoking_status, by = "smoking_status") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(median_median_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(quartile3_median_smoking_status, by = "smoking_status") %>%
  dplyr::select(smoking_status, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_smoking_status) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'smoking_status') %>%
  dplyr::mutate(variable="smoking_status", .before = 1 ) %>%
  dplyr::mutate(year="2021", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))









median_complete <- dplyr::bind_rows(median_2015, 
                                    median_2016, 
                                    median_2017, 
                                    median_2018, 
                                    median_2019, 
                                    median_2020,
                                    median_2021)



median_complete <- as.data.frame(median_complete) %>%
  dplyr::mutate(N = plyr::round_any(median_complete$N, 5)) %>% 
  dplyr::mutate(N_population = plyr::round_any(median_complete$N_population, 5)) 


write.csv (median_complete, here::here ("output/data","median_complete_smoking_status.csv"))