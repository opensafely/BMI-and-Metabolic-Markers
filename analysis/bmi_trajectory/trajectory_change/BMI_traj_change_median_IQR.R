
#### Author: M Samuel
#### Date: Oct 2022
#### This script calculates trajectory change


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

BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_models_data.feather"))


##1.  exclude low bmi and cancer

##*** Change to code.  FILTER OUT UNDERWEIGHT AND THOSE WITH CANCER
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(all_cancer == FALSE)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(precovid_bmi_category != "underweight")

# select relevant variables
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::select("sex",
                "age_group_2", 
                "region",                
                "imd",
                "hypertension",
                "diabetes_t1", 
                "diabetes_t2",
                "learning_disability", 
                "depression",               
                "psychosis_schiz_bipolar", 
                "dementia", 
                "asthma",
                "COPD",
                "stroke_and_TIA",          
                "chronic_cardiac",                 
                "all_cancer",                           
                "smoking_status", 
                "ethnic_no_miss",         
                "eth_group_16",           
                "complete_bmi_data", 
                "precovid_bmi_category", 
                "trajectory_change")



BMI_trajectories$precovid_bmi_category <- factor(BMI_trajectories$precovid_bmi_category, levels = c("healthy","overweight", "obese"))

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::ungroup() %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) 


BMI_trajectories$age_group_2 <- factor(BMI_trajectories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))




BMI_trajectories <- BMI_trajectories %>% 
  replace_na(list(smoking_status= 'M'))


BMI_trajectories %>%
  tabyl(imd)

length(BMI_trajectories$trajectory_change)
sum(is.na(BMI_trajectories$trajectory_change))
N_trajectory_all = (length(BMI_trajectories$trajectory_change) - sum(is.na(BMI_trajectories$trajectory_change)))
# trajectory_all = trajectory(BMI_trajectories$trajectory_change, na.rm = TRUE)
quantile_all = quantile(BMI_trajectories$trajectory_change, probs = c(0.25,0.5,0.75), na.rm = TRUE) # returns a list
N_population = length(BMI_trajectories$trajectory_change)

N_population

trajectory_all <-  data.frame(t(sapply(quantile_all,c))) %>%  # change list of qauntiles to a data frame
  dplyr::mutate(variable = "all") %>%
  dplyr::mutate(group = "all") %>%
  dplyr::mutate(N = N_trajectory_all) %>%
  dplyr::mutate(N_population = N_population) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

trajectory_all <- trajectory_all %>%
  dplyr::mutate( "25th" = X25.) %>%
  dplyr::mutate("50th" = X50. ) %>%
  dplyr::mutate ("75th" = X75.) %>%
  dplyr::mutate(group = "all") %>%
  dplyr::mutate(variable = "all") %>%
  dplyr::select('variable', 'group', 'N', 'N_population', '25th', '50th', '75th') 


BMI_trajectories %>%
  tabyl(imd)

## code to calcualte trajectory_change

BMI_trajectories_DT <- data.table(BMI_trajectories)



## Skewed population.  BMI trajectorys by subgroup
## Age Group 2

N_trajectory_age_group_2 <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="age_group_2"]
n_trajectory_age_group_2 <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="age_group_2"]
quartile1_trajectory_age_group_2 <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="age_group_2"]
trajectory_trajectory_age_group_2 <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="age_group_2"]
quartile3_trajectory_age_group_2 <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="age_group_2"]

KWRS_age_group_2 <- kruskal.test(age_group_2 ~ trajectory_change, BMI_trajectories)
significance_age_group_2 <- data.frame(t(sapply(KWRS_age_group_2, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_age_group_2 <- significance_age_group_2$p.value

trajectory_age_group_2 <- N_trajectory_age_group_2 %>%
  dplyr::left_join(n_trajectory_age_group_2, by = "age_group_2") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_age_group_2, by = "age_group_2") %>%
  dplyr::left_join(trajectory_trajectory_age_group_2, by = "age_group_2") %>%
  dplyr::left_join(quartile3_trajectory_age_group_2, by = "age_group_2") %>%
  dplyr::select(age_group_2, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_age_group_2) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'age_group_2') %>%
  dplyr::mutate(variable="age_group_2", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::arrange(group)





### SEX

N_trajectory_sex <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="sex"]
n_trajectory_sex <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="sex"]
quartile1_trajectory_sex <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="sex"]
trajectory_trajectory_sex <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="sex"]
quartile3_trajectory_sex <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="sex"]

KWRS_sex <- kruskal.test(sex ~ trajectory_change, BMI_trajectories)
significance_sex <- data.frame(t(sapply(KWRS_sex, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_sex <- significance_sex$p.value

trajectory_sex <- N_trajectory_sex %>%
  dplyr::left_join(n_trajectory_sex, by = "sex") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_sex, by = "sex") %>%
  dplyr::left_join(trajectory_trajectory_sex, by = "sex") %>%
  dplyr::left_join(quartile3_trajectory_sex, by = "sex") %>%
  dplyr::select(sex, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_sex) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'sex') %>%
  dplyr::mutate(variable="sex", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis")


## REGION


N_trajectory_region <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="region"]
n_trajectory_region <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="region"]
quartile1_trajectory_region <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="region"]
trajectory_trajectory_region <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="region"]
quartile3_trajectory_region <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="region"]

KWRS_region <- kruskal.test(region ~ trajectory_change, BMI_trajectories)
significance_region <- data.frame(t(sapply(KWRS_region, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_region <- significance_region$p.value

trajectory_region <- N_trajectory_region %>%
  dplyr::left_join(n_trajectory_region, by = "region") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_region, by = "region") %>%
  dplyr::left_join(trajectory_trajectory_region, by = "region") %>%
  dplyr::left_join(quartile3_trajectory_region, by = "region") %>%
  dplyr::select(region, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_region) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'region') %>%
  dplyr::mutate(variable="region", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis")

##  IMD

N_trajectory_imd <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="imd"]
n_trajectory_imd <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="imd"]
quartile1_trajectory_imd <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="imd"]
trajectory_trajectory_imd <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="imd"]
quartile3_trajectory_imd <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="imd"]

KWRS_imd <- kruskal.test(imd ~ trajectory_change, BMI_trajectories)
significance_imd <- data.frame(t(sapply(KWRS_imd, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_imd <- significance_imd$p.value

trajectory_imd <- N_trajectory_imd %>%
  dplyr::left_join(n_trajectory_imd, by = "imd") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_imd, by = "imd") %>%
  dplyr::left_join(trajectory_trajectory_imd, by = "imd") %>%
  dplyr::left_join(quartile3_trajectory_imd, by = "imd") %>%
  dplyr::select(imd, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_imd) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'imd') %>%
  dplyr::mutate(variable="imd", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::arrange(group)


## 6 category ethnicity

N_trajectory_ethnic_no_miss <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="ethnic_no_miss"]
n_trajectory_ethnic_no_miss <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="ethnic_no_miss"]
quartile1_trajectory_ethnic_no_miss <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="ethnic_no_miss"]
trajectory_trajectory_ethnic_no_miss <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="ethnic_no_miss"]
quartile3_trajectory_ethnic_no_miss <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="ethnic_no_miss"]

KWRS_ethnic_no_miss <- kruskal.test(ethnic_no_miss ~ trajectory_change, BMI_trajectories)
significance_ethnic_no_miss <- data.frame(t(sapply(KWRS_ethnic_no_miss, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_ethnic_no_miss <- significance_ethnic_no_miss$p.value

trajectory_ethnic_no_miss <- N_trajectory_ethnic_no_miss %>%
  dplyr::left_join(n_trajectory_ethnic_no_miss, by = "ethnic_no_miss") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_ethnic_no_miss, by = "ethnic_no_miss") %>%
  dplyr::left_join(trajectory_trajectory_ethnic_no_miss, by = "ethnic_no_miss") %>%
  dplyr::left_join(quartile3_trajectory_ethnic_no_miss, by = "ethnic_no_miss") %>%
  dplyr::select(ethnic_no_miss, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_ethnic_no_miss) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'ethnic_no_miss') %>%
  dplyr::mutate(variable="ethnic_no_miss", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::arrange(group)


## ethnic 16 categories

N_trajectory_eth_group_16 <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="eth_group_16"]
n_trajectory_eth_group_16 <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="eth_group_16"]
quartile1_trajectory_eth_group_16 <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="eth_group_16"]
trajectory_trajectory_eth_group_16 <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="eth_group_16"]
quartile3_trajectory_eth_group_16 <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="eth_group_16"]

KWRS_eth_group_16 <- kruskal.test(eth_group_16 ~ trajectory_change, BMI_trajectories)
significance_eth_group_16 <- data.frame(t(sapply(KWRS_eth_group_16, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_eth_group_16 <- significance_eth_group_16$p.value

trajectory_eth_group_16 <- N_trajectory_eth_group_16 %>%
  dplyr::left_join(n_trajectory_eth_group_16, by = "eth_group_16") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_eth_group_16, by = "eth_group_16") %>%
  dplyr::left_join(trajectory_trajectory_eth_group_16, by = "eth_group_16") %>%
  dplyr::left_join(quartile3_trajectory_eth_group_16, by = "eth_group_16") %>%
  dplyr::select(eth_group_16, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_eth_group_16) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'eth_group_16') %>%
  dplyr::mutate(variable="eth_group_16", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::arrange(group)


N_trajectory_learning_disability <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="learning_disability"]
n_trajectory_learning_disability <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="learning_disability"]
quartile1_trajectory_learning_disability <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="learning_disability"]
trajectory_trajectory_learning_disability <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="learning_disability"]
quartile3_trajectory_learning_disability <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="learning_disability"]

KWRS_learning_disability <- kruskal.test(learning_disability ~ trajectory_change, BMI_trajectories)
significance_learning_disability <- data.frame(t(sapply(KWRS_learning_disability, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_learning_disability <- significance_learning_disability$p.value

trajectory_learning_disability <- N_trajectory_learning_disability %>%
  dplyr::left_join(n_trajectory_learning_disability, by = "learning_disability") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_learning_disability, by = "learning_disability") %>%
  dplyr::left_join(trajectory_trajectory_learning_disability, by = "learning_disability") %>%
  dplyr::left_join(quartile3_trajectory_learning_disability, by = "learning_disability") %>%
  dplyr::select(learning_disability, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_learning_disability) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'learning_disability') %>%
  dplyr::mutate(variable="learning_disability", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))

## Depression

N_trajectory_depression <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="depression"]
n_trajectory_depression <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="depression"]
quartile1_trajectory_depression <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="depression"]
trajectory_trajectory_depression <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="depression"]
quartile3_trajectory_depression <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="depression"]

KWRS_depression <- kruskal.test(depression ~ trajectory_change, BMI_trajectories)
significance_depression <- data.frame(t(sapply(KWRS_depression, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_depression <- significance_depression$p.value

trajectory_depression <- N_trajectory_depression %>%
  dplyr::left_join(n_trajectory_depression, by = "depression") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_depression, by = "depression") %>%
  dplyr::left_join(trajectory_trajectory_depression, by = "depression") %>%
  dplyr::left_join(quartile3_trajectory_depression, by = "depression") %>%
  dplyr::select(depression, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_depression) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'depression') %>%
  dplyr::mutate(variable="depression", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Dementia

N_trajectory_dementia <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="dementia"]
n_trajectory_dementia <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="dementia"]
quartile1_trajectory_dementia <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="dementia"]
trajectory_trajectory_dementia <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="dementia"]
quartile3_trajectory_dementia <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="dementia"]

KWRS_dementia <- kruskal.test(dementia ~ trajectory_change, BMI_trajectories)
significance_dementia <- data.frame(t(sapply(KWRS_dementia, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_dementia <- significance_dementia$p.value

trajectory_dementia <- N_trajectory_dementia %>%
  dplyr::left_join(n_trajectory_dementia, by = "dementia") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_dementia, by = "dementia") %>%
  dplyr::left_join(trajectory_trajectory_dementia, by = "dementia") %>%
  dplyr::left_join(quartile3_trajectory_dementia, by = "dementia") %>%
  dplyr::select(dementia, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_dementia) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'dementia') %>%
  dplyr::mutate(variable="dementia", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Serious mental health

N_trajectory_psychosis_schiz_bipolar <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="psychosis_schiz_bipolar"]
n_trajectory_psychosis_schiz_bipolar <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="psychosis_schiz_bipolar"]
quartile1_trajectory_psychosis_schiz_bipolar <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="psychosis_schiz_bipolar"]
trajectory_trajectory_psychosis_schiz_bipolar <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="psychosis_schiz_bipolar"]
quartile3_trajectory_psychosis_schiz_bipolar <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="psychosis_schiz_bipolar"]

KWRS_psychosis_schiz_bipolar <- kruskal.test(psychosis_schiz_bipolar ~ trajectory_change, BMI_trajectories)
significance_psychosis_schiz_bipolar <- data.frame(t(sapply(KWRS_psychosis_schiz_bipolar, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_psychosis_schiz_bipolar <- significance_psychosis_schiz_bipolar$p.value

trajectory_psychosis_schiz_bipolar <- N_trajectory_psychosis_schiz_bipolar %>%
  dplyr::left_join(n_trajectory_psychosis_schiz_bipolar, by = "psychosis_schiz_bipolar") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_psychosis_schiz_bipolar, by = "psychosis_schiz_bipolar") %>%
  dplyr::left_join(trajectory_trajectory_psychosis_schiz_bipolar, by = "psychosis_schiz_bipolar") %>%
  dplyr::left_join(quartile3_trajectory_psychosis_schiz_bipolar, by = "psychosis_schiz_bipolar") %>%
  dplyr::select(psychosis_schiz_bipolar, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_psychosis_schiz_bipolar) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'psychosis_schiz_bipolar') %>%
  dplyr::mutate(variable="psychosis_schiz_bipolar", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Type 1 DM

N_trajectory_diabetes_t1 <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="diabetes_t1"]
n_trajectory_diabetes_t1 <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="diabetes_t1"]
quartile1_trajectory_diabetes_t1 <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="diabetes_t1"]
trajectory_trajectory_diabetes_t1 <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="diabetes_t1"]
quartile3_trajectory_diabetes_t1 <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="diabetes_t1"]

KWRS_diabetes_t1 <- kruskal.test(diabetes_t1 ~ trajectory_change, BMI_trajectories)
significance_diabetes_t1 <- data.frame(t(sapply(KWRS_diabetes_t1, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_diabetes_t1 <- significance_diabetes_t1$p.value

trajectory_diabetes_t1 <- N_trajectory_diabetes_t1 %>%
  dplyr::left_join(n_trajectory_diabetes_t1, by = "diabetes_t1") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_diabetes_t1, by = "diabetes_t1") %>%
  dplyr::left_join(trajectory_trajectory_diabetes_t1, by = "diabetes_t1") %>%
  dplyr::left_join(quartile3_trajectory_diabetes_t1, by = "diabetes_t1") %>%
  dplyr::select(diabetes_t1, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_diabetes_t1) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'diabetes_t1') %>%
  dplyr::mutate(variable="diabetes_t1", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))

## Type 2 DM

N_trajectory_diabetes_t2 <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="diabetes_t2"]
n_trajectory_diabetes_t2 <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="diabetes_t2"]
quartile1_trajectory_diabetes_t2 <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="diabetes_t2"]
trajectory_trajectory_diabetes_t2 <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="diabetes_t2"]
quartile3_trajectory_diabetes_t2 <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="diabetes_t2"]

KWRS_diabetes_t2 <- kruskal.test(diabetes_t2 ~ trajectory_change, BMI_trajectories)
significance_diabetes_t2 <- data.frame(t(sapply(KWRS_diabetes_t2, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_diabetes_t2 <- significance_diabetes_t2$p.value

trajectory_diabetes_t2 <- N_trajectory_diabetes_t2 %>%
  dplyr::left_join(n_trajectory_diabetes_t2, by = "diabetes_t2") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_diabetes_t2, by = "diabetes_t2") %>%
  dplyr::left_join(trajectory_trajectory_diabetes_t2, by = "diabetes_t2") %>%
  dplyr::left_join(quartile3_trajectory_diabetes_t2, by = "diabetes_t2") %>%
  dplyr::select(diabetes_t2, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_diabetes_t2) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'diabetes_t2') %>%
  dplyr::mutate(variable="diabetes_t2", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Asthma


N_trajectory_asthma <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="asthma"]
n_trajectory_asthma <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="asthma"]
quartile1_trajectory_asthma <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="asthma"]
trajectory_trajectory_asthma <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="asthma"]
quartile3_trajectory_asthma <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="asthma"]

KWRS_asthma <- kruskal.test(asthma ~ trajectory_change, BMI_trajectories)
significance_asthma <- data.frame(t(sapply(KWRS_asthma, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_asthma <- significance_asthma$p.value

trajectory_asthma <- N_trajectory_asthma %>%
  dplyr::left_join(n_trajectory_asthma, by = "asthma") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_asthma, by = "asthma") %>%
  dplyr::left_join(trajectory_trajectory_asthma, by = "asthma") %>%
  dplyr::left_join(quartile3_trajectory_asthma, by = "asthma") %>%
  dplyr::select(asthma, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_asthma) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'asthma') %>%
  dplyr::mutate(variable="asthma", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## COPD


N_trajectory_COPD <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="COPD"]
n_trajectory_COPD <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="COPD"]
quartile1_trajectory_COPD <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="COPD"]
trajectory_trajectory_COPD <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="COPD"]
quartile3_trajectory_COPD <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="COPD"]

KWRS_COPD <- kruskal.test(COPD ~ trajectory_change, BMI_trajectories)
significance_COPD <- data.frame(t(sapply(KWRS_COPD, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_COPD <- significance_COPD$p.value

trajectory_COPD <- N_trajectory_COPD %>%
  dplyr::left_join(n_trajectory_COPD, by = "COPD") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_COPD, by = "COPD") %>%
  dplyr::left_join(trajectory_trajectory_COPD, by = "COPD") %>%
  dplyr::left_join(quartile3_trajectory_COPD, by = "COPD") %>%
  dplyr::select(COPD, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_COPD) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'COPD') %>%
  dplyr::mutate(variable="COPD", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Stroke TIA

N_trajectory_stroke_and_TIA <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="stroke_and_TIA"]
n_trajectory_stroke_and_TIA <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="stroke_and_TIA"]
quartile1_trajectory_stroke_and_TIA <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="stroke_and_TIA"]
trajectory_trajectory_stroke_and_TIA <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="stroke_and_TIA"]
quartile3_trajectory_stroke_and_TIA <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="stroke_and_TIA"]

KWRS_stroke_and_TIA <- kruskal.test(stroke_and_TIA ~ trajectory_change, BMI_trajectories)
significance_stroke_and_TIA <- data.frame(t(sapply(KWRS_stroke_and_TIA, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_stroke_and_TIA <- significance_stroke_and_TIA$p.value

trajectory_stroke_and_TIA <- N_trajectory_stroke_and_TIA %>%
  dplyr::left_join(n_trajectory_stroke_and_TIA, by = "stroke_and_TIA") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_stroke_and_TIA, by = "stroke_and_TIA") %>%
  dplyr::left_join(trajectory_trajectory_stroke_and_TIA, by = "stroke_and_TIA") %>%
  dplyr::left_join(quartile3_trajectory_stroke_and_TIA, by = "stroke_and_TIA") %>%
  dplyr::select(stroke_and_TIA, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_stroke_and_TIA) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'stroke_and_TIA') %>%
  dplyr::mutate(variable="stroke_and_TIA", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Chronic Cardiac


N_trajectory_chronic_cardiac <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="chronic_cardiac"]
n_trajectory_chronic_cardiac <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="chronic_cardiac"]
quartile1_trajectory_chronic_cardiac <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="chronic_cardiac"]
trajectory_trajectory_chronic_cardiac <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="chronic_cardiac"]
quartile3_trajectory_chronic_cardiac <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="chronic_cardiac"]

KWRS_chronic_cardiac <- kruskal.test(chronic_cardiac ~ trajectory_change, BMI_trajectories)
significance_chronic_cardiac <- data.frame(t(sapply(KWRS_chronic_cardiac, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_chronic_cardiac <- significance_chronic_cardiac$p.value

trajectory_chronic_cardiac <- N_trajectory_chronic_cardiac %>%
  dplyr::left_join(n_trajectory_chronic_cardiac, by = "chronic_cardiac") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_chronic_cardiac, by = "chronic_cardiac") %>%
  dplyr::left_join(trajectory_trajectory_chronic_cardiac, by = "chronic_cardiac") %>%
  dplyr::left_join(quartile3_trajectory_chronic_cardiac, by = "chronic_cardiac") %>%
  dplyr::select(chronic_cardiac, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_chronic_cardiac) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'chronic_cardiac') %>%
  dplyr::mutate(variable="chronic_cardiac", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Hypertension

N_trajectory_hypertension <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="hypertension"]
n_trajectory_hypertension <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="hypertension"]
quartile1_trajectory_hypertension <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="hypertension"]
trajectory_trajectory_hypertension <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="hypertension"]
quartile3_trajectory_hypertension <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="hypertension"]

KWRS_hypertension <- kruskal.test(hypertension ~ trajectory_change, BMI_trajectories)
significance_hypertension <- data.frame(t(sapply(KWRS_hypertension, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_hypertension <- significance_hypertension$p.value

trajectory_hypertension <- N_trajectory_hypertension %>%
  dplyr::left_join(n_trajectory_hypertension, by = "hypertension") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_hypertension, by = "hypertension") %>%
  dplyr::left_join(trajectory_trajectory_hypertension, by = "hypertension") %>%
  dplyr::left_join(quartile3_trajectory_hypertension, by = "hypertension") %>%
  dplyr::select(hypertension, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_hypertension) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'hypertension') %>%
  dplyr::mutate(variable="hypertension", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## All Cancer

N_trajectory_all_cancer <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="all_cancer"]
n_trajectory_all_cancer <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="all_cancer"]
quartile1_trajectory_all_cancer <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="all_cancer"]
trajectory_trajectory_all_cancer <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="all_cancer"]
quartile3_trajectory_all_cancer <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="all_cancer"]

KWRS_all_cancer <- kruskal.test(all_cancer ~ trajectory_change, BMI_trajectories)
significance_all_cancer <- data.frame(t(sapply(KWRS_all_cancer, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_all_cancer <- significance_all_cancer$p.value

trajectory_all_cancer <- N_trajectory_all_cancer %>%
  dplyr::left_join(n_trajectory_all_cancer, by = "all_cancer") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_all_cancer, by = "all_cancer") %>%
  dplyr::left_join(trajectory_trajectory_all_cancer, by = "all_cancer") %>%
  dplyr::left_join(quartile3_trajectory_all_cancer, by = "all_cancer") %>%
  dplyr::select(all_cancer, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_all_cancer) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'all_cancer') %>%
  dplyr::mutate(variable="all_cancer", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))



## smoking

N_trajectory_smoking_status <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="smoking_status"]
n_trajectory_smoking_status <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="smoking_status"]
quartile1_trajectory_smoking_status <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="smoking_status"]
trajectory_trajectory_smoking_status <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="smoking_status"]
quartile3_trajectory_smoking_status <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="smoking_status"]

KWRS_smoking_status <- kruskal.test(smoking_status ~ trajectory_change, BMI_trajectories)
significance_smoking_status <- data.frame(t(sapply(KWRS_smoking_status, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_smoking_status <- significance_smoking_status$p.value

trajectory_smoking_status <- N_trajectory_smoking_status %>%
  dplyr::left_join(n_trajectory_smoking_status, by = "smoking_status") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(trajectory_trajectory_smoking_status, by = "smoking_status") %>%
  dplyr::left_join(quartile3_trajectory_smoking_status, by = "smoking_status") %>%
  dplyr::select(smoking_status, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_smoking_status) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'smoking_status') %>%
  dplyr::mutate(variable="smoking_status", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))



## precovid bmi

N_trajectory_precovid_bmi_category <- BMI_trajectories_DT [, .(N_population = length(trajectory_change)) , by="precovid_bmi_category"]
n_trajectory_precovid_bmi_category <- BMI_trajectories_DT [, .(N_missing = sum(is.na(trajectory_change))) , by="precovid_bmi_category"]
quartile1_trajectory_precovid_bmi_category <- BMI_trajectories_DT[, .( "25th" = quantile(trajectory_change, probs = c(0.25), na.rm = TRUE)), by="precovid_bmi_category"]
trajectory_trajectory_precovid_bmi_category <- BMI_trajectories_DT[, .( "50th" = quantile(trajectory_change, probs = c(0.5), na.rm = TRUE)), by="precovid_bmi_category"]
quartile3_trajectory_precovid_bmi_category <- BMI_trajectories_DT[, .( "75th" = quantile(trajectory_change, probs = c(0.75), na.rm = TRUE)), by="precovid_bmi_category"]

KWRS_precovid_bmi_category <- kruskal.test(precovid_bmi_category ~ trajectory_change, BMI_trajectories)
significance_precovid_bmi_category <- data.frame(t(sapply(KWRS_precovid_bmi_category, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_precovid_bmi_category <- significance_precovid_bmi_category$p.value

trajectory_precovid_bmi_category <- N_trajectory_precovid_bmi_category %>%
  dplyr::left_join(n_trajectory_precovid_bmi_category, by = "precovid_bmi_category") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_trajectory_precovid_bmi_category, by = "precovid_bmi_category") %>%
  dplyr::left_join(trajectory_trajectory_precovid_bmi_category, by = "precovid_bmi_category") %>%
  dplyr::left_join(quartile3_trajectory_precovid_bmi_category, by = "precovid_bmi_category") %>%
  dplyr::select(precovid_bmi_category, "N", N_population, "25th", '50th', "75th") %>%
  dplyr::mutate (p = p_precovid_bmi_category) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'precovid_bmi_category') %>%
  dplyr::mutate(variable="precovid_bmi_category", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))



trajectory_complete <- dplyr::bind_rows( trajectory_all, 
                                     trajectory_age_group_2, 
                                     trajectory_sex, 
                                     trajectory_region, 
                                     trajectory_imd, 
                                     trajectory_ethnic_no_miss,
                                     trajectory_eth_group_16,
                                     trajectory_hypertension, 
                                     trajectory_diabetes_t2,
                                     trajectory_diabetes_t1, 
                                     trajectory_learning_disability,
                                     trajectory_depression,
                                     trajectory_dementia,
                                     trajectory_psychosis_schiz_bipolar, 
                                     trajectory_COPD, 
                                     trajectory_asthma,
                                     trajectory_chronic_cardiac, 
                                     trajectory_stroke_and_TIA,
                                     trajectory_all_cancer,
                                     trajectory_smoking_status,
                                     trajectory_precovid_bmi_category)


write.csv (trajectory_complete, here::here ("output/data","trajectory_change_median_IQR.csv"))