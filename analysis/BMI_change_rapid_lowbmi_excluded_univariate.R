## M Samuel 
## 5th May 2022
## This script checks factors associated with rapid weight gain if patients with cancer and a low BMI prepandemic are removed


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

# webshot::install_phantomjs()



## Read in files
BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectory_data_long.feather"))

colnames(BMI_trajectories)





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
                "bmi_change_cat", 
                "precovid_bmi_category", 
                "pandemic_stage")

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(rapid_bmi_change = case_when(
    bmi_change_cat == 'over 0.5' ~ 1, 
    bmi_change_cat != 'over 0.5' ~ 0, 
  ))

BMI_trajectories$precovid_bmi_category <- factor(BMI_trajectories$precovid_bmi_category, levels = c("healthy","overweight", "obese", "underweight"))



explanatory_vars <- c("sex",
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
                      "smoking_status", 
                      "ethnic_no_miss",         
                      "eth_group_16",           
                      "precovid_bmi_category")



##*** Change to code.  FILTER OUT UNDERWEIGHT AND THOSE WITH CANCER
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(all_cancer == FALSE)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(precovid_bmi_category != "underweight")

BMI_trajectories %>% 
  tabyl(all_cancer)

BMI_trajectories %>% 
  tabyl(precovid_bmi_category)

## *** Change to code complete


## Precovid analysis proportions in groups


### Precovid analysis
precovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "precovid")

models_precov_rapidinc_bmi_univar <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = precovid_change)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))

models_precov_rapidinc_bmi_univar <- models_precov_rapidinc_bmi_univar %>%
  dplyr::mutate(stage = "precovid", .before = 1)




## population composition

# function to calculate
population_demog_function2 <- function(data, var){
  v1 <- deparse(substitute(var))
  
  data %>%
    tabyl({{var}}, rapid_bmi_change) %>% 
    dplyr::mutate(N_total = c(not_rapid) + c(rapid)) %>% 
    dplyr::mutate(percent_rapid = rapid/N_total*100) %>% 
    dplyr::mutate(rapid = plyr::round_any(rapid, 5)) %>% 
    dplyr::mutate(N_total = plyr::round_any(N_total, 5)) %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(variable = (v1))
}



## precovid population counts 



precovid_change$rapid_bmi_change[precovid_change$rapid_bmi_change==0] <- "not_rapid"
precovid_change$rapid_bmi_change[precovid_change$rapid_bmi_change==1] <- "rapid"


precovid_change %>% 
  tabyl(sex, rapid_bmi_change) %>% 
  dplyr::mutate(N_total = c(not_rapid) + c(rapid)) %>% 
  dplyr::mutate(percent_rapid = rapid/N_total*100) %>% 
  dplyr::mutate(rapid = plyr::round_any(rapid, 5)) %>% 
  dplyr::mutate(N_total = plyr::round_any(N_total, 5)) 




population_demog_function2(precovid_change, sex)

sex <- population_demog_function2(precovid_change, sex) %>% 
  dplyr::mutate(group = as.factor(group))


age_group_2 <- population_demog_function2(precovid_change, age_group_2) %>% 
  dplyr::mutate(group = as.factor(group))

region <- population_demog_function2(precovid_change, region) %>% 
  dplyr::mutate(group = as.factor(group))

imd <- population_demog_function2(precovid_change, imd) %>% 
  dplyr::mutate(group = as.factor(group))


hypertension <- population_demog_function2(precovid_change, hypertension) %>% 
  dplyr::mutate(group = as.factor(group)) 

diabetes_t1 <- population_demog_function2(precovid_change, diabetes_t1) %>% 
  dplyr::mutate(group = as.factor(group))

diabetes_t2 <- population_demog_function2(precovid_change, diabetes_t1) %>% 
  dplyr::mutate(group = as.factor(group))

learning_disability <- population_demog_function2(precovid_change,learning_disability) %>% 
  dplyr::mutate(group = as.factor(group))

depression <- population_demog_function2(precovid_change, depression) %>% 
  dplyr::mutate(group = as.factor(group))

psychosis_schiz_bipolar <- population_demog_function2(precovid_change, psychosis_schiz_bipolar) %>% 
  dplyr::mutate(group = as.factor(group))

dementia <- population_demog_function2(precovid_change, dementia) %>% 
  dplyr::mutate(group = as.factor(group))

asthma <- population_demog_function2(precovid_change, asthma) %>% 
  dplyr::mutate(group = as.factor(group))


COPD <- population_demog_function2(precovid_change, COPD) %>% 
  dplyr::mutate(group = as.factor(group))

stroke_and_TIA <- population_demog_function2(precovid_change, stroke_and_TIA) %>% 
  dplyr::mutate(group = as.factor(group))

chronic_cardiac <- population_demog_function2(precovid_change, chronic_cardiac) %>% 
  dplyr::mutate(group = as.factor(group))


smoking_status <- population_demog_function2(precovid_change, smoking_status) %>% 
  dplyr::mutate(group = as.factor(group))

ethnic_no_miss <- population_demog_function2(precovid_change, ethnic_no_miss) %>% 
  dplyr::mutate(group = as.factor(group))


eth_group_16 <- population_demog_function2(precovid_change, eth_group_16) %>% 
  dplyr::mutate(group = as.factor(group))

precovid_bmi_category <- population_demog_function2(precovid_change, precovid_bmi_category) %>% 
  dplyr::mutate(group = as.factor(group))




precovid_demog <- bind_rows(sex, 
                            age_group_2, 
                            ethnic_no_miss,
                            eth_group_16,
                            region, 
                            imd, 
                            hypertension,
                            diabetes_t1,
                            diabetes_t2,
                            learning_disability,
                            depression,
                            psychosis_schiz_bipolar,
                            dementia,
                            asthma,
                            COPD,
                            stroke_and_TIA,
                            chronic_cardiac,
                            smoking_status,
                            precovid_bmi_category) %>% 
  dplyr::mutate(stage = "precovid", .before=1)



## postcovid

postcovid_change <- BMI_trajectories %>% 
  dplyr::filter(pandemic_stage == "postcovid")

models_postcov_rapidinc_bmi_univar <- explanatory_vars %>%       # begin with variables of interest
  str_c("rapid_bmi_change ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = postcovid_change)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))


models_postcov_rapidinc_bmi_univar <- models_postcov_rapidinc_bmi_univar %>% 
  dplyr::mutate(stage="postcovid", .before=1)









postcovid_change$rapid_bmi_change[postcovid_change$rapid_bmi_change==0] <- "not_rapid"
postcovid_change$rapid_bmi_change[postcovid_change$rapid_bmi_change==1] <- "rapid"


postcovid_change %>% 
  tabyl(sex, rapid_bmi_change) %>% 
  dplyr::mutate(N_total = c(not_rapid) + c(rapid)) %>% 
  dplyr::mutate(percent_rapid = rapid/N_total*100) %>% 
  dplyr::mutate(rapid = plyr::round_any(rapid, 5)) %>% 
  dplyr::mutate(N_total = plyr::round_any(N_total, 5)) 




population_demog_function2(postcovid_change, sex)

sex <- population_demog_function2(postcovid_change, sex) %>% 
  dplyr::mutate(group = as.factor(group))


age_group_2 <- population_demog_function2(postcovid_change, age_group_2) %>% 
  dplyr::mutate(group = as.factor(group))

region <- population_demog_function2(postcovid_change, region) %>% 
  dplyr::mutate(group = as.factor(group))

imd <- population_demog_function2(postcovid_change, imd) %>% 
  dplyr::mutate(group = as.factor(group))


hypertension <- population_demog_function2(postcovid_change, hypertension) %>% 
  dplyr::mutate(group = as.factor(group)) 

diabetes_t1 <- population_demog_function2(postcovid_change, diabetes_t1) %>% 
  dplyr::mutate(group = as.factor(group))

diabetes_t2 <- population_demog_function2(postcovid_change, diabetes_t1) %>% 
  dplyr::mutate(group = as.factor(group))

learning_disability <- population_demog_function2(postcovid_change,learning_disability) %>% 
  dplyr::mutate(group = as.factor(group))

depression <- population_demog_function2(postcovid_change, depression) %>% 
  dplyr::mutate(group = as.factor(group))

psychosis_schiz_bipolar <- population_demog_function2(postcovid_change, psychosis_schiz_bipolar) %>% 
  dplyr::mutate(group = as.factor(group))

dementia <- population_demog_function2(postcovid_change, dementia) %>% 
  dplyr::mutate(group = as.factor(group))

asthma <- population_demog_function2(postcovid_change, asthma) %>% 
  dplyr::mutate(group = as.factor(group))


COPD <- population_demog_function2(postcovid_change, COPD) %>% 
  dplyr::mutate(group = as.factor(group))

stroke_and_TIA <- population_demog_function2(postcovid_change, stroke_and_TIA) %>% 
  dplyr::mutate(group = as.factor(group))

chronic_cardiac <- population_demog_function2(postcovid_change, chronic_cardiac) %>% 
  dplyr::mutate(group = as.factor(group))


smoking_status <- population_demog_function2(postcovid_change, smoking_status) %>% 
  dplyr::mutate(group = as.factor(group))

ethnic_no_miss <- population_demog_function2(postcovid_change, ethnic_no_miss) %>% 
  dplyr::mutate(group = as.factor(group))


eth_group_16 <- population_demog_function2(postcovid_change, eth_group_16) %>% 
  dplyr::mutate(group = as.factor(group))

precovid_bmi_category <- population_demog_function2(postcovid_change, precovid_bmi_category) %>% 
  dplyr::mutate(group = as.factor(group))




postcovid_demog <- bind_rows(sex, 
                             age_group_2, 
                             ethnic_no_miss,
                             eth_group_16,
                             region, 
                             imd, 
                             hypertension,
                             diabetes_t1,
                             diabetes_t2,
                             learning_disability,
                             depression,
                             psychosis_schiz_bipolar,
                             dementia,
                             asthma,
                             COPD,
                             stroke_and_TIA,
                             chronic_cardiac,
                             smoking_status,
                             precovid_bmi_category) %>% 
  dplyr::mutate(stage = "postcovid", .before=1) 











models_univariate <- models_precov_rapidinc_bmi_univar %>% 
  bind_rows(models_postcov_rapidinc_bmi_univar)


demog <- precovid_demog %>% 
  bind_rows(postcovid_demog) %>% 
  dplyr::select(stage,variable,group,not_rapid,rapid,N_total,percent_rapid)

### Write outputs



write_csv (models_univariate, here::here ("output/data","rapid_bmi_change_cancerandlowbmi_removed_univariate.csv"))
write_csv (demog, here::here ("output/data","rapid_bmi_change_cancerandlowbmi_removed_popcharac.csv"))