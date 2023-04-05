## Author: M Samuel 
## Date: 5th April 
##This file looks at the analysis of predictors of rapid pandemic weight gain in a population limited to those with T2D


library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(purrr)
library(broom)
library(data.table)
library(janitor)
library(skimr)
library(ggplot2)
library(gtsummary)



pandemic <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_data_pandemic.csv"))


### FILTER for hypertensives

pandemic <- pandemic %>% 
  dplyr::filter(hypertension == TRUE)





pandemic %>% 
  tabyl(eth_collapsed)

##
my_data <- pandemic




## Order ethnicity
my_data <- my_data %>%
  dplyr::mutate(eth_collapsed = factor(eth_collapsed, 
                                       levels = c("white",
                                                  "black",
                                                  "south_asian",
                                                  "chinese_other",
                                                  "mixed")) ) 


## IMD as factor
my_data <- my_data %>%
  dplyr::mutate(imd = as.factor(imd)) 





## Data table calculations
DT <- as.data.table(my_data)

## age and sex


eth_group_16 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

imd <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_t2 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


 

## Total Population


models_agesex <- eth_group_16 %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(precovid_bmi_category) %>% 
  dplyr::mutate(model = "age+sex")


##

## age and sex and imd





eth_group_16 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd + eth_collapsed, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 



region <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_t2 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 




## Total Population


models_ageseximd <- eth_group_16 %>%
  bind_rows(region) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(precovid_bmi_category) %>% 
  dplyr::mutate(model = "age+sex+imd")


## age and sex and ethnicity




imd <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t2 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 





models_agesexeth <- imd %>%
  bind_rows(region) %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(precovid_bmi_category) %>% 
  dplyr::mutate(model = "age+sex+eth")


##################################
## age and sex and ethnicity +imd


region <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t2 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


chronic_cardiac <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_collapsed + imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 




precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  eth_collapsed + imd + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 



## Total Population


models_agesexethimd <- region %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(precovid_bmi_category) %>%
  dplyr::mutate(model = "age+sex+eth+imd")


complete <- models_agesex %>% 
  bind_rows(models_ageseximd) %>% 
  bind_rows(models_agesexeth)  %>% 
  bind_rows(models_agesexethimd)




write_csv (complete, here::here ("output/data","CC_delta_pandemic_models_2_hypertension.csv"))