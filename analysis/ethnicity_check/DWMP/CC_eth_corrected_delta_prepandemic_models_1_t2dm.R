## Author: M Samuel 
## Date: 5th April 
##This file looks at the analysis of predictors of rapid prepandemic weight gain in a population limited to those with T2D


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



prepandemic <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_data_prepandemic.csv"))


## filter for patient with T2D
## Chunk of T2D specific analysis

prepandemic <- prepandemic %>% 
  dplyr::filter(diabetes_t2 == TRUE)

# create a variable with diabetes medication
# recode missing insulin and diabetic medication values as 0

prepandemic <- prepandemic %>% mutate_at(vars("insulin_meds", "oad_meds"), ~replace_na(.,0))


prepandemic <- prepandemic %>% dplyr::mutate(diabetes_med = case_when(
  insulin_meds == 1 ~ "insulin", 
  ((insulin_meds == 0) & (oad_meds == 1)) ~ "oad", 
  ((insulin_meds == 0) & (oad_meds == 0)) ~ "lifestyle"
))

prepandemic %>% tabyl(diabetes_med)


prepandemic %>% 
  tabyl(eth_collapsed)

##
my_data <- prepandemic



## Order meds
my_data <- my_data %>%
  dplyr::mutate(diabetes_med = factor(diabetes_med, 
                                      levels = c("lifestyle",
                                                 "oad", 
                                                 "insulin")))



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


age <- DT[, glm(rapid_bmi_change ~ age_group_2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


sex <- DT[, glm(rapid_bmi_change ~ sex, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


eth_group_16 <- DT[, glm(rapid_bmi_change ~eth_collapsed, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

imd <- DT[, glm(rapid_bmi_change ~ imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT[, glm(rapid_bmi_change ~ region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT[, glm(rapid_bmi_change ~ hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


chronic_cardiac <- DT[, glm(rapid_bmi_change ~ chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA <- DT[, glm(rapid_bmi_change ~ stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_med <- DT[, glm(rapid_bmi_change ~ diabetes_med, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


## Total Population


complete <- sex %>% 
  bind_rows(age) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(precovid_bmi_category) %>% 
  bind_rows(diabetes_med) %>% 
  dplyr::mutate(model = "univariate")

##
## age_group_2 adjusted


age <- DT[, glm(rapid_bmi_change ~ age_group_2 +  age_group_2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


sex <- DT[, glm(rapid_bmi_change ~ age_group_2 +  sex, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


eth_group_16 <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

imd <- DT[, glm(rapid_bmi_change ~ age_group_2 + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT[, glm(rapid_bmi_change ~ age_group_2 +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT[, glm(rapid_bmi_change ~ age_group_2 +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ age_group_2 +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ age_group_2 +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ age_group_2 +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ age_group_2 +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ age_group_2 +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ age_group_2 +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ age_group_2 +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ age_group_2 +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ age_group_2 + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_med <- DT[, glm(rapid_bmi_change ~ age_group_2 + diabetes_med, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 




## Total Population


models_age <- sex %>% 
  bind_rows(age) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(precovid_bmi_category) %>% 
  bind_rows(diabetes_med) %>%
  dplyr::mutate(model = "age")

#### 

## sex adjusted


age <- DT[, glm(rapid_bmi_change ~ sex +  age_group_2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


sex <- DT[, glm(rapid_bmi_change ~ sex +  sex, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


eth_group_16 <- DT[, glm(rapid_bmi_change ~ sex + eth_collapsed, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

imd <- DT[, glm(rapid_bmi_change ~ sex + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT[, glm(rapid_bmi_change ~ sex +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT[, glm(rapid_bmi_change ~ sex +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ sex +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ sex +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ sex +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ sex +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ sex +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ sex +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ sex +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ sex +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ sex + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_med <- DT[, glm(rapid_bmi_change ~ sex + diabetes_med, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 
## Total Population


models_sex <- sex %>% 
  bind_rows(age) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(precovid_bmi_category) %>% 
  bind_rows(diabetes_med) %>%
  dplyr::mutate(model = "sex")


## imd

age <- DT[, glm(rapid_bmi_change ~ imd +  age_group_2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


sex <- DT[, glm(rapid_bmi_change ~ imd +  sex, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


eth_group_16 <- DT[, glm(rapid_bmi_change ~ imd + eth_collapsed, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

imd <- DT[, glm(rapid_bmi_change ~ imd + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT[, glm(rapid_bmi_change ~ imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT[, glm(rapid_bmi_change ~ imd +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ imd + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_med <- DT[, glm(rapid_bmi_change ~ imd + diabetes_med, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_imd <- sex %>% 
  bind_rows(age) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(precovid_bmi_category) %>% 
  bind_rows(diabetes_med) %>%
  dplyr::mutate(model = "imd")


## ethnicity adjusted


age <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  age_group_2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


sex <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  sex, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


eth_group_16 <- DT[, glm(rapid_bmi_change ~ eth_collapsed + eth_collapsed, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

imd <- DT[, glm(rapid_bmi_change ~ eth_collapsed + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ eth_collapsed +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ eth_collapsed +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ eth_collapsed +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ eth_collapsed + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_med <- DT[, glm(rapid_bmi_change ~ eth_collapsed + diabetes_med, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethnicity <- sex %>% 
  bind_rows(age) %>%
  bind_rows(eth_group_16) %>%
  bind_rows(imd) %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) %>%
  bind_rows(stroke_and_TIA) %>%
  bind_rows(precovid_bmi_category) %>% 
  bind_rows(diabetes_med) %>%
  dplyr::mutate(model = "ethnicity")

complete <- complete %>% 
  dplyr::bind_rows(models_age) %>%
  dplyr::bind_rows(models_sex) %>%
  dplyr::bind_rows(models_imd) %>%
  dplyr::bind_rows(models_ethnicity) 


write_csv (complete, here::here ("output/data","CC_delta_prepandemic_models_1_t2d.csv"))