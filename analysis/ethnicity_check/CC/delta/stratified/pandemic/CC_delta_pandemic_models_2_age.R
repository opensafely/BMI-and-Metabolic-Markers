
## M Samuel
## This R file looks at the stratified analyses of delta change
## Ethnicities are collapsed to prevent disclosure


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

# my_data <- read_csv("Documents/Academic GP/Open Safely/Dummy Data/CC_stratified_analysis_delta_data_pandemic.csv")

my_data <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_data_pandemic.csv"))


#########################
## Order ethnicity


my_data <- my_data %>%
  dplyr::mutate(eth_collapsed = factor(eth_collapsed, 
                                       levels = c("white",
                                                  "black", 
                                                  "south_asian", 
                                                  "mixed", 
                                                  "chinese_other")) ) 

my_data %>% 
  tabyl (eth_collapsed)

## IMD as factor
my_data <- my_data %>%
  dplyr::mutate(imd = as.factor(imd)) 



## Data table calculations
DT <- as.data.table(my_data)


#########################
## FILTER BY ethnicity

DT_18_39 <- DT %>%
  dplyr::filter(age_collapsed == "18-39")


DT_40_59 <- DT %>%
  dplyr::filter(age_collapsed == "40-59")


DT_60_79 <- DT %>%
  dplyr::filter(age_collapsed == "60- 79")

#############################




##

## eth and  imd


sex <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd + sex, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 



region <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethimd <- sex %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
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
  bind_rows(smoking_status) %>% 
  dplyr::mutate(model = "eth+imd")


## eth + sex




imd <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethsex <- imd %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
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
  bind_rows(smoking_status) %>% 
  dplyr::mutate(model = "eth+sex")


##################################
## eth and sex +imd


region <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT_18_39[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethseximd <- region %>%
  bind_rows(hypertension) %>%
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
  bind_rows(smoking_status) %>% 
  dplyr::mutate(model = "eth+sex+imd")


complete_18_39 <- models_ethimd %>% 
  bind_rows(models_ethsex)  %>% 
  bind_rows(models_ethseximd)

#######################
#############################

## 40_59



##

## eth and  imd


sex <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd + sex, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 



region <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethimd <- sex %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
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
  bind_rows(smoking_status) %>% 
  dplyr::mutate(model = "eth+imd")


## eth + sex




imd <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethsex <- imd %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
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
  bind_rows(smoking_status) %>% 
  dplyr::mutate(model = "eth+sex")


##################################
## eth and sex +imd


region <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT_40_59[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethseximd <- region %>%
  bind_rows(hypertension) %>%
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
  bind_rows(smoking_status) %>% 
  dplyr::mutate(model = "eth+sex+imd")


complete_40_59 <- models_ethimd %>% 
  bind_rows(models_ethsex)  %>% 
  bind_rows(models_ethseximd)

#######################
#############################

## 60_79



##

## eth and  imd


sex <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd + sex, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 



region <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed +  imd +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethimd <- sex %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
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
  bind_rows(smoking_status) %>% 
  dplyr::mutate(model = "eth+imd")


## eth + sex




imd <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethsex <- imd %>%
  bind_rows(region) %>%
  bind_rows(hypertension) %>%
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
  bind_rows(smoking_status) %>% 
  dplyr::mutate(model = "eth+sex")


##################################
## eth and sex +imd


region <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT_60_79[, glm(rapid_bmi_change ~ eth_collapsed + sex + imd +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ethseximd <- region %>%
  bind_rows(hypertension) %>%
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
  bind_rows(smoking_status) %>% 
  dplyr::mutate(model = "eth+sex+imd")


complete_60_79 <- models_ethimd %>% 
  bind_rows(models_ethsex)  %>% 
  bind_rows(models_ethseximd)

#######################
#############################




write_csv (complete_60_79, here::here ("output/data","CC_delta_pandemic_models_2_60_79.csv"))

write_csv (complete_40_59, here::here ("output/data","CC_delta_pandemic_models_2_40_59.csv"))

write_csv (complete_18_39, here::here ("output/data","CC_delta_pandemic_models_2_18_39.csv"))
