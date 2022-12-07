
## M Samuel
## This R file defines the OR of having rapid BMI change
## The code can be applied to substrata population by filtering at the first stage.
## The total in each group, numbers with BMI data, mean and median BMI data are calculated


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



my_data <- read_csv (here::here ("output/data", "CC_delta_data.csv"))

my_data <- my_data %>% 
  dplyr::select(-c(ethnic_no_miss, eth_group_16))


## Order ethnicity
my_data <- my_data %>%
  dplyr::mutate(eth_16_corrected = factor(eth_16_corrected, 
                                          levels = c("White_British",
                                                     "White_Irish",
                                                     "Other_White",
                                                     "White_Black_Carib",
                                                     "White_Black_African",
                                                     "White_Asian",
                                                     "Other_Mixed",
                                                     "Indian",
                                                     "Pakistani",
                                                     "Bangladeshi",
                                                     "Other_Asian",
                                                     "Chinese",
                                                     "Caribbean",
                                                     "African",
                                                     "Other_Black",
                                                     "Other")) ) 


## IMD as factor
my_data <- my_data %>%
  dplyr::mutate(imd = as.factor(imd)) 


## filter pandemic stage
my_data <- my_data %>%
  dplyr::filter(pandemic_stage == "precovid")

my_data  %>%
  tabyl(pandemic_stage)


## Data table calculations
DT <- as.data.table(my_data)

## age and sex


eth_group_16 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

imd <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  hypertension, family = "binomial")] %>% 
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

smoking_status <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_agesex <- eth_group_16 %>%
  bind_rows(imd) %>%
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
  dplyr::mutate(model = "age+sex")


##

## age and sex and imd





eth_group_16 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd + eth_16_corrected, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 



region <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  hypertension, family = "binomial")] %>% 
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

smoking_status <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + imd +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_ageseximd <- eth_group_16 %>%
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
  dplyr::mutate(model = "age+sex+imd")


## age and sex and ethnicity




imd <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

region <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_agesexeth <- imd %>%
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
  dplyr::mutate(model = "age+sex+eth")


##################################
## age and sex and ethnicity +imd


region <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  region, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

hypertension <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

diabetes_t1 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  

diabetes_t2 <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA<- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

smoking_status <- DT[, glm(rapid_bmi_change ~ age_group_2 + sex + eth_16_corrected + imd +  smoking_status, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

## Total Population


models_agesexethimd <- region %>%
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
  dplyr::mutate(model = "age+sex+eth+imd")


complete <- models_agesex %>% 
  bind_rows(models_ageseximd) %>% 
  bind_rows(models_agesexeth)  %>% 
  bind_rows(models_agesexethimd)




write_csv (complete, here::here ("output/data","CC_delta_precovid_models_2.csv"))