## Author: M Samuel 
## Date: 5th April 


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


## filter for patient with T2D
## Chunk of T2D specific analysis

pandemic <- pandemic %>% 
  dplyr::filter(diabetes_t2 == TRUE)

# create a variable with diabetes medication
# recode missing insulin and diabetic medication values as 0

pandemic <- pandemic %>% mutate_at(vars("insulin_meds", "oad_meds"), ~replace_na(.,0))


pandemic <- pandemic %>% dplyr::mutate(diabetes_med = case_when(
  insulin_meds == 1 ~ "insulin", 
  ((insulin_meds == 0) & (oad_meds == 1)) ~ "oad", 
  ((insulin_meds == 0) & (oad_meds == 0)) ~ "lifestyle"
))

pandemic %>% tabyl(diabetes_med)


pandemic %>% 
  tabyl(eth_collapsed)

##
my_data <- pandemic



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


##############################
############################## 

# male

data_male <- my_data %>% 
  dplyr::filter((sex == "M"))

## Data table calculations
DT <- as.data.table(data_male)


age_group_2 <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 



hypertension <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


chronic_cardiac <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_med <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + diabetes_med, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


## Total Population


complete_male <- age_group_2 %>% 
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
  dplyr::mutate(model = "eth, imd, age")







##############################
############################## 

# female

data_female <- my_data %>% 
  dplyr::filter((sex == "F"))

## Data table calculations
DT <- as.data.table(data_female)


age_group_2 <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 



hypertension <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


chronic_cardiac <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_med <- DT[, glm(rapid_bmi_change ~ age_group_2 + eth_collapsed + imd + diabetes_med, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


## Total Population


complete_female <- age_group_2 %>% 
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





write_csv (complete_male, here::here ("output/data","CC_delta_pandemic_models_2_t2d_male.csv"))
write_csv (complete_female, here::here ("output/data","CC_delta_pandemic_models_2_t2d_female.csv"))
