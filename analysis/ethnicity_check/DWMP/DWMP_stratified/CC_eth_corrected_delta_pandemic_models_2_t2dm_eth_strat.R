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

# black

data_black <- my_data %>% 
  dplyr::filter((eth_collapsed == "black"))




## Data table calculations
DT <- as.data.table(data_black)
sex <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


hypertension <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


chronic_cardiac <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_med <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + diabetes_med, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


## Total Population


complete_black <- sex %>% 
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
  dplyr::mutate(model = "age, sex, imd")





##############################
############################## 

# asian

data_asian <- my_data %>% 
  dplyr::filter((eth_collapsed == "south_asian"))



## Data table calculations
DT <- as.data.table(data_asian)
sex <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


hypertension <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


chronic_cardiac <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

learning_disability <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

depression <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

dementia <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

asthma <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

COPD<- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

stroke_and_TIA <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 

precovid_bmi_category <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


diabetes_med <- DT[, glm(rapid_bmi_change ~ sex + age_group_2 + imd + diabetes_med, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) 


## Total Population


complete_south_asian <- sex %>% 
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
  dplyr::mutate(model = "age, sex, imd")





write_csv (complete_black, here::here ("output/data","CC_delta_pandemic_models_2_t2d_black.csv"))
write_csv (complete_south_asian, here::here ("output/data","CC_delta_pandemic_models_2_t2d_south_asian.csv"))
