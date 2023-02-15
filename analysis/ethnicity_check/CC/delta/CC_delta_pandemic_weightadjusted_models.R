
## M Samuel
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


# my_data <- read_csv("Documents/Academic GP/Open Safely/Dummy Data/CC_delta_data.csv")
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
  dplyr::filter(pandemic_stage == "postcovid")

my_data  %>%
  tabyl(pandemic_stage)


## Data table calculations
DT <- as.data.table(my_data)


hypertension <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + hypertension, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")


diabetes_t1 <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + diabetes_t1, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")


diabetes_t2 <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + diabetes_t2, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")

chronic_cardiac <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + chronic_cardiac, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")


learning_disability <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + learning_disability, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")


depression <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + depression, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")


dementia <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + dementia, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")



psychosis_schiz_bipolar <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + psychosis_schiz_bipolar, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")



asthma <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + asthma, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")


COPD <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + COPD, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")


stroke_and_TIA <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category + stroke_and_TIA, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "precovid_weight")


complete<- hypertension %>%
  bind_rows(diabetes_t1) %>%
  bind_rows(diabetes_t2) %>%
  bind_rows(chronic_cardiac) %>%
  bind_rows(learning_disability) %>%
  bind_rows(depression) %>%
  bind_rows(dementia) %>%
  bind_rows(psychosis_schiz_bipolar) %>%
  bind_rows(asthma) %>%
  bind_rows(COPD) 

write_csv (complete, here::here ("output/data","CC_delta_pandemic_weight_adjusted_models.csv"))
