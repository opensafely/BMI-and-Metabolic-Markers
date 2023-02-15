## M Samuel
## This R file defines the characteristics of the complete case delta pandemic and delta prepandemic - with data extracted in March 2022
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


univariate <- DT[, glm(rapid_bmi_change ~ precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "univariate")




models_age <- DT[, glm(rapid_bmi_change ~ age_group_2 +  precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "age")

#### 

## sex adjusted


models_sex <- DT[, glm(rapid_bmi_change ~ sex + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  %>% 
  dplyr::mutate(model = "sex")


## imd

models_imd <- DT[, glm(rapid_bmi_change ~ imd +  precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  %>% 
  dplyr::mutate(model = "imd")


## ethnicity adjusted

models_ethnicity <- DT[, glm(rapid_bmi_change ~ eth_16_corrected +  precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5))  %>% 
  dplyr::mutate(model = "ethnicity")


models_age_sex <- DT[, glm(rapid_bmi_change ~ age_group_2 +  sex + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "age+sex")


models_age_sex_eth <- DT[, glm(rapid_bmi_change ~ age_group_2 +  sex + eth_16_corrected + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "age+sex+eth")

models_age_sex_imd <- DT[, glm(rapid_bmi_change ~ age_group_2 +  sex + imd + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "age+sex+imd")

models_age_sex_imd_eth <- DT[, glm(rapid_bmi_change ~ age_group_2 +  sex + imd + eth_16_corrected + precovid_bmi_category, family = "binomial")] %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  dplyr::mutate(model = "age+sex+imd_eth")


complete <- univariate %>% 
  dplyr::bind_rows(models_age) %>%
  dplyr::bind_rows(models_sex) %>%
  dplyr::bind_rows(models_imd) %>%
  dplyr::bind_rows(models_ethnicity) %>% 
  dplyr::bind_rows(models_age_sex)%>% 
  dplyr::bind_rows(models_age_sex_eth)%>% 
  dplyr::bind_rows(models_age_sex_imd)%>% 
  dplyr::bind_rows(models_age_sex_imd_eth)





write_csv (complete, here::here ("output/data","CC_delta_pandemic_weight_models.csv"))
