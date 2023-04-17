##  This R script completes unvariate analyses in the imputed and sample data frame

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
library(mice)


BMI_trajectories <- read_csv (here::here ("output/data", "CC_imputation_DF_for_impute.csv"))
BMI_imp_long <- read_csv (here::here ("output/data", "CC_imputation_dataframe.csv"))





BMI_trajectories


BMI_trajectories$imd <- factor(BMI_trajectories$imd, 
                               levels = c('1','2','3','4','5'))


BMI_imp_long$imd <- factor(BMI_imp_long$imd, 
                           levels = c('1','2','3','4','5'))






BMI_trajectories$eth_16_corrected <- factor(BMI_trajectories$eth_16_corrected, 
                                        levels = c(
                                          "White_British",
                                          "White_Irish",
                                          "Other_White",
                                          "Indian",
                                          "Pakistani",
                                          "Bangladeshi",
                                          "Other_Asian",
                                          "Caribbean",
                                          "African",
                                          "Other_Black",
                                          "Chinese",
                                          "White_Asian",
                                          "White_Black_Carib",
                                          "White_Black_African",
                                          "Other_Mixed",
                                          "Other",
                                          "Missing"))


BMI_trajectories<- BMI_trajectories  %>% 
  dplyr::mutate(eth_collapsed = case_when(
    eth_16_corrected ==   "White_British" ~ "white",
    eth_16_corrected ==  "White_Irish" ~ "white",
    eth_16_corrected ==  "Other_White"  ~ "white",
    eth_16_corrected == "White_Black_Carib" ~ "mixed",
    eth_16_corrected == "White_Black_African" ~ "mixed",
    eth_16_corrected == "White_Asian" ~ "mixed",
    eth_16_corrected == "Other_Mixed" ~ "mixed",
    eth_16_corrected == "Indian" ~ "south_asian",
    eth_16_corrected == "Pakistani" ~ "south_asian",
    eth_16_corrected == "Bangladeshi" ~ "south_asian",
    eth_16_corrected == "Other_Asian" ~ "chinese_other",
    eth_16_corrected == "Chinese" ~ "chinese_other",
    eth_16_corrected == "Caribbean" ~ "black",
    eth_16_corrected == "African" ~ "black",
    eth_16_corrected == "Other_Black" ~ "black",
    eth_16_corrected == "Other" ~ "chinese_other"
  )) 


BMI_imp_long$eth_16_corrected <- factor(BMI_imp_long$eth_16_corrected, 
                                    levels = c(
                                      "White_British",
                                      "White_Irish",
                                      "Other_White",
                                      "Indian",
                                      "Pakistani",
                                      "Bangladeshi",
                                      "Other_Asian",
                                      "Caribbean",
                                      "African",
                                      "Other_Black",
                                      "Chinese",
                                      "White_Asian",
                                      "White_Black_Carib",
                                      "White_Black_African",
                                      "Other_Mixed",
                                      "Other",
                                      "Missing"))



BMI_imp_long <- BMI_imp_long %>% 
  dplyr::mutate(eth_collapsed = case_when(
    eth_16_corrected ==   "White_British" ~ "white",
    eth_16_corrected ==  "White_Irish" ~ "white",
    eth_16_corrected ==  "Other_White"  ~ "white",
    eth_16_corrected == "White_Black_Carib" ~ "mixed",
    eth_16_corrected == "White_Black_African" ~ "mixed",
    eth_16_corrected == "White_Asian" ~ "mixed",
    eth_16_corrected == "Other_Mixed" ~ "mixed",
    eth_16_corrected == "Indian" ~ "south_asian",
    eth_16_corrected == "Pakistani" ~ "south_asian",
    eth_16_corrected == "Bangladeshi" ~ "south_asian",
    eth_16_corrected == "Other_Asian" ~ "chinese_other",
    eth_16_corrected == "Chinese" ~ "chinese_other",
    eth_16_corrected == "Caribbean" ~ "black",
    eth_16_corrected == "African" ~ "black",
    eth_16_corrected == "Other_Black" ~ "black",
    eth_16_corrected == "Other" ~ "chinese_other"
  )) 


BMI_trajectories$eth_collapsed <- factor(BMI_trajectories$eth_collapsed, 
                               levels = c('white','black','south_asian','chinese_other','mixed'))


BMI_imp_long$eth_collapsed <- factor(BMI_imp_long$eth_collapsed, 
                               levels = c('white','black','south_asian','chinese_other','mixed'))

BMI_imp_long %>% 
tabyl(eth_collapsed)

BMI_trajectories %>% 
tabyl(eth_collapsed)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(rapid_change = case_when(
    bmi_change_cat == "over 0.5" ~ 1, 
    bmi_change_cat != "over 0.5"  ~ 0
  )) 

BMI_trajectories %>% 
  tabyl(bmi_change_cat, rapid_change)



BMI_imp_long <- BMI_imp_long %>% 
  dplyr::mutate(rapid_change = case_when(
    bmi_change_cat == "over 0.5" ~ 1, 
    bmi_change_cat != "over 0.5"  ~ 0
  )) 

BMI_imp_long %>% 
  tabyl(bmi_change_cat, rapid_change)

BMI_imp_long_mids<-as.mids(BMI_imp_long)


#####################################
#####################################

### IMPUTED MODELS COMPARED TO NON IMPUTED
age_1 <- glm(rapid_change ~ eth_collapsed, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_eth_univariate')

print(1)

imp_age_1 <- glm.mids((rapid_change) ~ eth_collapsed, data = BMI_imp_long_mids, family = binomial) 

imp_age_1 <- summary(pool(imp_age_1),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_eth_univariate')

print(2)

## multivariate
age_2 <- glm(rapid_change ~ eth_collapsed + sex + imd + age_group_2, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_eth_multivariate')

print(1)

imp_age_2 <- glm.mids((rapid_change) ~ eth_collapsed + sex + imd +  age_group_2, data = BMI_imp_long_mids, family = binomial) 

imp_age_2 <- summary(pool(imp_age_2),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_eth_multivariate')

print(2)
####

age <- age_1 %>% 
dplyr::bind_rows(age_2)


imp_age <- imp_age_1 %>% 
dplyr::bind_rows(imp_age_2)

write.csv (age, here::here ("output/data", "CC_imputation_sample_multivariate_ethorder.csv"))
write.csv (imp_age, here::here ("output/data", "CC_imputation_multivariate_ethorder.csv"))
