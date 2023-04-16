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


### IMPUTED MODELS COMPARED TO NON IMPUTED
age <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  age_group_2, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_age')

print(1)

imp_age <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  age_group_2, data = BMI_imp_long_mids, family = binomial) 

imp_age <- summary(pool(imp_age),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_age')

print(2)


##

hypertension <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  hypertension, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_hypertension')


imp_hypertension <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  hypertension, data = BMI_imp_long_mids, family = binomial) 

imp_hypertension <- summary(pool(imp_hypertension),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_hypertension')


##

diabetes_t1 <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  diabetes_t1, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_diabetes_t1')


imp_diabetes_t1 <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  diabetes_t1, data = BMI_imp_long_mids, family = binomial) 

imp_diabetes_t1 <- summary(pool(imp_diabetes_t1),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_diabetes_t1')

##

diabetes_t2 <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  diabetes_t2, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_diabetes_t2')


imp_diabetes_t2 <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  diabetes_t2, data = BMI_imp_long_mids, family = binomial) 

imp_diabetes_t2 <- summary(pool(imp_diabetes_t2),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_diabetes_t2')


##

chronic_cardiac <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  chronic_cardiac, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_chronic_cardiac')


imp_chronic_cardiac <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  chronic_cardiac, data = BMI_imp_long_mids, family = binomial) 

imp_chronic_cardiac <- summary(pool(imp_chronic_cardiac),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_chronic_cardiac')

##
learning_disability <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  learning_disability, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_learning_disability')


imp_learning_disability <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  learning_disability, data = BMI_imp_long_mids, family = binomial) 

imp_learning_disability <- summary(pool(imp_learning_disability),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_learning_disability')

##

psychosis_schiz_bipolar <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  psychosis_schiz_bipolar, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_psychosis_schiz_bipolar')


imp_psychosis_schiz_bipolar <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  psychosis_schiz_bipolar, data = BMI_imp_long_mids, family = binomial) 

imp_psychosis_schiz_bipolar <- summary(pool(imp_psychosis_schiz_bipolar),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_psychosis_schiz_bipolar')

##

depression <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  depression, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_depression')


imp_depression <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  depression, data = BMI_imp_long_mids, family = binomial) 

imp_depression <- summary(pool(imp_depression),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_depression')

##

COPD <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  COPD, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_COPD')


imp_COPD <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  COPD, data = BMI_imp_long_mids, family = binomial) 

imp_COPD <- summary(pool(imp_COPD),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_COPD')

##

asthma <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  asthma, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_asthma')


imp_asthma <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  asthma, data = BMI_imp_long_mids, family = binomial) 

imp_asthma <- summary(pool(imp_asthma),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_asthma')

##

dementia <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  dementia, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_dementia')


imp_dementia <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  dementia, data = BMI_imp_long_mids, family = binomial) 

imp_dementia <- summary(pool(imp_dementia),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_dementia')

##

stroke_and_TIA <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  stroke_and_TIA, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_stroke_and_TIA')


imp_stroke_and_TIA <- glm.mids((rapid_change) ~ age_group_2 + sex + imd + eth_collapsed +  stroke_and_TIA, data = BMI_imp_long_mids, family = binomial) 

imp_stroke_and_TIA <- summary(pool(imp_stroke_and_TIA),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_stroke_and_TIA')

print(3)

univariate_models <- age %>% 
  bind_rows(hypertension, 
            diabetes_t1,
            diabetes_t2,
            chronic_cardiac, 
            learning_disability,
            psychosis_schiz_bipolar,
            depression,
            COPD,
            asthma, 
            dementia, 
            stroke_and_TIA)

univariate_imputed <- imp_age %>% 
  bind_rows(imp_hypertension, 
            imp_diabetes_t1,
            imp_diabetes_t2,
            imp_chronic_cardiac, 
            imp_learning_disability,
            imp_psychosis_schiz_bipolar,
            imp_depression,
            imp_COPD,
            imp_asthma, 
            imp_dementia, 
            imp_stroke_and_TIA)



print(4)


write.csv (univariate_models, here::here ("output/data", "CC_imputation_sample_multivariate.csv"))
write.csv (univariate_imputed, here::here ("output/data", "CC_imputation_multivariate.csv"))

