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
age <- glm(rapid_change ~ age_group_2, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_age')

print(1)

imp_age <- glm.mids((rapid_change) ~ age_group_2, data = BMI_imp_long_mids, family = binomial) 

imp_age <- summary(pool(imp_age),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_age')

print(2)
##
sex <- glm(rapid_change ~ sex, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_sex')


imp_sex <- glm.mids((rapid_change) ~ sex, data = BMI_imp_long_mids, family = binomial) 

imp_sex <- summary(pool(imp_sex),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_sex')

##

eth_collapsed <- glm(rapid_change ~ eth_collapsed, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_eth_collapsed')


imp_eth_collapsed <- glm.mids((rapid_change) ~ eth_collapsed, data = BMI_imp_long_mids, family = binomial) 

imp_eth_collapsed <- summary(pool(imp_eth_collapsed),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_eth_collapsed')


##

imd <- glm(rapid_change ~ imd, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_imd')


imp_imd <- glm.mids((rapid_change) ~ imd, data = BMI_imp_long_mids, family = binomial) 

imp_imd <- summary(pool(imp_imd),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_imd')

##
region <- glm(rapid_change ~ region, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_region')


imp_region <- glm.mids((rapid_change) ~ region, data = BMI_imp_long_mids, family = binomial) 

imp_region <- summary(pool(imp_region),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_region')


##

hypertension <- glm(rapid_change ~ hypertension, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_hypertension')


imp_hypertension <- glm.mids((rapid_change) ~ hypertension, data = BMI_imp_long_mids, family = binomial) 

imp_hypertension <- summary(pool(imp_hypertension),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_hypertension')


##

diabetes_t1 <- glm(rapid_change ~ diabetes_t1, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_diabetes_t1')


imp_diabetes_t1 <- glm.mids((rapid_change) ~ diabetes_t1, data = BMI_imp_long_mids, family = binomial) 

imp_diabetes_t1 <- summary(pool(imp_diabetes_t1),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_diabetes_t1')

##

diabetes_t2 <- glm(rapid_change ~ diabetes_t2, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_diabetes_t2')


imp_diabetes_t2 <- glm.mids((rapid_change) ~ diabetes_t2, data = BMI_imp_long_mids, family = binomial) 

imp_diabetes_t2 <- summary(pool(imp_diabetes_t2),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_diabetes_t2')


##

chronic_cardiac <- glm(rapid_change ~ chronic_cardiac, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_chronic_cardiac')


imp_chronic_cardiac <- glm.mids((rapid_change) ~ chronic_cardiac, data = BMI_imp_long_mids, family = binomial) 

imp_chronic_cardiac <- summary(pool(imp_chronic_cardiac),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_chronic_cardiac')

##
learning_disability <- glm(rapid_change ~ learning_disability, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_learning_disability')


imp_learning_disability <- glm.mids((rapid_change) ~ learning_disability, data = BMI_imp_long_mids, family = binomial) 

imp_learning_disability <- summary(pool(imp_learning_disability),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_learning_disability')

##

psychosis_schiz_bipolar <- glm(rapid_change ~ psychosis_schiz_bipolar, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_psychosis_schiz_bipolar')


imp_psychosis_schiz_bipolar <- glm.mids((rapid_change) ~ psychosis_schiz_bipolar, data = BMI_imp_long_mids, family = binomial) 

imp_psychosis_schiz_bipolar <- summary(pool(imp_psychosis_schiz_bipolar),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_psychosis_schiz_bipolar')

##

depression <- glm(rapid_change ~ depression, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_depression')


imp_depression <- glm.mids((rapid_change) ~ depression, data = BMI_imp_long_mids, family = binomial) 

imp_depression <- summary(pool(imp_depression),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_depression')

##

COPD <- glm(rapid_change ~ COPD, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_COPD')


imp_COPD <- glm.mids((rapid_change) ~ COPD, data = BMI_imp_long_mids, family = binomial) 

imp_COPD <- summary(pool(imp_COPD),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_COPD')

##

asthma <- glm(rapid_change ~ asthma, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_asthma')


imp_asthma <- glm.mids((rapid_change) ~ asthma, data = BMI_imp_long_mids, family = binomial) 

imp_asthma <- summary(pool(imp_asthma),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_asthma')

##

dementia <- glm(rapid_change ~ dementia, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_dementia')


imp_dementia <- glm.mids((rapid_change) ~ dementia, data = BMI_imp_long_mids, family = binomial) 

imp_dementia <- summary(pool(imp_dementia),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_dementia')

##

stroke_and_TIA <- glm(rapid_change ~ stroke_and_TIA, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'sample_stroke_and_TIA')


imp_stroke_and_TIA <- glm.mids((rapid_change) ~ stroke_and_TIA, data = BMI_imp_long_mids, family = binomial) 

imp_stroke_and_TIA <- summary(pool(imp_stroke_and_TIA),  conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::mutate(variable = 'imp_stroke_and_TIA')

print(3)

univariate_models <- age %>% 
  bind_rows(sex, 
            imd,
            eth_collapsed, 
            region, 
            hypertension, 
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
  bind_rows(imp_sex, 
            imp_imd,
            imp_eth_collapsed, 
            imp_region, 
            imp_hypertension, 
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
######################

function_1 <- function(var){
  v1 <- deparse(substitute(var))
  
  BMI_trajectories  %>% 
    tabyl({{var}}, rapid_change) %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(variable = (v1), .before=1) 
}




sex <-  function_1(sex)
age <- function_1(age_group_2)
ethnic <- function_1(eth_collapsed)
imd <- function_1(imd)
region <- function_1(region)
hypertension <- function_1(hypertension)
T1DM <- function_1(diabetes_t1)
T2DM <- function_1(diabetes_t2)
cardiac <- function_1(chronic_cardiac)
LD <- function_1(learning_disability)
SMI <- function_1(psychosis_schiz_bipolar)
depression <- function_1(depression)
asthma <- function_1(asthma)
COPD <- function_1(COPD)
stroke <- function_1(stroke_and_TIA)
dementia <- function_1(dementia)

print(5)

demographics <- age %>% 
  bind_rows(sex,
            ethnic, 
            imd,
            region, 
            hypertension, 
            T2DM, 
            T1DM, 
            cardiac,
            LD, 
            SMI, 
            depression, 
            asthma, 
            COPD, 
            stroke,
            dementia)

demographics <- demographics %>% 
  dplyr::mutate (N = demographics$`0` + demographics$`1`) %>% 
  dplyr::mutate(rapid = demographics$`1`) %>% 
  dplyr::select(c(variable, group, N, rapid)) 

demographics <- demographics %>% 
  dplyr::mutate(N = plyr::round_any(demographics$N, 5)) %>% 
  plyr::mutate(rapid = plyr::round_any(demographics$rapid, 5)) 


  



write.csv (univariate_models, here::here ("output/data", "CC_imputation_sample_univariate.csv"))
write.csv (univariate_imputed, here::here ("output/data", "CC_imputation_univariate.csv"))
write.csv (demographics, here::here ("output/data", "CC_imputation_sample_pop_demographic_round.csv"))

