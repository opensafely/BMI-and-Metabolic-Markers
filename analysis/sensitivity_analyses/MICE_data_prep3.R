##  This R script develops the imputed data frame

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


BMI_trajectories <- read_csv (here::here ("output/data", "imputation_DF_for_impute.csv"))
BMI_imp_long <- read_csv (here::here ("output/data", "imputation_dataframe.csv"))

BMI_trajectories

BMI_imp_long_mids <- as.data.frame(BMI_imp_long)

BMI_imp_long

BMI_imp_long_mids<-as.mids(BMI_imp_long)

### IMPUTED MODELS COMPARED TO NON IMPUTED
age <- lm(trajectory_change ~ age_group_2, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)

age

imp_age <- with(BMI_imp_long_mids,
               lm(trajectory_change ~ age_group_2))

imp_age <- summary(pool(imp_age)) 
 
imp_age <- imp_age %>% 
  dplyr::mutate(variable = 'imp_age')




##
imp_sex <- with(BMI_imp_long_mids,
                lm(trajectory_change ~ sex))

imp_sex <- summary(pool(imp_sex)) 

imp_sex <- imp_sex %>% 
  dplyr::mutate(variable = 'imp_sex')

sex <- lm(trajectory_change ~ sex, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)

##
imp_ethnic <- with(BMI_imp_long_mids,
                   lm(trajectory_change ~ eth_group_16))

imp_ethnic <- summary(pool(imp_ethnic)) 

imp_ethnic <- imp_ethnic %>% 
  dplyr::mutate(variable = 'imp_ethnic')

ethnic <- lm(trajectory_change ~ eth_group_16, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)

##
imp_imd <- with(BMI_imp_long_mids,
                lm(trajectory_change ~ imd))

imp_imd <- summary(pool(imp_imd)) 

imp_imd <- imp_imd %>% 
  dplyr::mutate(variable = 'imp_imd')

imd <- lm(trajectory_change ~ imd, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


##
imp_region <- with(BMI_imp_long_mids,
                   lm(trajectory_change ~ region))

imp_region <- summary(pool(imp_region)) 

imp_region <- imp_region %>% 
  dplyr::mutate(variable = 'imp_region')

region <- lm(trajectory_change ~ region, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


##
imp_hypertension <- with(BMI_imp_long_mids,
                         lm(trajectory_change ~ hypertension))

imp_hypertension <- summary(pool(imp_hypertension)) 

imp_hypertension <- imp_hypertension %>% 
  dplyr::mutate(variable = 'imp_hypertension')

hypertension <- lm(trajectory_change ~ hypertension, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


##
imp_diabetes_t1 <- with(BMI_imp_long_mids,
                        lm(trajectory_change ~ diabetes_t1))

imp_diabetes_t1 <- summary(pool(imp_diabetes_t1)) 

imp_diabetes_t1 <- imp_diabetes_t1 %>% 
  dplyr::mutate(variable = 'imp_diabetes_t1')

diabetes_t1 <- lm(trajectory_change ~ diabetes_t1, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)

##
imp_diabetes_t2 <- with(BMI_imp_long_mids,
                        lm(trajectory_change ~ diabetes_t2))

imp_diabetes_t2 <- summary(pool(imp_diabetes_t2)) 

imp_diabetes_t2 <- imp_diabetes_t2 %>% 
  dplyr::mutate(variable = 'imp_diabetes_t2')

diabetes_t2 <- lm(trajectory_change ~ diabetes_t2, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


##
imp_chronic_cardiac <- with(BMI_imp_long_mids,
                            lm(trajectory_change ~ chronic_cardiac))

imp_chronic_cardiac <- summary(pool(imp_chronic_cardiac)) 

imp_chronic_cardiac <- imp_chronic_cardiac %>% 
  dplyr::mutate(variable = 'imp_chronic_cardiac')

chronic_cardiac <- lm(trajectory_change ~ chronic_cardiac, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


##
imp_learning_disability <- with(BMI_imp_long_mids,
                                lm(trajectory_change ~ learning_disability))

imp_learning_disability <- summary(pool(imp_learning_disability)) 

imp_learning_disability <- imp_learning_disability %>% 
  dplyr::mutate(variable = 'imp_learning_disability')

learning_disability <- lm(trajectory_change ~ learning_disability, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)

##
imp_psychosis_schiz_bipolar <- with(BMI_imp_long_mids,
                                    lm(trajectory_change ~ psychosis_schiz_bipolar))

imp_psychosis_schiz_bipolar <- summary(pool(imp_psychosis_schiz_bipolar)) 

imp_psychosis_schiz_bipolar <- imp_psychosis_schiz_bipolar %>% 
  dplyr::mutate(variable = 'imp_psychosis_schiz_bipolar')

psychosis_schiz_bipolar <- lm(trajectory_change ~ psychosis_schiz_bipolar, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


##
imp_depression <- with(BMI_imp_long_mids,
                       lm(trajectory_change ~ depression))

imp_depression <- summary(pool(imp_depression)) 

imp_depression <- imp_depression %>% 
  dplyr::mutate(variable = 'imp_depression')

depression <- lm(trajectory_change ~ depression, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


imp_COPD <- with(BMI_imp_long_mids,
                 lm(trajectory_change ~ COPD))

imp_COPD <- summary(pool(imp_COPD)) 

imp_COPD <- imp_COPD %>% 
  dplyr::mutate(variable = 'imp_COPD')

COPD <- lm(trajectory_change ~ COPD, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


##
imp_asthma <- with(BMI_imp_long_mids,
                   lm(trajectory_change ~ asthma))

imp_asthma <- summary(pool(imp_asthma)) 

imp_asthma <- imp_asthma %>% 
  dplyr::mutate(variable = 'imp_asthma')

asthma <- lm(trajectory_change ~ asthma, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)

##
imp_dementia <- with(BMI_imp_long_mids,
                     lm(trajectory_change ~ dementia))

imp_dementia <- summary(pool(imp_dementia)) 

imp_dementia <- imp_dementia %>% 
  dplyr::mutate(variable = 'imp_dementia')

dementia <- lm(trajectory_change ~ dementia, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


##
imp_stroke_and_TIA <- with(BMI_imp_long_mids,
                           lm(trajectory_change ~ stroke_and_TIA))

imp_stroke_and_TIA <- summary(pool(imp_stroke_and_TIA)) 

imp_stroke_and_TIA <- imp_stroke_and_TIA %>% 
  dplyr::mutate(variable = 'imp_stroke_and_TIA')

stroke_and_TIA <- lm(trajectory_change ~ stroke_and_TIA, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)

##
imp_smoking_status <- with(BMI_imp_long_mids,
                           lm(trajectory_change ~ smoking_status))

imp_smoking_status <- summary(pool(imp_smoking_status)) 

imp_smoking_status <- imp_smoking_status %>% 
  dplyr::mutate(variable = 'imp_smoking_status')

smoking_status <- lm(trajectory_change ~ smoking_status, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE)


##


univariate_models <- age %>% 
  bind_rows(imp_age, 
            sex, 
            imp_sex, 
            imp_ethnic, 
            ethnic, 
            imp_imd, 
            imd, 
            imp_region, 
            region, 
            imp_hypertension, 
            hypertension, 
            imp_diabetes_t1, 
            diabetes_t1,
            imp_diabetes_t2, 
            diabetes_t2,
            imp_chronic_cardiac, 
            chronic_cardiac,
            imp_learning_disability, 
            learning_disability,
            imp_psychosis_schiz_bipolar, 
            psychosis_schiz_bipolar,
            imp_depression, 
            depression,
            imp_COPD, 
            COPD,
            imp_asthma, 
            asthma,
            imp_dementia, 
            dementia, 
            imp_stroke_and_TIA, 
            stroke_and_TIA,
            imp_smoking_status, 
            smoking_status)


write.csv (univariate_models, here::here ("output/data", "imputation_univariate.csv"))
