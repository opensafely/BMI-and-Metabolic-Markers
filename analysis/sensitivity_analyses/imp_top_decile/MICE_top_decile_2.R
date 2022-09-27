##  This R script completes univariate analysis of predictors of top decile change in the imputed and sample data frame.

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


BMI_trajectories <- read_csv (here::here ("output/data", "imputation_sample_data_rapid_change.csv"))
BMI_imp_long <- read_csv (here::here ("output/data", "imputation_dataframe_rapid_change.csv"))

BMI_trajectories$imd <- factor(BMI_trajectories$imd)
BMI_imp_long$imd <- factor(BMI_imp_long$imd)

BMI_imp_long_mids<-as.mids(BMI_imp_long)  ## change data set into 'mids' to use MICE package



age <- glm(top_decile_change ~ age_group_2, family=binomial, data=BMI_trajectories) %>%
  broom::tidy(conf.int = TRUE, exp = TRUE)

imp_age <- with(BMI_imp_long_mids,
                glm(top_decile_change ~ age_group_2, family=binomial))

imp_age <- summary(pool(imp_age), conf.int = TRUE, exp = TRUE) 

imp_age <- imp_age %>% 
  dplyr::mutate(variable = 'imp_age')






##
sex <- glm(top_decile_change ~ sex, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_sex <- with(BMI_imp_long_mids,
                glm(top_decile_change ~ sex, family=binomial))

imp_sex <- summary(pool(imp_sex), conf.int = TRUE, exp = TRUE) 

imp_sex <- imp_sex %>% 
  dplyr::mutate(variable = 'imp_sex')



##
ethnic <- glm(top_decile_change ~   eth_group_16, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_ethnic <- with(BMI_imp_long_mids,
                   glm(top_decile_change ~   eth_group_16, family=binomial))

imp_ethnic <- summary(pool(imp_ethnic), conf.int = TRUE, exp = TRUE) 

imp_ethnic <- imp_ethnic %>% 
  dplyr::mutate(variable = 'imp_ethnic')

##
imd <- glm(top_decile_change ~ imd, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_imd <- with(BMI_imp_long_mids,
                glm(top_decile_change ~ imd, family=binomial))

imp_imd <- summary(pool(imp_imd), conf.int = TRUE, exp = TRUE) 

imp_imd <- imp_imd %>% 
  dplyr::mutate(variable = 'imp_imd')


##
region  <- glm(top_decile_change ~ region , family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_region  <- with(BMI_imp_long_mids,
                    glm(top_decile_change ~ region , family=binomial))

imp_region  <- summary(pool(imp_region ), conf.int = TRUE, exp = TRUE) 

imp_region  <- imp_region  %>% 
  dplyr::mutate(variable = 'imp_region')

##

hypertension <- glm(top_decile_change ~ hypertension, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_hypertension <- with(BMI_imp_long_mids,
                         glm(top_decile_change ~ hypertension, family=binomial))

imp_hypertension <- summary(pool(imp_hypertension), conf.int = TRUE, exp = TRUE) 

imp_hypertension <- imp_hypertension %>% 
  dplyr::mutate(variable = 'imp_hypertension')



##
diabetes_t1 <- glm(top_decile_change ~ diabetes_t1, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_diabetes_t1 <- with(BMI_imp_long_mids,
                        glm(top_decile_change ~ diabetes_t1, family=binomial))

imp_diabetes_t1 <- summary(pool(imp_diabetes_t1), conf.int = TRUE, exp = TRUE) 

imp_diabetes_t1 <- imp_diabetes_t1 %>% 
  dplyr::mutate(variable = 'imp_diabetes_t1')

##

diabetes_t2 <- glm(top_decile_change ~ diabetes_t2, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_diabetes_t2 <- with(BMI_imp_long_mids,
                        glm(top_decile_change ~ diabetes_t2, family=binomial))

imp_diabetes_t2 <- summary(pool(imp_diabetes_t2), conf.int = TRUE, exp = TRUE) 

imp_diabetes_t2 <- imp_diabetes_t2 %>% 
  dplyr::mutate(variable = 'imp_diabetes_t2')


##

chronic_cardiac <- glm(top_decile_change ~ chronic_cardiac, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_chronic_cardiac <- with(BMI_imp_long_mids,
                            glm(top_decile_change ~ chronic_cardiac, family=binomial))

imp_chronic_cardiac <- summary(pool(imp_chronic_cardiac), conf.int = TRUE, exp = TRUE) 

imp_chronic_cardiac <- imp_chronic_cardiac %>% 
  dplyr::mutate(variable = 'imp_chronic_cardiac')

##

learning_disability  <- glm(top_decile_change ~ learning_disability, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_learning_disability  <- with(BMI_imp_long_mids,
                                 glm(top_decile_change ~ learning_disability , family=binomial))

imp_learning_disability  <- summary(pool(imp_learning_disability ), conf.int = TRUE, exp = TRUE) 

imp_learning_disability  <- imp_learning_disability  %>% 
  dplyr::mutate(variable = 'imp_learning_disability')


##
psychosis_schiz_bipolar <- glm(top_decile_change ~ psychosis_schiz_bipolar, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_psychosis_schiz_bipolar <- with(BMI_imp_long_mids,
                                    glm(top_decile_change ~ psychosis_schiz_bipolar, family=binomial))

imp_psychosis_schiz_bipolar <- summary(pool(imp_psychosis_schiz_bipolar), conf.int = TRUE, exp = TRUE) 

imp_psychosis_schiz_bipolar <- imp_psychosis_schiz_bipolar %>% 
  dplyr::mutate(variable = 'imp_psychosis_schiz_bipolar')

##

depression <- glm(top_decile_change ~ depression, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_depression <- with(BMI_imp_long_mids,
                       glm(top_decile_change ~ depression, family=binomial))

imp_depression <- summary(pool(imp_depression), conf.int = TRUE, exp = TRUE) 

imp_depression <- imp_depression %>% 
  dplyr::mutate(variable = 'imp_depression')

##
COPD <- glm(top_decile_change ~ COPD, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_COPD <- with(BMI_imp_long_mids,
                 glm(top_decile_change ~ COPD, family=binomial))

imp_COPD <- summary(pool(imp_COPD), conf.int = TRUE, exp = TRUE) 

imp_COPD <- imp_COPD %>% 
  dplyr::mutate(variable = 'imp_COPD')

##
asthma  <- glm(top_decile_change ~ asthma, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_asthma  <- with(BMI_imp_long_mids,
                    glm(top_decile_change ~ asthma, family=binomial))

imp_asthma  <- summary(pool(imp_asthma    ), conf.int = TRUE, exp = TRUE) 

imp_asthma   <- imp_asthma     %>% 
  dplyr::mutate(variable = 'imp_asthma')


##
dementia  <- glm(top_decile_change ~ dementia, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_dementia  <- with(BMI_imp_long_mids,
                      glm(top_decile_change ~ dementia, family=binomial))

imp_dementia  <- summary(pool(imp_dementia    ), conf.int = TRUE, exp = TRUE) 

imp_dementia   <- imp_dementia     %>% 
  dplyr::mutate(variable = 'imp_dementia')

##
stroke_and_TIA  <- glm(top_decile_change ~ stroke_and_TIA, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_stroke_and_TIA  <- with(BMI_imp_long_mids,
                            glm(top_decile_change ~ stroke_and_TIA, family=binomial))

imp_stroke_and_TIA  <- summary(pool(imp_stroke_and_TIA    ), conf.int = TRUE, exp = TRUE) 

imp_stroke_and_TIA   <- imp_stroke_and_TIA     %>% 
  dplyr::mutate(variable = 'imp_stroke_and_TIA')

##

smoking_status  <- glm(top_decile_change ~ smoking_status, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_smoking_status  <- with(BMI_imp_long_mids,
                            glm(top_decile_change ~ smoking_status, family=binomial))

imp_smoking_status  <- summary(pool(imp_smoking_status    ), conf.int = TRUE, exp = TRUE) 

imp_smoking_status   <- imp_smoking_status     %>% 
  dplyr::mutate(variable = 'imp_smoking_status')


names(BMI_trajectories)



####





univariate_models <- age %>% 
  bind_rows(imp_age, 
            sex, 
            imp_sex, 
            imp_ethnic, 
            ethnic, 
            imp_imd, 
            imd, 
            imp_region, 
            region, #
            imp_hypertension, 
            hypertension, #
            imp_diabetes_t1, 
            diabetes_t1,#
            imp_diabetes_t2, 
            diabetes_t2,#
            imp_chronic_cardiac, 
            chronic_cardiac,#
            imp_learning_disability, 
            learning_disability,#
            imp_psychosis_schiz_bipolar, 
            psychosis_schiz_bipolar, #
            imp_depression, 
            depression,#
            imp_COPD, 
            COPD, #
            imp_asthma, 
            asthma, #
            imp_dementia, 
            dementia, #
            imp_stroke_and_TIA, 
            stroke_and_TIA,#
            imp_smoking_status, 
            smoking_status)#

write.csv (univariate_models, here::here ("output/data", "univariate_topdecile_imputation.csv"))