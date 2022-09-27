##  This R script completes univariate analysis of predictors of rapid weight gain in the imputed and sample data frame.

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













##
ethnic <- glm(postcovid_rapid ~ age_group_2 + sex +   eth_group_16, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_ethnic <- with(BMI_imp_long_mids,
                   glm(postcovid_rapid ~ age_group_2 + sex +   eth_group_16, family=binomial))

imp_ethnic <- summary(pool(imp_ethnic), conf.int = TRUE, exp = TRUE) 

imp_ethnic <- imp_ethnic %>% 
  dplyr::mutate(variable = 'imp_ethnic')

##
imd <- glm(postcovid_rapid ~ age_group_2 + sex + imd, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_imd <- with(BMI_imp_long_mids,
                glm(postcovid_rapid ~ age_group_2 + sex + imd, family=binomial))

imp_imd <- summary(pool(imp_imd), conf.int = TRUE, exp = TRUE) 

imp_imd <- imp_imd %>% 
  dplyr::mutate(variable = 'imp_imd')


##
region  <- glm(postcovid_rapid ~ age_group_2 + sex + region , family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_region  <- with(BMI_imp_long_mids,
                    glm(postcovid_rapid ~ age_group_2 + sex + region , family=binomial))

imp_region  <- summary(pool(imp_region ), conf.int = TRUE, exp = TRUE) 

imp_region  <- imp_region  %>% 
  dplyr::mutate(variable = 'imp_region')

##

hypertension <- glm(postcovid_rapid ~ age_group_2 + sex + hypertension, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_hypertension <- with(BMI_imp_long_mids,
                         glm(postcovid_rapid ~ age_group_2 + sex + hypertension, family=binomial))

imp_hypertension <- summary(pool(imp_hypertension), conf.int = TRUE, exp = TRUE) 

imp_hypertension <- imp_hypertension %>% 
  dplyr::mutate(variable = 'imp_hypertension')



##
diabetes_t1 <- glm(postcovid_rapid ~ age_group_2 + sex + diabetes_t1, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_diabetes_t1 <- with(BMI_imp_long_mids,
                        glm(postcovid_rapid ~ age_group_2 + sex + diabetes_t1, family=binomial))

imp_diabetes_t1 <- summary(pool(imp_diabetes_t1), conf.int = TRUE, exp = TRUE) 

imp_diabetes_t1 <- imp_diabetes_t1 %>% 
  dplyr::mutate(variable = 'imp_diabetes_t1')

##

diabetes_t2 <- glm(postcovid_rapid ~ age_group_2 + sex + diabetes_t2, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_diabetes_t2 <- with(BMI_imp_long_mids,
                        glm(postcovid_rapid ~ age_group_2 + sex + diabetes_t2, family=binomial))

imp_diabetes_t2 <- summary(pool(imp_diabetes_t2), conf.int = TRUE, exp = TRUE) 

imp_diabetes_t2 <- imp_diabetes_t2 %>% 
  dplyr::mutate(variable = 'imp_diabetes_t2')


##

chronic_cardiac <- glm(postcovid_rapid ~ age_group_2 + sex + chronic_cardiac, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_chronic_cardiac <- with(BMI_imp_long_mids,
                            glm(postcovid_rapid ~ age_group_2 + sex + chronic_cardiac, family=binomial))

imp_chronic_cardiac <- summary(pool(imp_chronic_cardiac), conf.int = TRUE, exp = TRUE) 

imp_chronic_cardiac <- imp_chronic_cardiac %>% 
  dplyr::mutate(variable = 'imp_chronic_cardiac')

##

learning_disability  <- glm(postcovid_rapid ~ age_group_2 + sex + learning_disability, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_learning_disability  <- with(BMI_imp_long_mids,
                                 glm(postcovid_rapid ~ age_group_2 + sex + learning_disability , family=binomial))

imp_learning_disability  <- summary(pool(imp_learning_disability ), conf.int = TRUE, exp = TRUE) 

imp_learning_disability  <- imp_learning_disability  %>% 
  dplyr::mutate(variable = 'imp_learning_disability')


##
psychosis_schiz_bipolar <- glm(postcovid_rapid ~ age_group_2 + sex + psychosis_schiz_bipolar, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_psychosis_schiz_bipolar <- with(BMI_imp_long_mids,
                                    glm(postcovid_rapid ~ age_group_2 + sex + psychosis_schiz_bipolar, family=binomial))

imp_psychosis_schiz_bipolar <- summary(pool(imp_psychosis_schiz_bipolar), conf.int = TRUE, exp = TRUE) 

imp_psychosis_schiz_bipolar <- imp_psychosis_schiz_bipolar %>% 
  dplyr::mutate(variable = 'imp_psychosis_schiz_bipolar')

##

depression <- glm(postcovid_rapid ~ age_group_2 + sex + depression, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_depression <- with(BMI_imp_long_mids,
                       glm(postcovid_rapid ~ age_group_2 + sex + depression, family=binomial))

imp_depression <- summary(pool(imp_depression), conf.int = TRUE, exp = TRUE) 

imp_depression <- imp_depression %>% 
  dplyr::mutate(variable = 'imp_depression')

##
COPD <- glm(postcovid_rapid ~ age_group_2 + sex + COPD, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_COPD <- with(BMI_imp_long_mids,
                 glm(postcovid_rapid ~ age_group_2 + sex + COPD, family=binomial))

imp_COPD <- summary(pool(imp_COPD), conf.int = TRUE, exp = TRUE) 

imp_COPD <- imp_COPD %>% 
  dplyr::mutate(variable = 'imp_COPD')

##
asthma  <- glm(postcovid_rapid ~ age_group_2 + sex + asthma, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_asthma  <- with(BMI_imp_long_mids,
                    glm(postcovid_rapid ~ age_group_2 + sex + asthma, family=binomial))

imp_asthma  <- summary(pool(imp_asthma    ), conf.int = TRUE, exp = TRUE) 

imp_asthma   <- imp_asthma     %>% 
  dplyr::mutate(variable = 'imp_asthma')


##
dementia  <- glm(postcovid_rapid ~ age_group_2 + sex + dementia, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_dementia  <- with(BMI_imp_long_mids,
                      glm(postcovid_rapid ~ age_group_2 + sex + dementia, family=binomial))

imp_dementia  <- summary(pool(imp_dementia    ), conf.int = TRUE, exp = TRUE) 

imp_dementia   <- imp_dementia     %>% 
  dplyr::mutate(variable = 'imp_dementia')

##
stroke_and_TIA  <- glm(postcovid_rapid ~ age_group_2 + sex + stroke_and_TIA, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_stroke_and_TIA  <- with(BMI_imp_long_mids,
                            glm(postcovid_rapid ~ age_group_2 + sex + stroke_and_TIA, family=binomial))

imp_stroke_and_TIA  <- summary(pool(imp_stroke_and_TIA    ), conf.int = TRUE, exp = TRUE) 

imp_stroke_and_TIA   <- imp_stroke_and_TIA     %>% 
  dplyr::mutate(variable = 'imp_stroke_and_TIA')

##

smoking_status  <- glm(postcovid_rapid ~ age_group_2 + sex + smoking_status, family=binomial, data=BMI_trajectories) %>% 
  broom::tidy(conf.int = TRUE, exp = TRUE)


imp_smoking_status  <- with(BMI_imp_long_mids,
                            glm(postcovid_rapid ~ age_group_2 + sex + smoking_status, family=binomial))

imp_smoking_status  <- summary(pool(imp_smoking_status    ), conf.int = TRUE, exp = TRUE) 

imp_smoking_status   <- imp_smoking_status     %>% 
  dplyr::mutate(variable = 'imp_smoking_status')


names(BMI_trajectories)



####





models <- ethnic %>% 
  bind_rows(imp_ethnic, 
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

write.csv (models, here::here ("output/data", "age_sex_rapid_weightgain_imputation.csv"))