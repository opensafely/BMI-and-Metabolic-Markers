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


BMI_trajectories <- read_csv (here::here ("output/data", "imputation_data_long.csv"))

## Sample
BMI_trajectories <- BMI_trajectories[sample(nrow(BMI_trajectories), 500000), ]

BMI_trajectories <- BMI_trajectories[ -c(1) ]

BMI_trajectories$imd <- factor(BMI_trajectories$imd, 
                                levels = c('1','2','3','4','5'))

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::select(-c(ends_with("_bmi"))) %>% 
  dplyr::select(-c(ends_with("_bmi_category"))) %>% 
  dplyr::select(-c(ends_with("all_cancer")))

p_missing <- unlist(lapply(BMI_trajectories, function(x) sum(is.na(x))))/nrow(BMI_trajectories)

p_missing <- as.data.frame(sort(p_missing[p_missing > 0], decreasing = TRUE))

p_missing

# Remove variables from the MICE predictor frame









# We run the mice code with 0 iterations 

imp <- mice(BMI_trajectories, maxit=0, seed = 123)

# Extract predictorMatrix and methods of imputation 

predM <- imp$predictorMatrix
meth <- imp$method

head(predM)


## Remove variables from the predictor matrix
predM[, c("patient_id")] <- 0

predM[, c("region")] <- 0
predM[, c("imd")] <- 0
predM[, c("hypertension")] <- 0
predM[, c("diabetes_t1")] <- 0
predM[, c("diabetes_t2")] <- 0
predM[, c("chronic_cardiac")] <- 0


predM[, c("COPD")] <- 0
predM[, c("asthma")] <- 0

predM[, c("learning_disability")] <- 0
predM[, c("psychosis_schiz_bipolar")] <- 0
predM[, c("depression")] <- 0

predM[, c("stroke_and_TIA")] <- 0
predM[, c("dementia")] <- 0


predM[, c("precovid_change")] <- 0



## methods used for imputation are appropriate, don't need to change

meth[c("age_group_2")]=""
meth[c("sex")]=""
meth[c("region")]=""
meth[c("eth_group_16")]=""
meth[c("imd")]=""
meth[c("smoking_status")]=""









## complete the imputation
# With this command, we tell mice to impute the anesimp2 data, create 5
# datasets, use predM as the predictor matrix and don't print the imputation
# process. If you would like to see the process, set print as TRUE

imp2 <- mice(BMI_trajectories, maxit = 5, seed = 123,
             predictorMatrix = predM, 
             method = meth, print =  TRUE)



BMI_imp_long <- mice::complete(imp2, action="long", include = TRUE)

write.csv (BMI_imp_long, here::here ("output/data", "imputation_dataframe.csv"))
write.csv (BMI_trajectories, here::here ("output/data", "imputation_DF_for_impute.csv"))
write.csv (p_missing, here::here ("output/data", "imputation_sample_missing.csv"))
