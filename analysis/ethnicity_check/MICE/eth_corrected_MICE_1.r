## Sensitivity analyses of ethnicity corrected data


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


BMI_traj <- read_feather (here::here ("output/data", "BMI_trajectory_data_long_eth_corrected.feather"))




## remove patients with cancer and who were underweight at onset of pandemic
BMI_trajectories <- BMI_traj %>% 
  dplyr::filter(all_cancer != TRUE) %>% 
  dplyr::filter(precovid_bmi_category != "underweight") %>% 
  dplyr::filter(pandemic_stage == "postcovid")

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::select(-c(
"age_group",
"cholesterol_test",        
"dbp",                     
"smoking_status",         
"ethnic_no_miss",
"eth_group_16",
"base_bmi",               
"precovid_bmi",            
"postcovid_bmi",           
"bmi_change1",            
"bmi_change2",             
"time_change1",
"time_change2",           
"period1_missing",
"period2_missing",
"complete_bmi_data",      
"timechange1_check",       
"time_change_error", 
"base_bmi_category", 
"postcovid_bmi_category")
)


BMI_trajectories <- BMI_trajectories[sample(nrow(BMI_trajectories), 300000), ]



BMI_trajectories$imd <- factor(BMI_trajectories$imd, 
                               levels = c('1','2','3','4','5'))

BMI_trajectories <- BMI_trajectories %>%
  dplyr::mutate(eth_16_corrected = as.factor(eth_16_corrected))

p_missing <- unlist(lapply(BMI_trajectories, function(x) sum(is.na(x))))/nrow(BMI_trajectories)

p_missing <- as.data.frame(sort(p_missing[p_missing > 0], decreasing = TRUE))

p_missing


#### MICE
# We run the mice code with 0 iterations 

imp <- mice(BMI_trajectories, maxit=0, seed = 123)

# Extract predictorMatrix and methods of imputation 

predM <- imp$predictorMatrix
meth <- imp$method

head(predM)

colnames(BMI_trajectories)

## Remove variables from the predictor matrix
predM[, c("patient_id")] <- 0
predM[, c("oad_meds")] <- 0
predM[, c("insulin_meds")] <- 0


## Select method of imputation 
meth["eth_16_corrected"] <- "polyreg"   ## unordered categorical
meth["imd"] <- "polr"   ## ordered categorial


## set the following values not to be imputed
meth["patient_id"] <- ""
meth["oad_meds"] <- ""
meth["insulin_meds"] <- ""

meth


## Run the imputation methods
imp2 <- mice(BMI_trajectories, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  FALSE)           
          


BMI_imp_long <- mice::complete(imp2, action="long", include = TRUE)

BMI_imp_long %>% 
  tabyl(.imp, eth_16_corrected)

BMI_imp_long %>% 
  tabyl(.imp, imd)


write.csv (BMI_imp_long, here::here ("output/data", "CC_imputation_dataframe.csv"))
write.csv (BMI_trajectories, here::here ("output/data", "CC_imputation_DF_for_impute.csv"))
write.csv (p_missing, here::here ("output/data", "CC_imputation_sample_missing.csv"))


