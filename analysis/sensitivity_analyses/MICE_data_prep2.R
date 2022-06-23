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
BMI_trajectories <- BMI_trajectories[sample(nrow(BMI_trajectories), 100000), ]



BMI_trajectories$imd <- factor(BMI_trajectories$imd, 
                                levels = c('1','2','3','4','5'))



# Unordered categorical variable 
poly2 <- c("sex", "age_group_2", "region", "imd", "eth_group_16",  "smoking_status")






# We run the mice code with 0 iterations 

imp <- mice(BMI_trajectories, maxit=0)

# Extract predictorMatrix and methods of imputation 

predM <- imp$predictorMatrix
meth <- imp$method

meth

## methods used for imputation are appropriate, don't need to change
meth[poly2] <- "polyreg"

meth

## complete the imputation
# With this command, we tell mice to impute the anesimp2 data, create 5
# datasets, use predM as the predictor matrix and don't print the imputation
# process. If you would like to see the process, set print as TRUE

imp2 <- mice(BMI_trajectories, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  TRUE)



BMI_imp_long <- mice::complete(imp2, action="long", include = TRUE)

write.csv (BMI_imp_long, here::here ("output/data", "imputation_dataframe.csv"))
