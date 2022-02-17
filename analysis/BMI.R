## Dummy Data to Review R - code for open safely analysis
## Miriam Samuel 
## 10th Feb 2022


## NOTES
## check ethnicity codes
## generate a variable for to state if eligible for DWMP - based on BMI and ethnicity





## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)



####################################
###################################




#####  read in files

input_all_2015_03_01<- read.csv (here::here ("output/data", "input_all_2015-03-01.csv"))

input_all_2016_03_01<- read.csv (here::here ("output/data", "input_all_2016-03-01.csv"))

input_all_2017_03_01<- read.csv (here::here ("output/data", "input_all_2017-03-01.csv"))

input_all_2018_03_01<- read.csv (here::here ("output/data", "input_all_2018-03-01.csv"))

input_all_2019_03_01<- read.csv (here::here ("output/data", "input_all_2019-03-01.csv"))

input_all_2020_03_01<- read.csv (here::here ("output/data", "input_all_2020-03-01.csv"))

input_all_2021_03_01<- read.csv (here::here ("output/data", "input_all_2021-03-01.csv"))


###################
## 2015 analysis
###################

BMI_2015 <- as_tibble (input_all_2015_03_01)
# Hmisc:: describe(BMI_2015)

## check column names
# names(BMI_2015) 


##  reshape data as long to allow grouping and calculation of average yearly BMI


long_bmi_2015 <- BMI_2015 %>%
pivot_longer(
  cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
  names_to = "date", 
  values_to = "monthly_bmi"
  )

# names(long_bmi_2015)

##  Keep relevant variable for analysis

long_bmi_2015 <- long_bmi_2015 %>%
  select("patient_id", 
         "type1_diabetes", 
         "type2_diabetes", 
         "unknown_diabetes",  
         "sex", 
         "age_group", 
         "region", 
         "imd", 
         "learning_disability", 
         "dementia", 
        "depression",                   
        "psychosis_schiz_bipolar", 
        "diabetes_type",               
        "diabetes_t1",                  
        "diabetes_t2",
        "bmi",
        "had_bmi",
        "asthma",                      
        "COPD",                        
        "stroke_and_TIA" ,
        "chronic_cardiac",              
        "hypertension",                 
        "all_cancer",                
        "eth", 
        "ethnicity_sus", 
        "ethnicity", 
        "date", 
        "monthly_bmi")              



#  Hmisc::describe(long_bmi_2015)
#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace
 

## replace very high and very low BMIs with NA
long_bmi_2015$monthly_bmi[long_bmi_2015$monthly_bmi<12|long_bmi_2015$monthly_bmi>65] <- NA

# Hmisc::describe(long_bmi_2015$monthly_bmi)
# recoding successful

 
### Calculate the mean BMI for each patient based on measurements in that year
 
bmi_2015_bypatid <- group_by(long_bmi_2015,patient_id)
mean_bmi_2015 <- dplyr::summarise(bmi_2015_bypatid,
                            mean_bmi = mean(monthly_bmi, na.rm=TRUE)
                            )
 
 ## check a few rows to see if mean calculation works - it does
 #check_bmi_mean <- bmi_2015_bypatid %>%
   #select(patient_id, monthly_bmi) %>%
   #filter(patient_id<80)

write.csv (mean_bmi_2015, here::here ("output/data","BMI_complete_categories.csv"))

##############################################################################################################CHECK 1
 
