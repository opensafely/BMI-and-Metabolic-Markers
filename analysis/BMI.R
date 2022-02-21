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
####################################################  NEW


## add mean BMI onto main data set using a merge
long_bmi_2015_mean <- left_join(long_bmi_2015, mean_bmi_2015)


## group and then slice head
BMI_2015_mean <- long_bmi_2015_mean %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "mean_bmi",
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
         "ethnicity")              

BMI_2015_mean <- BMI_2015_mean %>%
  mutate("year"= 2015)


## FINAL DATA SET for 2015:  BMI_2015_mean

#############################################################################NEW


################################################
##################################################

BMI_2016 <- as_tibble (input_all_2016_03_01)
# Hmisc:: describe(BMI_2016)


## check column names
# names(BMI_2016) 


##  reshape data as long to allow grouping and calculation of average yearly BMI


long_bmi_2016 <- BMI_2016 %>%
  pivot_longer(
    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
    names_to = "date", 
    values_to = "monthly_bmi"
  )

# names(long_bmi_2016)

##  Keep relevant variable for analysis

long_bmi_2016 <- long_bmi_2016 %>%
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



#  Hmisc::describe(long_bmi_2016)
#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace


## replace very high and very low BMIs with NA
long_bmi_2016$monthly_bmi[long_bmi_2016$monthly_bmi<12|long_bmi_2016$monthly_bmi>65] <- NA

# Hmisc::describe(long_bmi_2016$monthly_bmi)
# recoding successfulc


### Calculate the mean BMI for each patient based on measurements in that year

bmi_2016_bypatid <- group_by(long_bmi_2016,patient_id)
mean_bmi_2016 <- dplyr::summarise(bmi_2016_bypatid,
                           mean_bmi = mean(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if mean calculation works - it does
#check_bmi_mean <- bmi_2016_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add mean BMI onto main data set using a merge
long_bmi_2016_mean <- left_join(long_bmi_2016, mean_bmi_2016)


## group and then slice head
BMI_2016_mean <- long_bmi_2016_mean %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "mean_bmi",
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
         "ethnicity")              
BMI_2016_mean <- BMI_2016_mean %>%
  mutate("year"= 2016)

## FINAL DATA SET for 2016:  BMI_2016_mean

##########################################################################  NEW

BMI_2017 <- as_tibble (input_all_2017_03_01)
# Hmisc:: describe(BMI_2017)


## check column names
# names(BMI_2017) 


##  reshape data as long to allow grouping and calculation of average yearly BMI


long_bmi_2017 <- BMI_2017 %>%
  pivot_longer(
    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
    names_to = "date", 
    values_to = "monthly_bmi"
  )

# names(long_bmi_2017)

##  Keep relevant variable for analysis

long_bmi_2017 <- long_bmi_2017 %>%
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



#  Hmisc::describe(long_bmi_2017)
#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace


## replace very high and very low BMIs with NA
long_bmi_2017$monthly_bmi[long_bmi_2017$monthly_bmi<12|long_bmi_2017$monthly_bmi>65] <- NA

# Hmisc::describe(long_bmi_2017$monthly_bmi)
# recoding successfulc


### Calculate the mean BMI for each patient based on measurements in that year

bmi_2017_bypatid <- group_by(long_bmi_2017,patient_id)
mean_bmi_2017 <- summarise(bmi_2017_bypatid,
                           mean_bmi = mean(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if mean calculation works - it does
#check_bmi_mean <- bmi_2017_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add mean BMI onto main data set using a merge
long_bmi_2017_mean <- left_join(long_bmi_2017, mean_bmi_2017)


## group and then slice head
BMI_2017_mean <- long_bmi_2017_mean %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "mean_bmi",
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
         "ethnicity")              

BMI_2017_mean <- BMI_2017_mean %>%
  mutate("year"= 2017)

## FINAL DATA SET for 2017:  BMI_2017_meanr

###############################################
#################################################
BMI_2018 <- as_tibble (input_all_2018_03_01)
# Hmisc:: describe(BMI_2018)


## check column names
# names(BMI_2018) 


##  reshape data as long to allow grouping and calculation of average yearly BMI


long_bmi_2018 <- BMI_2018 %>%
  pivot_longer(
    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
    names_to = "date", 
    values_to = "monthly_bmi"
  )

# names(long_bmi_2018)

##  Keep relevant variable for analysis

long_bmi_2018 <- long_bmi_2018 %>%
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



#  Hmisc::describe(long_bmi_2018)
#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace


## replace very high and very low BMIs with NA
long_bmi_2018$monthly_bmi[long_bmi_2018$monthly_bmi<12|long_bmi_2018$monthly_bmi>65] <- NA

# Hmisc::describe(long_bmi_2018$monthly_bmi)
# recoding successfulc


### Calculate the mean BMI for each patient based on measurements in that year

bmi_2018_bypatid <- group_by(long_bmi_2018,patient_id)
mean_bmi_2018 <- summarise(bmi_2018_bypatid,
                           mean_bmi = mean(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if mean calculation works - it does
#check_bmi_mean <- bmi_2018_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add mean BMI onto main data set using a merge
long_bmi_2018_mean <- left_join(long_bmi_2018, mean_bmi_2018)


## group and then slice head
BMI_2018_mean <- long_bmi_2018_mean %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "mean_bmi",
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
         "ethnicity")              

BMI_2018_mean <- BMI_2018_mean %>%
  mutate("year"= 2018)

## FINAL DATA SET for 2018:  BMI_2018_meanr

####################################################
####################################################

BMI_2019 <- as_tibble (input_all_2019_03_01)
# Hmisc:: describe(BMI_2019)


## check column names
# names(BMI_2019) 


##  reshape data as long to allow grouping and calculation of average yearly BMI


long_bmi_2019 <- BMI_2019 %>%
  pivot_longer(
    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
    names_to = "date", 
    values_to = "monthly_bmi"
  )

# names(long_bmi_2019)

##  Keep relevant variable for analysis

long_bmi_2019 <- long_bmi_2019 %>%
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



#  Hmisc::describe(long_bmi_2019)
#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace


## replace very high and very low BMIs with NA
long_bmi_2019$monthly_bmi[long_bmi_2019$monthly_bmi<12|long_bmi_2019$monthly_bmi>65] <- NA

# Hmisc::describe(long_bmi_2019$monthly_bmi)
# recoding successfulc


### Calculate the mean BMI for each patient based on measurements in that year

bmi_2019_bypatid <- group_by(long_bmi_2019,patient_id)
mean_bmi_2019 <- summarise(bmi_2019_bypatid,
                           mean_bmi = mean(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if mean calculation works - it does
#check_bmi_mean <- bmi_2019_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add mean BMI onto main data set using a merge
long_bmi_2019_mean <- left_join(long_bmi_2019, mean_bmi_2019)


## group and then slice head
BMI_2019_mean <- long_bmi_2019_mean %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "mean_bmi",
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
         "ethnicity")              

BMI_2019_mean <- BMI_2019_mean %>%
  mutate("year"= 2019)

## FINAL DATA SET for 2019:  BMI_2019_meanr

##################################################
###################################################

BMI_2020 <- as_tibble (input_all_2020_03_01)
# Hmisc:: describe(BMI_2020)


## check column names
# names(BMI_2020) 


##  reshape data as long to allow grouping and calculation of average yearly BMI


long_bmi_2020 <- BMI_2020 %>%
  pivot_longer(
    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
    names_to = "date", 
    values_to = "monthly_bmi"
  )

# names(long_bmi_2020)

##  Keep relevant variable for analysis

long_bmi_2020 <- long_bmi_2020 %>%
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



#  Hmisc::describe(long_bmi_2020)
#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace


## replace very high and very low BMIs with NA
long_bmi_2020$monthly_bmi[long_bmi_2020$monthly_bmi<12|long_bmi_2020$monthly_bmi>65] <- NA

# Hmisc::describe(long_bmi_2020$monthly_bmi)
# recoding successfulc


### Calculate the mean BMI for each patient based on measurements in that year

bmi_2020_bypatid <- group_by(long_bmi_2020,patient_id)
mean_bmi_2020 <- summarise(bmi_2020_bypatid,
                           mean_bmi = mean(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if mean calculation works - it does
#check_bmi_mean <- bmi_2020_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add mean BMI onto main data set using a merge
long_bmi_2020_mean <- left_join(long_bmi_2020, mean_bmi_2020)


## group and then slice head
BMI_2020_mean <- long_bmi_2020_mean %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "mean_bmi",
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
         "ethnicity")              


BMI_2020_mean <- BMI_2020_mean %>%
  mutate("year"= 2020)

## FINAL DATA SET for 2020:  BMI_2020_meanr

###############################################
################################################

BMI_2021 <- as_tibble (input_all_2021_03_01)
# Hmisc:: describe(BMI_2021)


## check column names
# names(BMI_2021) 


##  reshape data as long to allow grouping and calculation of average yearly BMI


long_bmi_2021 <- BMI_2021 %>%
  pivot_longer(
    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
    names_to = "date", 
    values_to = "monthly_bmi"
  )

# names(long_bmi_2021)

##  Keep relevant variable for analysis

long_bmi_2021 <- long_bmi_2021 %>%
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



#  Hmisc::describe(long_bmi_2021)
#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace


## replace very high and very low BMIs with NA
long_bmi_2021$monthly_bmi[long_bmi_2021$monthly_bmi<12|long_bmi_2021$monthly_bmi>65] <- NA

# Hmisc::describe(long_bmi_2021$monthly_bmi)
# recoding successfulc


### Calculate the mean BMI for each patient based on measurements in that year

bmi_2021_bypatid <- group_by(long_bmi_2021,patient_id)
mean_bmi_2021 <- summarise(bmi_2021_bypatid,
                           mean_bmi = mean(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if mean calculation works - it does
#check_bmi_mean <- bmi_2021_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add mean BMI onto main data set using a merge
long_bmi_2021_mean <- left_join(long_bmi_2021, mean_bmi_2021)


## group and then slice head
BMI_2021_mean <- long_bmi_2021_mean %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "mean_bmi",
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
         "ethnicity")              

BMI_2021_mean <- BMI_2021_mean %>%
  mutate("year"= 2021)


## FINAL DATA SET for 2021:  BMI_2021_meanr







############################################################################  NEW CODE FOR TESTING

## APPEND THE DATA SETS FOR A COMPLETE DATA SET

BMI_complete_mean <- bind_rows(BMI_2015_mean, 
                               BMI_2016_mean, 
                               BMI_2017_mean, 
                               BMI_2018_mean, 
                               BMI_2019_mean, 
                               BMI_2020_mean,
                               BMI_2021_mean)


### FINAL DATA SET:  BMI_complete_mean

##names(BMI_complete_mean)

## Ungroup to assign BMI categories
BMI_complete_mean <- ungroup(BMI_complete_mean)


### classify as underweight, healthyweight, overweight, obese
BMI_complete_categories <- BMI_complete_mean
BMI_complete_categories$BMI_categories <- cut(BMI_complete_categories$mean_bmi, 
                                            breaks=c(0, 20,25,30,1000),
                                            labels= c("underweight", "healthy", "overweight", "obese"))
                                      
  
## classify as above 27.5

BMI_complete_categories$BMI_over27.5 <- cut(BMI_complete_categories$mean_bmi,
                                            breaks=c(0,27.5,1000),
                                            labels=c("<27.5", "27.5+"))
  

##########  
########## .. Generate variable indicating DWMP eligibility

## recode ethnicity so NA is unknown
BMI_complete_categories_DWMP <- BMI_complete_categories %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))


## confirm ethnicity categories
# dict_eth = {1: ‘White’, 2: ‘Mixed’, 3: ‘Asian’, 4: ‘Black’, 5: ‘Other’, np.nan: ‘Unknown’, 0: ‘Unknown’}
           
BMI_complete_categories_DWMP <- BMI_complete_categories_DWMP %>%
  dplyr::mutate(
    DWMP = if_else(
      condition = ((((ethnic_no_miss==1| ethnic_no_miss==0) & mean_bmi >=30) | ((ethnic_no_miss==2| ethnic_no_miss==3| ethnic_no_miss==4| ethnic_no_miss==5) & mean_bmi >=27.5))
                  & ((hypertension==1| diabetes_t1==1| diabetes_t2==1))),
      true = "eligible", 
      false = "not_eligible"
      )
  )




###########################################################################################################

write.csv (BMI_complete_categories_DWMP, here::here ("output/data","BMI_complete_categories.csv"))


 
