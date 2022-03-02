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


######  TEST DE-DUPLICATION OF VALUES
# Check for dupllicate entries by groups by 
long_bmi_2015 %>%
group_by(patient_id, date) %>%
slice_head

###################  END OF TEST



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
 

## replace very high and very low BMIs with NA:  This will exclude erroneus values and exclude patients who are severely underweight/severely obese whose change in BMI may not reflect general population trends
long_bmi_2015$monthly_bmi[long_bmi_2015$monthly_bmi<15|long_bmi_2015$monthly_bmi>65] <- NA

# Hmisc::describe(long_bmi_2015$monthly_bmi)
# recoding successful

 
### Calculate the median BMI for each patient based on measurements in that year
 
bmi_2015_bypatid <- group_by(long_bmi_2015,patient_id)
median_bmi_2015 <- dplyr::summarise(bmi_2015_bypatid,
                            median_bmi = median(monthly_bmi, na.rm=TRUE)
                            )
 
 ## check a few rows to see if median calculation works - it does
 #check_bmi_median <- bmi_2015_bypatid %>%
   #select(patient_id, monthly_bmi) %>%
   #filter(patient_id<80)
####################################################  NEW


## add median BMI onto main data set using a merge
long_bmi_2015_median <- left_join(long_bmi_2015, median_bmi_2015)


## group and then slice head
BMI_2015_median <- long_bmi_2015_median %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "median_bmi",
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

BMI_2015_median <- BMI_2015_median %>%
  mutate("year"= 2015)


## FINAL DATA SET for 2015:  BMI_2015_median

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


### Calculate the median BMI for each patient based on measurements in that year

bmi_2016_bypatid <- group_by(long_bmi_2016,patient_id)
median_bmi_2016 <- dplyr::summarise(bmi_2016_bypatid,
                           median_bmi = median(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if median calculation works - it does
#check_bmi_median <- bmi_2016_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add median BMI onto main data set using a merge
long_bmi_2016_median <- left_join(long_bmi_2016, median_bmi_2016)


## group and then slice head
BMI_2016_median <- long_bmi_2016_median %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "median_bmi",
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
BMI_2016_median <- BMI_2016_median %>%
  mutate("year"= 2016)

## FINAL DATA SET for 2016:  BMI_2016_median

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


### Calculate the median BMI for each patient based on measurements in that year

bmi_2017_bypatid <- group_by(long_bmi_2017,patient_id)
median_bmi_2017 <- summarise(bmi_2017_bypatid,
                           median_bmi = median(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if median calculation works - it does
#check_bmi_median <- bmi_2017_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add median BMI onto main data set using a merge
long_bmi_2017_median <- left_join(long_bmi_2017, median_bmi_2017)


## group and then slice head
BMI_2017_median <- long_bmi_2017_median %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "median_bmi",
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

BMI_2017_median <- BMI_2017_median %>%
  mutate("year"= 2017)

## FINAL DATA SET for 2017:  BMI_2017_medianr

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


### Calculate the median BMI for each patient based on measurements in that year

bmi_2018_bypatid <- group_by(long_bmi_2018,patient_id)
median_bmi_2018 <- summarise(bmi_2018_bypatid,
                           median_bmi = median(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if median calculation works - it does
#check_bmi_median <- bmi_2018_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add median BMI onto main data set using a merge
long_bmi_2018_median <- left_join(long_bmi_2018, median_bmi_2018)


## group and then slice head
BMI_2018_median <- long_bmi_2018_median %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "median_bmi",
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

BMI_2018_median <- BMI_2018_median %>%
  mutate("year"= 2018)

## FINAL DATA SET for 2018:  BMI_2018_medianr

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


### Calculate the median BMI for each patient based on measurements in that year

bmi_2019_bypatid <- group_by(long_bmi_2019,patient_id)
median_bmi_2019 <- summarise(bmi_2019_bypatid,
                           median_bmi = median(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if median calculation works - it does
#check_bmi_median <- bmi_2019_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add median BMI onto main data set using a merge
long_bmi_2019_median <- left_join(long_bmi_2019, median_bmi_2019)


## group and then slice head
BMI_2019_median <- long_bmi_2019_median %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "median_bmi",
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

BMI_2019_median <- BMI_2019_median %>%
  mutate("year"= 2019)

## FINAL DATA SET for 2019:  BMI_2019_medianr

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


### Calculate the median BMI for each patient based on measurements in that year

bmi_2020_bypatid <- group_by(long_bmi_2020,patient_id)
median_bmi_2020 <- summarise(bmi_2020_bypatid,
                           median_bmi = median(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if median calculation works - it does
#check_bmi_median <- bmi_2020_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add median BMI onto main data set using a merge
long_bmi_2020_median <- left_join(long_bmi_2020, median_bmi_2020)


## group and then slice head
BMI_2020_median <- long_bmi_2020_median %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "median_bmi",
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


BMI_2020_median <- BMI_2020_median %>%
  mutate("year"= 2020)

## FINAL DATA SET for 2020:  BMI_2020_medianr

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


### Calculate the median BMI for each patient based on measurements in that year

bmi_2021_bypatid <- group_by(long_bmi_2021,patient_id)
median_bmi_2021 <- summarise(bmi_2021_bypatid,
                           median_bmi = median(monthly_bmi, na.rm=TRUE)
)

## check a few rows to see if median calculation works - it does
#check_bmi_median <- bmi_2021_bypatid %>%
#select(patient_id, monthly_bmi) %>%
#filter(patient_id<80)


## add median BMI onto main data set using a merge
long_bmi_2021_median <- left_join(long_bmi_2021, median_bmi_2021)


## group and then slice head
BMI_2021_median <- long_bmi_2021_median %>%
  group_by(patient_id) %>%
  slice_head() %>%
  select("patient_id", 
         "median_bmi",
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

BMI_2021_median <- BMI_2021_median %>%
  mutate("year"= 2021)


## FINAL DATA SET for 2021:  BMI_2021_medianr







############################################################################  NEW CODE FOR TESTING

## APPEND THE DATA SETS FOR A COMPLETE DATA SET

BMI_complete_median <- bind_rows(BMI_2015_median, 
                               BMI_2016_median, 
                               BMI_2017_median, 
                               BMI_2018_median, 
                               BMI_2019_median, 
                               BMI_2020_median,
                               BMI_2021_median)


### FINAL DATA SET:  BMI_complete_median

##names(BMI_complete_median)

## Ungroup to assign BMI categories
BMI_complete_median <- ungroup(BMI_complete_median)


### classify as underweight, healthyweight, overweight, obese
BMI_complete_categories <- BMI_complete_median
BMI_complete_categories$BMI_categories <- cut(BMI_complete_categories$median_bmi, 
                                            breaks=c(0, 20,25,30,1000),
                                            labels= c("underweight", "healthy", "overweight", "obese"))
                                      
  
## classify as above 27.5

BMI_complete_categories$BMI_over27.5 <- cut(BMI_complete_categories$median_bmi,
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
      condition = ((((ethnic_no_miss==1| ethnic_no_miss==0) & median_bmi >=30) | ((ethnic_no_miss==2| ethnic_no_miss==3| ethnic_no_miss==4| ethnic_no_miss==5) & median_bmi >=27.5))
                  & ((hypertension==1| diabetes_t1==1| diabetes_t2==1))),
      true = "eligible", 
      false = "not_eligible"
      )
  )

BMI_complete_categories_DWMP <-BMI_complete_categories_DWMP %>%
  dplyr::mutate(
    across(
    .cols = c(learning_disability,depression, dementia,psychosis_schiz_bipolar, diabetes_type, diabetes_t1, diabetes_t2, asthma, COPD, stroke_and_TIA, chronic_cardiac, hypertension, all_cancer), 
    .names = "comorbid_{col}"
  )
  )

BMI_complete_categories_DWMP <- BMI_complete_categories_DWMP %>%
  dplyr::mutate(patient_id = as.numeric(patient_id)) %>%
  arrange(patient_id) %>%
  dplyr::group_by(patient_id) %>%
  dplyr::mutate(
    precovid_obese = (((median_bmi >=30) & ((year=="2015")| (year=="2016")| (year=="2017")| (year=="2018") | (year=="2019")))) , 
    .after = "patient_id") %>%
  dplyr::mutate(
    precovid_obese_flag = (any(precovid_obese == "TRUE")),
    .after = "precovid_obese"
  )


BMI_complete_categories_DWMP <- ungroup (BMI_complete_categories_DWMP)

BMI_complete_categories_DWMP <- BMI_complete_categories_DWMP %>%
 dplyr::select(
 patient_id, year, precovid_obese_flag, median_bmi, had_bmi, BMI_categories, BMI_over27.5, DWMP, sex, age_group, region, imd, eth, ethnicity_sus, ethnicity, ethnic_no_miss, starts_with("comorbid_"), 
 )


###  add binary obese variable
BMI_complete_categories_DWMP <- BMI_complete_categories_DWMP %>%
  dplyr::mutate(
    obese = if_else(
      condition = (median_bmi >= 30), 
      true = 1,
      false = 0
    ), 
    .after = "median_bmi"
  )                       

## Label variables ethnicity and IMD to make clearer

BMI_complete_categories <- BMI_complete_categories_DWMP 

BMI_complete_categories$ethnic_no_miss[BMI_complete_categories$ethnic_no_miss=="1"]<-"White"
BMI_complete_categories$ethnic_no_miss[BMI_complete_categories$ethnic_no_miss=="2"]<-"Mixed"
BMI_complete_categories$ethnic_no_miss[BMI_complete_categories$ethnic_no_miss=="3"]<-"Asian or Asian British"
BMI_complete_categories$ethnic_no_miss[BMI_complete_categories$ethnic_no_miss=="4"]<-"Black or Black British"
BMI_complete_categories$ethnic_no_miss[BMI_complete_categories$ethnic_no_miss=="5"]<-"Other ethnic groups"
BMI_complete_categories$ethnic_no_miss[BMI_complete_categories$ethnic_no_miss=="0"]<-"No ethnicity recorded"

BMI_complete_categories$imd[BMI_complete_categories$imd=="0"]<-"NA"
BMI_complete_categories$imd[BMI_complete_categories$imd=="1"]<-"1 most deprived"
BMI_complete_categories$imd[BMI_complete_categories$imd=="2"]<-"2"
BMI_complete_categories$imd[BMI_complete_categories$imd=="3"]<-"3"
BMI_complete_categories$imd[BMI_complete_categories$imd=="4"]<-"4"
BMI_complete_categories$imd[BMI_complete_categories$imd=="5"]<-"5 least deprived"

###########################################################################################################

write.csv (BMI_complete_categories, here::here ("output/data","BMI_complete_categories.csv"))



