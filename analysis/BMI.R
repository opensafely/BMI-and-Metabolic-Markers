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


#########################################  NEW  3rd march 2022
######################################### CODE to: 1) link BMI values and actual measured data; 2) de-duplicate any duplicate values due to monthly_bmi formula
#########################################  Will need to also create a cohort with all values for median_bmi analysis

bmi_2015_long <- BMI_2015 %>%   ## 1. pivot_longer date measured columns
    pivot_longer(
      cols = c('bmi_march_date_measured', 'bmi_apr_date_measured', 'bmi_may_date_measured', 'bmi_june_date_measured', 'bmi_july_date_measured', 'bmi_aug_date_measured', 'bmi_sep_date_measured', 'bmi_oct_date_measured', 'bmi_nov_date_measured', 'bmi_dec_date_measured', 'bmi_jan_date_measured', 'bmi_feb_date_measured', 'bmi_jan_date_measured'),
      values_to = "bmi_measured_date" ) %>% 
    dplyr::arrange(patient_id, bmi_measured_date) %>%
    tidyr::drop_na(bmi_measured_date) %>%
    group_by(patient_id, bmi_measured_date) %>%
    slice_head %>%                  #  step 2.  Pivot longer the values.  
    pivot_longer(
    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
    names_to = "date", 
    values_to = "monthly_bmi")  %>%    # step 3.  filter out duplicate row
    dplyr::select("patient_id", 
                  "name", 
                  "bmi_measured_date",
                  "date",
                  "monthly_bmi",
                  "type1_diabetes", 
                  "type2_diabetes", 
                  "unknown_diabetes",  
                  "sex", 
                  "age_group", 
                  "region", 
                  "imd", 
                  "ethnicity",
                  "ethnicity_16",
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
                  "all_cancer")   %>%
  mutate(measured_month = str_sub(name, 1, -15)) %>%  #3a.  create a column to identify matching events
  dplyr::filter(measured_month == date) %>%
  select(-'name', -'measured_month')



long_bmi_2015 <- bmi_2015_long







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
         "ethnicity",
         "ethnicity_16")              

BMI_2015_median <- BMI_2015_median %>%
  mutate("year"= 2015)


## FINAL DATA SET for 2015:  BMI_2015_median

#############################################################################NEW


	
###################
	## 2016 analysis
	###################
	

	BMI_2016 <- as_tibble (input_all_2016_03_01)
	# Hmisc:: describe(BMI_2016)
	

	## check column names
	# names(BMI_2016) 
	

	

	#########################################  NEW  3rd march 2022
	######################################### CODE to: 1) link BMI values and actual measured data; 2) de-duplicate any duplicate values due to monthly_bmi formula
	#########################################  Will need to also create a cohort with all values for median_bmi analysis
	

	bmi_2016_long <- BMI_2016 %>%   ## 1. pivot_longer date measured columns
	    pivot_longer(
	      cols = c('bmi_march_date_measured', 'bmi_apr_date_measured', 'bmi_may_date_measured', 'bmi_june_date_measured', 'bmi_july_date_measured', 'bmi_aug_date_measured', 'bmi_sep_date_measured', 'bmi_oct_date_measured', 'bmi_nov_date_measured', 'bmi_dec_date_measured', 'bmi_jan_date_measured', 'bmi_feb_date_measured', 'bmi_jan_date_measured'),
	      values_to = "bmi_measured_date" ) %>% 
	    dplyr::arrange(patient_id, bmi_measured_date) %>%
	    tidyr::drop_na(bmi_measured_date) %>%
	    group_by(patient_id, bmi_measured_date) %>%
	    slice_head %>%                  #  step 2.  Pivot longer the values.  
	    pivot_longer(
	    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
	    names_to = "date", 
	    values_to = "monthly_bmi")  %>%    # step 3.  filter out duplicate row
	    dplyr::select("patient_id", 
	                  "name", 
	                  "bmi_measured_date",
	                  "date",
	                  "monthly_bmi",
	                  "type1_diabetes", 
	                  "type2_diabetes", 
	                  "unknown_diabetes",  
	                  "sex", 
	                  "age_group", 
	                  "region", 
	                  "imd", 
	                  "ethnicity",
	                  "ethnicity_16",
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
	                  "all_cancer")   %>%
	  mutate(measured_month = str_sub(name, 1, -15)) %>%  #3a.  create a column to identify matching events
	  dplyr::filter(measured_month == date) %>%
	  select(-'name', -'measured_month')
	

	

	

	long_bmi_2016 <- bmi_2016_long
	

	

	

	

	

	

	

	#  Hmisc::describe(long_bmi_2016)
	#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace
	 
	

	## replace very high and very low BMIs with NA:  This will exclude erroneus values and exclude patients who are severely underweight/severely obese whose change in BMI may not reflect general population trends
	long_bmi_2016$monthly_bmi[long_bmi_2016$monthly_bmi<15|long_bmi_2016$monthly_bmi>65] <- NA
	

	# Hmisc::describe(long_bmi_2016$monthly_bmi)
	# recoding successful
	

	 
	### Calculate the median BMI for each patient based on measurements in that year
	 
	bmi_2016_bypatid <- group_by(long_bmi_2016,patient_id)
	median_bmi_2016 <- dplyr::summarise(bmi_2016_bypatid,
	                            median_bmi = median(monthly_bmi, na.rm=TRUE)
	                            )
	 
	 ## check a few rows to see if median calculation works - it does
	 #check_bmi_median <- bmi_2016_bypatid %>%
	   #select(patient_id, monthly_bmi) %>%
	   #filter(patient_id<80)
	####################################################  NEW
	

	

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
	         "ethnicity",
	         "ethnicity_16")              
	

	BMI_2016_median <- BMI_2016_median %>%
	  mutate("year"= 2016)
	

	

	## FINAL DATA SET for 2016:  BMI_2016_median
	

	#############################################################################NEW
	

	
###################
	## 2017 analysis
	###################
	

	BMI_2017 <- as_tibble (input_all_2017_03_01)
	# Hmisc:: describe(BMI_2017)
	

	## check column names
	# names(BMI_2017) 
	

	

	#########################################  NEW  3rd march 2022
	######################################### CODE to: 1) link BMI values and actual measured data; 2) de-duplicate any duplicate values due to monthly_bmi formula
	#########################################  Will need to also create a cohort with all values for median_bmi analysis
	

	bmi_2017_long <- BMI_2017 %>%   ## 1. pivot_longer date measured columns
	    pivot_longer(
	      cols = c('bmi_march_date_measured', 'bmi_apr_date_measured', 'bmi_may_date_measured', 'bmi_june_date_measured', 'bmi_july_date_measured', 'bmi_aug_date_measured', 'bmi_sep_date_measured', 'bmi_oct_date_measured', 'bmi_nov_date_measured', 'bmi_dec_date_measured', 'bmi_jan_date_measured', 'bmi_feb_date_measured', 'bmi_jan_date_measured'),
	      values_to = "bmi_measured_date" ) %>% 
	    dplyr::arrange(patient_id, bmi_measured_date) %>%
	    tidyr::drop_na(bmi_measured_date) %>%
	    group_by(patient_id, bmi_measured_date) %>%
	    slice_head %>%                  #  step 2.  Pivot longer the values.  
	    pivot_longer(
	    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
	    names_to = "date", 
	    values_to = "monthly_bmi")  %>%    # step 3.  filter out duplicate row
	    dplyr::select("patient_id", 
	                  "name", 
	                  "bmi_measured_date",
	                  "date",
	                  "monthly_bmi",
	                  "type1_diabetes", 
	                  "type2_diabetes", 
	                  "unknown_diabetes",  
	                  "sex", 
	                  "age_group", 
	                  "region", 
	                  "imd", 
	                  "ethnicity",
	                  "ethnicity_16",
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
	                  "all_cancer")   %>%
	  mutate(measured_month = str_sub(name, 1, -15)) %>%  #3a.  create a column to identify matching events
	  dplyr::filter(measured_month == date) %>%
	  select(-'name', -'measured_month')
	

	

	

	long_bmi_2017 <- bmi_2017_long
	

	

	

	

	

	

	

	#  Hmisc::describe(long_bmi_2017)
	#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace
	 
	

	## replace very high and very low BMIs with NA:  This will exclude erroneus values and exclude patients who are severely underweight/severely obese whose change in BMI may not reflect general population trends
	long_bmi_2017$monthly_bmi[long_bmi_2017$monthly_bmi<15|long_bmi_2017$monthly_bmi>65] <- NA
	

	# Hmisc::describe(long_bmi_2017$monthly_bmi)
	# recoding successful
	

	 
	### Calculate the median BMI for each patient based on measurements in that year
	 
	bmi_2017_bypatid <- group_by(long_bmi_2017,patient_id)
	median_bmi_2017 <- dplyr::summarise(bmi_2017_bypatid,
	                            median_bmi = median(monthly_bmi, na.rm=TRUE)
	                            )
	 
	 ## check a few rows to see if median calculation works - it does
	 #check_bmi_median <- bmi_2017_bypatid %>%
	   #select(patient_id, monthly_bmi) %>%
	   #filter(patient_id<80)
	####################################################  NEW
	

	

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
	         "ethnicity",
	         "ethnicity_16")              
	

	BMI_2017_median <- BMI_2017_median %>%
	  mutate("year"= 2017)
	

	

	## FINAL DATA SET for 2017:  BMI_2017_median
	

	#############################################################################NEW
	
	
###################
	## 2018 analysis
	###################
	

	BMI_2018 <- as_tibble (input_all_2018_03_01)
	# Hmisc:: describe(BMI_2018)
	

	## check column names
	# names(BMI_2018) 
	

	

	#########################################  NEW  3rd march 2022
	######################################### CODE to: 1) link BMI values and actual measured data; 2) de-duplicate any duplicate values due to monthly_bmi formula
	#########################################  Will need to also create a cohort with all values for median_bmi analysis
	

	bmi_2018_long <- BMI_2018 %>%   ## 1. pivot_longer date measured columns
	    pivot_longer(
	      cols = c('bmi_march_date_measured', 'bmi_apr_date_measured', 'bmi_may_date_measured', 'bmi_june_date_measured', 'bmi_july_date_measured', 'bmi_aug_date_measured', 'bmi_sep_date_measured', 'bmi_oct_date_measured', 'bmi_nov_date_measured', 'bmi_dec_date_measured', 'bmi_jan_date_measured', 'bmi_feb_date_measured', 'bmi_jan_date_measured'),
	      values_to = "bmi_measured_date" ) %>% 
	    dplyr::arrange(patient_id, bmi_measured_date) %>%
	    tidyr::drop_na(bmi_measured_date) %>%
	    group_by(patient_id, bmi_measured_date) %>%
	    slice_head %>%                  #  step 2.  Pivot longer the values.  
	    pivot_longer(
	    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
	    names_to = "date", 
	    values_to = "monthly_bmi")  %>%    # step 3.  filter out duplicate row
	    dplyr::select("patient_id", 
	                  "name", 
	                  "bmi_measured_date",
	                  "date",
	                  "monthly_bmi",
	                  "type1_diabetes", 
	                  "type2_diabetes", 
	                  "unknown_diabetes",  
	                  "sex", 
	                  "age_group", 
	                  "region", 
	                  "imd", 
	                  "ethnicity",
	                  "ethnicity_16",
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
	                  "all_cancer")   %>%
	  mutate(measured_month = str_sub(name, 1, -15)) %>%  #3a.  create a column to identify matching events
	  dplyr::filter(measured_month == date) %>%
	  select(-'name', -'measured_month')
	

	

	

	long_bmi_2018 <- bmi_2018_long
	

	

	

	

	

	

	

	#  Hmisc::describe(long_bmi_2018)
	#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace
	 
	

	## replace very high and very low BMIs with NA:  This will exclude erroneus values and exclude patients who are severely underweight/severely obese whose change in BMI may not reflect general population trends
	long_bmi_2018$monthly_bmi[long_bmi_2018$monthly_bmi<15|long_bmi_2018$monthly_bmi>65] <- NA
	

	# Hmisc::describe(long_bmi_2018$monthly_bmi)
	# recoding successful
	

	 
	### Calculate the median BMI for each patient based on measurements in that year
	 
	bmi_2018_bypatid <- group_by(long_bmi_2018,patient_id)
	median_bmi_2018 <- dplyr::summarise(bmi_2018_bypatid,
	                            median_bmi = median(monthly_bmi, na.rm=TRUE)
	                            )
	 
	 ## check a few rows to see if median calculation works - it does
	 #check_bmi_median <- bmi_2018_bypatid %>%
	   #select(patient_id, monthly_bmi) %>%
	   #filter(patient_id<80)
	####################################################  NEW
	

	

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
	         "ethnicity",
	         "ethnicity_16")              
	

	BMI_2018_median <- BMI_2018_median %>%
	  mutate("year"= 2018)
	

	

	## FINAL DATA SET for 2018:  BMI_2018_median
	

	#############################################################################NEW
	
	
###################
	## 2019 analysis
	###################
	

	BMI_2019 <- as_tibble (input_all_2019_03_01)
	# Hmisc:: describe(BMI_2019)
	

	## check column names
	# names(BMI_2019) 
	

	

	#########################################  NEW  3rd march 2022
	######################################### CODE to: 1) link BMI values and actual measured data; 2) de-duplicate any duplicate values due to monthly_bmi formula
	#########################################  Will need to also create a cohort with all values for median_bmi analysis
	

	bmi_2019_long <- BMI_2019 %>%   ## 1. pivot_longer date measured columns
	    pivot_longer(
	      cols = c('bmi_march_date_measured', 'bmi_apr_date_measured', 'bmi_may_date_measured', 'bmi_june_date_measured', 'bmi_july_date_measured', 'bmi_aug_date_measured', 'bmi_sep_date_measured', 'bmi_oct_date_measured', 'bmi_nov_date_measured', 'bmi_dec_date_measured', 'bmi_jan_date_measured', 'bmi_feb_date_measured', 'bmi_jan_date_measured'),
	      values_to = "bmi_measured_date" ) %>% 
	    dplyr::arrange(patient_id, bmi_measured_date) %>%
	    tidyr::drop_na(bmi_measured_date) %>%
	    group_by(patient_id, bmi_measured_date) %>%
	    slice_head %>%                  #  step 2.  Pivot longer the values.  
	    pivot_longer(
	    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
	    names_to = "date", 
	    values_to = "monthly_bmi")  %>%    # step 3.  filter out duplicate row
	    dplyr::select("patient_id", 
	                  "name", 
	                  "bmi_measured_date",
	                  "date",
	                  "monthly_bmi",
	                  "type1_diabetes", 
	                  "type2_diabetes", 
	                  "unknown_diabetes",  
	                  "sex", 
	                  "age_group", 
	                  "region", 
	                  "imd", 
	                  "ethnicity",
	                  "ethnicity_16",
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
	                  "all_cancer")   %>%
	  mutate(measured_month = str_sub(name, 1, -15)) %>%  #3a.  create a column to identify matching events
	  dplyr::filter(measured_month == date) %>%
	  select(-'name', -'measured_month')
	

	

	

	long_bmi_2019 <- bmi_2019_long
	

	

	

	

	

	

	

	#  Hmisc::describe(long_bmi_2019)
	#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace
	 
	

	## replace very high and very low BMIs with NA:  This will exclude erroneus values and exclude patients who are severely underweight/severely obese whose change in BMI may not reflect general population trends
	long_bmi_2019$monthly_bmi[long_bmi_2019$monthly_bmi<15|long_bmi_2019$monthly_bmi>65] <- NA
	

	# Hmisc::describe(long_bmi_2019$monthly_bmi)
	# recoding successful
	

	 
	### Calculate the median BMI for each patient based on measurements in that year
	 
	bmi_2019_bypatid <- group_by(long_bmi_2019,patient_id)
	median_bmi_2019 <- dplyr::summarise(bmi_2019_bypatid,
	                            median_bmi = median(monthly_bmi, na.rm=TRUE)
	                            )
	 
	 ## check a few rows to see if median calculation works - it does
	 #check_bmi_median <- bmi_2019_bypatid %>%
	   #select(patient_id, monthly_bmi) %>%
	   #filter(patient_id<80)
	####################################################  NEW
	

	

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
	         "ethnicity",
	         "ethnicity_16")              
	

	BMI_2019_median <- BMI_2019_median %>%
	  mutate("year"= 2019)
	

	

	## FINAL DATA SET for 2019:  BMI_2019_median
	

	#############################################################################NEW
	
	
###################
	## 2020 analysis
	###################
	

	BMI_2020 <- as_tibble (input_all_2020_03_01)
	# Hmisc:: describe(BMI_2020)
	

	## check column names
	# names(BMI_2020) 
	

	

	#########################################  NEW  3rd march 2022
	######################################### CODE to: 1) link BMI values and actual measured data; 2) de-duplicate any duplicate values due to monthly_bmi formula
	#########################################  Will need to also create a cohort with all values for median_bmi analysis
	

	bmi_2020_long <- BMI_2020 %>%   ## 1. pivot_longer date measured columns
	    pivot_longer(
	      cols = c('bmi_march_date_measured', 'bmi_apr_date_measured', 'bmi_may_date_measured', 'bmi_june_date_measured', 'bmi_july_date_measured', 'bmi_aug_date_measured', 'bmi_sep_date_measured', 'bmi_oct_date_measured', 'bmi_nov_date_measured', 'bmi_dec_date_measured', 'bmi_jan_date_measured', 'bmi_feb_date_measured', 'bmi_jan_date_measured'),
	      values_to = "bmi_measured_date" ) %>% 
	    dplyr::arrange(patient_id, bmi_measured_date) %>%
	    tidyr::drop_na(bmi_measured_date) %>%
	    group_by(patient_id, bmi_measured_date) %>%
	    slice_head %>%                  #  step 2.  Pivot longer the values.  
	    pivot_longer(
	    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
	    names_to = "date", 
	    values_to = "monthly_bmi")  %>%    # step 3.  filter out duplicate row
	    dplyr::select("patient_id", 
	                  "name", 
	                  "bmi_measured_date",
	                  "date",
	                  "monthly_bmi",
	                  "type1_diabetes", 
	                  "type2_diabetes", 
	                  "unknown_diabetes",  
	                  "sex", 
	                  "age_group", 
	                  "region", 
	                  "imd", 
	                  "ethnicity",
	                  "ethnicity_16",
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
	                  "all_cancer")   %>%
	  mutate(measured_month = str_sub(name, 1, -15)) %>%  #3a.  create a column to identify matching events
	  dplyr::filter(measured_month == date) %>%
	  select(-'name', -'measured_month')
	

	

	

	long_bmi_2020 <- bmi_2020_long
	

	

	

	

	

	

	

	#  Hmisc::describe(long_bmi_2020)
	#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace
	 
	

	## replace very high and very low BMIs with NA:  This will exclude erroneus values and exclude patients who are severely underweight/severely obese whose change in BMI may not reflect general population trends
	long_bmi_2020$monthly_bmi[long_bmi_2020$monthly_bmi<15|long_bmi_2020$monthly_bmi>65] <- NA
	

	# Hmisc::describe(long_bmi_2020$monthly_bmi)
	# recoding successful
	

	 
	### Calculate the median BMI for each patient based on measurements in that year
	 
	bmi_2020_bypatid <- group_by(long_bmi_2020,patient_id)
	median_bmi_2020 <- dplyr::summarise(bmi_2020_bypatid,
	                            median_bmi = median(monthly_bmi, na.rm=TRUE)
	                            )
	 
	 ## check a few rows to see if median calculation works - it does
	 #check_bmi_median <- bmi_2020_bypatid %>%
	   #select(patient_id, monthly_bmi) %>%
	   #filter(patient_id<80)
	####################################################  NEW
	

	

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
	         "ethnicity",
	         "ethnicity_16")              
	

	BMI_2020_median <- BMI_2020_median %>%
	  mutate("year"= 2020)
	

	

	## FINAL DATA SET for 2020:  BMI_2020_median
	

	#############################################################################NEW
	


	
###################
	## 2021 analysis
	###################
	

	BMI_2021 <- as_tibble (input_all_2021_03_01)
	# Hmisc:: describe(BMI_2021)
	

	## check column names
	# names(BMI_2021) 
	

	

	#########################################  NEW  3rd march 2022
	######################################### CODE to: 1) link BMI values and actual measured data; 2) de-duplicate any duplicate values due to monthly_bmi formula
	#########################################  Will need to also create a cohort with all values for median_bmi analysis
	

	bmi_2021_long <- BMI_2021 %>%   ## 1. pivot_longer date measured columns
	    pivot_longer(
	      cols = c('bmi_march_date_measured', 'bmi_apr_date_measured', 'bmi_may_date_measured', 'bmi_june_date_measured', 'bmi_july_date_measured', 'bmi_aug_date_measured', 'bmi_sep_date_measured', 'bmi_oct_date_measured', 'bmi_nov_date_measured', 'bmi_dec_date_measured', 'bmi_jan_date_measured', 'bmi_feb_date_measured', 'bmi_jan_date_measured'),
	      values_to = "bmi_measured_date" ) %>% 
	    dplyr::arrange(patient_id, bmi_measured_date) %>%
	    tidyr::drop_na(bmi_measured_date) %>%
	    group_by(patient_id, bmi_measured_date) %>%
	    slice_head %>%                  #  step 2.  Pivot longer the values.  
	    pivot_longer(
	    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
	    names_to = "date", 
	    values_to = "monthly_bmi")  %>%    # step 3.  filter out duplicate row
	    dplyr::select("patient_id", 
	                  "name", 
	                  "bmi_measured_date",
	                  "date",
	                  "monthly_bmi",
	                  "type1_diabetes", 
	                  "type2_diabetes", 
	                  "unknown_diabetes",  
	                  "sex", 
	                  "age_group", 
	                  "region", 
	                  "imd", 
	                  "ethnicity",
	                  "ethnicity_16",
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
	                  "all_cancer")   %>%
	  mutate(measured_month = str_sub(name, 1, -15)) %>%  #3a.  create a column to identify matching events
	  dplyr::filter(measured_month == date) %>%
	  select(-'name', -'measured_month')
	

	

	

	long_bmi_2021 <- bmi_2021_long
	

	

	

	

	

	

	

	#  Hmisc::describe(long_bmi_2021)
	#  Missing BMIs have been recorded as '0' - will affect stats. Need to replace
	 
	

	## replace very high and very low BMIs with NA:  This will exclude erroneus values and exclude patients who are severely underweight/severely obese whose change in BMI may not reflect general population trends
	long_bmi_2021$monthly_bmi[long_bmi_2021$monthly_bmi<15|long_bmi_2021$monthly_bmi>65] <- NA
	

	# Hmisc::describe(long_bmi_2021$monthly_bmi)
	# recoding successful
	

	 
	### Calculate the median BMI for each patient based on measurements in that year
	 
	bmi_2021_bypatid <- group_by(long_bmi_2021,patient_id)
	median_bmi_2021 <- dplyr::summarise(bmi_2021_bypatid,
	                            median_bmi = median(monthly_bmi, na.rm=TRUE)
	                            )
	 
	 ## check a few rows to see if median calculation works - it does
	 #check_bmi_median <- bmi_2021_bypatid %>%
	   #select(patient_id, monthly_bmi) %>%
	   #filter(patient_id<80)
	####################################################  NEW
	

	

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
	         "ethnicity",
	         "ethnicity_16")              
	

	BMI_2021_median <- BMI_2021_median %>%
	  mutate("year"= 2021)
	

	

	## FINAL DATA SET for 2021:  BMI_2021_median
	

	#############################################################################NEW
	







############################################################################
############################################################################  

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
 patient_id, year, precovid_obese_flag, median_bmi, had_bmi, BMI_categories, BMI_over27.5, DWMP, sex, age_group, region, imd, ethnicity, ethnicity_16, ethnic_no_miss, starts_with("comorbid_"), 
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



