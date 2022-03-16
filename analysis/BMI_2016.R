#####  YEARLY COHORTS FOR BMI
##### Author: M Samuel
##### Updated:  7th March 2022




## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)



####################################
###################################




#####  read in files

input_all_2016_03_01<- read_feather (here::here ("output/data", "input_all_2016-03-01.feather"))


###################
## 2016 analysis
###################

BMI_2016 <- as_tibble (input_all_2016_03_01)




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




## recode and label some demographic components


## recode ethnicity so NA is 0
BMI_complete_long <- long_bmi_2016 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

BMI_complete_long <- BMI_complete_long %>%
mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 


### label
BMI_complete_long$ethnic_no_miss[BMI_complete_long$ethnic_no_miss=="1"]<-"White"
BMI_complete_long$ethnic_no_miss[BMI_complete_long$ethnic_no_miss=="2"]<-"Mixed"
BMI_complete_long$ethnic_no_miss[BMI_complete_long$ethnic_no_miss=="3"]<-"Asian or Asian British"
BMI_complete_long$ethnic_no_miss[BMI_complete_long$ethnic_no_miss=="4"]<-"Black or Black British"
BMI_complete_long$ethnic_no_miss[BMI_complete_long$ethnic_no_miss=="5"]<-"Other ethnic groups"
BMI_complete_long$ethnic_no_miss[BMI_complete_long$ethnic_no_miss=="0"]<-"No ethnicity recorded"

BMI_complete_long$imd[BMI_complete_long$imd=="0"]<-"NA"
BMI_complete_long$imd[BMI_complete_long$imd=="1"]<-"1 most deprived"
BMI_complete_long$imd[BMI_complete_long$imd=="2"]<-"2"
BMI_complete_long$imd[BMI_complete_long$imd=="3"]<-"3"
BMI_complete_long$imd[BMI_complete_long$imd=="4"]<-"4"
BMI_complete_long$imd[BMI_complete_long$imd=="5"]<-"5 least deprived"


BMI_complete_long <- BMI_complete_long %>%
  mutate (eth_group_16=case_when(
    ethnicity_16_no_miss == "1" ~ "British",
    ethnicity_16_no_miss == "2" ~ "Irish",
    ethnicity_16_no_miss == "3" ~ "Other White",
    ethnicity_16_no_miss == "4" ~ "White and Black Caribbean",
    ethnicity_16_no_miss == "5" ~ "White and Black African",
    ethnicity_16_no_miss == "6" ~ "White and Asian",
    ethnicity_16_no_miss == "7" ~ "Other Mixed",
    ethnicity_16_no_miss == "8" ~ "Indian",
    ethnicity_16_no_miss == "9" ~ "Pakistani",
    ethnicity_16_no_miss == "10" ~ "Bangladeshi",
    ethnicity_16_no_miss == "11" ~ "Other Asian",
    ethnicity_16_no_miss == "12" ~ "Caribbean",
    ethnicity_16_no_miss == "13" ~ "African",
    ethnicity_16_no_miss == "14" ~ "Other Black",
    ethnicity_16_no_miss == "15" ~ "Chinese",
    ethnicity_16_no_miss == "16" ~ "Any other ethnic group",
    ethnicity_16_no_miss ==  "0" ~  "Missing"))  

BMI_complete_long <- BMI_complete_long %>%
 mutate("year"= 2016)

#>>>>>> FINAL LONG DATA SET::  BMI_complete_long

########################################################################################################################################################
#######################################################################################################################################################
## MEDIAN BMI ANALYSIS

### Calculate the median BMI for each patient based on measurements in that year
 
bmi_2016_bypatid <- group_by(long_bmi_2016,patient_id)
median_bmi_2016 <- dplyr::summarise(bmi_2016_bypatid,
                            median_bmi = median(monthly_bmi, na.rm=TRUE)
                            )
 




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


## 2016 mediam BMI per patient:  BMI_2016_median



## CREATE FLAGS and LABEL DEMOGRAPHIS

BMI_complete_median <- BMI_2016_median



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

BMI_complete_categories_DWMP <- BMI_complete_categories_DWMP %>%
mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 ))


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




BMI_complete_categories_DWMP <- ungroup (BMI_complete_categories_DWMP)

BMI_complete_categories_DWMP <- BMI_complete_categories_DWMP %>%
 dplyr::select(
 patient_id, year, median_bmi, had_bmi, BMI_categories, BMI_over27.5, DWMP, sex, age_group, region, imd, ethnicity, ethnicity_16, ethnic_no_miss, ethnicity_16_no_miss, starts_with("comorbid_"), 
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



BMI_complete_categories <- BMI_complete_categories_DWMP %>%
  mutate (eth_group_16=case_when(
    ethnicity_16_no_miss == "1" ~ "British",
    ethnicity_16_no_miss == "2" ~ "Irish",
    ethnicity_16_no_miss == "3" ~ "Other White",
    ethnicity_16_no_miss == "4" ~ "White and Black Caribbean",
    ethnicity_16_no_miss == "5" ~ "White and Black African",
    ethnicity_16_no_miss == "6" ~ "White and Asian",
    ethnicity_16_no_miss == "7" ~ "Other Mixed",
    ethnicity_16_no_miss == "8" ~ "Indian",
    ethnicity_16_no_miss == "9" ~ "Pakistani",
    ethnicity_16_no_miss == "10" ~ "Bangladeshi",
    ethnicity_16_no_miss == "11" ~ "Other Asian",
    ethnicity_16_no_miss == "12" ~ "Caribbean",
    ethnicity_16_no_miss == "13" ~ "African",
    ethnicity_16_no_miss == "14" ~ "Other Black",
    ethnicity_16_no_miss == "15" ~ "Chinese",
    ethnicity_16_no_miss == "16" ~ "Any other ethnic group",
    ethnicity_16_no_miss ==  "0" ~  "Missing"))  







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

write_feather (BMI_complete_categories, here::here ("output/data","BMI_complete_median_2016.feather"))

write_feather (BMI_complete_long, here::here ("output/data","BMI_complete_long_2016.feather"))
