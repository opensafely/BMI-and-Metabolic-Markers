#####  YEARLY COHORTS FOR BMI
##### Author: M Samuel
##### Updated:  7th March 2022




## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(broom)
library(dplyr)
library(janitor)



####################################
###################################
#####  read in files

input_all_2019_03_01<- read_feather (here::here ("output/data", "input_all_2019-03-01.feather"))
###################
## 2019 analysis
###################

BMI_2019 <- as_tibble (input_all_2019_03_01)




# label exposure variables
## recode and label some demographic components


## recode ethnicity so NA is 0
BMI_2019 <- BMI_2019 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

BMI_2019 <- BMI_2019 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 





### label and relevel factors

BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="1"]<-"White"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="2"]<-"Mixed"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="3"]<-"Asian"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="4"]<-"Black"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="5"]<-"Other"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="0"]<-"Not_recorded"

BMI_2019 <- BMI_2019 %>%             
  mutate (ethnic_no_miss = as.factor(ethnic_no_miss)) %>%
  mutate (ethnic_no_miss = fct_relevel(ethnic_no_miss, "White", "Asian", "Black", "Mixed","Other", "Not_recorded"))



BMI_2019$imd[BMI_2019$imd=="0"]<-"NA"
BMI_2019$imd[BMI_2019$imd=="1"]<-"1 most deprived"
BMI_2019$imd[BMI_2019$imd=="2"]<-"2"
BMI_2019$imd[BMI_2019$imd=="3"]<-"3"
BMI_2019$imd[BMI_2019$imd=="4"]<-"4"
BMI_2019$imd[BMI_2019$imd=="5"]<-"5 least deprived"

BMI_2019 <- BMI_2019 %>%             
  mutate (imd = as.factor(imd)) %>%
  mutate (imd = fct_relevel(imd, "1 most deprived", "2", "3", "4", "5 least deprived", "NA"))




BMI_2019 <- BMI_2019 %>%
  mutate (eth_group_16=case_when(
    ethnicity_16_no_miss == "1" ~ "British",
    ethnicity_16_no_miss == "2" ~ "Irish",
    ethnicity_16_no_miss == "3" ~ "Other_White",
    ethnicity_16_no_miss == "4" ~ "White_Black_Carib",
    ethnicity_16_no_miss == "5" ~ "White_Black_African",
    ethnicity_16_no_miss == "6" ~ "White_Asian",
    ethnicity_16_no_miss == "7" ~ "Other_Mixed",
    ethnicity_16_no_miss == "8" ~ "Indian",
    ethnicity_16_no_miss == "9" ~ "Pakistani",
    ethnicity_16_no_miss == "10" ~ "Bangladeshi",
    ethnicity_16_no_miss == "11" ~ "Other_Asian",
    ethnicity_16_no_miss == "12" ~ "Caribbean",
    ethnicity_16_no_miss == "13" ~ "African",
    ethnicity_16_no_miss == "14" ~ "Other_Black",
    ethnicity_16_no_miss == "15" ~ "Chinese",
    ethnicity_16_no_miss == "16" ~ "Other",
    ethnicity_16_no_miss ==  "0" ~  "Missing"))  


BMI_2019 <- BMI_2019 %>%             
  mutate (eth_group_16 = as.factor(eth_group_16)) %>%
  mutate ( eth_group_16= fct_relevel(eth_group_16, 
                                             "British",
                                             "Irish",
                                             "Other_White",
                                             "Indian",
                                             "Pakistani",
                                             "Bangladeshi",
                                             "Other_Asian",
                                             "Caribbean",
                                             "African",
                                             "Other_Black",
                                             "Chinese",
                                             "White_Asian",
                                             "White_Black_Carib",
                                             "White_Black_African",
                                             "Other_Mixed",
                                             "Other",
                                             "Missing"))





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


## Add a year flag
BMI_complete_long <- long_bmi_2019 %>%
  mutate("year"= 2019)



#>>>>>> FINAL LONG DATA SET::  BMI_complete_long

########################################################################################################################################################
#######################################################################################################################################################
## MEDIAN BMI ANALYSIS

### Calculate the median BMI for each patient based on measurements in that year

bmi_2019_bypatid <- group_by(long_bmi_2019,patient_id)
median_bmi_2019 <- dplyr::summarise(bmi_2019_bypatid,
                                    median_bmi = median(monthly_bmi, na.rm=TRUE)
)





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


## 2019 mediam BMI per patient:  BMI_2019_median



## CREATE FLAGS and LABEL DEMOGRAPHIS

BMI_complete_median <- BMI_2019_median



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
    patient_id, year,  median_bmi, had_bmi, BMI_categories, BMI_over27.5, DWMP, sex, age_group, region, imd, ethnicity, ethnicity_16, ethnic_no_miss, ethnicity_16_no_miss, starts_with("comorbid_"), 
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







###########################################################################################################




###########################################################################################################

write_feather (BMI_complete_categories_DWMP, here::here ("output/data","BMI_complete_median_2019.feather"))

write_feather (BMI_complete_long, here::here ("output/data","BMI_complete_long_2019.feather"))
