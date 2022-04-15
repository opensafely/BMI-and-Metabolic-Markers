#####  YEARLY COHORTS FOR BMI
##### Author: M Samuel
##### Updated:  7th March 2022, 15th April



## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(purrr)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)
library(skimr)



#BMI_2019 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "complete_meds_2019.feather"))





####################################
###################################
#####  read in files

BMI_2019 <- read_feather (here::here ("output/data", "complete_meds_2019.feather"))

###################
## 2019 analysis
###################
###################
## 2019 analysis
###################

BMI_2019 <- as_tibble (BMI_2019)




# label exposure variables
## recode and label some demographic components


## recode ethnicity so NA is 0
BMI_2019 <- BMI_2019 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

BMI_2019 <- BMI_2019 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 



### label
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="1"]<-"White"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="2"]<-"Mixed"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="3"]<-"Asian"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="4"]<-"Black"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="5"]<-"Other"
BMI_2019$ethnic_no_miss[BMI_2019$ethnic_no_miss=="0"]<-"Not_recorded"

BMI_2019 <- BMI_2019 %>%             
  mutate (ethnic_no_miss = as.factor(ethnic_no_miss)) %>%
  mutate (ethnic_no_miss = fct_relevel(ethnic_no_miss, "White", "Asian", "Black", "Mixed","Other", "Not_recorded"))

BMI_2019 <- BMI_2019  %>%
  dplyr::mutate(imd = na_if(imd, '0')) 



BMI_2019 <- BMI_2019 %>%             
  mutate (imd = as.factor(imd)) %>%
  mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5"))

BMI_2019 %>%
  tabyl(imd)


# had_bmi_imd_m <- glm(had_bmi ~ imd, data=BMI_2019, family=binomial) %>%
# broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
#  dplyr::mutate(across(where(is.numeric), round, digits = 2)) 

BMI_2019 <- BMI_2019 %>%
  mutate (eth_group_16=case_when(
    ethnicity_16_no_miss == "1" ~ "White_British",
    ethnicity_16_no_miss == "2" ~ "White_Irish",
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
                                     "White_British",
                                     "White_Irish",
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

## Generate a data set of demographic and exposure variables: To link to BMI data after data manipulation

demog_2019 <- BMI_2019  %>%
  dplyr::select(-starts_with("bmi")) %>% 
  dplyr::select(-starts_with("hba1c")) %>% 
  dplyr::select(-("eth")) %>%
  dplyr::select(-starts_with("ethnicity"))
  

colnames(demog_2019)




######################################### CODE to: 1) link BMI values and actual measured data; 2) de-duplicate any duplicate values due to monthly_bmi formula
#########################################  Will need to also create a cohort with all values for median_bmi analysis

bmi_2019_long <- BMI_2019 %>% 
  dplyr::select(patient_id, starts_with("bmi")) %>%
  dplyr::select(-("bmi"), -("bmi_date_measured")) %>%
   pivot_longer(                              ## 1. pivot_longer date measured columns
    cols = c('bmi_march_date_measured', 'bmi_apr_date_measured', 'bmi_may_date_measured', 'bmi_june_date_measured', 'bmi_july_date_measured', 'bmi_aug_date_measured', 'bmi_sep_date_measured', 'bmi_oct_date_measured', 'bmi_nov_date_measured', 'bmi_dec_date_measured', 'bmi_jan_date_measured', 'bmi_feb_date_measured', 'bmi_jan_date_measured'),
    values_to = "bmi_measured_date" ) %>% 
  dplyr::arrange(patient_id, bmi_measured_date) %>%
  tidyr::drop_na(bmi_measured_date) %>%
  group_by(patient_id, bmi_measured_date) %>%  ## filter out duplicate values!!
  slice_head %>%                  #  step 2.  Pivot longer the values.  
  pivot_longer(
    cols = c('bmi_march', 'bmi_apr', 'bmi_may', 'bmi_june', 'bmi_july', 'bmi_aug', 'bmi_sep', 'bmi_oct', 'bmi_nov', 'bmi_dec', 'bmi_jan', 'bmi_feb', 'bmi_jan'),
    names_to = "date", 
    values_to = "monthly_bmi")   %>%    # step 3.  filter out duplicate row created by the pivot step
  mutate(measured_month = str_sub(name, 1, -15)) %>%  #3a.  create a column to identify matching events
  dplyr::filter(measured_month == date) %>%
  select(-'name', -'measured_month')







##  Join with demographic data set to create a data set of all BMI values

BMI_complete_long <- demog_2019 %>%
  dplyr::left_join(bmi_2019_long) %>%
  dplyr::mutate(year=2019)

## DATA QUALITY CHECKS AND REPORTING:  HOW MANY VALUES FILTERED OUT ETC
### Check how many patients had a BMI check before filtering out very high and very low values


## TOTAL BMI VALUES BEFORE FILTERING HIGH AND LOW VALUES
BMI_data_checks <- BMI_complete_long %>%
  ungroup()%>%
  skim_without_charts%>%
  dplyr::select('skim_variable', 'n_missing', 'complete_rate') %>%
  dplyr::mutate(N_total = n_missing/(1-complete_rate)) %>% 
  dplyr::mutate(N_values = N_total - n_missing) %>% 
  dplyr::filter(skim_variable == 'monthly_bmi')




## NUMBER OF PATIENTS WITH A BMI BEFORE FILTERING OUT HIGH AND LOW VALUES
patients_with_bmi <- BMI_complete_long  %>%
  dplyr::group_by(patient_id) %>% 
  dplyr::summarise(patients_with_bmi_all = median(monthly_bmi, na.rm = TRUE))


patients_with_bmi_check <- skim_without_charts(patients_with_bmi)%>%
  dplyr::select('skim_variable', 'n_missing', 'complete_rate') %>%
  dplyr::mutate(N_total = n_missing/(1-complete_rate)) %>% 
  dplyr::mutate(N_values = N_total - n_missing) %>% 
  dplyr::filter(skim_variable == 'patients_with_bmi_all')



### REPLACE HIGH and LOW values with Missing

BMI_complete_long$monthly_bmi[BMI_complete_long$monthly_bmi<15|BMI_complete_long$monthly_bmi>65] <- NA

BMI_filtered_checks <- BMI_complete_long %>%
  dplyr::mutate(monthly_bmi_filtered = monthly_bmi) %>%
  ungroup()%>%
  skim_without_charts%>%
  dplyr::select('skim_variable', 'n_missing', 'complete_rate') %>%
  dplyr::mutate(N_total = n_missing/(1-complete_rate)) %>% 
  dplyr::mutate(N_values = N_total - n_missing) %>%
  dplyr::filter(skim_variable == 'monthly_bmi_filtered')

#>>>>>> FINAL LONG DATA SET::  BMI_complete_long




########################################################################################################################################################
#######################################################################################################################################################
## MEDIAN BMI ANALYSIS


## create a data set with patient's median BMI
### Very high and low values already filtered out

median_bmi <- BMI_complete_long  %>%
  dplyr::group_by(patient_id) %>% 
  dplyr::summarise(median_bmi = median(monthly_bmi, na.rm = TRUE))


## First complete the data checks to identify how many patients lost at each step
patients_with_bmi_filtered <- median_bmi %>%
  dplyr::mutate(patients_with_bmi_filtered = median_bmi) %>%
  skim_without_charts()%>%
  dplyr::select('skim_variable', 'n_missing', 'complete_rate') %>%
  dplyr::mutate(N_total = n_missing/(1-complete_rate)) %>% 
  dplyr::mutate(N_values = N_total - n_missing) %>% 
  dplyr::filter(skim_variable == 'patients_with_bmi_filtered')


## COMPLETE DATA CHECK TABLE

BMI_data_checks <- BMI_data_checks %>% 
  bind_rows(BMI_filtered_checks) %>%
  bind_rows(patients_with_bmi_check) %>%
  bind_rows(patients_with_bmi_filtered)


#################################################################################

## JOIN MEDIAN BMI RESULTS TO DEMOGRAPHIC AND COVARIATES TABLES
BMI_2019_median <- median_bmi %>% 
  left_join(demog_2019) %>% 
  dplyr::mutate("year" = "2019")


### classify as underweight, healthyweight, overweight, obese
BMI_complete_categories <- ungroup(BMI_2019_median)
BMI_complete_categories$BMI_categories <- cut(BMI_complete_categories$median_bmi, 
                                              breaks=c(0, 20,25,30,1000),
                                              labels= c("underweight", "healthy", "overweight", "obese"))

## classify as above 27.5

BMI_complete_categories$BMI_over27.5 <- cut(BMI_complete_categories$median_bmi,
                                            breaks=c(0,27.5,1000),
                                            labels=c("<27.5", "27.5+"))


##########  
########## .. Generate variable indicating DWMP eligibility




## confirm ethnicity categories
# dict_eth = {1: ‘White’, 2: ‘Mixed’, 3: ‘Asian’, 4: ‘Black’, 5: ‘Other’, np.nan: ‘Unknown’, 0: ‘Unknown’}

BMI_complete_categories_DWMP <- BMI_complete_categories %>%
  dplyr::mutate(
    DWMP = if_else(
      condition = ((((ethnic_no_miss=="White"| ethnic_no_miss=="Not_recorded") & median_bmi >=30) | ((ethnic_no_miss=="Asian"| ethnic_no_miss=="Black"| ethnic_no_miss=="Mixed"| ethnic_no_miss=="Other") & median_bmi >=27.5))
                   & ((hypertension==1| diabetes_t1==1| diabetes_t2==1))),
      true = "eligible", 
      false = "not_eligible"
    )
  )


# "White", "Asian", "Black", "Mixed","Other", "Not_recorded"

BMI_complete_categories_DWMP <-BMI_complete_categories_DWMP %>%
  dplyr::mutate(
    across(
      .cols = c(learning_disability,depression, dementia,psychosis_schiz_bipolar, diabetes_type, diabetes_t1, diabetes_t2, asthma, COPD, stroke_and_TIA, chronic_cardiac, hypertension, all_cancer), 
      .names = "comorbid_{col}"
    )
  )

BMI_complete_categories_DWMP <- BMI_complete_categories_DWMP %>% 
  dplyr::select(-c(learning_disability,depression, dementia,psychosis_schiz_bipolar, diabetes_type, diabetes_t1, diabetes_t2, asthma, COPD, stroke_and_TIA, chronic_cardiac, hypertension, all_cancer, type1_diabetes, type2_diabetes, unknown_diabetes))

BMI_complete_categories_DWMP <- ungroup (BMI_complete_categories_DWMP)



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


skim_without_charts(BMI_complete_categories_DWMP)
## SAVE BMI_data_checks as csv


###########################################################################################################
write.csv (BMI_data_checks, here::here ("output/data","BMI_data_checks_2019.csv"))

write_feather (BMI_complete_categories_DWMP, here::here ("output/data","BMI_complete_median_2019.feather"))

write_feather (BMI_complete_long, here::here ("output/data","BMI_complete_long_2019.feather"))