## Miriam Samuel
## Modified: 23rd March 2022
## Create a data set that has data on those who did and did not have BMI measured to perform a regression.



# 1) Read in files
#  BMI_complete_median_2021.feather :: contains BMI data on those who had a BMI check
# input_all_2021:: contains demographic data on whole population
# BMI_complete_median: contains the pre-covid obese flag
# >> Combining these files to create a file for the analysis of who had a BMI

##  packages
library(broom)
library(purrr)
library(dplyr)
library(janitor)
library(tidyverse)
library(arrow)

BMI_complete_categories <- read_feather (here::here ("output/data", "BMI_complete_median_2021.feather"))
input_all_2021_03_01 <- read_feather (here::here ("output/data", "input_all_2021-03-01.feather"))
precovid_obese <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))
  
## Input data on all patients (not just those with a BMI)
all_patients_2021 <- as_tibble (input_all_2021_03_01)




# 2) recode and label some demographic components


# recode ethnicity so NA is 0
all_patients_2021 <- all_patients_2021 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

all_patients_2021 <- all_patients_2021 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 





# label
all_patients_2021$ethnic_no_miss[all_patients_2021$ethnic_no_miss=="1"]<-"White"
all_patients_2021$ethnic_no_miss[all_patients_2021$ethnic_no_miss=="2"]<-"Mixed"
all_patients_2021$ethnic_no_miss[all_patients_2021$ethnic_no_miss=="3"]<-"Asian"
all_patients_2021$ethnic_no_miss[all_patients_2021$ethnic_no_miss=="4"]<-"Black"
all_patients_2021$ethnic_no_miss[all_patients_2021$ethnic_no_miss=="5"]<-"Other"
all_patients_2021$ethnic_no_miss[all_patients_2021$ethnic_no_miss=="0"]<-"Not_recorded"

all_patients_2021 <- all_patients_2021 %>%             
  mutate (ethnic_no_miss = as.factor(ethnic_no_miss)) %>%
  mutate (ethnic_no_miss = fct_relevel(ethnic_no_miss, "White", "Asian", "Black", "Mixed","Other", "Not_recorded"))



all_patients_2021$imd[all_patients_2021$imd=="0"]<-"NA"


all_patients_2021 <- all_patients_2021 %>%             
  mutate (imd = as.factor(imd)) %>%
  mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5", "NA"))




all_patients_2021 <- all_patients_2021 %>%
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


all_patients_2021 <- all_patients_2021 %>%             
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



# 3) Select relevant variables and restructure code to allow functions across rows

all_patients_2021 <- all_patients_2021 %>%
dplyr::select("patient_id", 
              "sex", 
              "age_group", 
              "region", 
              "imd",                 
              "ethnic_no_miss",
              "eth_group_16",
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
                "all_cancer") 


all_patients_2021 <- all_patients_2021 %>%
dplyr::mutate(
  across(
    .cols = c(learning_disability,depression, dementia,psychosis_schiz_bipolar, diabetes_type, diabetes_t1, diabetes_t2, asthma, COPD, stroke_and_TIA, chronic_cardiac, hypertension, all_cancer), 
    .names = "comorbid_{col}")) %>%
  dplyr::select(
    patient_id, had_bmi, sex, age_group, region, imd, ethnic_no_miss, eth_group_16, starts_with("comorbid_"))






#######################################################################

# 4)  BMI_complete_categories has all the data on BMI.  Need to link it with demographic data from the extracted cohort. 

BMI_complete_categories <- as_tibble(BMI_complete_categories)


BMI_complete_categories_all <- BMI_complete_categories %>%
  dplyr::select(patient_id,
                year,
                median_bmi,
                obese,
                BMI_categories,
                BMI_over27.5,
                DWMP)


BMI_complete_categories_all <- left_join(all_patients_2021, BMI_complete_categories_all, by='patient_id')


################################################################################
# 5) add the pre-covid obese flag
precovid_obese <- precovid_obese %>%
  dplyr::filter(year==2021) %>%
  dplyr::select(patient_id, 
                precovid_obese_flag)


BMI_complete_categories_all <- left_join(BMI_complete_categories_all, precovid_obese, by='patient_id')

### save outputs as feather

write_feather (BMI_complete_categories_all, here::here ("output/data","BMI_all_2021.feather"))
