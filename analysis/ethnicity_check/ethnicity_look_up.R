#####  Check of Ethnicity miscoding
##### Author: M Samuel
##### Updated:  No 2022, 15th April



 ## Due to a factor/numeric transformation ethnicity data has been miscoded.  The following tables will show how the groups have been miscoded.
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



#BMI_2021 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "complete_meds_2021.feather"))





####################################
###################################
#####  read in files

BMI_2021 <- read_feather (here::here ("output/data", "complete_meds_2021.feather"))

###################
## 2021 analysis
###################
###################
## 2021 analysis
###################

BMI_2021 <- as_tibble (BMI_2021)




# label exposure variables
## recode and label some demographic components


## recode ethnicity so NA is 0
## created an error in the coding of the groups.

BMI_2021 <- BMI_2021 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

## eth_check <- eth_check %>% 
## dplyr::mutate(ethnicity_numeric = as.numeric(ethnicity))
## !! This was the error.  Factors do not directly translate into the correct numeric codes 
   ## !! The groups were forced into the wrong groups in R during the if.else transformation




BMI_2021 <- BMI_2021 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 






eth_check <- BMI_2021 %>% 
  dplyr::select(ethnicity, ethnic_no_miss)


## create correlation chart
eth_correlation <- eth_check %>% 
  group_by(ethnicity) %>% 
  slice_head()



eth_correlation <- eth_correlation %>% 
  mutate (real_ethnicity=case_when(
    ethnicity == "1" ~ "White",
    ethnicity == "2" ~ "Mixed",
    ethnicity == "3" ~ "Asian",
    ethnicity == "4" ~ "Black",
    ethnicity == "5" ~ "Other"))


eth_correlation <- eth_correlation %>% 
  mutate (ethnic_group_reported =case_when(
    ethnic_no_miss == "1" ~ "White",
    ethnic_no_miss == "2" ~ "Mixed",
    ethnic_no_miss == "3" ~ "Asian",
    ethnic_no_miss == "4" ~ "Black",
    ethnic_no_miss == "5" ~ "Other", 
    ethnic_no_miss == "0" ~ "Not_recorded"))


eth_correlation <- eth_correlation %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity, ethnic_no_miss))



## check numbers
eth_check_number <- eth_check %>% 
  tabyl(ethnicity, ethnic_no_miss)


### label
eth_check$ethnic_no_miss[eth_check$ethnic_no_miss=="1"]<-"White"
eth_check$ethnic_no_miss[eth_check$ethnic_no_miss=="2"]<-"Mixed"
eth_check$ethnic_no_miss[eth_check$ethnic_no_miss=="3"]<-"Asian"
eth_check$ethnic_no_miss[eth_check$ethnic_no_miss=="4"]<-"Black"
eth_check$ethnic_no_miss[eth_check$ethnic_no_miss=="5"]<-"Other"
eth_check$ethnic_no_miss[eth_check$ethnic_no_miss=="0"]<-"Not_recorded"



eth_check <- eth_check %>% 
  mutate (ethnicity =case_when(
    ethnicity == "1" ~ "White",
    ethnicity == "2" ~ "Mixed",
    ethnicity == "3" ~ "Asian",
    ethnicity == "4" ~ "Black",
    ethnicity == "5" ~ "Other", 
    ethnicity == "0" ~ "Not_recorded"))


eth_check_labels <- eth_check %>% 
  tabyl(ethnicity, ethnic_no_miss)




### ETHNICITY 16 GROUPS

## Look up table for ethnicity_16

eth_16_check <- BMI_2021 %>% 
  dplyr::select (ethnicity_16, ethnicity_16_no_miss)


## create correlation chart
eth_16_correlation <- eth_16_check %>% 
  group_by(ethnicity_16) %>% 
  slice_head() %>% 
  ungroup()



eth_16_correlation <- eth_16_correlation  %>% 
  mutate (real_ethnicity_16 = case_when(
    ethnicity_16 == "1" ~ "White_British",
    ethnicity_16 == "2" ~ "White_Irish",
    ethnicity_16 == "3" ~ "Other_White",
    ethnicity_16 == "4" ~ "White_Black_Carib",
    ethnicity_16 == "5" ~ "White_Black_African",
    ethnicity_16 == "6" ~ "White_Asian",
    ethnicity_16 == "7" ~ "Other_Mixed",
    ethnicity_16 == "8" ~ "Indian",
    ethnicity_16 == "9" ~ "Pakistani",
    ethnicity_16 == "10" ~ "Bangladeshi",
    ethnicity_16 == "11" ~ "Other_Asian",
    ethnicity_16 == "12" ~ "Caribbean",
    ethnicity_16 == "13" ~ "African",
    ethnicity_16 == "14" ~ "Other_Black",
    ethnicity_16 == "15" ~ "Chinese",
    ethnicity_16 == "16" ~ "Other",
    ethnicity_16 ==  "0" ~  "Missing")) 



eth_16_correlation <- eth_16_correlation  %>% 
  mutate (ethnic_group_reported =case_when(
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

## look up table
eth_16_correlation <- eth_16_correlation %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity_16, ethnicity_16_no_miss))


eth_16_check_number <- eth_16_check %>% 
  tabyl(ethnicity_16, ethnicity_16_no_miss)


eth_16_check <- eth_16_check  %>% 
  mutate (ethnicity_16_no_miss=case_when(
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



eth_16_check <- eth_16_check  %>% 
  mutate (ethnicity_16=case_when(
    ethnicity_16 == "1" ~ "White_British",
    ethnicity_16 == "2" ~ "White_Irish",
    ethnicity_16 == "3" ~ "Other_White",
    ethnicity_16 == "4" ~ "White_Black_Carib",
    ethnicity_16 == "5" ~ "White_Black_African",
    ethnicity_16 == "6" ~ "White_Asian",
    ethnicity_16 == "7" ~ "Other_Mixed",
    ethnicity_16 == "8" ~ "Indian",
    ethnicity_16 == "9" ~ "Pakistani",
    ethnicity_16 == "10" ~ "Bangladeshi",
    ethnicity_16 == "11" ~ "Other_Asian",
    ethnicity_16 == "12" ~ "Caribbean",
    ethnicity_16 == "13" ~ "African",
    ethnicity_16 == "14" ~ "Other_Black",
    ethnicity_16 == "15" ~ "Chinese",
    ethnicity_16 == "16" ~ "Other",
    ethnicity_16 ==  "0" ~  "Missing")) 



eth_16_check_labels <- eth_16_check %>% 
  tabyl(ethnicity_16, ethnicity_16_no_miss)

eth_16_correlation
eth_16_check_labels

eth_correlation
eth_check_labels


###########################################################################################################
write.csv (eth_16_check_labels, here::here ("output/data","eth_16_labels.csv"))
write.csv (eth_16_correlation, here::here ("output/data","eth_16_correlation_table_21.csv"))

write.csv (eth_check_labels, here::here ("output/data","eth_6_labels.csv"))
write.csv (eth_correlation, here::here ("output/data","eth_6_correlation_table_21.csv"))

