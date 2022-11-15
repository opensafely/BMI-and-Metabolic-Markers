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
#BMI_2020 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "complete_meds_2020.feather"))
#BMI_2019 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "complete_meds_2019.feather"))
#BMI_2018 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "complete_meds_2018.feather"))
#BMI_2017 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "complete_meds_2017.feather"))




####################################
###################################
#####  read in files

BMI_2021 <- read_feather (here::here ("output/data", "complete_meds_2021.feather"))
BMI_2020 <- read_feather (here::here ("output/data", "complete_meds_2020.feather"))
BMI_2019 <- read_feather (here::here ("output/data", "complete_meds_2019.feather"))
BMI_2018 <- read_feather (here::here ("output/data", "complete_meds_2018.feather"))
BMI_2017 <- read_feather (here::here ("output/data", "complete_meds_2017.feather"))


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






eth_check_2021 <- BMI_2021 %>% 
  dplyr::select(ethnicity, ethnic_no_miss)


## create correlation_21 chart
eth_correlation_21 <- eth_check_2021 %>% 
  group_by(ethnicity) %>% 
  slice_head()



eth_correlation_21 <- eth_correlation_21 %>% 
  mutate (real_ethnicity=case_when(
    ethnicity == "1" ~ "White",
    ethnicity == "2" ~ "Mixed",
    ethnicity == "3" ~ "Asian",
    ethnicity == "4" ~ "Black",
    ethnicity == "5" ~ "Other"))


eth_correlation_21 <- eth_correlation_21 %>% 
  mutate (ethnic_group_reported_21 =case_when(
    ethnic_no_miss == "1" ~ "White",
    ethnic_no_miss == "2" ~ "Mixed",
    ethnic_no_miss == "3" ~ "Asian",
    ethnic_no_miss == "4" ~ "Black",
    ethnic_no_miss == "5" ~ "Other", 
    ethnic_no_miss == "0" ~ "Not_recorded"))


eth_correlation_21 <- eth_correlation_21 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity, ethnic_no_miss))%>% 
  dplyr::mutate (year = "2021")






### ETHNICITY 16 GROUPS

## Look up table for ethnicity_16

eth_16_check_2021 <- BMI_2021 %>% 
  dplyr::select (ethnicity_16, ethnicity_16_no_miss)


## create correlation_21 chart
eth_16_correlation_21 <- eth_16_check_2021 %>% 
  group_by(ethnicity_16) %>% 
  slice_head() %>% 
  ungroup()



eth_16_correlation_21 <- eth_16_correlation_21  %>% 
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



eth_16_correlation_21 <- eth_16_correlation_21  %>% 
  mutate (ethnic_group_reported_21 =case_when(
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
eth_16_correlation_21 <- eth_16_correlation_21 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity_16, ethnicity_16_no_miss)) %>% 
  dplyr::mutate (year = "2021")




## REPEAT FOR OTHER YEARS: 2020 
BMI_2020 <- as_tibble (BMI_2020)




# label exposure variables
## recode and label some demographic components


## recode ethnicity so NA is 0
## created an error in the coding of the groups.

BMI_2020 <- BMI_2020 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

## eth_check <- eth_check %>% 
## dplyr::mutate(ethnicity_numeric = as.numeric(ethnicity))
## !! This was the error.  Factors do not directly translate into the correct numeric codes 
## !! The groups were forced into the wrong groups in R during the if.else transformation




BMI_2020 <- BMI_2020 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 






eth_check_2020 <- BMI_2020 %>% 
  dplyr::select(ethnicity, ethnic_no_miss)


## create correlation_20 chart
eth_correlation_20 <- eth_check_2020 %>% 
  group_by(ethnicity) %>% 
  slice_head()



eth_correlation_20 <- eth_correlation_20 %>% 
  mutate (real_ethnicity=case_when(
    ethnicity == "1" ~ "White",
    ethnicity == "2" ~ "Mixed",
    ethnicity == "3" ~ "Asian",
    ethnicity == "4" ~ "Black",
    ethnicity == "5" ~ "Other"))


eth_correlation_20 <- eth_correlation_20 %>% 
  mutate (ethnic_group_reported_20 =case_when(
    ethnic_no_miss == "1" ~ "White",
    ethnic_no_miss == "2" ~ "Mixed",
    ethnic_no_miss == "3" ~ "Asian",
    ethnic_no_miss == "4" ~ "Black",
    ethnic_no_miss == "5" ~ "Other", 
    ethnic_no_miss == "0" ~ "Not_recorded"))


eth_correlation_20 <- eth_correlation_20 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity, ethnic_no_miss))%>% 
  dplyr::mutate (year = "2020")






### ETHNICITY 16 GROUPS

## Look up table for ethnicity_16

eth_16_check_2020 <- BMI_2020 %>% 
  dplyr::select (ethnicity_16, ethnicity_16_no_miss)


## create correlation_20 chart
eth_16_correlation_20 <- eth_16_check_2020 %>% 
  group_by(ethnicity_16) %>% 
  slice_head() %>% 
  ungroup()



eth_16_correlation_20 <- eth_16_correlation_20  %>% 
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



eth_16_correlation_20 <- eth_16_correlation_20  %>% 
  mutate (ethnic_group_reported_20 =case_when(
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
eth_16_correlation_20 <- eth_16_correlation_20 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity_16, ethnicity_16_no_miss)) %>% 
  dplyr::mutate (year = "2020")




## REPEAT FOR OTHER YEARS: 2019
BMI_2019 <- as_tibble (BMI_2019)




# label exposure variables
## recode and label some demographic components


## recode ethnicity so NA is 0
## created an error in the coding of the groups.

BMI_2019 <- BMI_2019 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

## eth_check <- eth_check %>% 
## dplyr::mutate(ethnicity_numeric = as.numeric(ethnicity))
## !! This was the error.  Factors do not directly translate into the correct numeric codes 
## !! The groups were forced into the wrong groups in R during the if.else transformation




BMI_2019 <- BMI_2019 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 






eth_check_2019 <- BMI_2019 %>% 
  dplyr::select(ethnicity, ethnic_no_miss)


## create correlation_19 chart
eth_correlation_19 <- eth_check_2019 %>% 
  group_by(ethnicity) %>% 
  slice_head()



eth_correlation_19 <- eth_correlation_19 %>% 
  mutate (real_ethnicity=case_when(
    ethnicity == "1" ~ "White",
    ethnicity == "2" ~ "Mixed",
    ethnicity == "3" ~ "Asian",
    ethnicity == "4" ~ "Black",
    ethnicity == "5" ~ "Other"))


eth_correlation_19 <- eth_correlation_19 %>% 
  mutate (ethnic_group_reported_19 =case_when(
    ethnic_no_miss == "1" ~ "White",
    ethnic_no_miss == "2" ~ "Mixed",
    ethnic_no_miss == "3" ~ "Asian",
    ethnic_no_miss == "4" ~ "Black",
    ethnic_no_miss == "5" ~ "Other", 
    ethnic_no_miss == "0" ~ "Not_recorded"))


eth_correlation_19 <- eth_correlation_19 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity, ethnic_no_miss)) %>% 
  dplyr::mutate (year = "2019")






### ETHNICITY 16 GROUPS

## Look up table for ethnicity_16

eth_16_check_2019 <- BMI_2019 %>% 
  dplyr::select (ethnicity_16, ethnicity_16_no_miss) 


## create correlation_19 chart
eth_16_correlation_19 <- eth_16_check_2019 %>% 
  group_by(ethnicity_16) %>% 
  slice_head() %>% 
  ungroup()



eth_16_correlation_19 <- eth_16_correlation_19  %>% 
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



eth_16_correlation_19 <- eth_16_correlation_19  %>% 
  mutate (ethnic_group_reported_19 =case_when(
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
eth_16_correlation_19 <- eth_16_correlation_19 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity_16, ethnicity_16_no_miss)) %>% 
  dplyr::mutate (year = "2019")



## repeat other years

BMI_2018 <- as_tibble (BMI_2018)




# label exposure variables
## recode and label some demographic components


## recode ethnicity so NA is 0
## created an error in the coding of the groups.

BMI_2018 <- BMI_2018 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

## eth_check <- eth_check %>% 
## dplyr::mutate(ethnicity_numeric = as.numeric(ethnicity))
## !! This was the error.  Factors do not directly translate into the correct numeric codes 
## !! The groups were forced into the wrong groups in R during the if.else transformation




BMI_2018 <- BMI_2018 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 






eth_check_2018 <- BMI_2018 %>% 
  dplyr::select(ethnicity, ethnic_no_miss)


## create correlation_18 chart
eth_correlation_18 <- eth_check_2018 %>% 
  group_by(ethnicity) %>% 
  slice_head()



eth_correlation_18 <- eth_correlation_18 %>% 
  mutate (real_ethnicity=case_when(
    ethnicity == "1" ~ "White",
    ethnicity == "2" ~ "Mixed",
    ethnicity == "3" ~ "Asian",
    ethnicity == "4" ~ "Black",
    ethnicity == "5" ~ "Other"))


eth_correlation_18 <- eth_correlation_18 %>% 
  mutate (ethnic_group_reported_18 =case_when(
    ethnic_no_miss == "1" ~ "White",
    ethnic_no_miss == "2" ~ "Mixed",
    ethnic_no_miss == "3" ~ "Asian",
    ethnic_no_miss == "4" ~ "Black",
    ethnic_no_miss == "5" ~ "Other", 
    ethnic_no_miss == "0" ~ "Not_recorded"))


eth_correlation_18 <- eth_correlation_18 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity, ethnic_no_miss))%>% 
  dplyr::mutate (year = "2018")






### ETHNICITY 16 GROUPS

## Look up table for ethnicity_16

eth_16_check_2018 <- BMI_2018 %>% 
  dplyr::select (ethnicity_16, ethnicity_16_no_miss)


## create correlation_18 chart
eth_16_correlation_18 <- eth_16_check_2018 %>% 
  group_by(ethnicity_16) %>% 
  slice_head() %>% 
  ungroup()



eth_16_correlation_18 <- eth_16_correlation_18  %>% 
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



eth_16_correlation_18 <- eth_16_correlation_18  %>% 
  mutate (ethnic_group_reported_18 =case_when(
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
eth_16_correlation_18 <- eth_16_correlation_18 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity_16, ethnicity_16_no_miss)) %>% 
  dplyr::mutate (year = "2018")



## REPEAT FOR OTHER YEARS


BMI_2017 <- as_tibble (BMI_2017)




# label exposure variables
## recode and label some demographic components


## recode ethnicity so NA is 0
## created an error in the coding of the groups.

BMI_2017 <- BMI_2017 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

## eth_check <- eth_check %>% 
## dplyr::mutate(ethnicity_numeric = as.numeric(ethnicity))
## !! This was the error.  Factors do not directly translate into the correct numeric codes 
## !! The groups were forced into the wrong groups in R during the if.else transformation



BMI_2017 <- BMI_2017 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 






eth_check_2017 <- BMI_2017 %>% 
  dplyr::select(ethnicity, ethnic_no_miss)


## create correlation_17 chart
eth_correlation_17 <- eth_check_2017 %>% 
  group_by(ethnicity) %>% 
  slice_head()



eth_correlation_17 <- eth_correlation_17 %>% 
  mutate (real_ethnicity=case_when(
    ethnicity == "1" ~ "White",
    ethnicity == "2" ~ "Mixed",
    ethnicity == "3" ~ "Asian",
    ethnicity == "4" ~ "Black",
    ethnicity == "5" ~ "Other"))


eth_correlation_17 <- eth_correlation_17 %>% 
  mutate (ethnic_group_reported_17 =case_when(
    ethnic_no_miss == "1" ~ "White",
    ethnic_no_miss == "2" ~ "Mixed",
    ethnic_no_miss == "3" ~ "Asian",
    ethnic_no_miss == "4" ~ "Black",
    ethnic_no_miss == "5" ~ "Other", 
    ethnic_no_miss == "0" ~ "Not_recorded"))


eth_correlation_17 <- eth_correlation_17 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity, ethnic_no_miss))%>% 
  dplyr::mutate (year = "2017")






### ETHNICITY 16 GROUPS

## Look up table for ethnicity_16

eth_16_check_2017 <- BMI_2017 %>% 
  dplyr::select (ethnicity_16, ethnicity_16_no_miss)


## create correlation_17 chart
eth_16_correlation_17 <- eth_16_check_2017 %>% 
  group_by(ethnicity_16) %>% 
  slice_head() %>% 
  ungroup()



eth_16_correlation_17 <- eth_16_correlation_17  %>% 
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



eth_16_correlation_17 <- eth_16_correlation_17  %>% 
  mutate (ethnic_group_reported_17 =case_when(
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
eth_16_correlation_17 <- eth_16_correlation_17 %>% 
  ungroup() %>% 
  dplyr::select(-c(ethnicity_16, ethnicity_16_no_miss)) %>% 
  dplyr::mutate (year = "2017")







## REPEAT FOR OTHER YEARS

eth_correlation <- eth_correlation_21 %>% 
  dplyr::left_join(eth_correlation_20, by = "real_ethnicity") %>% 
  dplyr::left_join(eth_correlation_19, by = "real_ethnicity") %>% 
  dplyr::left_join(eth_correlation_18, by = "real_ethnicity") %>% 
  dplyr::left_join(eth_correlation_17, by = "real_ethnicity")


eth_16_correlation <- eth_16_correlation_21 %>% 
  dplyr::left_join(eth_16_correlation_20, by = "real_ethnicity_16") %>% 
  dplyr::left_join(eth_16_correlation_19, by = "real_ethnicity_16") %>% 
  dplyr::left_join(eth_16_correlation_18, by = "real_ethnicity_16") %>% 
  dplyr::left_join(eth_16_correlation_17, by = "real_ethnicity_16")



###########################################################################################################

write.csv (eth_16_correlation, here::here ("output/data","eth_16_correlation_table.csv"))

write.csv (eth_correlation, here::here ("output/data","eth_6_correlation_table.csv"))

