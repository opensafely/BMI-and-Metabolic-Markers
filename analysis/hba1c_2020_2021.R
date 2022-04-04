
## author:  M Samuel
## developing script to see who had an hba1c test
## in development - 4th April 2022

library(lubridate)
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(broom)
library(dplyr)
library(janitor)

input_all_2020_03_01 <- read_feather (here::here ("output/data", "input_all_2020-03-01.feather"))



colnames(input_all_2020_03_01)

## select relevant variables for hba1c analysis
hba1c_2020 <- input_all_2020_03_01 %>%
  dplyr::select(-starts_with("bmi_"), -ends_with("diabetes"), -starts_with("sbp")) 



## label ethnicity
hba1c_2020 <- hba1c_2020 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

hba1c_2020 <- hba1c_2020 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 





### label
hba1c_2020$ethnic_no_miss[hba1c_2020$ethnic_no_miss=="1"]<-"White"
hba1c_2020$ethnic_no_miss[hba1c_2020$ethnic_no_miss=="2"]<-"Mixed"
hba1c_2020$ethnic_no_miss[hba1c_2020$ethnic_no_miss=="3"]<-"Asian"
hba1c_2020$ethnic_no_miss[hba1c_2020$ethnic_no_miss=="4"]<-"Black"
hba1c_2020$ethnic_no_miss[hba1c_2020$ethnic_no_miss=="5"]<-"Other"
hba1c_2020$ethnic_no_miss[hba1c_2020$ethnic_no_miss=="0"]<-"Not_recorded"

hba1c_2020 <- hba1c_2020 %>%             
  mutate (ethnic_no_miss = as.factor(ethnic_no_miss)) %>%
  mutate (ethnic_no_miss = fct_relevel(ethnic_no_miss, "White", "Asian", "Black", "Mixed","Other", "Not_recorded"))



hba1c_2020 <- hba1c_2020 %>%             
  dplyr::mutate(imd=as.numeric(imd)) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "0-17", "18-39", "40-65", "65-80", "80+"))





hba1c_2020 <- hba1c_2020 %>%
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


hba1c_2020 <- hba1c_2020 %>%             
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


hba1c_2020 <- hba1c_2020 %>%
  dplyr::select(-starts_with("ethnicity")) %>%
  dplyr::select(-"eth")


colnames(hba1c_2020)


########################  NEW CODE TO ADD dates for flag pre-covd hba1c

## Need HbA1c measured dates for long data

hba1c_2020_long <- hba1c_2020 %>%   ## 1. pivot_longer date measured columns
  pivot_longer(
    cols = ends_with('_date'),
    names_to = "month_hba1c",
    values_to = "date_hba1c")%>% 
  tidyr::drop_na("date_hba1c") %>%                # Drop rows with missing date values
  group_by(patient_id,date_hba1c) %>%   # Drop duplicate values
  slice_head %>%
  mutate(month_hba1c = str_sub(month_hba1c, 7, -6)) 





hba1c_2020_long <- hba1c_2020_long %>%
  pivot_longer(                          #  step 2.  Pivot longer the values.
    cols = starts_with("hba1c_"),
    names_to = "date", 
    values_to = "monthly_hba1c")  %>%    # step 3.  filter out duplicate row
  mutate(date = str_sub(date, 7)) %>%  #3a.  create a column to identify matching events
  dplyr::filter(month_hba1c == date) %>%
  dplyr::select(-'date') %>% 
  dplyr::filter(monthly_hba1c >= 15)  %>%  ## FILTER OUT ANY VERY LOW HbA1c (likely miscoded DCCT)
  dplyr::mutate(year = "2020")
## missing hba1c values are 0 >> change to NA
# hba1c_2020_long$hba1c[hba1c_2020$hba1c == 0] <- NA

## save the long version to merge for trajectory analysis and creating a flag of last hba1c


################################# need to make sure names are compatible with code below which did not include the date.  need date to identify most recent pre-covid hba1c

## generate a table of patients who had hba1c

had_hba1c_2020 <- hba1c_2020 %>%
  dplyr::select(-ends_with("_date"))

had_hba1c_2020 <- had_hba1c_2020 %>%
  dplyr::rowwise (patient_id) %>%
  dplyr::mutate( sum_hba1c = sum(c(
    hba1c_march, 
    hba1c_april,
    hba1c_may, 
    hba1c_june, 
    hba1c_july, 
    hba1c_aug, 
    hba1c_sep,               
    hba1c_oct,           
    hba1c_nov,              
    hba1c_dec,               
    hba1c_jan,              
    hba1c_feb)))

had_hba1c_2020 <- had_hba1c_2020 %>%
  mutate(had_hba1c = case_when(
    sum_hba1c == 0 ~ "FALSE", 
    sum_hba1c !=0 ~  "TRUE"
  ))

had_hba1c_2020 <- had_hba1c_2020 %>%
  dplyr::select(-"sum_hba1c") %>%
  dplyr::select(-starts_with("hba1c"))


## >>>  SAVE AS DATA FOR HbA1c analysis: just need to add last hba1c 2021/2020




##
## calculate each patient's median hbA1c
median_hba1c <- hba1c_2020_long %>%
  group_by(patient_id) %>%
  dplyr::summarise(median_hba1c = (median(monthly_hba1c, na.rm=TRUE)))


## attach median Hba1c to had_hba1c_2020 >just contains exposure coviariates and whether they had hba1c

hba1c_2020 <- had_hba1c_2020 %>%
  dplyr::left_join(median_hba1c, by='patient_id') %>%
  dplyr::mutate(year = "2020")

##################################################################

## 2021 analysis





input_all_2021_03_01 <- read_feather (here::here ("output/data", "input_all_2021-03-01.feather"))





## select relevant variables for hba1c analysis
hba1c_2021 <- input_all_2021_03_01 %>%
  dplyr::select(-starts_with("bmi_"), -ends_with("diabetes"), -starts_with("sbp")) 



## label ethnicity
hba1c_2021 <- hba1c_2021 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

hba1c_2021 <- hba1c_2021 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 





### label
hba1c_2021$ethnic_no_miss[hba1c_2021$ethnic_no_miss=="1"]<-"White"
hba1c_2021$ethnic_no_miss[hba1c_2021$ethnic_no_miss=="2"]<-"Mixed"
hba1c_2021$ethnic_no_miss[hba1c_2021$ethnic_no_miss=="3"]<-"Asian"
hba1c_2021$ethnic_no_miss[hba1c_2021$ethnic_no_miss=="4"]<-"Black"
hba1c_2021$ethnic_no_miss[hba1c_2021$ethnic_no_miss=="5"]<-"Other"
hba1c_2021$ethnic_no_miss[hba1c_2021$ethnic_no_miss=="0"]<-"Not_recorded"

hba1c_2021 <- hba1c_2021 %>%             
  mutate (ethnic_no_miss = as.factor(ethnic_no_miss)) %>%
  mutate (ethnic_no_miss = fct_relevel(ethnic_no_miss, "White", "Asian", "Black", "Mixed","Other", "Not_recorded"))



hba1c_2021 <- hba1c_2021 %>%             
  dplyr::mutate(imd=as.numeric(imd)) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "0-17", "18-39", "40-65", "65-80", "80+"))





hba1c_2021 <- hba1c_2021 %>%
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


hba1c_2021 <- hba1c_2021 %>%             
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


hba1c_2021 <- hba1c_2021 %>%
  dplyr::select(-starts_with("ethnicity")) %>%
  dplyr::select(-"eth")


colnames(hba1c_2021)


########################  NEW CODE TO ADD dates for flag pre-covd hba1c

## Need HbA1c measured dates for long data

hba1c_2021_long <- hba1c_2021 %>%   ## 1. pivot_longer date measured columns
  pivot_longer(
    cols = ends_with('_date'),
    names_to = "month_hba1c",
    values_to = "date_hba1c")%>% 
  tidyr::drop_na("date_hba1c") %>%                # Drop rows with missing date values
  group_by(patient_id,date_hba1c) %>%   # Drop duplicate values
  slice_head %>%
  mutate(month_hba1c = str_sub(month_hba1c, 7, -6)) 





hba1c_2021_long <- hba1c_2021_long %>%
  pivot_longer(                          #  step 2.  Pivot longer the values.
    cols = starts_with("hba1c_"),
    names_to = "date", 
    values_to = "monthly_hba1c")  %>%    # step 3.  filter out duplicate row
  mutate(date = str_sub(date, 7)) %>%  #3a.  create a column to identify matching events
  dplyr::filter(month_hba1c == date) %>%
  dplyr::select(-'date') %>% 
  dplyr::filter(monthly_hba1c >= 15)  %>%  ## FILTER OUT ANY VERY LOW HbA1c (likely miscoded DCCT)
  dplyr::mutate(year = "2021")
## missing hba1c values are 0 >> change to NA
# hba1c_2021_long$hba1c[hba1c_2021$hba1c == 0] <- NA

## save the long version to merge for trajectory analysis and creating a flag of last hba1c


################################# need to make sure names are compatible with code below which did not include the date.  need date to identify most recent pre-covid hba1c

## generate a table of patients who had hba1c

had_hba1c_2021 <- hba1c_2021 %>%
  dplyr::select(-ends_with("_date"))

had_hba1c_2021 <- had_hba1c_2021 %>%
  dplyr::rowwise (patient_id) %>%
  dplyr::mutate( sum_hba1c = sum(c(
    hba1c_march, 
    hba1c_april,
    hba1c_may, 
    hba1c_june, 
    hba1c_july, 
    hba1c_aug, 
    hba1c_sep,               
    hba1c_oct,           
    hba1c_nov,              
    hba1c_dec,               
    hba1c_jan,              
    hba1c_feb)))

had_hba1c_2021 <- had_hba1c_2021 %>%
  mutate(had_hba1c = case_when(
    sum_hba1c == 0 ~ "FALSE", 
    sum_hba1c !=0 ~  "TRUE"
  ))

had_hba1c_2021 <- had_hba1c_2021 %>%
  dplyr::select(-"sum_hba1c") %>%
  dplyr::select(-starts_with("hba1c"))


## >>>  SAVE AS DATA FOR HbA1c analysis: just need to add last hba1c 2021/2021




##
## calculate each patient's median hbA1c
median_hba1c <- hba1c_2021_long %>%
  group_by(patient_id) %>%
  dplyr::summarise(median_hba1c = (median(monthly_hba1c, na.rm=TRUE)))


## attach median Hba1c to had_hba1c_2021 >just contains exposure coviariates and whether they had hba1c

hba1c_2021 <- had_hba1c_2021 %>%
  dplyr::left_join(median_hba1c, by='patient_id') %>%
  dplyr::mutate(year = "2021")



                  

write_feather (hba1c_2020, here::here ("output/data","hba1c_2020_summary.feather"))
write_feather (hba1c_2021, here::here ("output/data","hba1c_2021_summary.feather"))
write_feather (hba1c_2020_long, here::here ("output/data","hba1c_2020_long.feather"))
write_feather (hba1c_2021_long, here::here ("output/data","hba1c_2021_long.feather"))



