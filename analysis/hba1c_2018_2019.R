# input_all_2019_03_01 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "input_all_2019-03-01.feather"))
# input_all_2018_03_01 <- read_feather (here::here ("Documents/Academic GP/Open Safely/Dummy Data", "input_all_2018-03-01.feather"))



library(lubridate)
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(broom)
library(dplyr)
library(janitor)

input_all_2019_03_01 <- read_feather (here::here ("output/data", "complete_meds_2019.feather"))
input_all_2018_03_01 <- read_feather (here::here ("output/data", "complete_meds_2018.feather"))


colnames(input_all_2019_03_01)

## select relevant variables for hba1c analysis
hba1c_2019 <- input_all_2019_03_01 %>%
  ungroup() %>%
  dplyr::select(-starts_with("bmi_"), -ends_with("diabetes"), -starts_with("sbp")) 



## label ethnicity
hba1c_2019 <- hba1c_2019 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

hba1c_2019 <- hba1c_2019 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 





### label
hba1c_2019$ethnic_no_miss[hba1c_2019$ethnic_no_miss=="1"]<-"White"
hba1c_2019$ethnic_no_miss[hba1c_2019$ethnic_no_miss=="2"]<-"Mixed"
hba1c_2019$ethnic_no_miss[hba1c_2019$ethnic_no_miss=="3"]<-"Asian"
hba1c_2019$ethnic_no_miss[hba1c_2019$ethnic_no_miss=="4"]<-"Black"
hba1c_2019$ethnic_no_miss[hba1c_2019$ethnic_no_miss=="5"]<-"Other"
hba1c_2019$ethnic_no_miss[hba1c_2019$ethnic_no_miss=="0"]<-"Not_recorded"

hba1c_2019 <- hba1c_2019 %>%             
  mutate (ethnic_no_miss = as.factor(ethnic_no_miss)) %>%
  mutate (ethnic_no_miss = fct_relevel(ethnic_no_miss, "White", "Asian", "Black", "Mixed","Other", "Not_recorded"))



hba1c_2019 <- hba1c_2019 %>%             
  dplyr::mutate(imd=as.numeric(imd)) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "0-17", "18-39", "40-65", "65-80", "80+"))





hba1c_2019 <- hba1c_2019 %>%
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


hba1c_2019 <- hba1c_2019 %>%             
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


hba1c_2019 <- hba1c_2019 %>%
  dplyr::select(-starts_with("ethnicity")) %>%
  dplyr::select(-"eth")


colnames(hba1c_2019)




## Need HbA1c measured dates for long data

hba1c_2019_long <- hba1c_2019 %>%   ## 1. pivot_longer date measured columns
  pivot_longer(
    cols = ends_with('_date'),
    names_to = "month_hba1c",
    values_to = "date_hba1c")%>% 
  tidyr::drop_na("date_hba1c") %>%                # Drop rows with missing date values
  group_by(patient_id,date_hba1c) %>%   # Drop duplicate values
  slice_head %>%
  mutate(month_hba1c = str_sub(month_hba1c, 7, -6)) 





hba1c_2019_long <- hba1c_2019_long %>%
  pivot_longer(                          #  step 2.  Pivot longer the values.
    cols = starts_with("hba1c_"),
    names_to = "date", 
    values_to = "monthly_hba1c")  %>%    # step 3.  filter out duplicate row
  mutate(date = str_sub(date, 7)) %>%  #3a.  create a column to identify matching events
  dplyr::filter(month_hba1c == date) %>%
  dplyr::select(-'date') %>% 
  dplyr::filter(monthly_hba1c >= 15)  %>%  ## FILTER OUT ANY VERY LOW HbA1c (likely miscoded DCCT)(do this in the analysis)
  dplyr::mutate(year = "2019")
## missing hba1c values are 0 >> change to NA
# hba1c_2019_long$hba1c[hba1c_2019$hba1c == 0] <- NA

## save the long version to merge for trajectory analysis and creating a flag of last hba1c


################################# need to make sure names are compatible with code below which did not include the date.  need date to identify most recent pre-covid hba1c


## calculate each patient's median hbA1c
median_hba1c <- hba1c_2019_long %>%
  group_by(patient_id) %>%
  dplyr::summarise(median_hba1c = (median(monthly_hba1c, na.rm=TRUE))) %>%
  dplyr::mutate(year="2019")



summary_hba1c_2019 <- hba1c_2019 %>% 
  left_join(median_hba1c) %>%
  dplyr::select(-starts_with('hba1c')) %>%
  dplyr::select(-ends_with("_date"))

summary_hba1c_2019 <- summary_hba1c_2019 %>%
  dplyr::mutate(had_hba1c = median_hba1c) %>%
  dplyr::mutate(had_hba1c = replace_na(median_hba1c, 0)) %>%
  dplyr::mutate(had_hba1c = case_when(
    had_hba1c == 0 ~ 'FALSE', 
    had_hba1c !=0 ~ "TRUE"
  ))

summary_hba1c_2019 %>% 
  tabyl(had_hba1c)

##################################################################

## 2018 analysis


## select relevant variables for hba1c analysis
hba1c_2018 <- input_all_2018_03_01 %>%
  ungroup() %>%
  dplyr::select(-starts_with("bmi_"), -ends_with("diabetes"), -starts_with("sbp")) 



## label ethnicity
hba1c_2018 <- hba1c_2018 %>%
  mutate(ethnic_no_miss = ifelse(is.na(ethnicity), 0, ethnicity ))

hba1c_2018 <- hba1c_2018 %>%
  mutate(ethnicity_16_no_miss = ifelse(is.na(ethnicity_16), 0, ethnicity_16 )) 





### label
hba1c_2018$ethnic_no_miss[hba1c_2018$ethnic_no_miss=="1"]<-"White"
hba1c_2018$ethnic_no_miss[hba1c_2018$ethnic_no_miss=="2"]<-"Mixed"
hba1c_2018$ethnic_no_miss[hba1c_2018$ethnic_no_miss=="3"]<-"Asian"
hba1c_2018$ethnic_no_miss[hba1c_2018$ethnic_no_miss=="4"]<-"Black"
hba1c_2018$ethnic_no_miss[hba1c_2018$ethnic_no_miss=="5"]<-"Other"
hba1c_2018$ethnic_no_miss[hba1c_2018$ethnic_no_miss=="0"]<-"Not_recorded"

hba1c_2018 <- hba1c_2018 %>%             
  mutate (ethnic_no_miss = as.factor(ethnic_no_miss)) %>%
  mutate (ethnic_no_miss = fct_relevel(ethnic_no_miss, "White", "Asian", "Black", "Mixed","Other", "Not_recorded"))



hba1c_2018 <- hba1c_2018 %>%             
  dplyr::mutate(imd=as.numeric(imd)) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "0-17", "18-39", "40-65", "65-80", "80+"))





hba1c_2018 <- hba1c_2018 %>%
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


hba1c_2018 <- hba1c_2018 %>%             
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


hba1c_2018 <- hba1c_2018 %>%
  dplyr::select(-starts_with("ethnicity")) %>%
  dplyr::select(-"eth")


colnames(hba1c_2018)



## Need HbA1c measured dates for long data

hba1c_2018_long <- hba1c_2018 %>%   ## 1. pivot_longer date measured columns
  pivot_longer(
    cols = ends_with('_date'),
    names_to = "month_hba1c",
    values_to = "date_hba1c")%>% 
  tidyr::drop_na("date_hba1c") %>%                # Drop rows with missing date values
  group_by(patient_id,date_hba1c) %>%   # Drop duplicate values
  slice_head %>%
  mutate(month_hba1c = str_sub(month_hba1c, 7, -6)) 





hba1c_2018_long <- hba1c_2018_long %>%
  pivot_longer(                          #  step 2.  Pivot longer the values.
    cols = starts_with("hba1c_"),
    names_to = "date", 
    values_to = "monthly_hba1c")  %>%    # step 3.  filter out duplicate row
  mutate(date = str_sub(date, 7)) %>%  #3a.  create a column to identify matching events
  dplyr::filter(month_hba1c == date) %>%
  dplyr::select(-'date') %>% 
  dplyr::filter(monthly_hba1c >= 15)  %>%  ## FILTER OUT ANY VERY LOW HbA1c (likely miscoded DCCT)(do this in the main analysis)
  dplyr::mutate(year = "2018")
## missing hba1c values are 0 >> change to NA
# hba1c_2018_long$hba1c[hba1c_2018$hba1c == 0] <- NA

## save the long version to merge for trajectory analysis and creating a flag of last hba1c



## calculate each patient's median hbA1c
median_hba1c <- hba1c_2018_long %>%
  group_by(patient_id) %>%
  dplyr::summarise(median_hba1c = (median(monthly_hba1c, na.rm=TRUE))) %>%
  dplyr::mutate(year="2018")


## attach median Hba1c to had_hba1c_2018 >just contains exposure coviariates and whether they had hba1c




summary_hba1c_2018 <- hba1c_2018 %>% 
  left_join(median_hba1c) %>%
  dplyr::select(-starts_with('hba1c')) %>%
  dplyr::select(-ends_with("_date"))

summary_hba1c_2018 <- summary_hba1c_2018 %>%
  dplyr::mutate(had_hba1c = median_hba1c) %>%
  dplyr::mutate(had_hba1c = replace_na(median_hba1c, 0)) %>%
  dplyr::mutate(had_hba1c = case_when(
    had_hba1c == 0 ~ 'FALSE', 
    had_hba1c !=0 ~ "TRUE"
  ))

summary_hba1c_2018 %>% 
  tabyl(had_hba1c)





#######################################################
## identify last pre_covid hba1c

precovid_hba1c <- hba1c_2019_long %>%
  bind_rows(hba1c_2018_long)

precovid_hba1c <- precovid_hba1c %>% 
  dplyr::select(patient_id, date_hba1c, monthly_hba1c) %>% 
  dplyr::mutate(date_hba1c = as.Date(date_hba1c))

precovid_hba1c <- precovid_hba1c %>% 
  dplyr::group_by(patient_id) %>%
  dplyr::arrange(desc(ymd(precovid_hba1c$date_hba1c))) %>% 
  dplyr::arrange(patient_id) %>% 
  dplyr::group_by(patient_id) %>% 
  slice_head()

precovid_hba1c <- precovid_hba1c %>%
  rename(last_precovid_hba1c = monthly_hba1c)

precovid_hba1c <- precovid_hba1c %>%
  dplyr::mutate(precovid_control = case_when(
    last_precovid_hba1c <=48  ~ "<48",
    last_precovid_hba1c >48 & last_precovid_hba1c<=59 ~ ">48-59", 
    last_precovid_hba1c >59 & last_precovid_hba1c<=65 ~ ">59-65", 
    last_precovid_hba1c >65 & last_precovid_hba1c <=75 ~ ">65-75", 
    last_precovid_hba1c >75 ~ ">75"
  ))


write_feather (summary_hba1c_2019, here::here ("output/data","hba1c_2019_summary.feather"))
write_feather (summary_hba1c_2018, here::here ("output/data","hba1c_2018_summary.feather"))
write_feather (hba1c_2019_long, here::here ("output/data","hba1c_2019_long.feather"))
write_feather (hba1c_2018_long, here::here ("output/data","hba1c_2018_long.feather"))
write_feather (precovid_hba1c, here::here ("output/data","precovid_hba1c_control.feather"))