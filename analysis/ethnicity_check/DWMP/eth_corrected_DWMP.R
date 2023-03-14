
## This script will calculate the propotion eligible for DWMP

# 1.  load libraries
# 2.  load the correct CSV file


dt <- BMI_complete_median_eth_corrected


names(dt)

dt <- dt %>% 
  dplyr::select(
    "patient_id", 
    "year",   
    "median_bmi", 
    "sex", 
    "region", 
    "imd", 
    "smoking_status", 
    "oad_meds", 
    "insulin_meds", 
    "eth_16_corrected", 
    starts_with("comorbid")             
  )


## Filter out years witha  missing BMI
dt <- dt %>% 
  drop_na(median_bmi)




dt <- dt %>% 
  dplyr::mutate(minority = case_when(
    (eth_16_corrected == "White_British" | eth_16_corrected == "White_Irish" | eth_16_corrected == "Other_White") ~ "white", 
    eth_16_corrected == "None" ~ "None"
  )) 

 dt <- dt %>%  mutate(
  minority  = as.character(minority),
  minority = ifelse(is.na(minority), "not_white", minority),
  minority = as.factor(minority)) 

 dt %>%
   tabyl(minority)
 

dt_2019 <- dt %>%
  dplyr::filter(year <= 2019) 



dt_2021 <- dt %>% 
  dplyr::filter(year >= 2017) 



dt_2019 <- dt_2019 %>% 
  dplyr::group_by(patient_id) %>% 
  arrange(desc(year), .by_group=TRUE) %>% 
  slice_head()



dt_2021 <- dt_2021 %>% 
  dplyr::group_by(patient_id) %>% 
  arrange(desc(year), .by_group=TRUE)  %>% 
  slice_head()



######################################################
## Categorise weight_universal:  BMI < 18.5 underweight; BMI 18 < 25 healthy, BMI 25 < 30 overweight , BMI >= 30 obese
## Categorise weight_DWMP (ethnicity specific cut offs): obese:  BMI >= 30 in white, BMI >= 27.5 in minority ethnic groups
## minority variable flags individuals as white/not white:  None, not_white, white
######################################################

dt_2021 <- dt_2021 %>% 
  dplyr::mutate(weight_universal = case_when(
    median_bmi <18.5 ~ "underweight",
    median_bmi >= 18.5 & median_bmi < 25 ~ "healthy",
    median_bmi >= 25 & median_bmi < 30 ~ "overweight",
    median_bmi >= 30 ~ "obese",
  ))


dt_2021 <- dt_2021 %>% 
  dplyr::mutate(weight_DWMP = case_when(
    ((median_bmi >= 30 & minority == "white") | (median_bmi >= 27.5 & minority == "not_white"))    ~  "obese",
    ((median_bmi < 30 & minority == "white") | (median_bmi < 27.5 & minority == "not_white"))    ~  "not_obese"
  )) 