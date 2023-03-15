
## This script will calculate the propotion eligible for DWMP

# 1.  load libraries
# 2.  load the correct CSV file


dt <- BMI_complete_median_eth_corrected




dt <- dt %>% 
  dplyr::select(
    "patient_id", 
    "year",   
    "median_bmi", 
    "sex", 
    "age_group_2",
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
    ((median_bmi >= 30 & minority == "white") | (median_bmi >= 27.5 & minority == "not_white"))    ~  "DWMP_obese",
    ((median_bmi < 30 & minority == "white") | (median_bmi < 27.5 & minority == "not_white"))    ~  "DWMP_not_obese"
  )) 


###


dt_2019 <- dt_2019 %>% 
  dplyr::mutate(weight_universal = case_when(
    median_bmi <18.5 ~ "underweight",
    median_bmi >= 18.5 & median_bmi < 25 ~ "healthy",
    median_bmi >= 25 & median_bmi < 30 ~ "overweight",
    median_bmi >= 30 ~ "obese",
  ))



 dt_2019 <- dt_2019 %>% 
  dplyr::mutate(weight_DWMP = case_when(
    ((median_bmi >= 30 & minority == "white") | (median_bmi >= 27.5 & minority == "not_white"))    ~  "DWMP_obese",
    ((median_bmi < 30 & minority == "white") | (median_bmi < 27.5 & minority == "not_white"))    ~  "DWMP_not_obese"
  )) 



 ### WRITE FUNCTIONS

universal_weight_f <- function (data, var){
  data %>% 
  tabyl({{ var }}, weight_universal) %>%
  dplyr::rename(group = 1) %>% 
  dplyr::mutate(group = as.factor(group))
}



DWMP_weight_f <- function (data, var){
  data %>% 
    tabyl({{ var }}, weight_DWMP) %>%
    dplyr::rename(group = 1) %>% 
    dplyr::mutate(group = as.factor(group))
}


#####


sex <-  universal_weight_f(dt_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(dt_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(dt_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(dt_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(dt_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)



#########################


universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)









sex <-  DWMP_weight_f(dt_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(dt_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(dt_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(dt_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(dt_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


summary_2021 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group")) 


### T2D 2021
#### 
T2D_2021 <- dt_2021 %>% 
  dplyr::filter(comorbid_diabetes_t2 == TRUE)


sex <-  universal_weight_f(T2D_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(T2D_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(T2D_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(T2D_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(T2D_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


###
sex <-  DWMP_weight_f(T2D_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(T2D_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(T2D_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(T2D_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(T2D_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


T2D_2021 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))


######## T1D_2021


T1D_2021 <- dt_2021 %>% 
  dplyr::filter(comorbid_diabetes_t1 == TRUE)




sex <-  universal_weight_f(T1D_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(T1D_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(T1D_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(T1D_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(T1D_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)









sex <-  DWMP_weight_f(T1D_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(T1D_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(T1D_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(T1D_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(T1D_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


T1D_2021 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))


### Hypertension_2021

BP_2021 <- dt_2021 %>% 
  dplyr::filter(comorbid_hypertension == TRUE)




sex <-  universal_weight_f(BP_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(BP_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(BP_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(BP_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(BP_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)









sex <-  DWMP_weight_f(BP_2021,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(BP_2021,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(BP_2021,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(BP_2021,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(BP_2021,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


BP_2021 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))




############## 2019



sex <-  universal_weight_f(dt_2019,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(dt_2019,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(dt_2019,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(dt_2019,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(dt_2019,region)%>%
  dplyr::mutate (variable = 'region', .before=1)



universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


##

sex <-  DWMP_weight_f(dt_2019,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(dt_2019,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(dt_2019,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(dt_2019,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(dt_2019,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


summary_2019 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))


### T2D 2019
#### 
T2D_2019 <- dt_2019 %>% 
  dplyr::filter(comorbid_diabetes_t2 == TRUE)


sex <-  universal_weight_f(T2D_2019,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(T2D_2019,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(T2D_2019,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(T2D_2019,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(T2D_2019,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


###
sex <-  DWMP_weight_f(T2D_2019,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(T2D_2019,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(T2D_2019,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(T2D_2019,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(T2D_2019,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


T2D_2019 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))


######## T1D_2019


T1D_2019 <- dt_2019 %>% 
  dplyr::filter(comorbid_diabetes_t1 == TRUE)




sex <-  universal_weight_f(T1D_2019,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(T1D_2019,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(T1D_2019,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(T1D_2019,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(T1D_2019,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)









sex <-  DWMP_weight_f(T1D_2019,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(T1D_2019,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(T1D_2019,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(T1D_2019,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(T1D_2019,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


T1D_2019 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))


### Hypertension_2019

BP_2019 <- dt_2019 %>% 
  dplyr::filter(comorbid_hypertension == TRUE)




sex <-  universal_weight_f(BP_2019,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- universal_weight_f(BP_2019,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- universal_weight_f(BP_2019,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- universal_weight_f(BP_2019,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- universal_weight_f(BP_2019,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


universal_weight <- sex %>% 
  bind_rows(age, eth, imd, region)









sex <-  DWMP_weight_f(BP_2019,sex) %>%
  dplyr::mutate (variable = 'sex', .before=1)


age <- DWMP_weight_f(BP_2019,age_group_2)%>%
  dplyr::mutate (variable = 'age', .before=1)

eth <- DWMP_weight_f(BP_2019,eth_16_corrected)%>%
  dplyr::mutate (variable = 'eth', .before=1)

imd <- DWMP_weight_f(BP_2019,imd)%>%
  dplyr::mutate (variable = 'imd', .before=1)


region <- DWMP_weight_f(BP_2019,region)%>%
  dplyr::mutate (variable = 'region', .before=1)


DWMP_weight <- sex %>% 
  bind_rows(age, eth, imd, region)


BP_2019 <- universal_weight %>% 
  left_join(DWMP_weight, by = c("variable", "group"))