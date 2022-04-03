
#### Author: M Samuel
#### Date: 4th April 2022
#### This script calculates proportions of people who were eligible for DWMP and differences between groups using chi squared.


## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(broom)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)

## Read in files  >>> Change PATH!!

# check working directory:  getwd()

BMI_complete_categories_2019 <- read_feather (here::here ("output/data", "BMI_all_2019.feather"))
BMI_complete_categories_2020 <- read_feather (here::here ("output/data", "BMI_all_2020.feather"))
BMI_complete_categories_2021 <- read_feather (here::here ("output/data", "BMI_all_2021.feather"))


BMI_complete_categories_2019 <- BMI_complete_categories_2019 %>%
  dplyr::mutate(DWMP_eligible = case_when(
    DWMP == "eligible"  ~ "TRUE", 
    DWMP == "not_eligible" ~ "FALSE"
  ))

BMI_complete_categories_2019 <- BMI_complete_categories_2019 %>%
  dplyr::mutate(DWMP_eligible = as.logical(DWMP_eligible))



BMI_complete_categories <- BMI_complete_categories_2019 %>% 
  dplyr::mutate(imd=as.numeric(imd)) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "0-17", "18-39", "40-65", "65-80", "80+"))

BMI_complete_categories$DWMP_eligible[BMI_complete_categories$DWMP_eligible == 1] <- "TRUE"
BMI_complete_categories$DWMP_eligible[BMI_complete_categories$DWMP_eligible == 0] <- "FALSE"



BMI_complete_categories_2019_DT <- data.table(BMI_complete_categories)



# develop a vector of explanatory variables
explanatory_vars <- c("sex", 
                      "age_group", 
                      "region", 
                      "imd", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "comorbid_learning_disability",     
                      "comorbid_depression",            
                      "comorbid_dementia",               
                      "comorbid_psychosis_schiz_bipolar",
                      "comorbid_diabetes_type",
                      "comorbid_diabetes_t1",           
                      "comorbid_diabetes_t2",             
                      "comorbid_asthma",                
                      "comorbid_COPD",                   
                      "comorbid_stroke_and_TIA",         
                      "comorbid_chronic_cardiac", 
                      "comorbid_hypertension",           
                      "comorbid_all_cancer")



N_DWMP_eligible <- BMI_complete_categories_2019_DT[, .N, ]
DWMP_eligible_table<- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N),] 

DWMP_eligible_table <- dplyr::mutate (DWMP_eligible_table, N = N_DWMP_eligible) %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(group = "all", .before=1) %>%
  dplyr::mutate(variable = "all", .before=1)




##  Develop count table and chi squared test of difference between population long hand for each variable to reduce memory


#########  AGE GROUPS #######

#1.  count by age_group
N_age_group <- BMI_complete_categories_2019_DT[, .N, by="age_group"]
DWMP_eligible_age_group <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="age_group"] 
DWMP_eligible_age_group <-   DWMP_eligible_age_group[order(age_group)]

DWMP_eligible_age_group <- dplyr::left_join(DWMP_eligible_age_group, N_age_group)
DWMP_eligible_age_group <- DWMP_eligible_age_group %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_age_group <- DWMP_eligible_age_group %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "age_group", .before=1) %>%
  dplyr::rename(group = age_group)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
age_group_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(age_group, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
age_group_chisq <- dplyr::mutate (age_group_chisq, variable = "age_group") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_age_group <- DWMP_eligible_age_group %>%
  dplyr::left_join(age_group_chisq, by = "variable")





###### SEX

#1.  count by sex
N_sex <- BMI_complete_categories_2019_DT[, .N, by="sex"]
DWMP_eligible_sex <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="sex"] 
DWMP_eligible_sex <-   DWMP_eligible_sex[order(sex)]

DWMP_eligible_sex <- dplyr::left_join(DWMP_eligible_sex, N_sex)
DWMP_eligible_sex <- DWMP_eligible_sex %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_sex <- DWMP_eligible_sex %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "sex", .before=1) %>%
  dplyr::rename(group = sex)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
sex_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(sex, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
sex_chisq <- dplyr::mutate (sex_chisq, variable = "sex") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_sex <- DWMP_eligible_sex %>%
  dplyr::left_join(sex_chisq, by = "variable")


#########  region #######

#1.  count by region
N_region <- BMI_complete_categories_2019_DT[, .N, by="region"]
DWMP_eligible_region <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="region"] 
DWMP_eligible_region <-   DWMP_eligible_region[order(region)]

DWMP_eligible_region <- dplyr::left_join(DWMP_eligible_region, N_region)
DWMP_eligible_region <- DWMP_eligible_region %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_region <- DWMP_eligible_region %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "region", .before=1) %>%
  dplyr::rename(group = region)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
region_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(region, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
region_chisq <- dplyr::mutate (region_chisq, variable = "region") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_region <- DWMP_eligible_region %>%
  dplyr::left_join(region_chisq, by = "variable")

#########  imd #######

#1.  count by imd
N_imd <- BMI_complete_categories_2019_DT[, .N, by="imd"]
DWMP_eligible_imd <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="imd"] 
DWMP_eligible_imd <-   DWMP_eligible_imd[order(imd)]

DWMP_eligible_imd <- dplyr::left_join(DWMP_eligible_imd, N_imd)
DWMP_eligible_imd <- DWMP_eligible_imd %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_imd <- DWMP_eligible_imd %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "imd", .before=1) %>%
  dplyr::rename(group = imd)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
imd_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(imd, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
imd_chisq <- dplyr::mutate (imd_chisq, variable = "imd") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_imd <- DWMP_eligible_imd %>%
  dplyr::left_join(imd_chisq, by = "variable")

#########  ethnic_no_miss #######

#1.  count by ethnic_no_miss
N_ethnic_no_miss <- BMI_complete_categories_2019_DT[, .N, by="ethnic_no_miss"]
DWMP_eligible_ethnic_no_miss <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="ethnic_no_miss"] 
DWMP_eligible_ethnic_no_miss <-   DWMP_eligible_ethnic_no_miss[order(ethnic_no_miss)]

DWMP_eligible_ethnic_no_miss <- dplyr::left_join(DWMP_eligible_ethnic_no_miss, N_ethnic_no_miss)
DWMP_eligible_ethnic_no_miss <- DWMP_eligible_ethnic_no_miss %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_ethnic_no_miss <- DWMP_eligible_ethnic_no_miss %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "ethnic_no_miss", .before=1) %>%
  dplyr::rename(group = ethnic_no_miss)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
ethnic_no_miss_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(ethnic_no_miss, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
ethnic_no_miss_chisq <- dplyr::mutate (ethnic_no_miss_chisq, variable = "ethnic_no_miss") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_ethnic_no_miss <- DWMP_eligible_ethnic_no_miss %>%
  dplyr::left_join(ethnic_no_miss_chisq, by = "variable")

#########  eth_group_16 #######

#1.  count by eth_group_16
N_eth_group_16 <- BMI_complete_categories_2019_DT[, .N, by="eth_group_16"]
DWMP_eligible_eth_group_16 <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="eth_group_16"] 
DWMP_eligible_eth_group_16 <-   DWMP_eligible_eth_group_16[order(eth_group_16)]

DWMP_eligible_eth_group_16 <- dplyr::left_join(DWMP_eligible_eth_group_16, N_eth_group_16)
DWMP_eligible_eth_group_16 <- DWMP_eligible_eth_group_16 %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_eth_group_16 <- DWMP_eligible_eth_group_16 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "eth_group_16", .before=1) %>%
  dplyr::rename(group = eth_group_16)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
eth_group_16_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(eth_group_16, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
eth_group_16_chisq <- dplyr::mutate (eth_group_16_chisq, variable = "eth_group_16") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_eth_group_16 <- DWMP_eligible_eth_group_16 %>%
  dplyr::left_join(eth_group_16_chisq, by = "variable")

########  comorbid_learning_disability #######

#1.  count by comorbid_learning_disability
N_comorbid_learning_disability <- BMI_complete_categories_2019_DT[, .N, by="comorbid_learning_disability"]
DWMP_eligible_comorbid_learning_disability <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_learning_disability"] 
DWMP_eligible_comorbid_learning_disability <-   DWMP_eligible_comorbid_learning_disability[order(comorbid_learning_disability)]

DWMP_eligible_comorbid_learning_disability <- dplyr::left_join(DWMP_eligible_comorbid_learning_disability, N_comorbid_learning_disability)
DWMP_eligible_comorbid_learning_disability <- DWMP_eligible_comorbid_learning_disability %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_learning_disability <- DWMP_eligible_comorbid_learning_disability %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_learning_disability", .before=1) %>%
  dplyr::rename(group = comorbid_learning_disability) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_learning_disability_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_learning_disability, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_learning_disability_chisq <- dplyr::mutate (comorbid_learning_disability_chisq, variable = "comorbid_learning_disability") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_learning_disability <- DWMP_eligible_comorbid_learning_disability %>%
  dplyr::left_join(comorbid_learning_disability_chisq, by = "variable")



#########  comorbid_depression #######

#1.  count by comorbid_depression
N_comorbid_depression <- BMI_complete_categories_2019_DT[, .N, by="comorbid_depression"]
DWMP_eligible_comorbid_depression <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_depression"] 
DWMP_eligible_comorbid_depression <-   DWMP_eligible_comorbid_depression[order(comorbid_depression)]

DWMP_eligible_comorbid_depression <- dplyr::left_join(DWMP_eligible_comorbid_depression, N_comorbid_depression)
DWMP_eligible_comorbid_depression <- DWMP_eligible_comorbid_depression %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_depression <- DWMP_eligible_comorbid_depression %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_depression", .before=1) %>%
  dplyr::rename(group = comorbid_depression) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_depression_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_depression, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_depression_chisq <- dplyr::mutate (comorbid_depression_chisq, variable = "comorbid_depression") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_depression <- DWMP_eligible_comorbid_depression %>%
  dplyr::left_join(comorbid_depression_chisq, by = "variable")


#########  comorbid_dementia #######

#1.  count by comorbid_dementia
N_comorbid_dementia <- BMI_complete_categories_2019_DT[, .N, by="comorbid_dementia"]
DWMP_eligible_comorbid_dementia <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_dementia"] 
DWMP_eligible_comorbid_dementia <-   DWMP_eligible_comorbid_dementia[order(comorbid_dementia)]

DWMP_eligible_comorbid_dementia <- dplyr::left_join(DWMP_eligible_comorbid_dementia, N_comorbid_dementia)
DWMP_eligible_comorbid_dementia <- DWMP_eligible_comorbid_dementia %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_dementia <- DWMP_eligible_comorbid_dementia %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_dementia", .before=1) %>%
  dplyr::rename(group = comorbid_dementia) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_dementia_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_dementia, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_dementia_chisq <- dplyr::mutate (comorbid_dementia_chisq, variable = "comorbid_dementia") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_dementia <- DWMP_eligible_comorbid_dementia %>%
  dplyr::left_join(comorbid_dementia_chisq, by = "variable")


#########  comorbid_psychosis_schiz_bipolar #######

#1.  count by comorbid_psychosis_schiz_bipolar
N_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_2019_DT[, .N, by="comorbid_psychosis_schiz_bipolar"]
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_psychosis_schiz_bipolar"] 
DWMP_eligible_comorbid_psychosis_schiz_bipolar <-   DWMP_eligible_comorbid_psychosis_schiz_bipolar[order(comorbid_psychosis_schiz_bipolar)]

DWMP_eligible_comorbid_psychosis_schiz_bipolar <- dplyr::left_join(DWMP_eligible_comorbid_psychosis_schiz_bipolar, N_comorbid_psychosis_schiz_bipolar)
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- DWMP_eligible_comorbid_psychosis_schiz_bipolar %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- DWMP_eligible_comorbid_psychosis_schiz_bipolar %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_psychosis_schiz_bipolar", .before=1) %>%
  dplyr::rename(group = comorbid_psychosis_schiz_bipolar) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_psychosis_schiz_bipolar_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_psychosis_schiz_bipolar, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_psychosis_schiz_bipolar_chisq <- dplyr::mutate (comorbid_psychosis_schiz_bipolar_chisq, variable = "comorbid_psychosis_schiz_bipolar") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- DWMP_eligible_comorbid_psychosis_schiz_bipolar %>%
  dplyr::left_join(comorbid_psychosis_schiz_bipolar_chisq, by = "variable")

########################

#########  comorbid_diabetes_t1 #######

#1.  count by comorbid_diabetes_t1
N_comorbid_diabetes_t1 <- BMI_complete_categories_2019_DT[, .N, by="comorbid_diabetes_t1"]
DWMP_eligible_comorbid_diabetes_t1 <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_diabetes_t1"] 
DWMP_eligible_comorbid_diabetes_t1 <-   DWMP_eligible_comorbid_diabetes_t1[order(comorbid_diabetes_t1)]

DWMP_eligible_comorbid_diabetes_t1 <- dplyr::left_join(DWMP_eligible_comorbid_diabetes_t1, N_comorbid_diabetes_t1)
DWMP_eligible_comorbid_diabetes_t1 <- DWMP_eligible_comorbid_diabetes_t1 %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_diabetes_t1 <- DWMP_eligible_comorbid_diabetes_t1 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_diabetes_t1", .before=1) %>%
  dplyr::rename(group = comorbid_diabetes_t1) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_diabetes_t1_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_diabetes_t1, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_diabetes_t1_chisq <- dplyr::mutate (comorbid_diabetes_t1_chisq, variable = "comorbid_diabetes_t1") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_diabetes_t1 <- DWMP_eligible_comorbid_diabetes_t1 %>%
  dplyr::left_join(comorbid_diabetes_t1_chisq, by = "variable")


#########  comorbid_diabetes_t2 #######

#1.  count by comorbid_diabetes_t2
N_comorbid_diabetes_t2 <- BMI_complete_categories_2019_DT[, .N, by="comorbid_diabetes_t2"]
DWMP_eligible_comorbid_diabetes_t2 <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_diabetes_t2"] 
DWMP_eligible_comorbid_diabetes_t2 <-   DWMP_eligible_comorbid_diabetes_t2[order(comorbid_diabetes_t2)]

DWMP_eligible_comorbid_diabetes_t2 <- dplyr::left_join(DWMP_eligible_comorbid_diabetes_t2, N_comorbid_diabetes_t2)
DWMP_eligible_comorbid_diabetes_t2 <- DWMP_eligible_comorbid_diabetes_t2 %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_diabetes_t2 <- DWMP_eligible_comorbid_diabetes_t2 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_diabetes_t2", .before=1) %>%
  dplyr::rename(group = comorbid_diabetes_t2) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_diabetes_t2_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_diabetes_t2, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_diabetes_t2_chisq <- dplyr::mutate (comorbid_diabetes_t2_chisq, variable = "comorbid_diabetes_t2") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_diabetes_t2 <- DWMP_eligible_comorbid_diabetes_t2 %>%
  dplyr::left_join(comorbid_diabetes_t2_chisq, by = "variable")



#########  comorbid_asthma #######

#1.  count by comorbid_asthma
N_comorbid_asthma <- BMI_complete_categories_2019_DT[, .N, by="comorbid_asthma"]
DWMP_eligible_comorbid_asthma <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_asthma"] 
DWMP_eligible_comorbid_asthma <-   DWMP_eligible_comorbid_asthma[order(comorbid_asthma)]

DWMP_eligible_comorbid_asthma <- dplyr::left_join(DWMP_eligible_comorbid_asthma, N_comorbid_asthma)
DWMP_eligible_comorbid_asthma <- DWMP_eligible_comorbid_asthma %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_asthma <- DWMP_eligible_comorbid_asthma %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_asthma", .before=1) %>%
  dplyr::rename(group = comorbid_asthma) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_asthma_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_asthma, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_asthma_chisq <- dplyr::mutate (comorbid_asthma_chisq, variable = "comorbid_asthma") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_asthma <- DWMP_eligible_comorbid_asthma %>%
  dplyr::left_join(comorbid_asthma_chisq, by = "variable")


#########  comorbid_COPD #######

#1.  count by comorbid_COPD
N_comorbid_COPD <- BMI_complete_categories_2019_DT[, .N, by="comorbid_COPD"]
DWMP_eligible_comorbid_COPD <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_COPD"] 
DWMP_eligible_comorbid_COPD <-   DWMP_eligible_comorbid_COPD[order(comorbid_COPD)]

DWMP_eligible_comorbid_COPD <- dplyr::left_join(DWMP_eligible_comorbid_COPD, N_comorbid_COPD)
DWMP_eligible_comorbid_COPD <- DWMP_eligible_comorbid_COPD %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_COPD <- DWMP_eligible_comorbid_COPD %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_COPD", .before=1) %>%
  dplyr::rename(group = comorbid_COPD) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_COPD_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_COPD, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_COPD_chisq <- dplyr::mutate (comorbid_COPD_chisq, variable = "comorbid_COPD") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_COPD <- DWMP_eligible_comorbid_COPD %>%
  dplyr::left_join(comorbid_COPD_chisq, by = "variable")

#########  comorbid_stroke_and_TIA #######

#1.  count by comorbid_stroke_and_TIA
N_comorbid_stroke_and_TIA <- BMI_complete_categories_2019_DT[, .N, by="comorbid_stroke_and_TIA"]
DWMP_eligible_comorbid_stroke_and_TIA <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_stroke_and_TIA"] 
DWMP_eligible_comorbid_stroke_and_TIA <-   DWMP_eligible_comorbid_stroke_and_TIA[order(comorbid_stroke_and_TIA)]

DWMP_eligible_comorbid_stroke_and_TIA <- dplyr::left_join(DWMP_eligible_comorbid_stroke_and_TIA, N_comorbid_stroke_and_TIA)
DWMP_eligible_comorbid_stroke_and_TIA <- DWMP_eligible_comorbid_stroke_and_TIA %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_stroke_and_TIA <- DWMP_eligible_comorbid_stroke_and_TIA %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_stroke_and_TIA", .before=1) %>%
  dplyr::rename(group = comorbid_stroke_and_TIA) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_stroke_and_TIA_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_stroke_and_TIA, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_stroke_and_TIA_chisq <- dplyr::mutate (comorbid_stroke_and_TIA_chisq, variable = "comorbid_stroke_and_TIA") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_stroke_and_TIA <- DWMP_eligible_comorbid_stroke_and_TIA %>%
  dplyr::left_join(comorbid_stroke_and_TIA_chisq, by = "variable")

#########  comorbid_chronic_cardiac #######

#1.  count by comorbid_chronic_cardiac
N_comorbid_chronic_cardiac <- BMI_complete_categories_2019_DT[, .N, by="comorbid_chronic_cardiac"]
DWMP_eligible_comorbid_chronic_cardiac <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_chronic_cardiac"] 
DWMP_eligible_comorbid_chronic_cardiac <-   DWMP_eligible_comorbid_chronic_cardiac[order(comorbid_chronic_cardiac)]

DWMP_eligible_comorbid_chronic_cardiac <- dplyr::left_join(DWMP_eligible_comorbid_chronic_cardiac, N_comorbid_chronic_cardiac)
DWMP_eligible_comorbid_chronic_cardiac <- DWMP_eligible_comorbid_chronic_cardiac %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_chronic_cardiac <- DWMP_eligible_comorbid_chronic_cardiac %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_chronic_cardiac", .before=1) %>%
  dplyr::rename(group = comorbid_chronic_cardiac) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_chronic_cardiac_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_chronic_cardiac, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_chronic_cardiac_chisq <- dplyr::mutate (comorbid_chronic_cardiac_chisq, variable = "comorbid_chronic_cardiac") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_chronic_cardiac <- DWMP_eligible_comorbid_chronic_cardiac %>%
  dplyr::left_join(comorbid_chronic_cardiac_chisq, by = "variable")


#########  comorbid_hypertension #######

#1.  count by comorbid_hypertension
N_comorbid_hypertension <- BMI_complete_categories_2019_DT[, .N, by="comorbid_hypertension"]
DWMP_eligible_comorbid_hypertension <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_hypertension"] 
DWMP_eligible_comorbid_hypertension <-   DWMP_eligible_comorbid_hypertension[order(comorbid_hypertension)]

DWMP_eligible_comorbid_hypertension <- dplyr::left_join(DWMP_eligible_comorbid_hypertension, N_comorbid_hypertension)
DWMP_eligible_comorbid_hypertension <- DWMP_eligible_comorbid_hypertension %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_hypertension <- DWMP_eligible_comorbid_hypertension %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_hypertension", .before=1) %>%
  dplyr::rename(group = comorbid_hypertension) %>%
  dplyr::mutate(group = as.character(group)) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_hypertension_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_hypertension, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_hypertension_chisq <- dplyr::mutate (comorbid_hypertension_chisq, variable = "comorbid_hypertension") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_hypertension <- DWMP_eligible_comorbid_hypertension %>%
  dplyr::left_join(comorbid_hypertension_chisq, by = "variable")


#########  comorbid_all_cancer #######

#1.  count by comorbid_all_cancer
N_comorbid_all_cancer <- BMI_complete_categories_2019_DT[, .N, by="comorbid_all_cancer"]
DWMP_eligible_comorbid_all_cancer <- BMI_complete_categories_2019_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_all_cancer"] 
DWMP_eligible_comorbid_all_cancer <-   DWMP_eligible_comorbid_all_cancer[order(comorbid_all_cancer)]

DWMP_eligible_comorbid_all_cancer <- dplyr::left_join(DWMP_eligible_comorbid_all_cancer, N_comorbid_all_cancer)
DWMP_eligible_comorbid_all_cancer <- DWMP_eligible_comorbid_all_cancer %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_all_cancer <- DWMP_eligible_comorbid_all_cancer %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_all_cancer", .before=1) %>%
  dplyr::rename(group = comorbid_all_cancer) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_all_cancer_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_all_cancer, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_all_cancer_chisq <- dplyr::mutate (comorbid_all_cancer_chisq, variable = "comorbid_all_cancer") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_all_cancer <- DWMP_eligible_comorbid_all_cancer %>%
  dplyr::left_join(comorbid_all_cancer_chisq, by = "variable")





DWMP_eligible_table_2019 <- DWMP_eligible_table %>%
  bind_rows(DWMP_eligible_age_group) %>%
  bind_rows (DWMP_eligible_sex) %>%
  bind_rows (DWMP_eligible_region) %>%
  bind_rows (DWMP_eligible_imd) %>%
  bind_rows (DWMP_eligible_ethnic_no_miss) %>%
  bind_rows (DWMP_eligible_eth_group_16) %>%   
  bind_rows (DWMP_eligible_comorbid_hypertension) %>%
  bind_rows (DWMP_eligible_comorbid_diabetes_t1) %>%
  bind_rows (DWMP_eligible_comorbid_diabetes_t2) %>%
  bind_rows (DWMP_eligible_comorbid_asthma) %>%
  bind_rows (DWMP_eligible_comorbid_COPD) %>%
  bind_rows (DWMP_eligible_comorbid_learning_disability) %>%
  bind_rows (DWMP_eligible_comorbid_depression) %>%
  bind_rows (DWMP_eligible_comorbid_psychosis_schiz_bipolar) %>%    
  bind_rows (DWMP_eligible_comorbid_dementia) %>%
  bind_rows (DWMP_eligible_comorbid_stroke_and_TIA) %>%
  bind_rows (DWMP_eligible_comorbid_chronic_cardiac) %>%
  bind_rows (DWMP_eligible_comorbid_all_cancer)




#####################################################################################
###  2020


BMI_complete_categories_2020 <- BMI_complete_categories_2020 %>%
  dplyr::mutate(DWMP_eligible = case_when(
    DWMP == "eligible"  ~ "TRUE", 
    DWMP == "not_eligible" ~ "FALSE"
  ))

BMI_complete_categories_2020 <- BMI_complete_categories_2020 %>%
  dplyr::mutate(DWMP_eligible = as.logical(DWMP_eligible))



BMI_complete_categories <- BMI_complete_categories_2020 %>% 
  dplyr::mutate(imd=as.numeric(imd)) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "0-17", "18-39", "40-65", "65-80", "80+"))

BMI_complete_categories$DWMP_eligible[BMI_complete_categories$DWMP_eligible == 1] <- "TRUE"
BMI_complete_categories$DWMP_eligible[BMI_complete_categories$DWMP_eligible == 0] <- "FALSE"



BMI_complete_categories_2020_DT <- data.table(BMI_complete_categories)



# develop a vector of explanatory variables
explanatory_vars <- c("sex", 
                      "age_group", 
                      "region", 
                      "imd", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "comorbid_learning_disability",     
                      "comorbid_depression",            
                      "comorbid_dementia",               
                      "comorbid_psychosis_schiz_bipolar",
                      "comorbid_diabetes_type",
                      "comorbid_diabetes_t1",           
                      "comorbid_diabetes_t2",             
                      "comorbid_asthma",                
                      "comorbid_COPD",                   
                      "comorbid_stroke_and_TIA",         
                      "comorbid_chronic_cardiac", 
                      "comorbid_hypertension",           
                      "comorbid_all_cancer")



N_DWMP_eligible <- BMI_complete_categories_2020_DT[, .N, ]
DWMP_eligible_table<- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N),] 

DWMP_eligible_table <- dplyr::mutate (DWMP_eligible_table, N = N_DWMP_eligible) %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(group = "all", .before=1) %>%
  dplyr::mutate(variable = "all", .before=1)




##  Develop count table and chi squared test of difference between population long hand for each variable to reduce memory


#########  AGE GROUPS #######

#1.  count by age_group
N_age_group <- BMI_complete_categories_2020_DT[, .N, by="age_group"]
DWMP_eligible_age_group <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="age_group"] 
DWMP_eligible_age_group <-   DWMP_eligible_age_group[order(age_group)]

DWMP_eligible_age_group <- dplyr::left_join(DWMP_eligible_age_group, N_age_group)
DWMP_eligible_age_group <- DWMP_eligible_age_group %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_age_group <- DWMP_eligible_age_group %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "age_group", .before=1) %>%
  dplyr::rename(group = age_group)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
age_group_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(age_group, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
age_group_chisq <- dplyr::mutate (age_group_chisq, variable = "age_group") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_age_group <- DWMP_eligible_age_group %>%
  dplyr::left_join(age_group_chisq, by = "variable")





###### SEX

#1.  count by sex
N_sex <- BMI_complete_categories_2020_DT[, .N, by="sex"]
DWMP_eligible_sex <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="sex"] 
DWMP_eligible_sex <-   DWMP_eligible_sex[order(sex)]

DWMP_eligible_sex <- dplyr::left_join(DWMP_eligible_sex, N_sex)
DWMP_eligible_sex <- DWMP_eligible_sex %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_sex <- DWMP_eligible_sex %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "sex", .before=1) %>%
  dplyr::rename(group = sex)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
sex_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(sex, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
sex_chisq <- dplyr::mutate (sex_chisq, variable = "sex") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_sex <- DWMP_eligible_sex %>%
  dplyr::left_join(sex_chisq, by = "variable")


#########  region #######

#1.  count by region
N_region <- BMI_complete_categories_2020_DT[, .N, by="region"]
DWMP_eligible_region <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="region"] 
DWMP_eligible_region <-   DWMP_eligible_region[order(region)]

DWMP_eligible_region <- dplyr::left_join(DWMP_eligible_region, N_region)
DWMP_eligible_region <- DWMP_eligible_region %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_region <- DWMP_eligible_region %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "region", .before=1) %>%
  dplyr::rename(group = region)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
region_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(region, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
region_chisq <- dplyr::mutate (region_chisq, variable = "region") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_region <- DWMP_eligible_region %>%
  dplyr::left_join(region_chisq, by = "variable")

#########  imd #######

#1.  count by imd
N_imd <- BMI_complete_categories_2020_DT[, .N, by="imd"]
DWMP_eligible_imd <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="imd"] 
DWMP_eligible_imd <-   DWMP_eligible_imd[order(imd)]

DWMP_eligible_imd <- dplyr::left_join(DWMP_eligible_imd, N_imd)
DWMP_eligible_imd <- DWMP_eligible_imd %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_imd <- DWMP_eligible_imd %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "imd", .before=1) %>%
  dplyr::rename(group = imd)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
imd_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(imd, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
imd_chisq <- dplyr::mutate (imd_chisq, variable = "imd") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_imd <- DWMP_eligible_imd %>%
  dplyr::left_join(imd_chisq, by = "variable")

#########  ethnic_no_miss #######

#1.  count by ethnic_no_miss
N_ethnic_no_miss <- BMI_complete_categories_2020_DT[, .N, by="ethnic_no_miss"]
DWMP_eligible_ethnic_no_miss <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="ethnic_no_miss"] 
DWMP_eligible_ethnic_no_miss <-   DWMP_eligible_ethnic_no_miss[order(ethnic_no_miss)]

DWMP_eligible_ethnic_no_miss <- dplyr::left_join(DWMP_eligible_ethnic_no_miss, N_ethnic_no_miss)
DWMP_eligible_ethnic_no_miss <- DWMP_eligible_ethnic_no_miss %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_ethnic_no_miss <- DWMP_eligible_ethnic_no_miss %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "ethnic_no_miss", .before=1) %>%
  dplyr::rename(group = ethnic_no_miss)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
ethnic_no_miss_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(ethnic_no_miss, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
ethnic_no_miss_chisq <- dplyr::mutate (ethnic_no_miss_chisq, variable = "ethnic_no_miss") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_ethnic_no_miss <- DWMP_eligible_ethnic_no_miss %>%
  dplyr::left_join(ethnic_no_miss_chisq, by = "variable")

#########  eth_group_16 #######

#1.  count by eth_group_16
N_eth_group_16 <- BMI_complete_categories_2020_DT[, .N, by="eth_group_16"]
DWMP_eligible_eth_group_16 <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="eth_group_16"] 
DWMP_eligible_eth_group_16 <-   DWMP_eligible_eth_group_16[order(eth_group_16)]

DWMP_eligible_eth_group_16 <- dplyr::left_join(DWMP_eligible_eth_group_16, N_eth_group_16)
DWMP_eligible_eth_group_16 <- DWMP_eligible_eth_group_16 %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_eth_group_16 <- DWMP_eligible_eth_group_16 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "eth_group_16", .before=1) %>%
  dplyr::rename(group = eth_group_16)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
eth_group_16_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(eth_group_16, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
eth_group_16_chisq <- dplyr::mutate (eth_group_16_chisq, variable = "eth_group_16") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_eth_group_16 <- DWMP_eligible_eth_group_16 %>%
  dplyr::left_join(eth_group_16_chisq, by = "variable")

########  comorbid_learning_disability #######

#1.  count by comorbid_learning_disability
N_comorbid_learning_disability <- BMI_complete_categories_2020_DT[, .N, by="comorbid_learning_disability"]
DWMP_eligible_comorbid_learning_disability <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_learning_disability"] 
DWMP_eligible_comorbid_learning_disability <-   DWMP_eligible_comorbid_learning_disability[order(comorbid_learning_disability)]

DWMP_eligible_comorbid_learning_disability <- dplyr::left_join(DWMP_eligible_comorbid_learning_disability, N_comorbid_learning_disability)
DWMP_eligible_comorbid_learning_disability <- DWMP_eligible_comorbid_learning_disability %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_learning_disability <- DWMP_eligible_comorbid_learning_disability %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_learning_disability", .before=1) %>%
  dplyr::rename(group = comorbid_learning_disability) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_learning_disability_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_learning_disability, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_learning_disability_chisq <- dplyr::mutate (comorbid_learning_disability_chisq, variable = "comorbid_learning_disability") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_learning_disability <- DWMP_eligible_comorbid_learning_disability %>%
  dplyr::left_join(comorbid_learning_disability_chisq, by = "variable")



#########  comorbid_depression #######

#1.  count by comorbid_depression
N_comorbid_depression <- BMI_complete_categories_2020_DT[, .N, by="comorbid_depression"]
DWMP_eligible_comorbid_depression <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_depression"] 
DWMP_eligible_comorbid_depression <-   DWMP_eligible_comorbid_depression[order(comorbid_depression)]

DWMP_eligible_comorbid_depression <- dplyr::left_join(DWMP_eligible_comorbid_depression, N_comorbid_depression)
DWMP_eligible_comorbid_depression <- DWMP_eligible_comorbid_depression %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_depression <- DWMP_eligible_comorbid_depression %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_depression", .before=1) %>%
  dplyr::rename(group = comorbid_depression) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_depression_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_depression, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_depression_chisq <- dplyr::mutate (comorbid_depression_chisq, variable = "comorbid_depression") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_depression <- DWMP_eligible_comorbid_depression %>%
  dplyr::left_join(comorbid_depression_chisq, by = "variable")


#########  comorbid_dementia #######

#1.  count by comorbid_dementia
N_comorbid_dementia <- BMI_complete_categories_2020_DT[, .N, by="comorbid_dementia"]
DWMP_eligible_comorbid_dementia <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_dementia"] 
DWMP_eligible_comorbid_dementia <-   DWMP_eligible_comorbid_dementia[order(comorbid_dementia)]

DWMP_eligible_comorbid_dementia <- dplyr::left_join(DWMP_eligible_comorbid_dementia, N_comorbid_dementia)
DWMP_eligible_comorbid_dementia <- DWMP_eligible_comorbid_dementia %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_dementia <- DWMP_eligible_comorbid_dementia %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_dementia", .before=1) %>%
  dplyr::rename(group = comorbid_dementia) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_dementia_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_dementia, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_dementia_chisq <- dplyr::mutate (comorbid_dementia_chisq, variable = "comorbid_dementia") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_dementia <- DWMP_eligible_comorbid_dementia %>%
  dplyr::left_join(comorbid_dementia_chisq, by = "variable")


#########  comorbid_psychosis_schiz_bipolar #######

#1.  count by comorbid_psychosis_schiz_bipolar
N_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_2020_DT[, .N, by="comorbid_psychosis_schiz_bipolar"]
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_psychosis_schiz_bipolar"] 
DWMP_eligible_comorbid_psychosis_schiz_bipolar <-   DWMP_eligible_comorbid_psychosis_schiz_bipolar[order(comorbid_psychosis_schiz_bipolar)]

DWMP_eligible_comorbid_psychosis_schiz_bipolar <- dplyr::left_join(DWMP_eligible_comorbid_psychosis_schiz_bipolar, N_comorbid_psychosis_schiz_bipolar)
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- DWMP_eligible_comorbid_psychosis_schiz_bipolar %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- DWMP_eligible_comorbid_psychosis_schiz_bipolar %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_psychosis_schiz_bipolar", .before=1) %>%
  dplyr::rename(group = comorbid_psychosis_schiz_bipolar) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_psychosis_schiz_bipolar_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_psychosis_schiz_bipolar, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_psychosis_schiz_bipolar_chisq <- dplyr::mutate (comorbid_psychosis_schiz_bipolar_chisq, variable = "comorbid_psychosis_schiz_bipolar") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- DWMP_eligible_comorbid_psychosis_schiz_bipolar %>%
  dplyr::left_join(comorbid_psychosis_schiz_bipolar_chisq, by = "variable")

########################

#########  comorbid_diabetes_t1 #######

#1.  count by comorbid_diabetes_t1
N_comorbid_diabetes_t1 <- BMI_complete_categories_2020_DT[, .N, by="comorbid_diabetes_t1"]
DWMP_eligible_comorbid_diabetes_t1 <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_diabetes_t1"] 
DWMP_eligible_comorbid_diabetes_t1 <-   DWMP_eligible_comorbid_diabetes_t1[order(comorbid_diabetes_t1)]

DWMP_eligible_comorbid_diabetes_t1 <- dplyr::left_join(DWMP_eligible_comorbid_diabetes_t1, N_comorbid_diabetes_t1)
DWMP_eligible_comorbid_diabetes_t1 <- DWMP_eligible_comorbid_diabetes_t1 %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_diabetes_t1 <- DWMP_eligible_comorbid_diabetes_t1 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_diabetes_t1", .before=1) %>%
  dplyr::rename(group = comorbid_diabetes_t1) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_diabetes_t1_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_diabetes_t1, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_diabetes_t1_chisq <- dplyr::mutate (comorbid_diabetes_t1_chisq, variable = "comorbid_diabetes_t1") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_diabetes_t1 <- DWMP_eligible_comorbid_diabetes_t1 %>%
  dplyr::left_join(comorbid_diabetes_t1_chisq, by = "variable")


#########  comorbid_diabetes_t2 #######

#1.  count by comorbid_diabetes_t2
N_comorbid_diabetes_t2 <- BMI_complete_categories_2020_DT[, .N, by="comorbid_diabetes_t2"]
DWMP_eligible_comorbid_diabetes_t2 <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_diabetes_t2"] 
DWMP_eligible_comorbid_diabetes_t2 <-   DWMP_eligible_comorbid_diabetes_t2[order(comorbid_diabetes_t2)]

DWMP_eligible_comorbid_diabetes_t2 <- dplyr::left_join(DWMP_eligible_comorbid_diabetes_t2, N_comorbid_diabetes_t2)
DWMP_eligible_comorbid_diabetes_t2 <- DWMP_eligible_comorbid_diabetes_t2 %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_diabetes_t2 <- DWMP_eligible_comorbid_diabetes_t2 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_diabetes_t2", .before=1) %>%
  dplyr::rename(group = comorbid_diabetes_t2) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_diabetes_t2_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_diabetes_t2, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_diabetes_t2_chisq <- dplyr::mutate (comorbid_diabetes_t2_chisq, variable = "comorbid_diabetes_t2") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_diabetes_t2 <- DWMP_eligible_comorbid_diabetes_t2 %>%
  dplyr::left_join(comorbid_diabetes_t2_chisq, by = "variable")



#########  comorbid_asthma #######

#1.  count by comorbid_asthma
N_comorbid_asthma <- BMI_complete_categories_2020_DT[, .N, by="comorbid_asthma"]
DWMP_eligible_comorbid_asthma <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_asthma"] 
DWMP_eligible_comorbid_asthma <-   DWMP_eligible_comorbid_asthma[order(comorbid_asthma)]

DWMP_eligible_comorbid_asthma <- dplyr::left_join(DWMP_eligible_comorbid_asthma, N_comorbid_asthma)
DWMP_eligible_comorbid_asthma <- DWMP_eligible_comorbid_asthma %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_asthma <- DWMP_eligible_comorbid_asthma %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_asthma", .before=1) %>%
  dplyr::rename(group = comorbid_asthma) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_asthma_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_asthma, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_asthma_chisq <- dplyr::mutate (comorbid_asthma_chisq, variable = "comorbid_asthma") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_asthma <- DWMP_eligible_comorbid_asthma %>%
  dplyr::left_join(comorbid_asthma_chisq, by = "variable")


#########  comorbid_COPD #######

#1.  count by comorbid_COPD
N_comorbid_COPD <- BMI_complete_categories_2020_DT[, .N, by="comorbid_COPD"]
DWMP_eligible_comorbid_COPD <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_COPD"] 
DWMP_eligible_comorbid_COPD <-   DWMP_eligible_comorbid_COPD[order(comorbid_COPD)]

DWMP_eligible_comorbid_COPD <- dplyr::left_join(DWMP_eligible_comorbid_COPD, N_comorbid_COPD)
DWMP_eligible_comorbid_COPD <- DWMP_eligible_comorbid_COPD %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_COPD <- DWMP_eligible_comorbid_COPD %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_COPD", .before=1) %>%
  dplyr::rename(group = comorbid_COPD) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_COPD_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_COPD, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_COPD_chisq <- dplyr::mutate (comorbid_COPD_chisq, variable = "comorbid_COPD") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_COPD <- DWMP_eligible_comorbid_COPD %>%
  dplyr::left_join(comorbid_COPD_chisq, by = "variable")

#########  comorbid_stroke_and_TIA #######

#1.  count by comorbid_stroke_and_TIA
N_comorbid_stroke_and_TIA <- BMI_complete_categories_2020_DT[, .N, by="comorbid_stroke_and_TIA"]
DWMP_eligible_comorbid_stroke_and_TIA <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_stroke_and_TIA"] 
DWMP_eligible_comorbid_stroke_and_TIA <-   DWMP_eligible_comorbid_stroke_and_TIA[order(comorbid_stroke_and_TIA)]

DWMP_eligible_comorbid_stroke_and_TIA <- dplyr::left_join(DWMP_eligible_comorbid_stroke_and_TIA, N_comorbid_stroke_and_TIA)
DWMP_eligible_comorbid_stroke_and_TIA <- DWMP_eligible_comorbid_stroke_and_TIA %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_stroke_and_TIA <- DWMP_eligible_comorbid_stroke_and_TIA %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_stroke_and_TIA", .before=1) %>%
  dplyr::rename(group = comorbid_stroke_and_TIA) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_stroke_and_TIA_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_stroke_and_TIA, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_stroke_and_TIA_chisq <- dplyr::mutate (comorbid_stroke_and_TIA_chisq, variable = "comorbid_stroke_and_TIA") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_stroke_and_TIA <- DWMP_eligible_comorbid_stroke_and_TIA %>%
  dplyr::left_join(comorbid_stroke_and_TIA_chisq, by = "variable")

#########  comorbid_chronic_cardiac #######

#1.  count by comorbid_chronic_cardiac
N_comorbid_chronic_cardiac <- BMI_complete_categories_2020_DT[, .N, by="comorbid_chronic_cardiac"]
DWMP_eligible_comorbid_chronic_cardiac <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_chronic_cardiac"] 
DWMP_eligible_comorbid_chronic_cardiac <-   DWMP_eligible_comorbid_chronic_cardiac[order(comorbid_chronic_cardiac)]

DWMP_eligible_comorbid_chronic_cardiac <- dplyr::left_join(DWMP_eligible_comorbid_chronic_cardiac, N_comorbid_chronic_cardiac)
DWMP_eligible_comorbid_chronic_cardiac <- DWMP_eligible_comorbid_chronic_cardiac %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_chronic_cardiac <- DWMP_eligible_comorbid_chronic_cardiac %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_chronic_cardiac", .before=1) %>%
  dplyr::rename(group = comorbid_chronic_cardiac) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_chronic_cardiac_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_chronic_cardiac, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_chronic_cardiac_chisq <- dplyr::mutate (comorbid_chronic_cardiac_chisq, variable = "comorbid_chronic_cardiac") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_chronic_cardiac <- DWMP_eligible_comorbid_chronic_cardiac %>%
  dplyr::left_join(comorbid_chronic_cardiac_chisq, by = "variable")


#########  comorbid_hypertension #######

#1.  count by comorbid_hypertension
N_comorbid_hypertension <- BMI_complete_categories_2020_DT[, .N, by="comorbid_hypertension"]
DWMP_eligible_comorbid_hypertension <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_hypertension"] 
DWMP_eligible_comorbid_hypertension <-   DWMP_eligible_comorbid_hypertension[order(comorbid_hypertension)]

DWMP_eligible_comorbid_hypertension <- dplyr::left_join(DWMP_eligible_comorbid_hypertension, N_comorbid_hypertension)
DWMP_eligible_comorbid_hypertension <- DWMP_eligible_comorbid_hypertension %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_hypertension <- DWMP_eligible_comorbid_hypertension %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_hypertension", .before=1) %>%
  dplyr::rename(group = comorbid_hypertension) %>%
  dplyr::mutate(group = as.character(group)) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_hypertension_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_hypertension, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_hypertension_chisq <- dplyr::mutate (comorbid_hypertension_chisq, variable = "comorbid_hypertension") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_hypertension <- DWMP_eligible_comorbid_hypertension %>%
  dplyr::left_join(comorbid_hypertension_chisq, by = "variable")


#########  comorbid_all_cancer #######

#1.  count by comorbid_all_cancer
N_comorbid_all_cancer <- BMI_complete_categories_2020_DT[, .N, by="comorbid_all_cancer"]
DWMP_eligible_comorbid_all_cancer <- BMI_complete_categories_2020_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_all_cancer"] 
DWMP_eligible_comorbid_all_cancer <-   DWMP_eligible_comorbid_all_cancer[order(comorbid_all_cancer)]

DWMP_eligible_comorbid_all_cancer <- dplyr::left_join(DWMP_eligible_comorbid_all_cancer, N_comorbid_all_cancer)
DWMP_eligible_comorbid_all_cancer <- DWMP_eligible_comorbid_all_cancer %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_all_cancer <- DWMP_eligible_comorbid_all_cancer %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_all_cancer", .before=1) %>%
  dplyr::rename(group = comorbid_all_cancer) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_all_cancer_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_all_cancer, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_all_cancer_chisq <- dplyr::mutate (comorbid_all_cancer_chisq, variable = "comorbid_all_cancer") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_all_cancer <- DWMP_eligible_comorbid_all_cancer %>%
  dplyr::left_join(comorbid_all_cancer_chisq, by = "variable")





DWMP_eligible_table_2020 <- DWMP_eligible_table %>%
  bind_rows(DWMP_eligible_age_group) %>%
  bind_rows (DWMP_eligible_sex) %>%
  bind_rows (DWMP_eligible_region) %>%
  bind_rows (DWMP_eligible_imd) %>%
  bind_rows (DWMP_eligible_ethnic_no_miss) %>%
  bind_rows (DWMP_eligible_eth_group_16) %>%   
  bind_rows (DWMP_eligible_comorbid_hypertension) %>%
  bind_rows (DWMP_eligible_comorbid_diabetes_t1) %>%
  bind_rows (DWMP_eligible_comorbid_diabetes_t2) %>%
  bind_rows (DWMP_eligible_comorbid_asthma) %>%
  bind_rows (DWMP_eligible_comorbid_COPD) %>%
  bind_rows (DWMP_eligible_comorbid_learning_disability) %>%
  bind_rows (DWMP_eligible_comorbid_depression) %>%
  bind_rows (DWMP_eligible_comorbid_psychosis_schiz_bipolar) %>%    
  bind_rows (DWMP_eligible_comorbid_dementia) %>%
  bind_rows (DWMP_eligible_comorbid_stroke_and_TIA) %>%
  bind_rows (DWMP_eligible_comorbid_chronic_cardiac) %>%
  bind_rows (DWMP_eligible_comorbid_all_cancer)


##################################################################
#########  2021 DATA



BMI_complete_categories_2021 <- BMI_complete_categories_2021 %>%
  dplyr::mutate(DWMP_eligible = case_when(
    DWMP == "eligible"  ~ "TRUE", 
    DWMP == "not_eligible" ~ "FALSE"
  ))

BMI_complete_categories_2021 <- BMI_complete_categories_2021 %>%
  dplyr::mutate(DWMP_eligible = as.logical(DWMP_eligible))



BMI_complete_categories <- BMI_complete_categories_2021 %>% 
  dplyr::mutate(imd=as.numeric(imd)) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "0-17", "18-39", "40-65", "65-80", "80+"))

BMI_complete_categories$DWMP_eligible[BMI_complete_categories$DWMP_eligible == 1] <- "TRUE"
BMI_complete_categories$DWMP_eligible[BMI_complete_categories$DWMP_eligible == 0] <- "FALSE"



BMI_complete_categories_2021_DT <- data.table(BMI_complete_categories)



# develop a vector of explanatory variables
explanatory_vars <- c("sex", 
                      "age_group", 
                      "region", 
                      "imd", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "comorbid_learning_disability",     
                      "comorbid_depression",            
                      "comorbid_dementia",               
                      "comorbid_psychosis_schiz_bipolar",
                      "comorbid_diabetes_type",
                      "comorbid_diabetes_t1",           
                      "comorbid_diabetes_t2",             
                      "comorbid_asthma",                
                      "comorbid_COPD",                   
                      "comorbid_stroke_and_TIA",         
                      "comorbid_chronic_cardiac", 
                      "comorbid_hypertension",           
                      "comorbid_all_cancer")



N_DWMP_eligible <- BMI_complete_categories_2021_DT[, .N, ]
DWMP_eligible_table<- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N),] 

DWMP_eligible_table <- dplyr::mutate (DWMP_eligible_table, N = N_DWMP_eligible) %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(group = "all", .before=1) %>%
  dplyr::mutate(variable = "all", .before=1)




##  Develop count table and chi squared test of difference between population long hand for each variable to reduce memory


#########  AGE GROUPS #######

#1.  count by age_group
N_age_group <- BMI_complete_categories_2021_DT[, .N, by="age_group"]
DWMP_eligible_age_group <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="age_group"] 
DWMP_eligible_age_group <-   DWMP_eligible_age_group[order(age_group)]

DWMP_eligible_age_group <- dplyr::left_join(DWMP_eligible_age_group, N_age_group)
DWMP_eligible_age_group <- DWMP_eligible_age_group %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_age_group <- DWMP_eligible_age_group %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "age_group", .before=1) %>%
  dplyr::rename(group = age_group)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
age_group_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(age_group, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
age_group_chisq <- dplyr::mutate (age_group_chisq, variable = "age_group") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_age_group <- DWMP_eligible_age_group %>%
  dplyr::left_join(age_group_chisq, by = "variable")





###### SEX

#1.  count by sex
N_sex <- BMI_complete_categories_2021_DT[, .N, by="sex"]
DWMP_eligible_sex <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="sex"] 
DWMP_eligible_sex <-   DWMP_eligible_sex[order(sex)]

DWMP_eligible_sex <- dplyr::left_join(DWMP_eligible_sex, N_sex)
DWMP_eligible_sex <- DWMP_eligible_sex %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_sex <- DWMP_eligible_sex %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "sex", .before=1) %>%
  dplyr::rename(group = sex)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
sex_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(sex, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
sex_chisq <- dplyr::mutate (sex_chisq, variable = "sex") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_sex <- DWMP_eligible_sex %>%
  dplyr::left_join(sex_chisq, by = "variable")


#########  region #######

#1.  count by region
N_region <- BMI_complete_categories_2021_DT[, .N, by="region"]
DWMP_eligible_region <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="region"] 
DWMP_eligible_region <-   DWMP_eligible_region[order(region)]

DWMP_eligible_region <- dplyr::left_join(DWMP_eligible_region, N_region)
DWMP_eligible_region <- DWMP_eligible_region %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_region <- DWMP_eligible_region %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "region", .before=1) %>%
  dplyr::rename(group = region)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
region_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(region, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
region_chisq <- dplyr::mutate (region_chisq, variable = "region") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_region <- DWMP_eligible_region %>%
  dplyr::left_join(region_chisq, by = "variable")

#########  imd #######

#1.  count by imd
N_imd <- BMI_complete_categories_2021_DT[, .N, by="imd"]
DWMP_eligible_imd <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="imd"] 
DWMP_eligible_imd <-   DWMP_eligible_imd[order(imd)]

DWMP_eligible_imd <- dplyr::left_join(DWMP_eligible_imd, N_imd)
DWMP_eligible_imd <- DWMP_eligible_imd %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_imd <- DWMP_eligible_imd %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "imd", .before=1) %>%
  dplyr::rename(group = imd)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
imd_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(imd, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
imd_chisq <- dplyr::mutate (imd_chisq, variable = "imd") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_imd <- DWMP_eligible_imd %>%
  dplyr::left_join(imd_chisq, by = "variable")

#########  ethnic_no_miss #######

#1.  count by ethnic_no_miss
N_ethnic_no_miss <- BMI_complete_categories_2021_DT[, .N, by="ethnic_no_miss"]
DWMP_eligible_ethnic_no_miss <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="ethnic_no_miss"] 
DWMP_eligible_ethnic_no_miss <-   DWMP_eligible_ethnic_no_miss[order(ethnic_no_miss)]

DWMP_eligible_ethnic_no_miss <- dplyr::left_join(DWMP_eligible_ethnic_no_miss, N_ethnic_no_miss)
DWMP_eligible_ethnic_no_miss <- DWMP_eligible_ethnic_no_miss %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_ethnic_no_miss <- DWMP_eligible_ethnic_no_miss %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "ethnic_no_miss", .before=1) %>%
  dplyr::rename(group = ethnic_no_miss)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
ethnic_no_miss_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(ethnic_no_miss, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
ethnic_no_miss_chisq <- dplyr::mutate (ethnic_no_miss_chisq, variable = "ethnic_no_miss") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_ethnic_no_miss <- DWMP_eligible_ethnic_no_miss %>%
  dplyr::left_join(ethnic_no_miss_chisq, by = "variable")

#########  eth_group_16 #######

#1.  count by eth_group_16
N_eth_group_16 <- BMI_complete_categories_2021_DT[, .N, by="eth_group_16"]
DWMP_eligible_eth_group_16 <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="eth_group_16"] 
DWMP_eligible_eth_group_16 <-   DWMP_eligible_eth_group_16[order(eth_group_16)]

DWMP_eligible_eth_group_16 <- dplyr::left_join(DWMP_eligible_eth_group_16, N_eth_group_16)
DWMP_eligible_eth_group_16 <- DWMP_eligible_eth_group_16 %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_eth_group_16 <- DWMP_eligible_eth_group_16 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "eth_group_16", .before=1) %>%
  dplyr::rename(group = eth_group_16)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
eth_group_16_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(eth_group_16, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
eth_group_16_chisq <- dplyr::mutate (eth_group_16_chisq, variable = "eth_group_16") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_eth_group_16 <- DWMP_eligible_eth_group_16 %>%
  dplyr::left_join(eth_group_16_chisq, by = "variable")

########  comorbid_learning_disability #######

#1.  count by comorbid_learning_disability
N_comorbid_learning_disability <- BMI_complete_categories_2021_DT[, .N, by="comorbid_learning_disability"]
DWMP_eligible_comorbid_learning_disability <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_learning_disability"] 
DWMP_eligible_comorbid_learning_disability <-   DWMP_eligible_comorbid_learning_disability[order(comorbid_learning_disability)]

DWMP_eligible_comorbid_learning_disability <- dplyr::left_join(DWMP_eligible_comorbid_learning_disability, N_comorbid_learning_disability)
DWMP_eligible_comorbid_learning_disability <- DWMP_eligible_comorbid_learning_disability %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_learning_disability <- DWMP_eligible_comorbid_learning_disability %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_learning_disability", .before=1) %>%
  dplyr::rename(group = comorbid_learning_disability) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_learning_disability_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_learning_disability, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_learning_disability_chisq <- dplyr::mutate (comorbid_learning_disability_chisq, variable = "comorbid_learning_disability") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_learning_disability <- DWMP_eligible_comorbid_learning_disability %>%
  dplyr::left_join(comorbid_learning_disability_chisq, by = "variable")



#########  comorbid_depression #######

#1.  count by comorbid_depression
N_comorbid_depression <- BMI_complete_categories_2021_DT[, .N, by="comorbid_depression"]
DWMP_eligible_comorbid_depression <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_depression"] 
DWMP_eligible_comorbid_depression <-   DWMP_eligible_comorbid_depression[order(comorbid_depression)]

DWMP_eligible_comorbid_depression <- dplyr::left_join(DWMP_eligible_comorbid_depression, N_comorbid_depression)
DWMP_eligible_comorbid_depression <- DWMP_eligible_comorbid_depression %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_depression <- DWMP_eligible_comorbid_depression %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_depression", .before=1) %>%
  dplyr::rename(group = comorbid_depression) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_depression_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_depression, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_depression_chisq <- dplyr::mutate (comorbid_depression_chisq, variable = "comorbid_depression") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_depression <- DWMP_eligible_comorbid_depression %>%
  dplyr::left_join(comorbid_depression_chisq, by = "variable")


#########  comorbid_dementia #######

#1.  count by comorbid_dementia
N_comorbid_dementia <- BMI_complete_categories_2021_DT[, .N, by="comorbid_dementia"]
DWMP_eligible_comorbid_dementia <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_dementia"] 
DWMP_eligible_comorbid_dementia <-   DWMP_eligible_comorbid_dementia[order(comorbid_dementia)]

DWMP_eligible_comorbid_dementia <- dplyr::left_join(DWMP_eligible_comorbid_dementia, N_comorbid_dementia)
DWMP_eligible_comorbid_dementia <- DWMP_eligible_comorbid_dementia %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_dementia <- DWMP_eligible_comorbid_dementia %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_dementia", .before=1) %>%
  dplyr::rename(group = comorbid_dementia) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_dementia_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_dementia, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_dementia_chisq <- dplyr::mutate (comorbid_dementia_chisq, variable = "comorbid_dementia") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_dementia <- DWMP_eligible_comorbid_dementia %>%
  dplyr::left_join(comorbid_dementia_chisq, by = "variable")


#########  comorbid_psychosis_schiz_bipolar #######

#1.  count by comorbid_psychosis_schiz_bipolar
N_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_2021_DT[, .N, by="comorbid_psychosis_schiz_bipolar"]
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_psychosis_schiz_bipolar"] 
DWMP_eligible_comorbid_psychosis_schiz_bipolar <-   DWMP_eligible_comorbid_psychosis_schiz_bipolar[order(comorbid_psychosis_schiz_bipolar)]

DWMP_eligible_comorbid_psychosis_schiz_bipolar <- dplyr::left_join(DWMP_eligible_comorbid_psychosis_schiz_bipolar, N_comorbid_psychosis_schiz_bipolar)
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- DWMP_eligible_comorbid_psychosis_schiz_bipolar %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- DWMP_eligible_comorbid_psychosis_schiz_bipolar %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_psychosis_schiz_bipolar", .before=1) %>%
  dplyr::rename(group = comorbid_psychosis_schiz_bipolar) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_psychosis_schiz_bipolar_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_psychosis_schiz_bipolar, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_psychosis_schiz_bipolar_chisq <- dplyr::mutate (comorbid_psychosis_schiz_bipolar_chisq, variable = "comorbid_psychosis_schiz_bipolar") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_psychosis_schiz_bipolar <- DWMP_eligible_comorbid_psychosis_schiz_bipolar %>%
  dplyr::left_join(comorbid_psychosis_schiz_bipolar_chisq, by = "variable")

########################

#########  comorbid_diabetes_t1 #######

#1.  count by comorbid_diabetes_t1
N_comorbid_diabetes_t1 <- BMI_complete_categories_2021_DT[, .N, by="comorbid_diabetes_t1"]
DWMP_eligible_comorbid_diabetes_t1 <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_diabetes_t1"] 
DWMP_eligible_comorbid_diabetes_t1 <-   DWMP_eligible_comorbid_diabetes_t1[order(comorbid_diabetes_t1)]

DWMP_eligible_comorbid_diabetes_t1 <- dplyr::left_join(DWMP_eligible_comorbid_diabetes_t1, N_comorbid_diabetes_t1)
DWMP_eligible_comorbid_diabetes_t1 <- DWMP_eligible_comorbid_diabetes_t1 %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_diabetes_t1 <- DWMP_eligible_comorbid_diabetes_t1 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_diabetes_t1", .before=1) %>%
  dplyr::rename(group = comorbid_diabetes_t1) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_diabetes_t1_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_diabetes_t1, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_diabetes_t1_chisq <- dplyr::mutate (comorbid_diabetes_t1_chisq, variable = "comorbid_diabetes_t1") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_diabetes_t1 <- DWMP_eligible_comorbid_diabetes_t1 %>%
  dplyr::left_join(comorbid_diabetes_t1_chisq, by = "variable")


#########  comorbid_diabetes_t2 #######

#1.  count by comorbid_diabetes_t2
N_comorbid_diabetes_t2 <- BMI_complete_categories_2021_DT[, .N, by="comorbid_diabetes_t2"]
DWMP_eligible_comorbid_diabetes_t2 <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_diabetes_t2"] 
DWMP_eligible_comorbid_diabetes_t2 <-   DWMP_eligible_comorbid_diabetes_t2[order(comorbid_diabetes_t2)]

DWMP_eligible_comorbid_diabetes_t2 <- dplyr::left_join(DWMP_eligible_comorbid_diabetes_t2, N_comorbid_diabetes_t2)
DWMP_eligible_comorbid_diabetes_t2 <- DWMP_eligible_comorbid_diabetes_t2 %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_diabetes_t2 <- DWMP_eligible_comorbid_diabetes_t2 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_diabetes_t2", .before=1) %>%
  dplyr::rename(group = comorbid_diabetes_t2) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_diabetes_t2_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_diabetes_t2, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_diabetes_t2_chisq <- dplyr::mutate (comorbid_diabetes_t2_chisq, variable = "comorbid_diabetes_t2") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_diabetes_t2 <- DWMP_eligible_comorbid_diabetes_t2 %>%
  dplyr::left_join(comorbid_diabetes_t2_chisq, by = "variable")



#########  comorbid_asthma #######

#1.  count by comorbid_asthma
N_comorbid_asthma <- BMI_complete_categories_2021_DT[, .N, by="comorbid_asthma"]
DWMP_eligible_comorbid_asthma <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_asthma"] 
DWMP_eligible_comorbid_asthma <-   DWMP_eligible_comorbid_asthma[order(comorbid_asthma)]

DWMP_eligible_comorbid_asthma <- dplyr::left_join(DWMP_eligible_comorbid_asthma, N_comorbid_asthma)
DWMP_eligible_comorbid_asthma <- DWMP_eligible_comorbid_asthma %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_asthma <- DWMP_eligible_comorbid_asthma %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_asthma", .before=1) %>%
  dplyr::rename(group = comorbid_asthma) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_asthma_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_asthma, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_asthma_chisq <- dplyr::mutate (comorbid_asthma_chisq, variable = "comorbid_asthma") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_asthma <- DWMP_eligible_comorbid_asthma %>%
  dplyr::left_join(comorbid_asthma_chisq, by = "variable")


#########  comorbid_COPD #######

#1.  count by comorbid_COPD
N_comorbid_COPD <- BMI_complete_categories_2021_DT[, .N, by="comorbid_COPD"]
DWMP_eligible_comorbid_COPD <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_COPD"] 
DWMP_eligible_comorbid_COPD <-   DWMP_eligible_comorbid_COPD[order(comorbid_COPD)]

DWMP_eligible_comorbid_COPD <- dplyr::left_join(DWMP_eligible_comorbid_COPD, N_comorbid_COPD)
DWMP_eligible_comorbid_COPD <- DWMP_eligible_comorbid_COPD %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_COPD <- DWMP_eligible_comorbid_COPD %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_COPD", .before=1) %>%
  dplyr::rename(group = comorbid_COPD) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_COPD_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_COPD, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_COPD_chisq <- dplyr::mutate (comorbid_COPD_chisq, variable = "comorbid_COPD") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_COPD <- DWMP_eligible_comorbid_COPD %>%
  dplyr::left_join(comorbid_COPD_chisq, by = "variable")

#########  comorbid_stroke_and_TIA #######

#1.  count by comorbid_stroke_and_TIA
N_comorbid_stroke_and_TIA <- BMI_complete_categories_2021_DT[, .N, by="comorbid_stroke_and_TIA"]
DWMP_eligible_comorbid_stroke_and_TIA <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_stroke_and_TIA"] 
DWMP_eligible_comorbid_stroke_and_TIA <-   DWMP_eligible_comorbid_stroke_and_TIA[order(comorbid_stroke_and_TIA)]

DWMP_eligible_comorbid_stroke_and_TIA <- dplyr::left_join(DWMP_eligible_comorbid_stroke_and_TIA, N_comorbid_stroke_and_TIA)
DWMP_eligible_comorbid_stroke_and_TIA <- DWMP_eligible_comorbid_stroke_and_TIA %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_stroke_and_TIA <- DWMP_eligible_comorbid_stroke_and_TIA %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_stroke_and_TIA", .before=1) %>%
  dplyr::rename(group = comorbid_stroke_and_TIA) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_stroke_and_TIA_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_stroke_and_TIA, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_stroke_and_TIA_chisq <- dplyr::mutate (comorbid_stroke_and_TIA_chisq, variable = "comorbid_stroke_and_TIA") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_stroke_and_TIA <- DWMP_eligible_comorbid_stroke_and_TIA %>%
  dplyr::left_join(comorbid_stroke_and_TIA_chisq, by = "variable")

#########  comorbid_chronic_cardiac #######

#1.  count by comorbid_chronic_cardiac
N_comorbid_chronic_cardiac <- BMI_complete_categories_2021_DT[, .N, by="comorbid_chronic_cardiac"]
DWMP_eligible_comorbid_chronic_cardiac <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_chronic_cardiac"] 
DWMP_eligible_comorbid_chronic_cardiac <-   DWMP_eligible_comorbid_chronic_cardiac[order(comorbid_chronic_cardiac)]

DWMP_eligible_comorbid_chronic_cardiac <- dplyr::left_join(DWMP_eligible_comorbid_chronic_cardiac, N_comorbid_chronic_cardiac)
DWMP_eligible_comorbid_chronic_cardiac <- DWMP_eligible_comorbid_chronic_cardiac %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_chronic_cardiac <- DWMP_eligible_comorbid_chronic_cardiac %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_chronic_cardiac", .before=1) %>%
  dplyr::rename(group = comorbid_chronic_cardiac) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_chronic_cardiac_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_chronic_cardiac, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_chronic_cardiac_chisq <- dplyr::mutate (comorbid_chronic_cardiac_chisq, variable = "comorbid_chronic_cardiac") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_chronic_cardiac <- DWMP_eligible_comorbid_chronic_cardiac %>%
  dplyr::left_join(comorbid_chronic_cardiac_chisq, by = "variable")


#########  comorbid_hypertension #######

#1.  count by comorbid_hypertension
N_comorbid_hypertension <- BMI_complete_categories_2021_DT[, .N, by="comorbid_hypertension"]
DWMP_eligible_comorbid_hypertension <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_hypertension"] 
DWMP_eligible_comorbid_hypertension <-   DWMP_eligible_comorbid_hypertension[order(comorbid_hypertension)]

DWMP_eligible_comorbid_hypertension <- dplyr::left_join(DWMP_eligible_comorbid_hypertension, N_comorbid_hypertension)
DWMP_eligible_comorbid_hypertension <- DWMP_eligible_comorbid_hypertension %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_hypertension <- DWMP_eligible_comorbid_hypertension %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_hypertension", .before=1) %>%
  dplyr::rename(group = comorbid_hypertension) %>%
  dplyr::mutate(group = as.character(group)) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_hypertension_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_hypertension, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_hypertension_chisq <- dplyr::mutate (comorbid_hypertension_chisq, variable = "comorbid_hypertension") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_hypertension <- DWMP_eligible_comorbid_hypertension %>%
  dplyr::left_join(comorbid_hypertension_chisq, by = "variable")


#########  comorbid_all_cancer #######

#1.  count by comorbid_all_cancer
N_comorbid_all_cancer <- BMI_complete_categories_2021_DT[, .N, by="comorbid_all_cancer"]
DWMP_eligible_comorbid_all_cancer <- BMI_complete_categories_2021_DT[DWMP_eligible=="TRUE", .(n_DWMP_eligible= .N), by="comorbid_all_cancer"] 
DWMP_eligible_comorbid_all_cancer <-   DWMP_eligible_comorbid_all_cancer[order(comorbid_all_cancer)]

DWMP_eligible_comorbid_all_cancer <- dplyr::left_join(DWMP_eligible_comorbid_all_cancer, N_comorbid_all_cancer)
DWMP_eligible_comorbid_all_cancer <- DWMP_eligible_comorbid_all_cancer %>%
  dplyr::mutate(proportion=n_DWMP_eligible/N) 

# 2. calculate confidence interval of propotions
DWMP_eligible_comorbid_all_cancer <- DWMP_eligible_comorbid_all_cancer %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_all_cancer", .before=1) %>%
  dplyr::rename(group = comorbid_all_cancer) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_all_cancer_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(comorbid_all_cancer, DWMP_eligible) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_all_cancer_chisq <- dplyr::mutate (comorbid_all_cancer_chisq, variable = "comorbid_all_cancer") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
DWMP_eligible_comorbid_all_cancer <- DWMP_eligible_comorbid_all_cancer %>%
  dplyr::left_join(comorbid_all_cancer_chisq, by = "variable")





DWMP_eligible_table_2021 <- DWMP_eligible_table %>%
  bind_rows(DWMP_eligible_age_group) %>%
  bind_rows (DWMP_eligible_sex) %>%
  bind_rows (DWMP_eligible_region) %>%
  bind_rows (DWMP_eligible_imd) %>%
  bind_rows (DWMP_eligible_ethnic_no_miss) %>%
  bind_rows (DWMP_eligible_eth_group_16) %>%   
  bind_rows (DWMP_eligible_comorbid_hypertension) %>%
  bind_rows (DWMP_eligible_comorbid_diabetes_t1) %>%
  bind_rows (DWMP_eligible_comorbid_diabetes_t2) %>%
  bind_rows (DWMP_eligible_comorbid_asthma) %>%
  bind_rows (DWMP_eligible_comorbid_COPD) %>%
  bind_rows (DWMP_eligible_comorbid_learning_disability) %>%
  bind_rows (DWMP_eligible_comorbid_depression) %>%
  bind_rows (DWMP_eligible_comorbid_psychosis_schiz_bipolar) %>%    
  bind_rows (DWMP_eligible_comorbid_dementia) %>%
  bind_rows (DWMP_eligible_comorbid_stroke_and_TIA) %>%
  bind_rows (DWMP_eligible_comorbid_chronic_cardiac) %>%
  bind_rows (DWMP_eligible_comorbid_all_cancer)

DWMP_eligible_table








write.csv (DWMP_eligible_table_2021, here::here ("output/data","proportion_DWMP_eligible_2021.csv"))

write.csv (DWMP_eligible_table_2020, here::here ("output/data","proportion_DWMP_eligible_2020.csv"))

write.csv (DWMP_eligible_table_2019, here::here ("output/data","proportion_DWMP_eligible_2019.csv"))