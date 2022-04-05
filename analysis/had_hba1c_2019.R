
#### Author: M Samuel
#### Date: 24th March 2022
#### This script calculates proportions of people having their hba1c checked differences between groups using chi squared.
## Analysis is limited to patients with a T2DM code
## Analysis excludes patients with T1DM as more likely to have HbA1c assessed in secondary care

## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(purrr)
library(broom)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)

## Read in files  >>> Change PATH!!

# check working directory:  getwd()

hba1c_summary <- read_feather (here::here ("output/data", "hba1c_2019_summary.feather"))

## limit analysis to type 2 diabetics
hba1c_summary <- dplyr::filter(hba1c_summary, (diabetes_t2 == 'TRUE' & diabetes_t1=='FALSE'))



hba1c_summary <-hba1c_summary %>%
  dplyr::mutate(
    across(
      .cols = c(learning_disability,depression, dementia,psychosis_schiz_bipolar, diabetes_type, diabetes_t1, diabetes_t2, asthma, COPD, stroke_and_TIA, chronic_cardiac, hypertension, all_cancer), 
      .names = "comorbid_{col}"
    )
  )



hba1c_summary_2019_DT <- data.table(hba1c_summary)




N_had_hba1c <- hba1c_summary_2019_DT[, .N, ]
had_hba1c_table<- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N),] 

had_hba1c_table <- dplyr::mutate (had_hba1c_table, N = N_had_hba1c) %>%
  dplyr::mutate(proportion=n_had_hba1c/N) %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(group = "all", .before=1) %>%
  dplyr::mutate(variable = "all", .before=1) 



##  Develop count table and chi squared test of difference between population long hand for each variable to reduce memory


#########  AGE GROUPS #######

#1.  count by age_group
N_age_group <- hba1c_summary_2019_DT[, .N, by="age_group"]
had_hba1c_age_group <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="age_group"] 
had_hba1c_age_group <-   had_hba1c_age_group[order(age_group)]

had_hba1c_age_group <- dplyr::left_join(had_hba1c_age_group, N_age_group)
had_hba1c_age_group <- had_hba1c_age_group %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_age_group <- had_hba1c_age_group %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "age_group", .before=1) %>%
  dplyr::rename(group = age_group)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
age_group_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(age_group, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
age_group_chisq <- dplyr::mutate (age_group_chisq, variable = "age_group") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_age_group <- had_hba1c_age_group %>%
  dplyr::left_join(age_group_chisq, by = "variable")





###### SEX

#1.  count by sex
N_sex <- hba1c_summary_2019_DT[, .N, by="sex"]
had_hba1c_sex <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="sex"] 
had_hba1c_sex <-   had_hba1c_sex[order(sex)]

had_hba1c_sex <- dplyr::left_join(had_hba1c_sex, N_sex)
had_hba1c_sex <- had_hba1c_sex %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_sex <- had_hba1c_sex %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "sex", .before=1) %>%
  dplyr::rename(group = sex)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
sex_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(sex, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
sex_chisq <- dplyr::mutate (sex_chisq, variable = "sex") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_sex <- had_hba1c_sex %>%
  dplyr::left_join(sex_chisq, by = "variable")


#########  region #######

#1.  count by region
N_region <- hba1c_summary_2019_DT[, .N, by="region"]
had_hba1c_region <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="region"] 
had_hba1c_region <-   had_hba1c_region[order(region)]

had_hba1c_region <- dplyr::left_join(had_hba1c_region, N_region)
had_hba1c_region <- had_hba1c_region %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_region <- had_hba1c_region %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "region", .before=1) %>%
  dplyr::rename(group = region)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
region_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(region, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
region_chisq <- dplyr::mutate (region_chisq, variable = "region") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_region <- had_hba1c_region %>%
  dplyr::left_join(region_chisq, by = "variable")

#########  imd #######

#1.  count by imd
N_imd <- hba1c_summary_2019_DT[, .N, by="imd"]
had_hba1c_imd <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="imd"] 
had_hba1c_imd <-   had_hba1c_imd[order(imd)]

had_hba1c_imd <- dplyr::left_join(had_hba1c_imd, N_imd)
had_hba1c_imd <- had_hba1c_imd %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_imd <- had_hba1c_imd %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "imd", .before=1) %>%
  dplyr::rename(group = imd)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
imd_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(imd, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
imd_chisq <- dplyr::mutate (imd_chisq, variable = "imd") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_imd <- had_hba1c_imd %>%
  dplyr::left_join(imd_chisq, by = "variable")

#########  ethnic_no_miss #######

#1.  count by ethnic_no_miss
N_ethnic_no_miss <- hba1c_summary_2019_DT[, .N, by="ethnic_no_miss"]
had_hba1c_ethnic_no_miss <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="ethnic_no_miss"] 
had_hba1c_ethnic_no_miss <-   had_hba1c_ethnic_no_miss[order(ethnic_no_miss)]

had_hba1c_ethnic_no_miss <- dplyr::left_join(had_hba1c_ethnic_no_miss, N_ethnic_no_miss)
had_hba1c_ethnic_no_miss <- had_hba1c_ethnic_no_miss %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_ethnic_no_miss <- had_hba1c_ethnic_no_miss %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "ethnic_no_miss", .before=1) %>%
  dplyr::rename(group = ethnic_no_miss)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
ethnic_no_miss_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(ethnic_no_miss, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
ethnic_no_miss_chisq <- dplyr::mutate (ethnic_no_miss_chisq, variable = "ethnic_no_miss") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_ethnic_no_miss <- had_hba1c_ethnic_no_miss %>%
  dplyr::left_join(ethnic_no_miss_chisq, by = "variable")

#########  eth_group_16 #######

#1.  count by eth_group_16
N_eth_group_16 <- hba1c_summary_2019_DT[, .N, by="eth_group_16"]
had_hba1c_eth_group_16 <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="eth_group_16"] 
had_hba1c_eth_group_16 <-   had_hba1c_eth_group_16[order(eth_group_16)]

had_hba1c_eth_group_16 <- dplyr::left_join(had_hba1c_eth_group_16, N_eth_group_16)
had_hba1c_eth_group_16 <- had_hba1c_eth_group_16 %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_eth_group_16 <- had_hba1c_eth_group_16 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "eth_group_16", .before=1) %>%
  dplyr::rename(group = eth_group_16)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
eth_group_16_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(eth_group_16, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
eth_group_16_chisq <- dplyr::mutate (eth_group_16_chisq, variable = "eth_group_16") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_eth_group_16 <- had_hba1c_eth_group_16 %>%
  dplyr::left_join(eth_group_16_chisq, by = "variable")

########  comorbid_learning_disability #######

#1.  count by comorbid_learning_disability
N_comorbid_learning_disability <- hba1c_summary_2019_DT[, .N, by="comorbid_learning_disability"]
had_hba1c_comorbid_learning_disability <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_learning_disability"] 
had_hba1c_comorbid_learning_disability <-   had_hba1c_comorbid_learning_disability[order(comorbid_learning_disability)]

had_hba1c_comorbid_learning_disability <- dplyr::left_join(had_hba1c_comorbid_learning_disability, N_comorbid_learning_disability)
had_hba1c_comorbid_learning_disability <- had_hba1c_comorbid_learning_disability %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_learning_disability <- had_hba1c_comorbid_learning_disability %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_learning_disability", .before=1) %>%
  dplyr::rename(group = comorbid_learning_disability) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_learning_disability_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_learning_disability, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_learning_disability_chisq <- dplyr::mutate (comorbid_learning_disability_chisq, variable = "comorbid_learning_disability") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_learning_disability <- had_hba1c_comorbid_learning_disability %>%
  dplyr::left_join(comorbid_learning_disability_chisq, by = "variable")



#########  comorbid_depression #######

#1.  count by comorbid_depression
N_comorbid_depression <- hba1c_summary_2019_DT[, .N, by="comorbid_depression"]
had_hba1c_comorbid_depression <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_depression"] 
had_hba1c_comorbid_depression <-   had_hba1c_comorbid_depression[order(comorbid_depression)]

had_hba1c_comorbid_depression <- dplyr::left_join(had_hba1c_comorbid_depression, N_comorbid_depression)
had_hba1c_comorbid_depression <- had_hba1c_comorbid_depression %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_depression <- had_hba1c_comorbid_depression %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_depression", .before=1) %>%
  dplyr::rename(group = comorbid_depression) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_depression_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_depression, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_depression_chisq <- dplyr::mutate (comorbid_depression_chisq, variable = "comorbid_depression") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_depression <- had_hba1c_comorbid_depression %>%
  dplyr::left_join(comorbid_depression_chisq, by = "variable")


#########  comorbid_dementia #######

#1.  count by comorbid_dementia
N_comorbid_dementia <- hba1c_summary_2019_DT[, .N, by="comorbid_dementia"]
had_hba1c_comorbid_dementia <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_dementia"] 
had_hba1c_comorbid_dementia <-   had_hba1c_comorbid_dementia[order(comorbid_dementia)]

had_hba1c_comorbid_dementia <- dplyr::left_join(had_hba1c_comorbid_dementia, N_comorbid_dementia)
had_hba1c_comorbid_dementia <- had_hba1c_comorbid_dementia %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_dementia <- had_hba1c_comorbid_dementia %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_dementia", .before=1) %>%
  dplyr::rename(group = comorbid_dementia) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_dementia_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_dementia, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_dementia_chisq <- dplyr::mutate (comorbid_dementia_chisq, variable = "comorbid_dementia") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_dementia <- had_hba1c_comorbid_dementia %>%
  dplyr::left_join(comorbid_dementia_chisq, by = "variable")


#########  comorbid_psychosis_schiz_bipolar #######

#1.  count by comorbid_psychosis_schiz_bipolar
N_comorbid_psychosis_schiz_bipolar <- hba1c_summary_2019_DT[, .N, by="comorbid_psychosis_schiz_bipolar"]
had_hba1c_comorbid_psychosis_schiz_bipolar <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_psychosis_schiz_bipolar"] 
had_hba1c_comorbid_psychosis_schiz_bipolar <-   had_hba1c_comorbid_psychosis_schiz_bipolar[order(comorbid_psychosis_schiz_bipolar)]

had_hba1c_comorbid_psychosis_schiz_bipolar <- dplyr::left_join(had_hba1c_comorbid_psychosis_schiz_bipolar, N_comorbid_psychosis_schiz_bipolar)
had_hba1c_comorbid_psychosis_schiz_bipolar <- had_hba1c_comorbid_psychosis_schiz_bipolar %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_psychosis_schiz_bipolar <- had_hba1c_comorbid_psychosis_schiz_bipolar %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_psychosis_schiz_bipolar", .before=1) %>%
  dplyr::rename(group = comorbid_psychosis_schiz_bipolar) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_psychosis_schiz_bipolar_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_psychosis_schiz_bipolar, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_psychosis_schiz_bipolar_chisq <- dplyr::mutate (comorbid_psychosis_schiz_bipolar_chisq, variable = "comorbid_psychosis_schiz_bipolar") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_psychosis_schiz_bipolar <- had_hba1c_comorbid_psychosis_schiz_bipolar %>%
  dplyr::left_join(comorbid_psychosis_schiz_bipolar_chisq, by = "variable")

########################


#########  comorbid_asthma #######

#1.  count by comorbid_asthma
N_comorbid_asthma <- hba1c_summary_2019_DT[, .N, by="comorbid_asthma"]
had_hba1c_comorbid_asthma <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_asthma"] 
had_hba1c_comorbid_asthma <-   had_hba1c_comorbid_asthma[order(comorbid_asthma)]

had_hba1c_comorbid_asthma <- dplyr::left_join(had_hba1c_comorbid_asthma, N_comorbid_asthma)
had_hba1c_comorbid_asthma <- had_hba1c_comorbid_asthma %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_asthma <- had_hba1c_comorbid_asthma %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_asthma", .before=1) %>%
  dplyr::rename(group = comorbid_asthma) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_asthma_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_asthma, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_asthma_chisq <- dplyr::mutate (comorbid_asthma_chisq, variable = "comorbid_asthma") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_asthma <- had_hba1c_comorbid_asthma %>%
  dplyr::left_join(comorbid_asthma_chisq, by = "variable")


#########  comorbid_COPD #######

#1.  count by comorbid_COPD
N_comorbid_COPD <- hba1c_summary_2019_DT[, .N, by="comorbid_COPD"]
had_hba1c_comorbid_COPD <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_COPD"] 
had_hba1c_comorbid_COPD <-   had_hba1c_comorbid_COPD[order(comorbid_COPD)]

had_hba1c_comorbid_COPD <- dplyr::left_join(had_hba1c_comorbid_COPD, N_comorbid_COPD)
had_hba1c_comorbid_COPD <- had_hba1c_comorbid_COPD %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_COPD <- had_hba1c_comorbid_COPD %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_COPD", .before=1) %>%
  dplyr::rename(group = comorbid_COPD) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_COPD_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_COPD, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_COPD_chisq <- dplyr::mutate (comorbid_COPD_chisq, variable = "comorbid_COPD") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_COPD <- had_hba1c_comorbid_COPD %>%
  dplyr::left_join(comorbid_COPD_chisq, by = "variable")

#########  comorbid_stroke_and_TIA #######

#1.  count by comorbid_stroke_and_TIA
N_comorbid_stroke_and_TIA <- hba1c_summary_2019_DT[, .N, by="comorbid_stroke_and_TIA"]
had_hba1c_comorbid_stroke_and_TIA <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_stroke_and_TIA"] 
had_hba1c_comorbid_stroke_and_TIA <-   had_hba1c_comorbid_stroke_and_TIA[order(comorbid_stroke_and_TIA)]

had_hba1c_comorbid_stroke_and_TIA <- dplyr::left_join(had_hba1c_comorbid_stroke_and_TIA, N_comorbid_stroke_and_TIA)
had_hba1c_comorbid_stroke_and_TIA <- had_hba1c_comorbid_stroke_and_TIA %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_stroke_and_TIA <- had_hba1c_comorbid_stroke_and_TIA %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_stroke_and_TIA", .before=1) %>%
  dplyr::rename(group = comorbid_stroke_and_TIA) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_stroke_and_TIA_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_stroke_and_TIA, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_stroke_and_TIA_chisq <- dplyr::mutate (comorbid_stroke_and_TIA_chisq, variable = "comorbid_stroke_and_TIA") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_stroke_and_TIA <- had_hba1c_comorbid_stroke_and_TIA %>%
  dplyr::left_join(comorbid_stroke_and_TIA_chisq, by = "variable")

#########  comorbid_chronic_cardiac #######

#1.  count by comorbid_chronic_cardiac
N_comorbid_chronic_cardiac <- hba1c_summary_2019_DT[, .N, by="comorbid_chronic_cardiac"]
had_hba1c_comorbid_chronic_cardiac <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_chronic_cardiac"] 
had_hba1c_comorbid_chronic_cardiac <-   had_hba1c_comorbid_chronic_cardiac[order(comorbid_chronic_cardiac)]

had_hba1c_comorbid_chronic_cardiac <- dplyr::left_join(had_hba1c_comorbid_chronic_cardiac, N_comorbid_chronic_cardiac)
had_hba1c_comorbid_chronic_cardiac <- had_hba1c_comorbid_chronic_cardiac %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_chronic_cardiac <- had_hba1c_comorbid_chronic_cardiac %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_chronic_cardiac", .before=1) %>%
  dplyr::rename(group = comorbid_chronic_cardiac) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_chronic_cardiac_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_chronic_cardiac, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_chronic_cardiac_chisq <- dplyr::mutate (comorbid_chronic_cardiac_chisq, variable = "comorbid_chronic_cardiac") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_chronic_cardiac <- had_hba1c_comorbid_chronic_cardiac %>%
  dplyr::left_join(comorbid_chronic_cardiac_chisq, by = "variable")


#########  comorbid_hypertension #######

#1.  count by comorbid_hypertension
N_comorbid_hypertension <- hba1c_summary_2019_DT[, .N, by="comorbid_hypertension"]
had_hba1c_comorbid_hypertension <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_hypertension"] 
had_hba1c_comorbid_hypertension <-   had_hba1c_comorbid_hypertension[order(comorbid_hypertension)]

had_hba1c_comorbid_hypertension <- dplyr::left_join(had_hba1c_comorbid_hypertension, N_comorbid_hypertension)
had_hba1c_comorbid_hypertension <- had_hba1c_comorbid_hypertension %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_hypertension <- had_hba1c_comorbid_hypertension %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_hypertension", .before=1) %>%
  dplyr::rename(group = comorbid_hypertension) %>%
  dplyr::mutate(group = as.character(group)) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_hypertension_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_hypertension, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_hypertension_chisq <- dplyr::mutate (comorbid_hypertension_chisq, variable = "comorbid_hypertension") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_hypertension <- had_hba1c_comorbid_hypertension %>%
  dplyr::left_join(comorbid_hypertension_chisq, by = "variable")


#########  comorbid_all_cancer #######

#1.  count by comorbid_all_cancer
N_comorbid_all_cancer <- hba1c_summary_2019_DT[, .N, by="comorbid_all_cancer"]
had_hba1c_comorbid_all_cancer <- hba1c_summary_2019_DT[had_hba1c=="TRUE", .(n_had_hba1c= .N), by="comorbid_all_cancer"] 
had_hba1c_comorbid_all_cancer <-   had_hba1c_comorbid_all_cancer[order(comorbid_all_cancer)]

had_hba1c_comorbid_all_cancer <- dplyr::left_join(had_hba1c_comorbid_all_cancer, N_comorbid_all_cancer)
had_hba1c_comorbid_all_cancer <- had_hba1c_comorbid_all_cancer %>%
  dplyr::mutate(proportion=n_had_hba1c/N) 

# 2. calculate confidence interval of propotions
had_hba1c_comorbid_all_cancer <- had_hba1c_comorbid_all_cancer %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "comorbid_all_cancer", .before=1) %>%
  dplyr::rename(group = comorbid_all_cancer) %>%
  dplyr::mutate(group = as.character(group))

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
comorbid_all_cancer_chisq <- as_tibble(hba1c_summary) %>%
  tabyl(comorbid_all_cancer, had_hba1c) %>%
  select(-1) %>% 
  chisq_test() 
comorbid_all_cancer_chisq <- dplyr::mutate (comorbid_all_cancer_chisq, variable = "comorbid_all_cancer") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_hba1c_comorbid_all_cancer <- had_hba1c_comorbid_all_cancer %>%
  dplyr::left_join(comorbid_all_cancer_chisq, by = "variable")





had_hba1c_table <- had_hba1c_table %>%
  bind_rows(had_hba1c_age_group) %>%
  bind_rows (had_hba1c_sex) %>%
  bind_rows (had_hba1c_region) %>%
  bind_rows (had_hba1c_imd) %>%
  bind_rows (had_hba1c_ethnic_no_miss) %>%
  bind_rows (had_hba1c_eth_group_16) %>%   
  bind_rows (had_hba1c_comorbid_hypertension) %>%
  bind_rows (had_hba1c_comorbid_asthma) %>%
  bind_rows (had_hba1c_comorbid_COPD) %>%
  bind_rows (had_hba1c_comorbid_learning_disability) %>%
  bind_rows (had_hba1c_comorbid_depression) %>%
  bind_rows (had_hba1c_comorbid_psychosis_schiz_bipolar) %>%    
  bind_rows (had_hba1c_comorbid_dementia) %>%
  bind_rows (had_hba1c_comorbid_stroke_and_TIA) %>%
  bind_rows (had_hba1c_comorbid_chronic_cardiac) %>%
  bind_rows (had_hba1c_comorbid_all_cancer)

had_hba1c_table

write.csv (had_hba1c_table, here::here ("output/data","proportion_had_hba1c_2019.csv"))
