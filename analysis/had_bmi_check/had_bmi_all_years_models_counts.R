#### Author: M Samuel
#### Date: 15th June
####  This script looks at the odds of having a BMI in different groups


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
library(skimr)

BMI_median_2 <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

BMI_complete_categories <- BMI_median_2 %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2019"|year == "2021" ) %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group_2 = as.factor(age_group_2)) %>%
  dplyr::mutate(age_group_2 = fct_relevel(age_group_2, "18-29", "30-39", "40-49", "50-59", "60-69", "70-79","80+")) %>% 
  dplyr::mutate(sex = fct_relevel(sex, 'F', "M"))

BMI_complete_categories %>% 
  tabyl(age_group_2)

BMI_DT <- as.data.table(BMI_complete_categories)

#########  AGE GROUPS #######

#1.  count by age_group_2
N_age_group_2 <- BMI_DT[, .N, by="age_group_2"]
had_bmi_age_group_2 <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="age_group_2"] 
had_bmi_age_group_2 <-   had_bmi_age_group_2[order(age_group_2)]

had_bmi_age_group_2 <- dplyr::left_join(had_bmi_age_group_2, N_age_group_2)
had_bmi_age_group_2 <- had_bmi_age_group_2 %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_age_group_2 <- had_bmi_age_group_2 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "age_group_2", .before=1) %>%
  dplyr::rename(group = age_group_2)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
age_group_2_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(age_group_2, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
age_group_2_chisq <- dplyr::mutate (age_group_2_chisq, variable = "age_group_2") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_bmi_age_group_2 <- had_bmi_age_group_2 %>%
  dplyr::left_join(age_group_2_chisq, by = "variable")



#########  sex #######

#1.  count by sex
N_sex <- BMI_DT[, .N, by="sex"]
had_bmi_sex <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="sex"] 
had_bmi_sex <-   had_bmi_sex[order(sex)]

had_bmi_sex <- dplyr::left_join(had_bmi_sex, N_sex)
had_bmi_sex <- had_bmi_sex %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_sex <- had_bmi_sex %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "sex", .before=1) %>%
  dplyr::rename(group = sex)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
sex_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(sex, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
sex_chisq <- dplyr::mutate (sex_chisq, variable = "sex") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_bmi_sex <- had_bmi_sex %>%
  dplyr::left_join(sex_chisq, by = "variable")


#########  eth_group_16 #######

#1.  count by eth_group_16
N_eth_group_16 <- BMI_DT[, .N, by="eth_group_16"]
had_bmi_eth_group_16 <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="eth_group_16"] 
had_bmi_eth_group_16 <-   had_bmi_eth_group_16[order(eth_group_16)]

had_bmi_eth_group_16 <- dplyr::left_join(had_bmi_eth_group_16, N_eth_group_16)
had_bmi_eth_group_16 <- had_bmi_eth_group_16 %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_eth_group_16 <- had_bmi_eth_group_16 %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "eth_group_16", .before=1) %>%
  dplyr::rename(group = eth_group_16)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
eth_group_16_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(eth_group_16, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
eth_group_16_chisq <- dplyr::mutate (eth_group_16_chisq, variable = "eth_group_16") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_bmi_eth_group_16 <- had_bmi_eth_group_16 %>%
  dplyr::left_join(eth_group_16_chisq, by = "variable")



#########  region #######

#1.  count by region
N_region <- BMI_DT[, .N, by="region"]
had_bmi_region <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="region"] 
had_bmi_region <-   had_bmi_region[order(region)]

had_bmi_region <- dplyr::left_join(had_bmi_region, N_region)
had_bmi_region <- had_bmi_region %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_region <- had_bmi_region %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "region", .before=1) %>%
  dplyr::rename(group = region)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
region_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(region, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
region_chisq <- dplyr::mutate (region_chisq, variable = "region") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_bmi_region <- had_bmi_region %>%
  dplyr::left_join(region_chisq, by = "variable")


#########  imd #######

#1.  count by imd
N_imd <- BMI_DT[, .N, by="imd"]
had_bmi_imd <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="imd"] 
had_bmi_imd <-   had_bmi_imd[order(imd)]

had_bmi_imd <- dplyr::left_join(had_bmi_imd, N_imd)
had_bmi_imd <- had_bmi_imd %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_imd <- had_bmi_imd %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "imd", .before=1) %>%
  dplyr::rename(group = imd)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
imd_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(imd, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
imd_chisq <- dplyr::mutate (imd_chisq, variable = "imd") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
had_bmi_imd <- had_bmi_imd %>%
  dplyr::left_join(imd_chisq, by = "variable")



had_bmi_all <- had_bmi_age_group_2 %>% 
  bind_rows(had_bmi_sex) %>% 
  bind_rows(had_bmi_imd) %>% 
  bind_rows(had_bmi_eth_group_16) %>% 
  bind_rows(had_bmi_region)

had_bmi_all <- had_bmi_all %>% 
  dplyr::mutate(N = plyr::round_any(had_bmi_all$N, 5))  %>% 
  dplyr::mutate(n_had_bmi = plyr::round_any(had_bmi_all$n_had_bmi, 5))  



## proportions in each group
year <- BMI_complete_categories %>% 
  tabyl(year) %>% 
  adorn_totals() %>% 
  dplyr::rename(group = year)

sex <- BMI_complete_categories %>% 
  tabyl(sex) %>% 
  adorn_totals() %>%
  dplyr::rename(group = sex)

age_group_2 <- BMI_complete_categories %>% 
  tabyl(age_group_2) %>% 
  adorn_totals() %>% 
  dplyr::rename(group = age_group_2)

ethnicity  <- BMI_complete_categories %>% 
  tabyl(eth_group_16) %>% 
  adorn_totals() %>% 
  dplyr::rename(group = eth_group_16)

imd <- BMI_complete_categories %>% 
  tabyl(imd) %>% 
  adorn_totals() %>% 
  dplyr::rename(group = imd)

region <-BMI_complete_categories %>% 
  tabyl(region) %>% 
  adorn_totals() %>% 
  dplyr::rename(group = region)

demographics <- year %>% 
  bind_rows(age_group_2) %>%
  bind_rows(sex) %>%
  bind_rows(imd) %>%
  bind_rows(ethnicity) %>%
  bind_rows(region) %>% 
  dplyr::mutate(across(where(is.numeric), round, 4))


## save outputs

#1.  counts of numbers in each group
had_bmi_all
write.csv (had_bmi_all, here::here ("output/data","had_bmi_2019_21_models_1_counts.csv"))


#2. demographics of the groups
demographics 
write.csv (demographics, here::here ("output/data","had_bmi_2019_21_models_1_demographics.csv"))

