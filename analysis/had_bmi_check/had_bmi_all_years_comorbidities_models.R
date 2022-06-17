#### Author: M Samuel
#### Date: 15th June
####  This script looks at the odds of having a BMI measures.  Stratified by each of the comorbidities below.


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



model_T1DM <- BMI_complete_categories %>%
  dplyr::filter(comorbid_diabetes_t1 == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "diabetes_t1")

model_T1DM_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_diabetes_t1 == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "diabetes_t1")


model_T2DM <- BMI_complete_categories %>%
  dplyr::filter(comorbid_diabetes_t2 == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "diabetes_t2")

model_T2DM_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_diabetes_t2 == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "diabetes_t2")


model_hypertension <- BMI_complete_categories %>%
  dplyr::filter(comorbid_hypertension == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "hypertension")

model_hypertension_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_hypertension == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "hypertension")



model_learning_disability <- BMI_complete_categories %>%
  dplyr::filter(comorbid_learning_disability == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "learning_disability")

model_learning_disability_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_learning_disability == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "learning_disability")





model_depression <- BMI_complete_categories %>%
  dplyr::filter(comorbid_depression == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "depression")


model_depression_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_depression == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "depression")



model_psychosis_schiz_bipolar <- BMI_complete_categories %>%
  dplyr::filter(comorbid_psychosis_schiz_bipolar == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "psychosis_schiz_bipolar")


model_psychosis_schiz_bipolar_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_psychosis_schiz_bipolar == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "psychosis_schiz_bipolar")


model_asthma <- BMI_complete_categories %>%
  dplyr::filter(comorbid_asthma == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "asthma")


model_asthma_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_asthma == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "asthma")




model_COPD <- BMI_complete_categories %>%
  dplyr::filter(comorbid_COPD == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "COPD")


model_COPD_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_COPD == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "COPD")


model_stroke_and_TIA <- BMI_complete_categories %>%
  dplyr::filter(comorbid_stroke_and_TIA == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "stroke_and_TIA")


model_stroke_and_TIA_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_stroke_and_TIA == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "stroke_and_TIA")




model_dementia <- BMI_complete_categories %>%
  dplyr::filter(comorbid_dementia == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "dementia")


model_dementia_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_dementia == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "dementia")




model_chronic_cardiac <- BMI_complete_categories %>%
  dplyr::filter(comorbid_chronic_cardiac == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "chronic_cardiac")


model_chronic_cardiac_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_chronic_cardiac == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "chronic_cardiac")


model_all_cancer <- BMI_complete_categories %>%
  dplyr::filter(comorbid_all_cancer == TRUE) %>% 
  glm(had_bmi ~ year, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year") %>%
  dplyr::mutate(group = "all_cancer")


model_all_cancer_2 <- BMI_complete_categories %>%
  dplyr::filter(comorbid_all_cancer == TRUE) %>% 
  glm(had_bmi ~ year + age_group_2, data=., family=binomial) %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  dplyr::mutate(predictor = "year_age") %>%
  dplyr::mutate(group = "all_cancer")


models_all <- model_T2DM %>%
  bind_rows(model_T2DM_2) %>%
  bind_rows(model_T1DM) %>% 
  bind_rows(model_T1DM_2) %>% 
  bind_rows(model_hypertension) %>% 
  bind_rows(model_hypertension_2) %>% 
  bind_rows(model_chronic_cardiac) %>% 
  bind_rows(model_chronic_cardiac_2) %>% 
  bind_rows(model_learning_disability) %>% 
  bind_rows(model_learning_disability_2) %>% 
  bind_rows(model_psychosis_schiz_bipolar) %>% 
  bind_rows(model_psychosis_schiz_bipolar_2) %>% 
  bind_rows(model_depression) %>% 
  bind_rows(model_depression_2) %>% 
  bind_rows(model_asthma) %>% 
  bind_rows(model_asthma_2) %>% 
  bind_rows(model_COPD) %>% 
  bind_rows(model_COPD_2) %>% 
  bind_rows(model_stroke_and_TIA) %>%
  bind_rows(model_stroke_and_TIA_2) %>% 
  bind_rows(model_dementia) %>% 
  bind_rows(model_dementia_2) %>% 
  bind_rows(model_all_cancer) %>% 
  bind_rows(model_all_cancer_2)


models_all <- models_all  %>% 
dplyr::mutate(across(where(is.numeric), round, 5)) 

    
  
################  COUNTS

## counts for regression models
BMI_DT <- as.data.table(BMI_complete_categories)

#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_all <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "all")




## COUNTS  T2DM
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_diabetes_t2 == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_T2DM <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "T2DM")




## COUNTS  T1DM
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_diabetes_t1 == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_T1DM <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "T1DM")




## COUNTS  hypertension
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_hypertension == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_hypertension <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "hypertension")



## COUNTS  chronic_cardiac
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_chronic_cardiac == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_chronic_cardiac <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "chronic_cardiac")



## COUNTS  learning_disability
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_learning_disability == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_learning_disability <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "LD")




## COUNTS  psychosis_schiz_bipolar
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_psychosis_schiz_bipolar == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_psychosis_schiz_bipolar <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "SMI")





## COUNTS  depression
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_depression == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_depression <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "depression")



## COUNTS  asthma
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_asthma == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_asthma <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "asthma")




## COUNTS  COPD
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_COPD == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_COPD <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "COPD")



## COUNTS  dementia
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_dementia == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_dementia <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "dementia")




## COUNTS  stroke_and_TIA
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_stroke_and_TIA == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_stroke_and_TIA <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "stroke_TIA")




## COUNTS  all_cancer
BMI_DT <- as.data.table(BMI_complete_categories) %>%
  dplyr::filter(comorbid_all_cancer == "TRUE")



#1.  count by year
N_year <- BMI_DT[, .N, by="year"]
had_bmi_year <- BMI_DT[had_bmi=="TRUE", .(n_had_bmi= .N), by="year"] 
had_bmi_year <-   had_bmi_year[order(year)]

had_bmi_year <- dplyr::left_join(had_bmi_year, N_year)
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(proportion=n_had_bmi/N) 

# 2. calculate confidence interval of propotions
had_bmi_year <- had_bmi_year %>%
  dplyr::mutate(lower_limit = (proportion - ((proportion*(1-proportion))/N*1.96))) %>%   # confidence interval of proporion
  dplyr::mutate(upper_limit = (proportion + ((proportion*(1-proportion))/N*1.96))) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>%
  dplyr::mutate(variable = "year", .before=1) %>%
  dplyr::rename(group = year)

#.... confidence interval proportion: (((proportion(1-proportion)/N))^0.5) * 1.96

# 3. chisq test
year_chisq <- as_tibble(BMI_complete_categories) %>%
  tabyl(year, had_bmi) %>%
  select(-1) %>% 
  chisq_test() 
year_chisq <- dplyr::mutate (year_chisq, variable = "year") %>%
  dplyr::select("variable", "p", "method")

# 4.  Final table 
counts_all_cancer <- had_bmi_year %>%
  dplyr::left_join(year_chisq, by = "variable") %>% 
  dplyr::mutate(variable = "cancer")


counts_all <- counts_all %>% 
  bind_rows(counts_T2DM) %>% 
  bind_rows(counts_T1DM) %>% 
  bind_rows(counts_hypertension) %>% 
  bind_rows(counts_chronic_cardiac) %>% 
  bind_rows(counts_learning_disability) %>% 
  bind_rows(counts_psychosis_schiz_bipolar) %>%
  bind_rows(counts_depression) %>% 
  bind_rows(counts_asthma) %>%
  bind_rows(counts_COPD) %>% 
  bind_rows(counts_dementia) %>% 
  bind_rows(counts_stroke_and_TIA)%>%
  bind_rows(counts_all_cancer)
  
counts_all <- counts_all %>% 
  dplyr::mutate(N = plyr::round_any(counts_all$N, 5))  %>% 
  dplyr::mutate(n_had_bmi = plyr::round_any(counts_all$n_had_bmi, 5)) 

write.csv (models_all, here::here ("output/data","had_bmi_2019_21_comorbidity_models.csv"))

write.csv (counts_all, here::here ("output/data","had_bmi_2019_21_comorbidity_models_counts.csv"))
