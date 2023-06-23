library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(purrr)
library(broom)
library(data.table)
library(janitor)
library(gtsummary)

my_data <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_change_data.csv"))




my_data <- my_data  %>% 
  dplyr::mutate(eth_collapsed = case_when(
    eth_16_corrected ==   "White_British" ~ "white",
    eth_16_corrected ==  "White_Irish" ~ "white",
    eth_16_corrected ==  "Other_White"  ~ "white",
    eth_16_corrected == "White_Black_Carib" ~ "mixed",
    eth_16_corrected == "White_Black_African" ~ "mixed",
    eth_16_corrected == "White_Asian" ~ "mixed",
    eth_16_corrected == "Other_Mixed" ~ "mixed",
    eth_16_corrected == "Indian" ~ "south_asian",
    eth_16_corrected == "Pakistani" ~ "south_asian",
    eth_16_corrected == "Bangladeshi" ~ "south_asian",
    eth_16_corrected == "Other_Asian" ~ "chinese_other",
    eth_16_corrected == "Chinese" ~ "chinese_other",
    eth_16_corrected == "Caribbean" ~ "black",
    eth_16_corrected == "African" ~ "black",
    eth_16_corrected == "Other_Black" ~ "black",
    eth_16_corrected == "Other" ~ "chinese_other"
  ))  

print("check ethnicity collapsed correctly")
my_data %>%
  tabyl(eth_16_corrected, eth_collapsed)

my_data %>%
  tabyl(age_group_2) 

my_data <- my_data %>% 
  dplyr::mutate(age_collapsed = case_when(
    age_group_2 == "18-29" ~ "18-39", 
    age_group_2 ==  "30-39" ~ "18-39", 
    age_group_2 ==  "40-49" ~ "40-59", 
    age_group_2 == "50-59" ~ "40-59", 
    age_group_2 == "60-69" ~ "60- 79",
    age_group_2 ==  "70-79" ~ "60- 79",
  ))

print("check age collapsed correctly")
my_data %>%
  tabyl(age_group_2, age_collapsed)

# Collapse medication
my_data <- my_data %>% 
  dplyr::mutate(diabetes_med = case_when(
    insulin_meds == 1 ~ "insulin", 
    ((insulin_meds == 0) & (oad_meds == 1)) ~ "oad", 
    ((insulin_meds == 0) & (oad_meds == 0)) ~ "lifestyle"
  ))



t2d_data <- my_data %>%
  dplyr::filter(diabetes_t2 == TRUE)



population_demog_function2 <- function(my_var) {
  t2d_data %>%
    tabyl({{my_var}}, change_90th)%>% 
    dplyr::rename(not_90th = '0') %>% 
    dplyr::rename(top_decile = '1') %>% 
    dplyr::mutate(total = not_90th + top_decile) %>% 
    dplyr::select(-('not_90th')) %>% 
    dplyr::rename(group={{my_var}})
} 


bp_data <- my_data %>%
  dplyr::filter(hypertension == TRUE)


population_demog_function3 <- function(my_var) {
  bp_data %>%
    tabyl({{my_var}}, change_90th)%>% 
    dplyr::rename(not_90th = '0') %>% 
    dplyr::rename(top_decile = '1') %>% 
    dplyr::mutate(total = not_90th + top_decile) %>% 
    dplyr::select(-('not_90th')) %>% 
    dplyr::rename(group={{my_var}})
} 




sex <- population_demog_function2(sex) %>% 
  dplyr::mutate(variable = 'sex') %>% 
  dplyr::mutate(group = as.factor(group))


age_group_2 <- population_demog_function2(age_group_2) %>% 
  dplyr::mutate(variable = 'age_group_2')%>% 
  dplyr::mutate(group = as.factor(group))

region <- population_demog_function2(region) %>% 
  dplyr::mutate(variable = 'region')%>% 
  dplyr::mutate(group = as.factor(group))

imd <- population_demog_function2(imd) %>% 
  dplyr::mutate(variable = 'imd') %>% 
  dplyr::mutate(group = as.factor(group))


hypertension <- population_demog_function2(hypertension) %>% 
  dplyr::mutate(variable = 'hypertension') %>% 
  dplyr::mutate(group = as.factor(group)) 


diabetes_t1 <- population_demog_function2(diabetes_t1) %>% 
  dplyr::mutate(variable = 'diabetes_t1')%>% 
  dplyr::mutate(group = as.factor(group))

diabetes_t2 <- population_demog_function2(diabetes_t2) %>% 
  dplyr::mutate(variable = 'diabetes_t2')%>% 
  dplyr::mutate(group = as.factor(group))

learning_disability <- population_demog_function2(learning_disability) %>% 
  dplyr::mutate(variable = 'learning_disability')%>% 
  dplyr::mutate(group = as.factor(group))

depression <- population_demog_function2(depression) %>% 
  dplyr::mutate(variable = 'depression')%>% 
  dplyr::mutate(group = as.factor(group))

psychosis_schiz_bipolar <- population_demog_function2(psychosis_schiz_bipolar) %>% 
  dplyr::mutate(variable = 'psychosis_schiz_bipolar')%>% 
  dplyr::mutate(group = as.factor(group))

dementia <- population_demog_function2(dementia) %>% 
  dplyr::mutate(variable = 'dementia')%>% 
  dplyr::mutate(group = as.factor(group))

asthma <- population_demog_function2(asthma) %>% 
  dplyr::mutate(variable = 'asthma')%>% 
  dplyr::mutate(group = as.factor(group))


COPD <- population_demog_function2(COPD) %>% 
  dplyr::mutate(variable = 'COPD')%>% 
  dplyr::mutate(group = as.factor(group))

stroke_and_TIA <- population_demog_function2(stroke_and_TIA) %>% 
  dplyr::mutate(variable = 'stroke_and_TIA')%>% 
  dplyr::mutate(group = as.factor(group))

chronic_cardiac <- population_demog_function2(chronic_cardiac) %>% 
  dplyr::mutate(variable = 'chronic_cardiac')%>% 
  dplyr::mutate(group = as.factor(group))


all_cancer <- population_demog_function2(all_cancer) %>% 
  dplyr::mutate(variable = 'all_cancer')%>% 
  dplyr::mutate(group = as.factor(group))

diabetes_med <- population_demog_function2(diabetes_med) %>% 
  dplyr::mutate(variable = 'diabetes_med')%>% 
  dplyr::mutate(group = as.factor(group))



eth_16_corrected <- population_demog_function2(eth_collapsed) %>% 
  dplyr::mutate(variable = 'eth_collapsed')%>% 
  dplyr::mutate(group = as.factor(group))

precovid_bmi_category <- population_demog_function2(precovid_bmi_category) %>% 
  dplyr::mutate(variable = 'precovid_bmi_category')%>% 
  dplyr::mutate(group = as.factor(group))




t2d_demog <- sex %>% 
  bind_rows(
    age_group_2,
    eth_16_corrected,
    region, 
    imd, 
    hypertension,
    diabetes_t2,
    diabetes_t1,
    chronic_cardiac,
    learning_disability,
    depression,
    psychosis_schiz_bipolar,
    dementia,
    asthma,
    COPD,
    stroke_and_TIA,
    precovid_bmi_category, 
    diabetes_med)




t2d_demog <- t2d_demog %>% 
  dplyr::mutate(top_decile = plyr::round_any(t2d_demog$top_decile, 5)) %>%
  dplyr::mutate(total = plyr::round_any(t2d_demog$total, 5)) %>% 
  dplyr::mutate(population = "t2d") %>% 
  dplyr::select("population","variable", "group", "top_decile", "total")


####

sex <- population_demog_function3(sex) %>% 
  dplyr::mutate(variable = 'sex') %>% 
  dplyr::mutate(group = as.factor(group))


age_group_2 <- population_demog_function3(age_group_2) %>% 
  dplyr::mutate(variable = 'age_group_2')%>% 
  dplyr::mutate(group = as.factor(group))

region <- population_demog_function3(region) %>% 
  dplyr::mutate(variable = 'region')%>% 
  dplyr::mutate(group = as.factor(group))

imd <- population_demog_function3(imd) %>% 
  dplyr::mutate(variable = 'imd') %>% 
  dplyr::mutate(group = as.factor(group))


hypertension <- population_demog_function3(hypertension) %>% 
  dplyr::mutate(variable = 'hypertension') %>% 
  dplyr::mutate(group = as.factor(group)) 


diabetes_t1 <- population_demog_function3(diabetes_t1) %>% 
  dplyr::mutate(variable = 'diabetes_t1')%>% 
  dplyr::mutate(group = as.factor(group))

diabetes_t2 <- population_demog_function3(diabetes_t2) %>% 
  dplyr::mutate(variable = 'diabetes_t2')%>% 
  dplyr::mutate(group = as.factor(group))

learning_disability <- population_demog_function3(learning_disability) %>% 
  dplyr::mutate(variable = 'learning_disability')%>% 
  dplyr::mutate(group = as.factor(group))

depression <- population_demog_function3(depression) %>% 
  dplyr::mutate(variable = 'depression')%>% 
  dplyr::mutate(group = as.factor(group))

psychosis_schiz_bipolar <- population_demog_function3(psychosis_schiz_bipolar) %>% 
  dplyr::mutate(variable = 'psychosis_schiz_bipolar')%>% 
  dplyr::mutate(group = as.factor(group))

dementia <- population_demog_function3(dementia) %>% 
  dplyr::mutate(variable = 'dementia')%>% 
  dplyr::mutate(group = as.factor(group))

asthma <- population_demog_function3(asthma) %>% 
  dplyr::mutate(variable = 'asthma')%>% 
  dplyr::mutate(group = as.factor(group))


COPD <- population_demog_function3(COPD) %>% 
  dplyr::mutate(variable = 'COPD')%>% 
  dplyr::mutate(group = as.factor(group))

stroke_and_TIA <- population_demog_function3(stroke_and_TIA) %>% 
  dplyr::mutate(variable = 'stroke_and_TIA')%>% 
  dplyr::mutate(group = as.factor(group))

chronic_cardiac <- population_demog_function3(chronic_cardiac) %>% 
  dplyr::mutate(variable = 'chronic_cardiac')%>% 
  dplyr::mutate(group = as.factor(group))


all_cancer <- population_demog_function3(all_cancer) %>% 
  dplyr::mutate(variable = 'all_cancer')%>% 
  dplyr::mutate(group = as.factor(group))

diabetes_med <- population_demog_function3(diabetes_med) %>% 
  dplyr::mutate(variable = 'diabetes_med')%>% 
  dplyr::mutate(group = as.factor(group))



eth_16_corrected <- population_demog_function3(eth_collapsed) %>% 
  dplyr::mutate(variable = 'eth_collapsed')%>% 
  dplyr::mutate(group = as.factor(group))

precovid_bmi_category <- population_demog_function3(precovid_bmi_category) %>% 
  dplyr::mutate(variable = 'precovid_bmi_category')%>% 
  dplyr::mutate(group = as.factor(group))




bp_demog <- sex %>% 
  bind_rows(
    age_group_2,
    eth_16_corrected,
    region, 
    imd, 
    hypertension,
    diabetes_t2,
    diabetes_t1,
    chronic_cardiac,
    learning_disability,
    depression,
    psychosis_schiz_bipolar,
    dementia,
    asthma,
    COPD,
    stroke_and_TIA,
    precovid_bmi_category)


bp_demog <- bp_demog %>% 
  dplyr::mutate(top_decile = plyr::round_any(bp_demog$top_decile, 5)) %>%
  dplyr::mutate(total = plyr::round_any(bp_demog$total, 5))  %>%
  dplyr::mutate(population = "bp") %>% 
  dplyr::select("population","variable", "group", "top_decile", "total")


demog <- t2d_demog %>%
bind_rows(bp_demog)

write_csv (demog, here::here ("output/data","change_90th_DWMP_models_2_counts.csv"))



