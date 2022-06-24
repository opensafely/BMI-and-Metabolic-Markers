
#### Author: M Samuel
#### Date: 24th March 2022
#### This script calculates median BMI and (univariate) evidence of differences between groups 


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

BMI_complete_categories <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))

BMI_complete_categories <- BMI_complete_categories %>% 
  dplyr::ungroup() %>%
  dplyr::filter (year == "2020") %>%
  dplyr::mutate (imd = as.factor(imd)) %>%
  dplyr::mutate (imd = fct_relevel(imd, "1", "2", "3", "4", "5")) %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", "40-65", "65-80", "80+"))


BMI_complete_categories$age_group_2 <- factor(BMI_complete_categories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))



BMI_complete_categories <- BMI_complete_categories %>% 
  replace_na(list(precovid_obese_flag=FALSE))




BMI_complete_categories %>%
  tabyl(imd)

length(BMI_complete_categories$median_bmi)
sum(is.na(BMI_complete_categories$median_bmi))
N_median_all = (length(BMI_complete_categories$median_bmi) - sum(is.na(BMI_complete_categories$median_bmi)))
# median_all = median(BMI_complete_categories$median_bmi, na.rm = TRUE)
quantile_all = quantile(BMI_complete_categories$median_bmi, probs = c(0.25,0.5,0.75), na.rm = TRUE) # returns a list
N_population = length(BMI_complete_categories$median_bmi)

N_population

median_all <-  data.frame(t(sapply(quantile_all,c))) %>%  # change list of qauntiles to a data frame
  dplyr::mutate(variable = "all") %>%
  dplyr::mutate(group = "all") %>%
  dplyr::mutate(N = N_median_all) %>%
  dplyr::mutate(N_population = N_population) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

median_all <- median_all %>%
  dplyr::mutate( "25th" = X25.) %>%
  dplyr::mutate("median" = X50. ) %>%
  dplyr::mutate ("75th" = X75.) %>%
  dplyr::mutate(group = "all") %>%
  dplyr::mutate(variable = "all") %>%
  dplyr::select('variable', 'group', 'N', 'N_population', '25th', 'median', '75th') 


BMI_complete_categories %>%
  tabyl(imd)

## code to calcualte median_bmi

BMI_complete_categories_DT <- data.table(BMI_complete_categories)


## Skewed population.  BMI medians by subgroup
## Age Group

N_median_age_group <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="age_group"]
n_median_age_group <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="age_group"]
quartile1_median_age_group <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="age_group"]
median_median_age_group <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="age_group"]
quartile3_median_age_group <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="age_group"]

KWRS_age_group <- kruskal.test(age_group ~ median_bmi, BMI_complete_categories)
significance_age_group <- data.frame(t(sapply(KWRS_age_group, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_age_group <- significance_age_group$p.value

median_age_group <- N_median_age_group %>%
  dplyr::left_join(n_median_age_group, by = "age_group") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_age_group, by = "age_group") %>%
  dplyr::left_join(median_median_age_group, by = "age_group") %>%
  dplyr::left_join(quartile3_median_age_group, by = "age_group") %>%
  dplyr::select(age_group, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_age_group) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'age_group') %>%
  dplyr::mutate(variable="age_group", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::arrange(group)

BMI_complete_categories %>%
  tabyl(imd)
## Skewed population.  BMI medians by subgroup
## Age Group 2

N_median_age_group_2 <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="age_group_2"]
n_median_age_group_2 <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="age_group_2"]
quartile1_median_age_group_2 <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="age_group_2"]
median_median_age_group_2 <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="age_group_2"]
quartile3_median_age_group_2 <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="age_group_2"]

KWRS_age_group_2 <- kruskal.test(age_group_2 ~ median_bmi, BMI_complete_categories)
significance_age_group_2 <- data.frame(t(sapply(KWRS_age_group_2, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_age_group_2 <- significance_age_group_2$p.value

median_age_group_2 <- N_median_age_group_2 %>%
  dplyr::left_join(n_median_age_group_2, by = "age_group_2") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_age_group_2, by = "age_group_2") %>%
  dplyr::left_join(median_median_age_group_2, by = "age_group_2") %>%
  dplyr::left_join(quartile3_median_age_group_2, by = "age_group_2") %>%
  dplyr::select(age_group_2, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_age_group_2) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'age_group_2') %>%
  dplyr::mutate(variable="age_group_2", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::arrange(group)





### SEX

N_median_sex <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="sex"]
n_median_sex <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="sex"]
quartile1_median_sex <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="sex"]
median_median_sex <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="sex"]
quartile3_median_sex <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="sex"]

KWRS_sex <- kruskal.test(sex ~ median_bmi, BMI_complete_categories)
significance_sex <- data.frame(t(sapply(KWRS_sex, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_sex <- significance_sex$p.value

median_sex <- N_median_sex %>%
  dplyr::left_join(n_median_sex, by = "sex") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_sex, by = "sex") %>%
  dplyr::left_join(median_median_sex, by = "sex") %>%
  dplyr::left_join(quartile3_median_sex, by = "sex") %>%
  dplyr::select(sex, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_sex) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'sex') %>%
  dplyr::mutate(variable="sex", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis")


## REGION


N_median_region <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="region"]
n_median_region <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="region"]
quartile1_median_region <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="region"]
median_median_region <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="region"]
quartile3_median_region <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="region"]

KWRS_region <- kruskal.test(region ~ median_bmi, BMI_complete_categories)
significance_region <- data.frame(t(sapply(KWRS_region, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_region <- significance_region$p.value

median_region <- N_median_region %>%
  dplyr::left_join(n_median_region, by = "region") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_region, by = "region") %>%
  dplyr::left_join(median_median_region, by = "region") %>%
  dplyr::left_join(quartile3_median_region, by = "region") %>%
  dplyr::select(region, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_region) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'region') %>%
  dplyr::mutate(variable="region", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis")

##  IMD

N_median_imd <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="imd"]
n_median_imd <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="imd"]
quartile1_median_imd <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="imd"]
median_median_imd <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="imd"]
quartile3_median_imd <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="imd"]

KWRS_imd <- kruskal.test(imd ~ median_bmi, BMI_complete_categories)
significance_imd <- data.frame(t(sapply(KWRS_imd, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_imd <- significance_imd$p.value

median_imd <- N_median_imd %>%
  dplyr::left_join(n_median_imd, by = "imd") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_imd, by = "imd") %>%
  dplyr::left_join(median_median_imd, by = "imd") %>%
  dplyr::left_join(quartile3_median_imd, by = "imd") %>%
  dplyr::select(imd, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_imd) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'imd') %>%
  dplyr::mutate(variable="imd", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::arrange(group)


## 6 category ethnicity

N_median_ethnic_no_miss <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="ethnic_no_miss"]
n_median_ethnic_no_miss <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="ethnic_no_miss"]
quartile1_median_ethnic_no_miss <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="ethnic_no_miss"]
median_median_ethnic_no_miss <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="ethnic_no_miss"]
quartile3_median_ethnic_no_miss <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="ethnic_no_miss"]

KWRS_ethnic_no_miss <- kruskal.test(ethnic_no_miss ~ median_bmi, BMI_complete_categories)
significance_ethnic_no_miss <- data.frame(t(sapply(KWRS_ethnic_no_miss, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_ethnic_no_miss <- significance_ethnic_no_miss$p.value

median_ethnic_no_miss <- N_median_ethnic_no_miss %>%
  dplyr::left_join(n_median_ethnic_no_miss, by = "ethnic_no_miss") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_ethnic_no_miss, by = "ethnic_no_miss") %>%
  dplyr::left_join(median_median_ethnic_no_miss, by = "ethnic_no_miss") %>%
  dplyr::left_join(quartile3_median_ethnic_no_miss, by = "ethnic_no_miss") %>%
  dplyr::select(ethnic_no_miss, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_ethnic_no_miss) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'ethnic_no_miss') %>%
  dplyr::mutate(variable="ethnic_no_miss", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::arrange(group)


## ethnic 16 categories

N_median_eth_group_16 <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="eth_group_16"]
n_median_eth_group_16 <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="eth_group_16"]
quartile1_median_eth_group_16 <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="eth_group_16"]
median_median_eth_group_16 <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="eth_group_16"]
quartile3_median_eth_group_16 <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="eth_group_16"]

KWRS_eth_group_16 <- kruskal.test(eth_group_16 ~ median_bmi, BMI_complete_categories)
significance_eth_group_16 <- data.frame(t(sapply(KWRS_eth_group_16, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_eth_group_16 <- significance_eth_group_16$p.value

median_eth_group_16 <- N_median_eth_group_16 %>%
  dplyr::left_join(n_median_eth_group_16, by = "eth_group_16") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_eth_group_16, by = "eth_group_16") %>%
  dplyr::left_join(median_median_eth_group_16, by = "eth_group_16") %>%
  dplyr::left_join(quartile3_median_eth_group_16, by = "eth_group_16") %>%
  dplyr::select(eth_group_16, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_eth_group_16) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'eth_group_16') %>%
  dplyr::mutate(variable="eth_group_16", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::arrange(group)


N_median_comorbid_learning_disability <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_learning_disability"]
n_median_comorbid_learning_disability <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_learning_disability"]
quartile1_median_comorbid_learning_disability <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_learning_disability"]
median_median_comorbid_learning_disability <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_learning_disability"]
quartile3_median_comorbid_learning_disability <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_learning_disability"]

KWRS_comorbid_learning_disability <- kruskal.test(comorbid_learning_disability ~ median_bmi, BMI_complete_categories)
significance_comorbid_learning_disability <- data.frame(t(sapply(KWRS_comorbid_learning_disability, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_learning_disability <- significance_comorbid_learning_disability$p.value

median_comorbid_learning_disability <- N_median_comorbid_learning_disability %>%
  dplyr::left_join(n_median_comorbid_learning_disability, by = "comorbid_learning_disability") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_learning_disability, by = "comorbid_learning_disability") %>%
  dplyr::left_join(median_median_comorbid_learning_disability, by = "comorbid_learning_disability") %>%
  dplyr::left_join(quartile3_median_comorbid_learning_disability, by = "comorbid_learning_disability") %>%
  dplyr::select(comorbid_learning_disability, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_learning_disability) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_learning_disability') %>%
  dplyr::mutate(variable="comorbid_learning_disability", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))

## Depression

N_median_comorbid_depression <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_depression"]
n_median_comorbid_depression <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_depression"]
quartile1_median_comorbid_depression <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_depression"]
median_median_comorbid_depression <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_depression"]
quartile3_median_comorbid_depression <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_depression"]

KWRS_comorbid_depression <- kruskal.test(comorbid_depression ~ median_bmi, BMI_complete_categories)
significance_comorbid_depression <- data.frame(t(sapply(KWRS_comorbid_depression, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_depression <- significance_comorbid_depression$p.value

median_comorbid_depression <- N_median_comorbid_depression %>%
  dplyr::left_join(n_median_comorbid_depression, by = "comorbid_depression") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_depression, by = "comorbid_depression") %>%
  dplyr::left_join(median_median_comorbid_depression, by = "comorbid_depression") %>%
  dplyr::left_join(quartile3_median_comorbid_depression, by = "comorbid_depression") %>%
  dplyr::select(comorbid_depression, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_depression) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_depression') %>%
  dplyr::mutate(variable="comorbid_depression", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Dementia

N_median_comorbid_dementia <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_dementia"]
n_median_comorbid_dementia <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_dementia"]
quartile1_median_comorbid_dementia <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_dementia"]
median_median_comorbid_dementia <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_dementia"]
quartile3_median_comorbid_dementia <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_dementia"]

KWRS_comorbid_dementia <- kruskal.test(comorbid_dementia ~ median_bmi, BMI_complete_categories)
significance_comorbid_dementia <- data.frame(t(sapply(KWRS_comorbid_dementia, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_dementia <- significance_comorbid_dementia$p.value

median_comorbid_dementia <- N_median_comorbid_dementia %>%
  dplyr::left_join(n_median_comorbid_dementia, by = "comorbid_dementia") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_dementia, by = "comorbid_dementia") %>%
  dplyr::left_join(median_median_comorbid_dementia, by = "comorbid_dementia") %>%
  dplyr::left_join(quartile3_median_comorbid_dementia, by = "comorbid_dementia") %>%
  dplyr::select(comorbid_dementia, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_dementia) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_dementia') %>%
  dplyr::mutate(variable="comorbid_dementia", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Serious mental health

N_median_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_psychosis_schiz_bipolar"]
n_median_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_psychosis_schiz_bipolar"]
quartile1_median_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_psychosis_schiz_bipolar"]
median_median_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_psychosis_schiz_bipolar"]
quartile3_median_comorbid_psychosis_schiz_bipolar <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_psychosis_schiz_bipolar"]

KWRS_comorbid_psychosis_schiz_bipolar <- kruskal.test(comorbid_psychosis_schiz_bipolar ~ median_bmi, BMI_complete_categories)
significance_comorbid_psychosis_schiz_bipolar <- data.frame(t(sapply(KWRS_comorbid_psychosis_schiz_bipolar, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_psychosis_schiz_bipolar <- significance_comorbid_psychosis_schiz_bipolar$p.value

median_comorbid_psychosis_schiz_bipolar <- N_median_comorbid_psychosis_schiz_bipolar %>%
  dplyr::left_join(n_median_comorbid_psychosis_schiz_bipolar, by = "comorbid_psychosis_schiz_bipolar") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_psychosis_schiz_bipolar, by = "comorbid_psychosis_schiz_bipolar") %>%
  dplyr::left_join(median_median_comorbid_psychosis_schiz_bipolar, by = "comorbid_psychosis_schiz_bipolar") %>%
  dplyr::left_join(quartile3_median_comorbid_psychosis_schiz_bipolar, by = "comorbid_psychosis_schiz_bipolar") %>%
  dplyr::select(comorbid_psychosis_schiz_bipolar, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_psychosis_schiz_bipolar) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_psychosis_schiz_bipolar') %>%
  dplyr::mutate(variable="comorbid_psychosis_schiz_bipolar", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Type 1 DM

N_median_comorbid_diabetes_t1 <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_diabetes_t1"]
n_median_comorbid_diabetes_t1 <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_diabetes_t1"]
quartile1_median_comorbid_diabetes_t1 <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_diabetes_t1"]
median_median_comorbid_diabetes_t1 <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_diabetes_t1"]
quartile3_median_comorbid_diabetes_t1 <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_diabetes_t1"]

KWRS_comorbid_diabetes_t1 <- kruskal.test(comorbid_diabetes_t1 ~ median_bmi, BMI_complete_categories)
significance_comorbid_diabetes_t1 <- data.frame(t(sapply(KWRS_comorbid_diabetes_t1, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_diabetes_t1 <- significance_comorbid_diabetes_t1$p.value

median_comorbid_diabetes_t1 <- N_median_comorbid_diabetes_t1 %>%
  dplyr::left_join(n_median_comorbid_diabetes_t1, by = "comorbid_diabetes_t1") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_diabetes_t1, by = "comorbid_diabetes_t1") %>%
  dplyr::left_join(median_median_comorbid_diabetes_t1, by = "comorbid_diabetes_t1") %>%
  dplyr::left_join(quartile3_median_comorbid_diabetes_t1, by = "comorbid_diabetes_t1") %>%
  dplyr::select(comorbid_diabetes_t1, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_diabetes_t1) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_diabetes_t1') %>%
  dplyr::mutate(variable="comorbid_diabetes_t1", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))

## Type 2 DM

N_median_comorbid_diabetes_t2 <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_diabetes_t2"]
n_median_comorbid_diabetes_t2 <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_diabetes_t2"]
quartile1_median_comorbid_diabetes_t2 <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_diabetes_t2"]
median_median_comorbid_diabetes_t2 <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_diabetes_t2"]
quartile3_median_comorbid_diabetes_t2 <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_diabetes_t2"]

KWRS_comorbid_diabetes_t2 <- kruskal.test(comorbid_diabetes_t2 ~ median_bmi, BMI_complete_categories)
significance_comorbid_diabetes_t2 <- data.frame(t(sapply(KWRS_comorbid_diabetes_t2, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_diabetes_t2 <- significance_comorbid_diabetes_t2$p.value

median_comorbid_diabetes_t2 <- N_median_comorbid_diabetes_t2 %>%
  dplyr::left_join(n_median_comorbid_diabetes_t2, by = "comorbid_diabetes_t2") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_diabetes_t2, by = "comorbid_diabetes_t2") %>%
  dplyr::left_join(median_median_comorbid_diabetes_t2, by = "comorbid_diabetes_t2") %>%
  dplyr::left_join(quartile3_median_comorbid_diabetes_t2, by = "comorbid_diabetes_t2") %>%
  dplyr::select(comorbid_diabetes_t2, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_diabetes_t2) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_diabetes_t2') %>%
  dplyr::mutate(variable="comorbid_diabetes_t2", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Asthma


N_median_comorbid_asthma <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_asthma"]
n_median_comorbid_asthma <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_asthma"]
quartile1_median_comorbid_asthma <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_asthma"]
median_median_comorbid_asthma <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_asthma"]
quartile3_median_comorbid_asthma <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_asthma"]

KWRS_comorbid_asthma <- kruskal.test(comorbid_asthma ~ median_bmi, BMI_complete_categories)
significance_comorbid_asthma <- data.frame(t(sapply(KWRS_comorbid_asthma, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_asthma <- significance_comorbid_asthma$p.value

median_comorbid_asthma <- N_median_comorbid_asthma %>%
  dplyr::left_join(n_median_comorbid_asthma, by = "comorbid_asthma") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_asthma, by = "comorbid_asthma") %>%
  dplyr::left_join(median_median_comorbid_asthma, by = "comorbid_asthma") %>%
  dplyr::left_join(quartile3_median_comorbid_asthma, by = "comorbid_asthma") %>%
  dplyr::select(comorbid_asthma, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_asthma) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_asthma') %>%
  dplyr::mutate(variable="comorbid_asthma", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## COPD


N_median_comorbid_COPD <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_COPD"]
n_median_comorbid_COPD <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_COPD"]
quartile1_median_comorbid_COPD <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_COPD"]
median_median_comorbid_COPD <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_COPD"]
quartile3_median_comorbid_COPD <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_COPD"]

KWRS_comorbid_COPD <- kruskal.test(comorbid_COPD ~ median_bmi, BMI_complete_categories)
significance_comorbid_COPD <- data.frame(t(sapply(KWRS_comorbid_COPD, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_COPD <- significance_comorbid_COPD$p.value

median_comorbid_COPD <- N_median_comorbid_COPD %>%
  dplyr::left_join(n_median_comorbid_COPD, by = "comorbid_COPD") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_COPD, by = "comorbid_COPD") %>%
  dplyr::left_join(median_median_comorbid_COPD, by = "comorbid_COPD") %>%
  dplyr::left_join(quartile3_median_comorbid_COPD, by = "comorbid_COPD") %>%
  dplyr::select(comorbid_COPD, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_COPD) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_COPD') %>%
  dplyr::mutate(variable="comorbid_COPD", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Stroke TIA

N_median_comorbid_stroke_and_TIA <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_stroke_and_TIA"]
n_median_comorbid_stroke_and_TIA <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_stroke_and_TIA"]
quartile1_median_comorbid_stroke_and_TIA <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_stroke_and_TIA"]
median_median_comorbid_stroke_and_TIA <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_stroke_and_TIA"]
quartile3_median_comorbid_stroke_and_TIA <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_stroke_and_TIA"]

KWRS_comorbid_stroke_and_TIA <- kruskal.test(comorbid_stroke_and_TIA ~ median_bmi, BMI_complete_categories)
significance_comorbid_stroke_and_TIA <- data.frame(t(sapply(KWRS_comorbid_stroke_and_TIA, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_stroke_and_TIA <- significance_comorbid_stroke_and_TIA$p.value

median_comorbid_stroke_and_TIA <- N_median_comorbid_stroke_and_TIA %>%
  dplyr::left_join(n_median_comorbid_stroke_and_TIA, by = "comorbid_stroke_and_TIA") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_stroke_and_TIA, by = "comorbid_stroke_and_TIA") %>%
  dplyr::left_join(median_median_comorbid_stroke_and_TIA, by = "comorbid_stroke_and_TIA") %>%
  dplyr::left_join(quartile3_median_comorbid_stroke_and_TIA, by = "comorbid_stroke_and_TIA") %>%
  dplyr::select(comorbid_stroke_and_TIA, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_stroke_and_TIA) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_stroke_and_TIA') %>%
  dplyr::mutate(variable="comorbid_stroke_and_TIA", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Chronic Cardiac


N_median_comorbid_chronic_cardiac <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_chronic_cardiac"]
n_median_comorbid_chronic_cardiac <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_chronic_cardiac"]
quartile1_median_comorbid_chronic_cardiac <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_chronic_cardiac"]
median_median_comorbid_chronic_cardiac <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_chronic_cardiac"]
quartile3_median_comorbid_chronic_cardiac <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_chronic_cardiac"]

KWRS_comorbid_chronic_cardiac <- kruskal.test(comorbid_chronic_cardiac ~ median_bmi, BMI_complete_categories)
significance_comorbid_chronic_cardiac <- data.frame(t(sapply(KWRS_comorbid_chronic_cardiac, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_chronic_cardiac <- significance_comorbid_chronic_cardiac$p.value

median_comorbid_chronic_cardiac <- N_median_comorbid_chronic_cardiac %>%
  dplyr::left_join(n_median_comorbid_chronic_cardiac, by = "comorbid_chronic_cardiac") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_chronic_cardiac, by = "comorbid_chronic_cardiac") %>%
  dplyr::left_join(median_median_comorbid_chronic_cardiac, by = "comorbid_chronic_cardiac") %>%
  dplyr::left_join(quartile3_median_comorbid_chronic_cardiac, by = "comorbid_chronic_cardiac") %>%
  dplyr::select(comorbid_chronic_cardiac, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_chronic_cardiac) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_chronic_cardiac') %>%
  dplyr::mutate(variable="comorbid_chronic_cardiac", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## Hypertension

N_median_comorbid_hypertension <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_hypertension"]
n_median_comorbid_hypertension <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_hypertension"]
quartile1_median_comorbid_hypertension <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_hypertension"]
median_median_comorbid_hypertension <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_hypertension"]
quartile3_median_comorbid_hypertension <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_hypertension"]

KWRS_comorbid_hypertension <- kruskal.test(comorbid_hypertension ~ median_bmi, BMI_complete_categories)
significance_comorbid_hypertension <- data.frame(t(sapply(KWRS_comorbid_hypertension, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_hypertension <- significance_comorbid_hypertension$p.value

median_comorbid_hypertension <- N_median_comorbid_hypertension %>%
  dplyr::left_join(n_median_comorbid_hypertension, by = "comorbid_hypertension") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_hypertension, by = "comorbid_hypertension") %>%
  dplyr::left_join(median_median_comorbid_hypertension, by = "comorbid_hypertension") %>%
  dplyr::left_join(quartile3_median_comorbid_hypertension, by = "comorbid_hypertension") %>%
  dplyr::select(comorbid_hypertension, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_hypertension) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_hypertension') %>%
  dplyr::mutate(variable="comorbid_hypertension", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))


## All Cancer

N_median_comorbid_all_cancer <- BMI_complete_categories_DT [, .(N_population = length(median_bmi)) , by="comorbid_all_cancer"]
n_median_comorbid_all_cancer <- BMI_complete_categories_DT [, .(N_missing = sum(is.na(median_bmi))) , by="comorbid_all_cancer"]
quartile1_median_comorbid_all_cancer <- BMI_complete_categories_DT[, .( "25th" = quantile(median_bmi, probs = c(0.25), na.rm = TRUE)), by="comorbid_all_cancer"]
median_median_comorbid_all_cancer <- BMI_complete_categories_DT[, .( "median" = quantile(median_bmi, probs = c(0.5), na.rm = TRUE)), by="comorbid_all_cancer"]
quartile3_median_comorbid_all_cancer <- BMI_complete_categories_DT[, .( "75th" = quantile(median_bmi, probs = c(0.75), na.rm = TRUE)), by="comorbid_all_cancer"]

KWRS_comorbid_all_cancer <- kruskal.test(comorbid_all_cancer ~ median_bmi, BMI_complete_categories)
significance_comorbid_all_cancer <- data.frame(t(sapply(KWRS_comorbid_all_cancer, c)))  %>%
  dplyr::select(p.value, method) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

p_comorbid_all_cancer <- significance_comorbid_all_cancer$p.value

median_comorbid_all_cancer <- N_median_comorbid_all_cancer %>%
  dplyr::left_join(n_median_comorbid_all_cancer, by = "comorbid_all_cancer") %>%
  dplyr::mutate("N" = N_population - N_missing) %>%
  dplyr::left_join(quartile1_median_comorbid_all_cancer, by = "comorbid_all_cancer") %>%
  dplyr::left_join(median_median_comorbid_all_cancer, by = "comorbid_all_cancer") %>%
  dplyr::left_join(quartile3_median_comorbid_all_cancer, by = "comorbid_all_cancer") %>%
  dplyr::select(comorbid_all_cancer, "N", N_population, "25th", median, "75th") %>%
  dplyr::mutate (p = p_comorbid_all_cancer) %>%
  dplyr::mutate (p= (as.numeric(p))) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::rename('group' = 'comorbid_all_cancer') %>%
  dplyr::mutate(variable="comorbid_all_cancer", .before = 1 ) %>%
  dplyr::mutate(method = "Kruskal_Wallis") %>%
  dplyr::mutate(group = as.character(group))



median_complete <- dplyr::bind_rows( median_all, 
                                     median_age_group, 
                                     median_age_group_2, 
                                     median_sex, 
                                     median_region, 
                                     median_imd, 
                                     median_ethnic_no_miss,
                                     median_eth_group_16,
                                     median_comorbid_hypertension, 
                                     median_comorbid_diabetes_t2,
                                     median_comorbid_diabetes_t1, 
                                     median_comorbid_learning_disability,
                                     median_comorbid_depression,
                                     median_comorbid_dementia,
                                     median_comorbid_psychosis_schiz_bipolar, 
                                     median_comorbid_COPD, 
                                     median_comorbid_asthma,
                                     median_comorbid_chronic_cardiac, 
                                     median_comorbid_stroke_and_TIA,
                                     median_comorbid_all_cancer)


write.csv (median_complete, here::here ("output/data","median_range_bmi_2020.csv"))
