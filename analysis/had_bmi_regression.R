

##   Miriam Samuel
##   28th Feb 2022
##   Had_bmi regression and count outputs, 2020 and 2021

##  packages
library(broom)
library(purrr)
library(dplyr)
library(janitor)

#read in file
BMI_complete_categories <- read.csv (here::here ("output/data", "BMI_complete_categories.csv"))

## select the variables needed for analysis

BMI_complete_categories_2020 <- BMI_complete_categories


##  Filter by year
BMI_complete_categories_2020 <- BMI_complete_categories_2020 %>%
  dplyr::filter(year==2020) %>%
  dplyr::select(had_bmi, 
                sex, 
                age_group, 
                region, 
                imd, 
                ethnic_no_miss, 
                precovid_obese_flag, 
                starts_with("comorbid_"))

str(BMI_complete_categories_2020)




explanatory_vars <- c("sex", 
                      "age_group", 
                      "region", 
                      "imd", 
                      "ethnic_no_miss", 
                      "precovid_obese_flag", 
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


## convert had_bmi to a logical output
BMI_complete_categories_2020 %>%
  dplyr::mutate(had_bmi = as.logical(had_bmi))


## Try to change base level  >>  NOTE:  co-efficient for base group = log.odds of event in base group
BMI_complete_categories_2020 <- BMI_complete_categories_2020 %>%
  dplyr::mutate(age_group = as.factor(age_group)) %>%
  dplyr::mutate(age_group = fct_relevel(age_group, "18-39", after = 0))


## instructions from R for epi
# Univariate model with age
had_bmi_age_m <- glm(had_bmi ~ age_group, data=BMI_complete_categories_2020, family=binomial) %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  dplyr::mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns


had_bmi_age_table <- BMI_complete_categories_2020 %>%
  janitor::tabyl (age_group, had_bmi) 

had_bmi_age_table

combined <- had_bmi_age_table %>%           # begin with table of counts
  bind_cols(., had_bmi_age_m) %>%              # combine with the outputs of the regression 
  select(term, 2:3, estimate,          # select and re-order cols
         conf.low, conf.high, p.value) %>% 
  mutate(across(where(is.numeric), round, digits = 2)) ## round to 2 decimal places


## Use PURR to loop over the different exposures in a univariate analysis and create a combined table

#1.  use stringer to create a vector listing each item to run the logistic regression over
models <- explanatory_vars %>%       # begin with variables of interest
  str_c("had_bmi ~ ", .) %>%    ## creates a vector of characters....  each cell contains outcome ~ var (the structure needed for regression formula)
  
  # iterate through each univariate formula ... using map function from purr
  map(                               ##  Map each element of the preceding vector the following formula
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = BMI_complete_categories_2020))  %>%        # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))


##########  Develop a count table
## for each explanatory variable (defined in explanatolry_vars) do the following:
univ_tab_base <- explanatory_vars %>% 
  map(.f = 
        ~{BMI_complete_categories_2020 %>%                ## begin with dataset
            group_by(had_bmi) %>%     ## group data set by outcome
            count(.data[[.x]]) %>%    ## produce counts for variable of interest
            pivot_wider(              ## spread to wide format (as in cross-tabulation)
              names_from = had_bmi,   ## column names '0', '1'................
              values_from = n) %>% 
            drop_na(.data[[.x]]) %>%         ## drop rows with missing
            rename("variable" = .x) %>%      ## change variable of interest column to "variable"
            mutate(variable = as.character(variable))} ## convert to character, else non-dichotomous (categorical) variables come out as factor and cant be merged
  )  %>% 
  
  # (These steps produce a list of with each element a tibble with column names variable, factor.1, factor.2 etc...)
  
  
  
  ## Binds rows in tibble list into one data frame
  bind_rows() %>% 
  
  ## merge with the outputs of the regression 
  bind_cols(., models) %>% 
  
  ## only keep columns interested in 
  select(term, 2:3, estimate, conf.low, conf.high, p.value) %>% 
  
  ## round decimal places
  mutate(across(where(is.numeric), round, digits = 2))


## # generate a column of total counts
univariate_had_bmi_2020 <- univ_tab_base %>%
  mutate(n_had_bmi = univ_tab_base$'1', .after = term) %>%
  mutate(N_total = (univ_tab_base$'0' + univ_tab_base$'1'), .after = term ) %>%
  mutate(proportion_had_bmi = n_had_bmi/N_total, .after = n_had_bmi) %>%
  select(term, n_had_bmi,  proportion_had_bmi, estimate, conf.low, conf.high, p.value)



write.csv (univariate_had_bmi_2020, here::here ("output/data","regression_had_bmi_2020.csv"))

##################################################################################




