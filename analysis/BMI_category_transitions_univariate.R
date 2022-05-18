##This R script will describe how patients transitioned between BMI categories.  
## Also univariate models predicting category change

## Load libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(data.table)
library(forcats)
library(rstatix)
library(janitor)
library(lubridate)
library(skimr)
library(mlogit)

data <- read_feather (here::here ("output/data", "BMI_category_transition.feather"))

data <- data %>%
   rename_all(~stringr::str_replace(.,"^comorbid_",""))


##  Add category for severely obese 

# 1. For 1st BMI
categories_change <- data %>% 
  dplyr::mutate(category_1a = bmi_1)

categories_change$category_1a <- cut(categories_change$category_1a, 
                                    breaks=c(0, 20,25,30,35,1000),
                                    labels= c("underweight", "healthy", "overweight", "obese", "severely obese"))


# 2. For 2nd BMI
categories_change <- categories_change %>% 
  dplyr::mutate(category_2a = bmi_2)

categories_change$category_2a <- cut(categories_change$category_2a, 
                                     breaks=c(0, 20,25,30,35,1000),
                                     labels= c("underweight", "healthy", "overweight", "obese", "severely obese"))



categories_change$age_group_2 <- factor(categories_change$age_group_2,      # Reordering group factor levels
                                                   levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))


#########

cat_1 <- categories_change %>% 
  dplyr::group_by(year, category_1) %>% 
  tally() %>% 
  dplyr::mutate(total = n)   %>%
  dplyr::select(-("n"))
  

# Calculate transitions
 transitions <- categories_change %>% 
  dplyr::group_by(year, category_1, category_2)  %>% 
  tally() %>% 
  left_join(cat_1) %>% 
  dplyr::mutate(percent = n/total*100)
 
transitions$percent <- round(transitions$percent, digits=1)


### models to predict transitions in different groups

explanatory_vars_1 <- c("sex", 
                      "age_group_2", 
                      "ethnic_no_miss", 
                      "eth_group_16",
                      "imd", 
                      "region", 
                      "hypertension",  
                      "diabetes_t1",           
                      "diabetes_t2",
                      "chronic_cardiac", 
                      "learning_disability",     
                      "depression",            
                      "dementia",               
                      "psychosis_schiz_bipolar",
                      "asthma",                
                      "COPD",                   
                      "stroke_and_TIA",       
                      "all_cancer", 
                      "smoking_status", 
                      "year")

explanatory_vars_2 <- c("sex", 
                        "age_group_2", 
                        "ethnic_no_miss", 
                        "eth_group_16",
                        "imd", 
                        "region", 
                        "hypertension",  
                        "diabetes_t1",           
                        "diabetes_t2",
                        "chronic_cardiac", 
                        "learning_disability",     
                        "depression",            
                        "dementia",               
                        "psychosis_schiz_bipolar",
                        "asthma",                
                        "COPD",                   
                        "stroke_and_TIA",       
                        "all_cancer", 
                        "smoking_status")

## models to predict variable association with transitions
normal_weight <- categories_change %>% 
  dplyr::filter(category_1 =="healthy")



## variable to show if normal became overweight/obese

normal_weight <- normal_weight %>% 
  dplyr::mutate(transition_bmi25 = healthy_cat_change)

normal_weight$transition_bmi25[normal_weight$transition_bmi25==1] <- 0
normal_weight$transition_bmi25[normal_weight$transition_bmi25==2] <- 1
normal_weight$transition_bmi25[normal_weight$transition_bmi25==3] <- 1

normal_weight %>% 
  tabyl(healthy_cat_change, transition_bmi25)



models_normal_weight <- explanatory_vars_1 %>%       # begin with variables of interest
  str_c("transition_bmi25  ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = normal_weight)) %>%          # dataset
  
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








## limit to 2017 data

normal_weight_2017 <- normal_weight %>% 
  dplyr::filter(year == "2017")


models_normal_weight_2017 <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("transition_bmi25  ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = normal_weight_2017)) %>%          # dataset
  
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







## limit to 2019 data

normal_weight_2019 <- normal_weight %>% 
  dplyr::filter(year == "2019")


models_normal_weight_2019 <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("transition_bmi25  ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = normal_weight_2019)) %>%          # dataset
  
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



########### OVERWEIGHT TO OBESE
## models to predict variable association with transitions
overweight <- categories_change %>% 
  dplyr::filter(category_1 =="overweight")



## variable to show if normal became overweight/obese

overweight <- overweight %>% 
  dplyr::mutate(bmi2_under25= as.character(category_2))


overweight$bmi2_under25[overweight$bmi2_under25 == "obese"] <- 0
overweight$bmi2_under25[overweight$bmi2_under25 == "overweight" ] <- 0
overweight$bmi2_under25[overweight$bmi2_under25 == "healthy" ] <- 1
overweight$bmi2_under25[overweight$bmi2_under25 == "underweight" ] <- 1


overweight %>% 
  tabyl(category_2, bmi2_under25)




## models_overweight to obese

models_overweight <- explanatory_vars_1 %>%       # begin with variables of interest
  str_c("become_obese ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = overweight)) %>%          # dataset
  
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




## limit to 2017 data

overweight_2017 <- overweight %>% 
  dplyr::filter(year == "2017")


models_overweight_2017 <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("become_obese ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = overweight_2017)) %>%          # dataset
  
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



## limit to 2019 data

overweight_2019 <- overweight %>% 
  dplyr::filter(year == "2019")


models_overweight_2019 <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("become_obese ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = overweight_2019)) %>%          # dataset
  
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

######################
######################
## models overweight to normal/underweight

## models_overweight_weightloss to obese

models_overweight_weightloss <- explanatory_vars_1 %>%       # begin with variables of interest
  str_c("bmi2_under25 ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = overweight)) %>%          # dataset
  
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




## limit to 2017 data

overweight_2017 <- overweight %>% 
  dplyr::filter(year == "2017")


models_overweight_weightloss_2017 <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("bmi2_under25 ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = overweight_2017)) %>%          # dataset
  
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



## limit to 2019 data

overweight_2019 <- overweight %>% 
  dplyr::filter(year == "2019")


models_overweight_weightloss_2019 <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("bmi2_under25 ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = overweight_2019)) %>%          # dataset
  
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


########################
########################
obese <- categories_change %>% 
  dplyr::filter(category_1 =="obese")


obese <- obese %>% 
  dplyr::mutate(weightloss= as.character(category_2))


obese$weightloss[obese$weightloss == "obese"] <- 0
obese$weightloss[obese$weightloss == "overweight" ] <- 1
obese$weightloss[obese$weightloss == "healthy" ] <- 1
obese$weightloss[obese$weightloss == "underweight" ] <- 1


obese %>% 
  tabyl(category_2, weightloss)




## models_obese_weightloss 

models_obese_weightloss <- explanatory_vars_1 %>%       # begin with variables of interest
  str_c("weightloss ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = obese)) %>%          # dataset
  
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




## limit to 2017 data

obese_2017 <- obese %>% 
  dplyr::filter(year == "2017")


models_obese_weightloss_2017 <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("weightloss ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = obese_2017)) %>%          # dataset
  
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



## limit to 2019 data

obese_2019 <- obese %>% 
  dplyr::filter(year == "2019")


models_obese_weightloss_2019 <- explanatory_vars_2 %>%       # begin with variables of interest
  str_c("weightloss ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~lm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      data = obese_2019)) %>%          # dataset
  
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





## save outputs









# save outputs






###################################################################


write.csv (transitions, here::here ("output/data","bmi_category_transitions.csv"))


## healthy weight to overweight
write.csv (models_normal_weight, here::here ("output/data","healthy_transition_weightgain_univariate.csv"))
write.csv (models_normal_weight_2017, here::here ("output/data","healthy_transition_weightgain_2017_univariate.csv"))
write.csv (models_normal_weight_2019, here::here ("output/data","healthy_transition_weightgain_2019_univariate.csv"))


write.csv (models_overweight, here::here ("output/data","overweight_transition_weightgain_univariate.csv"))
write.csv (models_overweight_2017, here::here ("output/data","overweight_transition_weightgain_2017_univariate.csv"))
write.csv (models_overweight_2019, here::here ("output/data","overweight_transition_weightgain_2019_univariate.csv"))


write.csv (models_overweight_weightloss, here::here ("output/data","overweight_transition_weightloss_univariate.csv"))
write.csv (models_overweight_weightloss_2017, here::here ("output/data","overweight_transition_weightloss_2017_univariate.csv"))
write.csv (models_overweight_weightloss_2019, here::here ("output/data","overweight_transition_weightloss_2019_univariate.csv"))

write.csv (models_obese_weightloss, here::here ("output/data","obese_transition_weightloss_univariate.csv"))
write.csv (models_obese_weightloss_2017, here::here ("output/data","obese_transition_weightloss_2017_univariate.csv"))
write.csv (models_obese_weightloss_2019, here::here ("output/data","obese_transition_weightloss_2019_univariate.csv"))


models_obese_weightloss 
