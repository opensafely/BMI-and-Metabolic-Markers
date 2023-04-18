##  This R script looks at the multivariate analysis of the complete cases from the sample data used in the imputation.

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
library(lubridate)
library(skimr)
library(ggplot2)
library(mice)


BMI_trajectories <- read_csv (here::here ("output/data", "CC_imputation_DF_for_impute.csv"))







BMI_trajectories$imd <- factor(BMI_trajectories$imd, 
                               levels = c('1','2','3','4','5'))







BMI_trajectories$eth_16_corrected <- factor(BMI_trajectories$eth_16_corrected, 
                                            levels = c(
                                              "White_British",
                                              "White_Irish",
                                              "Other_White",
                                              "Indian",
                                              "Pakistani",
                                              "Bangladeshi",
                                              "Other_Asian",
                                              "Caribbean",
                                              "African",
                                              "Other_Black",
                                              "Chinese",
                                              "White_Asian",
                                              "White_Black_Carib",
                                              "White_Black_African",
                                              "Other_Mixed",
                                              "Other",
                                              "Missing"))


BMI_trajectories<- BMI_trajectories  %>% 
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


BMI_trajectories$eth_collapsed <- factor(BMI_trajectories$eth_collapsed, 
                                         levels = c('white','black','south_asian','chinese_other','mixed'))




BMI_trajectories %>% 
  tabyl(eth_collapsed)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(rapid_change = case_when(
    bmi_change_cat == "over 0.5" ~ 1, 
    bmi_change_cat != "over 0.5"  ~ 0
  )) 

BMI_trajectories %>% 
  tabyl(bmi_change_cat, rapid_change)



## create complete case data set
BMI_trajectories <- BMI_trajectories %>% 
  tidyr::drop_na(imd)  %>% 
  tidyr::drop_na(eth_collapsed)



#####################################
## total population

## multivariate
all <- glm(rapid_change ~ eth_collapsed + sex + imd + age_group_2, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')

##

hypertension <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  hypertension, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')


##

diabetes_t1 <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  diabetes_t1, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')



##

diabetes_t2 <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  diabetes_t2, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')



##

chronic_cardiac <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  chronic_cardiac, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')



##
learning_disability <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  learning_disability, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')



##

psychosis_schiz_bipolar <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  psychosis_schiz_bipolar, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')



##

depression <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  depression, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')



##

COPD <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  COPD, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')



##

asthma <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  asthma, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')



##

dementia <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  dementia, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')



##

stroke_and_TIA <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  stroke_and_TIA, data=BMI_trajectories, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'all_multivariate')


all_models <- all %>% 
  dplyr::bind_rows(all, 
                   hypertension, 
                   diabetes_t2, 
                   diabetes_t1, 
                   chronic_cardiac, 
                   depression, 
                  psychosis_schiz_bipolar, 
                  learning_disability, 
                  dementia, 
                  stroke_and_TIA, 
                  asthma,
                  COPD
                  )

print(2)
########## 
## Hypertension

BMI_hypertension <- BMI_trajectories %>% 
  dplyr::filter (hypertension == TRUE)


## multivariate
all <- glm(rapid_change ~ eth_collapsed + sex + imd + age_group_2, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')


##

diabetes_t1 <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  diabetes_t1, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')



##

diabetes_t2 <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  diabetes_t2, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')



##

chronic_cardiac <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  chronic_cardiac, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')



##
learning_disability <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  learning_disability, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')



##

psychosis_schiz_bipolar <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  psychosis_schiz_bipolar, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')



##

depression <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  depression, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')



##

COPD <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  COPD, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')



##

asthma <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  asthma, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')



##

dementia <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  dementia, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')



##

stroke_and_TIA <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  stroke_and_TIA, data=BMI_hypertension, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'hypertension')


hypertension_models <- all %>% 
  dplyr::bind_rows(all, 
                   hypertension, 
                   diabetes_t2, 
                   diabetes_t1, 
                   chronic_cardiac, 
                   depression, 
                   psychosis_schiz_bipolar, 
                   learning_disability, 
                   dementia, 
                   stroke_and_TIA, 
                   asthma,
                   COPD
  )


############################


BMI_diabetes_t2 <- BMI_trajectories %>% 
  dplyr::filter (diabetes_t2 == TRUE)


## multivariate
all <- glm(rapid_change ~ eth_collapsed + sex + imd + age_group_2, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')

##

hypertension <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  hypertension, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')


##

chronic_cardiac <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  chronic_cardiac, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')



##
learning_disability <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  learning_disability, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')



##

psychosis_schiz_bipolar <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  psychosis_schiz_bipolar, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')



##

depression <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  depression, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')



##

COPD <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  COPD, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')



##

asthma <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  asthma, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')



##

dementia <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  dementia, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')



##

stroke_and_TIA <- glm(rapid_change ~ age_group_2 + sex + imd + eth_collapsed +  stroke_and_TIA, data=BMI_diabetes_t2, family = "binomial") %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>% 
  dplyr::mutate(variable = 'diabetes_t2')


diabetes_models <- all %>% 
  dplyr::bind_rows(all, 
                   hypertension, 
                   chronic_cardiac, 
                   depression, 
                   psychosis_schiz_bipolar, 
                   learning_disability, 
                   dementia, 
                   stroke_and_TIA, 
                   asthma,
                   COPD
  )

multivariate_models <- all_models %>% 
  bind_rows(hypertension_models, diabetes_models)



############################

## Demographics

######################

function_1 <- function(var){
  v1 <- deparse(substitute(var))
  
  BMI_trajectories  %>% 
    tabyl({{var}}, rapid_change) %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(variable = (v1), .before=1) 
}




sex <-  function_1(sex)
age <- function_1(age_group_2)
ethnic <- function_1(eth_collapsed)
imd <- function_1(imd)
region <- function_1(region)
hypertension <- function_1(hypertension)
T1DM <- function_1(diabetes_t1)
T2DM <- function_1(diabetes_t2)
cardiac <- function_1(chronic_cardiac)
LD <- function_1(learning_disability)
SMI <- function_1(psychosis_schiz_bipolar)
depression <- function_1(depression)
asthma <- function_1(asthma)
COPD <- function_1(COPD)
stroke <- function_1(stroke_and_TIA)
dementia <- function_1(dementia)

print(5)

demographics <- age %>% 
  bind_rows(sex,
            ethnic, 
            imd,
            region, 
            hypertension, 
            T2DM, 
            T1DM, 
            cardiac,
            LD, 
            SMI, 
            depression, 
            asthma, 
            COPD, 
            stroke,
            dementia)

demographics <- demographics %>% 
  dplyr::mutate (N = demographics$`0` + demographics$`1`) %>% 
  dplyr::mutate(rapid = demographics$`1`) %>% 
  dplyr::select(c(variable, group, N, rapid)) 

demographics <- demographics %>% 
  dplyr::mutate(N = plyr::round_any(demographics$N, 5)) %>% 
  plyr::mutate(rapid = plyr::round_any(demographics$rapid, 5)) %>% 
  dplyr::mutate(dataset = "all")

##

function_2 <- function(var){
  v1 <- deparse(substitute(var))
  
  BMI_hypertension  %>% 
    tabyl({{var}}, rapid_change) %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(variable = (v1), .before=1) 
}




sex <-  function_2(sex)
age <- function_2(age_group_2)
ethnic <- function_2(eth_collapsed)
imd <- function_2(imd)
region <- function_2(region)
hypertension <- function_2(hypertension)
T1DM <- function_2(diabetes_t1)
T2DM <- function_2(diabetes_t2)
cardiac <- function_2(chronic_cardiac)
LD <- function_2(learning_disability)
SMI <- function_2(psychosis_schiz_bipolar)
depression <- function_2(depression)
asthma <- function_2(asthma)
COPD <- function_2(COPD)
stroke <- function_2(stroke_and_TIA)
dementia <- function_2(dementia)

print(5)

demographics_hypertension <- age %>% 
  bind_rows(sex,
            ethnic, 
            imd,
            region, 
            hypertension, 
            T2DM, 
            T1DM, 
            cardiac,
            LD, 
            SMI, 
            depression, 
            asthma, 
            COPD, 
            stroke,
            dementia)

demographics_hypertension <- demographics_hypertension %>% 
  dplyr::mutate (N = demographics_hypertension$`0` + demographics_hypertension$`1`) %>% 
  dplyr::mutate(rapid = demographics_hypertension$`1`) %>% 
  dplyr::select(c(variable, group, N, rapid)) 

demographics_hypertension <- demographics_hypertension %>% 
  dplyr::mutate(N = plyr::round_any(demographics_hypertension$N, 5)) %>% 
  plyr::mutate(rapid = plyr::round_any(demographics_hypertension$rapid, 5))  %>%
  dplyr::mutate(dataset = "hypertension")

###

function_3 <- function(var){
  v1 <- deparse(substitute(var))
  
  BMI_diabetes_t2  %>% 
    tabyl({{var}}, rapid_change) %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(variable = (v1), .before=1) 
}




sex <-  function_3(sex)
age <- function_3(age_group_2)
ethnic <- function_3(eth_collapsed)
imd <- function_3(imd)
region <- function_3(region)
hypertension <- function_3(hypertension)
T1DM <- function_3(diabetes_t1)
T2DM <- function_3(diabetes_t2)
cardiac <- function_3(chronic_cardiac)
LD <- function_3(learning_disability)
SMI <- function_3(psychosis_schiz_bipolar)
depression <- function_3(depression)
asthma <- function_3(asthma)
COPD <- function_3(COPD)
stroke <- function_3(stroke_and_TIA)
dementia <- function_3(dementia)

print(5)

demographics_t2d <- age %>% 
  bind_rows(sex,
            ethnic, 
            imd,
            region, 
            hypertension, 
            T2DM, 
            T1DM, 
            cardiac,
            LD, 
            SMI, 
            depression, 
            asthma, 
            COPD, 
            stroke,
            dementia)

demographics_t2d <- demographics_t2d %>% 
  dplyr::mutate (N = demographics_t2d$`0` + demographics_t2d$`1`) %>% 
  dplyr::mutate(rapid = demographics_t2d$`1`) %>% 
  dplyr::select(c(variable, group, N, rapid)) 

demographics_t2d <- demographics_t2d %>% 
  dplyr::mutate(N = plyr::round_any(demographics_t2d$N, 5)) %>% 
  plyr::mutate(rapid = plyr::round_any(demographics_t2d$rapid, 5)) %>% 
  dplyr::mutate(dataset = "t2d")



demographics <- demographics %>% 
  bind_rows(demographics_hypertension, demographics_t2d)

write.csv (multivariate_models, here::here ("output/data", "CC_imputation_complete_case_multivariate.csv"))
write.csv (demographics, here::here ("output/data", "CC_imputation_sample_complete_case_demographics.csv"))
