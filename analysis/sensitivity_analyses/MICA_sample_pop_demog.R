## This script looks as the counts of the sample poulation. 


library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(broom)
library(forcats)
library(rstatix)
library(janitor)


BMI_data <- read_csv (here::here ("output/data", "imputation_DF_for_impute.csv"))


BMI_data$imd <- factor(BMI_data$imd, 
                               levels = c('1','2','3','4','5'))





##
BMI_data$smoking_status <- factor(BMI_data$smoking_status, 
                                          levels = c('N',"S", "E", "M"))



BMI_data$eth_group_16 <- factor(BMI_data$eth_group_16, 
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





function_1 <- function(var){
  v1 <- deparse(substitute(var))
  
  BMI_data  %>% 
    tabyl({{var}}) %>% 
    dplyr::rename(group = {{var}}) %>%
    ungroup() %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(variable = (v1), .before=1) 
  
}

sex <-  function_1(sex)
age <- function_1(age_group_2)
ethnic <- function_1(eth_group_16)
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
smoking_status <- function_1(smoking_status)


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
            dementia, 
            smoking_status)


write.csv (demographics, here::here ("output/data", "MICE_sample_pop_demographics.csv"))



