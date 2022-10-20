#### Author: M Samuel
#### Date: Oct 2022
####  This script looks redacts


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





univariate <- read_csv ( here::here ("output/data","change_90th_univariate_lowbmiexc_T2DM.csv"))


counts <- read_csv ( here::here ("output/data","change_90th_counts_lowbmiexc_T2DM.csv"))

age <- read_csv (here::here ("output/data","change_90th_age_adj_lowbmiexc_T2DM.csv"))

sex <- read_csv ( here::here ("output/data","change_90th_sex_adj_lowbmiexc_T2DM.csv"))

agesex <- read_csv ( here::here ("output/data","change_90th_agesex_adj_lowbmiexc_T2DM.csv"))

ageseximd <- read_csv ( here::here ("output/data","change_90th_ageseximd_adj_lowbmiexc_T2DM.csv"))



univariate <- univariate %>% 
  dplyr::filter(term != "smoking_statusM")

age <- age %>% 
  dplyr::filter(term != "smoking_statusM")  

sex <- sex %>% 
  dplyr::filter(term != "smoking_statusM")  

agesex <- agesex %>% 
  dplyr::filter(term != "smoking_statusM")  

ageseximd <- ageseximd %>% 
  dplyr::filter(term != "smoking_statusM")  

counts <- counts %>% 
  dplyr::filter(group != "M")


write_csv (univariate, here::here ("output/data","change_90th_univariate_lowbmiexc_T2DM_r.csv"))


write_csv (counts, here::here ("output/data","change_90th_counts_lowbmiexc_T2DM_r.csv"))

write_csv (age, here::here ("output/data","change_90th_age_adj_lowbmiexc_T2DM_r.csv"))

write_csv (sex, here::here ("output/data","change_90th_sex_adj_lowbmiexc_T2DM_r.csv"))

write_csv (agesex, here::here ("output/data","change_90th_agesex_adj_lowbmiexc_T2DM_r.csv"))

write_csv (ageseximd, here::here ("output/data","change_90th_ageseximd_adj_lowbmiexc_T2DM_r.csv"))
