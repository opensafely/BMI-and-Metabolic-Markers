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

all_2015 <- read_feather (here::here ("output/data", "input_all_2015-03-01.feather"))

check_age_group <- all_2015 %>%
  tabyl(age_group)  %>%
  dplyr:: rename(check = 'percent') %>%
  dplyr::rename(group = 'age_group') %>%
  dplyr::mutate(variable = 'age_group') %>%
  dplyr::select('group', 'check', 'variable') %>%
  dplyr::mutate(check = as.character(check))

check_age_group_2 <- all_2015 %>%
  tabyl(age_group_2)  %>%
  dplyr:: rename(check = 'percent') %>%
  dplyr::rename(group = 'age_group_2') %>%
  dplyr::mutate(variable = 'age_group_2') %>%
  dplyr::select('group', 'check', 'variable') %>%
  dplyr::mutate(check = as.character(check))



check_hba1c <- all_2015 %>%
  ungroup() %>%
  dplyr::filter(hba1c_march >0 ) %>%
  dplyr::group_by(sex) %>%
  dplyr::rename(group = 'sex') %>%
  dplyr::summarise(check = mean(hba1c_march)) %>%
  dplyr::mutate(check = as.character(check)) %>%
  dplyr::mutate(variable = 'hba1c')



 check_bmi <- all_2015 %>%
  ungroup() %>%
  dplyr::filter(bmi_march >0 ) %>%
  dplyr::group_by(sex) %>%
   dplyr::rename(group = 'sex') %>%
  dplyr::summarise(check = mean(bmi_march)) %>%
   dplyr::mutate(check = as.character(check)) %>%
   dplyr::mutate(variable = 'bmi')
 
 
 check_chol <- all_2015 %>%
   tabyl(sex, cholesterol_test) %>%
   adorn_percentages("row") %>%
   adorn_pct_formatting(digits = 2) %>%
   adorn_ns() %>%
   dplyr:: rename(check = 'TRUE') %>%
   dplyr::select(sex, check) %>%
   dplyr::rename(group = 'sex') %>%
   dplyr::mutate(variable = 'chol')



 

 
check_sbp <- all_2015 %>%
  filter(sbp>0) %>%
  group_by(sex) %>%
  dplyr::rename(group = 'sex') %>%
  summarise_at(vars(sbp), list(check=mean)) %>%
  dplyr::mutate(check = as.character(check)) %>%
  dplyr::mutate(variable = 'sbp')
 
 
check_dbp <- all_2015 %>%
  filter(dbp>0) %>%
  group_by(sex) %>%
  dplyr::rename(group = 'sex') %>%
  summarise_at(vars(dbp), list(check = mean)) %>%
  dplyr::mutate(check = as.character(check)) %>%
  dplyr::mutate(variable = 'dbp')

check_hba1c <- check_hba1c %>%
  bind_rows(check_bmi) %>%
  bind_rows(check_chol) %>%
  bind_rows(check_sbp) %>%
  bind_rows(check_dbp) %>%
  bind_rows(check_chol) %>%
  bind_rows(check_age_group) %>%
  bind_rows(check_age_group_2)

check_hba1c

write.csv (check_hba1c, here::here ("output/data","check_hba1c.csv"))
