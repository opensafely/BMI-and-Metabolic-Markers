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









 check_bmi <- all_2015 %>%
  ungroup() %>%
  dplyr::filter(bmi_march >0 ) %>%
  dplyr::group_by(sex) %>%
   dplyr::rename(group = 'sex') %>%
  dplyr::summarise(check = mean(bmi_march)) %>%
   dplyr::mutate(check = as.character(check)) %>%
   dplyr::mutate(variable = 'bmi')
 
 




 

 
check_sbp <- all_2015 %>%
  filter(sbp>0) %>%
  group_by(sex) %>%
  dplyr::rename(group = 'sex') %>%
  summarise_at(vars(sbp), list(check=mean)) %>%
  dplyr::mutate(check = as.character(check)) %>%
  dplyr::mutate(variable = 'sbp')
 
 


check_hba1c <- check_bmi %>%
  bind_rows(check_sbp) %>%
  bind_rows(check_age_group) 

check_hba1c

write.csv (check_hba1c, here::here ("output/data","check_hba1c.csv"))
