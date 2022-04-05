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

check_hba1c <- read_feather (here::here ("output/data", "input_all_2018-03-01.feather"))

check_hba1c <- check_hba1c %>%
  ungroup() %>%
  dplyr::filter(hba1c_march >0 ) %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(mean_hba1c_march = mean(hba1c_march))

write.csv (check_hba1c, here::here ("output/data","check_hba1c.csv"))
