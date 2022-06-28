## This script rounds and redacts tables to ensure no counts <5 and all values rounded to 5


## Add libraries
## Load libraries
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
library(gtsummary)
library(stringr)




d <- read_csv (here::here ("output/data", "DWMP_eligible_5yr_data_T1DM_2019.csv"))


d$eligible[d$eligible == 5] <- "6-10"
d$no_bmi[d$no_bmi == 5] <- "6-10"
d$not_eligible[d$not_eligible == 5] <- "6-10"

write.csv (d, here::here ("output/data", "DWMP_eligible_5yr_data_T1DM_2019_redact.csv"))
