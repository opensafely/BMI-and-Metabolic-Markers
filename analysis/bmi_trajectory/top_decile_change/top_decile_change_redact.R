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




d <- read_csv (here::here ("output/data", "change_90th_counts_lowbmiexc_T1DM.csv"))


d$top_decile[d$top_decile == 5] <- "6-10"


write.csv (d, here::here ("output/data", "change_90th_counts_lowbmiexc_T1DM_redact.csv"))
