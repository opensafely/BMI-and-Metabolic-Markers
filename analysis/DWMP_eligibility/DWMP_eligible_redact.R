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




d_T1DM <- read_csv (here::here ("output/data", "DWMP_eligible_5yr_data_T1DM.csv"))
d_T2DM <- read_csv (here::here ("output/data", "DWMP_eligible_5yr_data_T2DM.csv"))
d_hypertension <- read_csv (here::here ("output/data", "DWMP_eligible_5yr_data_hypertension.csv"))


d_T1DM$eligible[d_T1DM$eligible == 5] <- "6-10"
d_T1DM$no_bmi[d_T1DM$no_bmi == 5] <- "6-10"
d_T1DM$not_eligible[d_T1DM$not_eligible == 5] <- "6-10"




d_T2DM$eligible[d_T2DM$eligible == 5] <- "6-10"
d_T2DM$no_bmi[d_T2DM$no_bmi == 5] <- "6-10"
d_T2DM$not_eligible[d_T2DM$not_eligible == 5] <- "6-10"




d_hypertension$eligible[d_hypertension$eligible == 5] <- "6-10"
d_hypertension$no_bmi[d_hypertension$no_bmi == 5] <- "6-10"
d_hypertension$not_eligible[d_hypertension$not_eligible == 5] <- "6-10"




write.csv (d_T1DM, here::here ("output/data", "DWMP_eligible_5yr_data_T1DM_redact.csv"))

write.csv (d_T2DM, here::here ("output/data", "DWMP_eligible_5yr_data_T2DM_redact.csv"))


write.csv (d_hypertension, here::here ("output/data", "DWMP_eligible_5yr_data_hypertension_redact.csv"))
