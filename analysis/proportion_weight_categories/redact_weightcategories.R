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




d_2015 <- read_csv (here::here ("output/data", "weight_categories_2015.csv"))
d_2016 <- read_csv (here::here ("output/data", "weight_categories_2016.csv"))
d_2017 <- read_csv (here::here ("output/data", "weight_categories_2017.csv"))
d_2018 <- read_csv (here::here ("output/data", "weight_categories_2018.csv"))
d_2019 <- read_csv (here::here ("output/data", "weight_categories_2019.csv"))
d_2020 <- read_csv (here::here ("output/data", "weight_categories_2020.csv"))
d_2021 <- read_csv (here::here ("output/data", "weight_categories_2021.csv"))






d_2015$healthy[d_2015$healthy == 5] <- "6-10"
d_2015$no_bmi[d_2015$no_bmi == 5] <- "6-10"
d_2015$overweight[d_2015$overweight == 5] <- "6-10"
d_2015$obese[d_2015$obese == 5] <- "6-10"
d_2015$underweight[d_2015$underweight == 5] <- "6-10"
d_2015$total[d_2015$total == 5] <- "6-10"



d_2016$healthy[d_2016$healthy == 5] <- "6-10"
d_2016$no_bmi[d_2016$no_bmi == 5] <- "6-10"
d_2016$overweight[d_2016$overweight == 5] <- "6-10"
d_2016$obese[d_2016$obese == 5] <- "6-10"
d_2016$underweight[d_2016$underweight == 5] <- "6-10"
d_2016$total[d_2016$total == 5] <- "6-10"



d_2017$healthy[d_2017$healthy == 5] <- "6-10"
d_2017$no_bmi[d_2017$no_bmi == 5] <- "6-10"
d_2017$overweight[d_2017$overweight == 5] <- "6-10"
d_2017$obese[d_2017$obese == 5] <- "6-10"
d_2017$underweight[d_2017$underweight == 5] <- "6-10"
d_2017$total[d_2017$total == 5] <- "6-10"



d_2018$healthy[d_2018$healthy == 5] <- "6-10"
d_2018$no_bmi[d_2018$no_bmi == 5] <- "6-10"
d_2018$overweight[d_2018$overweight == 5] <- "6-10"
d_2018$obese[d_2018$obese == 5] <- "6-10"
d_2018$underweight[d_2018$underweight == 5] <- "6-10"
d_2018$total[d_2018$total == 5] <- "6-10"



d_2019$healthy[d_2019$healthy == 5] <- "6-10"
d_2019$no_bmi[d_2019$no_bmi == 5] <- "6-10"
d_2019$overweight[d_2019$overweight == 5] <- "6-10"
d_2019$obese[d_2019$obese == 5] <- "6-10"
d_2019$underweight[d_2019$underweight == 5] <- "6-10"
d_2019$total[d_2019$total == 5] <- "6-10"



d_2020$healthy[d_2020$healthy == 5] <- "6-10"
d_2020$no_bmi[d_2020$no_bmi == 5] <- "6-10"
d_2020$overweight[d_2020$overweight == 5] <- "6-10"
d_2020$obese[d_2020$obese == 5] <- "6-10"
d_2020$underweight[d_2020$underweight == 5] <- "6-10"
d_2020$total[d_2020$total == 5] <- "6-10"




d_2021$healthy[d_2021$healthy == 5] <- "6-10"
d_2021$no_bmi[d_2021$no_bmi == 5] <- "6-10"
d_2021$overweight[d_2021$overweight == 5] <- "6-10"
d_2021$obese[d_2021$obese == 5] <- "6-10"
d_2021$underweight[d_2021$underweight == 5] <- "6-10"
d_2021$total[d_2021$total == 5] <- "6-10"






write.csv (d_2021, here::here ("output/data", "weight_categories_2021_redact.csv"))

write.csv (d_2020, here::here ("output/data", "weight_categories_2020_redact.csv"))

write.csv (d_2019, here::here ("output/data", "weight_categories_2019_redact.csv"))

write.csv (d_2018, here::here ("output/data", "weight_categories_2018_redact.csv"))

write.csv (d_2017, here::here ("output/data", "weight_categories_2017_redact.csv"))

write.csv (d_2016, here::here ("output/data", "weight_categories_2016_redact.csv"))

write.csv (d_2015, here::here ("output/data", "weight_categories_2015_redact.csv"))


