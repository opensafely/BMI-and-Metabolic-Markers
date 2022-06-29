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




d_2021 <- read_csv (here::here ("output/data", "obese_2021.csv"))
d_2020 <- read_csv (here::here ("output/data", "obese_2020.csv"))
d_2019 <- read_csv (here::here ("output/data", "obese_2019.csv"))
d_2018 <- read_csv (here::here ("output/data", "obese_2018.csv"))
d_2017 <- read_csv (here::here ("output/data", "obese_2017.csv"))
d_2016 <- read_csv (here::here ("output/data", "obese_2016.csv"))
d_2015 <- read_csv (here::here ("output/data", "obese_2015.csv"))


d_2021$not_obese[d_2021$not_obese == 5] <- "6-10"
d_2021$no_bmi[d_2021$no_bmi == 5] <- "6-10"
d_2021$obese[d_2021$obese == 5] <- "6-10"

d_2020$not_obese[d_2020$not_obese == 5] <- "6-10"
d_2020$no_bmi[d_2020$no_bmi == 5] <- "6-10"
d_2020$obese[d_2020$obese == 5] <- "6-10"

d_2019$not_obese[d_2019$not_obese == 5] <- "6-10"
d_2019$no_bmi[d_2019$no_bmi == 5] <- "6-10"
d_2019$obese[d_2019$obese == 5] <- "6-10"


d_2018$not_obese[d_2018$not_obese == 5] <- "6-10"
d_2018$no_bmi[d_2018$no_bmi == 5] <- "6-10"
d_2018$obese[d_2018$obese == 5] <- "6-10"

d_2017$not_obese[d_2017$not_obese == 5] <- "6-10"
d_2017$no_bmi[d_2017$no_bmi == 5] <- "6-10"
d_2017$obese[d_2017$obese == 5] <- "6-10"

d_2016$not_obese[d_2016$not_obese == 5] <- "6-10"
d_2016$no_bmi[d_2016$no_bmi == 5] <- "6-10"
d_2016$obese[d_2016$obese == 5] <- "6-10"

d_2015$not_obese[d_2015$not_obese == 5] <- "6-10"
d_2015$no_bmi[d_2015$no_bmi == 5] <- "6-10"
d_2015$obese[d_2015$obese == 5] <- "6-10"


write.csv (d_2015, here::here ("output/data", "obese_2015_redact.csv"))
write.csv (d_2016, here::here ("output/data", "obese_2016_redact.csv"))
write.csv (d_2017, here::here ("output/data", "obese_2017_redact.csv"))
write.csv (d_2018, here::here ("output/data", "obese_2018_redact.csv"))
write.csv (d_2019, here::here ("output/data", "obese_2019_redact.csv"))
write.csv (d_2020, here::here ("output/data", "obese_2020_redact.csv"))
write.csv (d_2021, here::here ("output/data", "obese_2021_redact.csv"))





