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

hba1c_summary <- read_feather (here::here ("output/data", "hba1c_2019_summary.feather"))

check_hba1c <- hba1c_summary %>%
ungroup()%>%
tabyl(had_hba1c)

write.csv (check_hba1c, here::here ("output/data","check_hba1c.csv"))
