## Dummy Data to Review R - code for open safely analysis
## Miriam Samuel 
## 10th Feb 2022


## NOTES
## check ethnicity codes
## generate a variable for to state if eligible for DWMP - based on BMI and ethnicity





## Specify libraries
library(pacman)
library(tidyverse)
library(Hmisc)
library(here)



####################################
###################################




#####  read in files

input_all_2015_03_01<- read.csv (here::here ("output/data", "input_all_2015-03-01.csv"))

input_all_2016_03_01<- read.csv (here::here ("output/data", "input_all_2016-03-01.csv"))

input_all_2017_03_01<- read.csv (here::here ("output/data", "input_all_2017-03-01.csv"))

input_all_2018_03_01<- read.csv (here::here ("output/data", "input_all_2018-03-01.csv"))

input_all_2019_03_01<- read.csv (here::here ("output/data", "input_all_2019-03-01.csv"))

input_all_2020_03_01<- read.csv (here::here ("output/data", "input_all_2020-03-01.csv"))

input_all_2021_03_01<- read.csv (here::here ("output/data", "input_all_2021-03-01.csv"))


###################
## 2015 analysis
###################

BMI_2015 <- as_tibble (input_all_2015_03_01)
# Hmisc:: describe(BMI_2015)



 

write.csv (BMI_2015, here::here ("output/data","BMI_complete_categories.csv")

##############################################################################################################CHECK 1
 
