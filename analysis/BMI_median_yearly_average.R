##########################################
## Author: Miriam Samuel
## Updated: 21st March 2022
## Yearly BMI average. 2019, 2020, 2021 (in those who had BMI measured). 


##  packages
library(broom)
library(purrr)
library(dplyr)
library(janitor)
library(tidyverse)
library(arrow)
library(data.table)

# 1. Read in data
BMI_complete_categories <- read_feather (here::here ("output/data", "BMI_complete_median.feather"))


# 2. create model comparing yearly BMI (univariate). 
BMI_complete_categories_lm <- BMI_complete_categories %>%
  filter(year==2019| year==2020 | year == 2021) %>%
  mutate(year = as.factor(year)) %>%
  mutate(year = fct_relevel(year, "2019", "2020", "2021", after=0))



median_year_lm <- lm(median_bmi ~ year, data = BMI_complete_categories_lm) %>%
broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # turn into a table with tidy, exponentiate and produce CIs
  dplyr::mutate(across(where(is.numeric), round, digits = 2))  %>%
  mutate(year = term) %>%     # create a column called year
  mutate(year=case_when(
    year == "(Intercept)" ~ "2019",  # rename years
    year == "year2020" ~ "2020",
    year == "year2021" ~ "2021"))



# 3.create other summary stats using data table for efficiency
standard_error <- function(x) sd(x) / sqrt(length(x)) 
DT_bmi_med <- data.table(BMI_complete_categories_lm)  ## change frame into a data.table for manipulation
median_bmi_year_table <- DT_bmi_med[,  .(med_bmi = median(median_bmi)), by="year"]  ## calculate the median bmi by year in data table
IQR_bmi_year_table <- DT_bmi_med[,  .(iqr_bmi = IQR(median_bmi)), by="year"]         ## calculate IQR
mean_bmi_year_table <- DT_bmi_med[, .(mean_bmi = mean(median_bmi)), by = "year"]    ## calculate mean
se_bmi_year_table <- DT_bmi_med[, .(se_bmi = standard_error(median_bmi)), by = "year" ]
N_bmi_year_table <- DT_bmi_med[,  .N, by="year"] 
sd_bmi_year_table <- DT_bmi_med[, .(sd_bmi = sd(median_bmi)), by = "year" ]

# 4. join to  regression predictions to create combined table
median_year_lm2 <- median_year_lm %>%
  dplyr::left_join(median_bmi_year_table) %>%
  dplyr::left_join(IQR_bmi_year_table) %>%
  dplyr::left_join(mean_bmi_year_table) %>%
  dplyr::left_join(se_bmi_year_table)  %>%
  dplyr::left_join(N_bmi_year_table) %>%
  dplyr::left_join(sd_bmi_year_table) %>%
  dplyr::select(   # select and re-order valid outpute.  Note base co-efficient is the average bmi
    year, 
    N,
    med_bmi, 
    iqr_bmi,
    mean_bmi, 
    sd_bmi,
    se_bmi,
    estimate, 
    std.error,
    statistic, 
    p.value, 
    conf.low,
    conf.high) %>% 
  dplyr::mutate(across(where(is.numeric), round, digits = 2)) 


# 5. write output - moderately sensitive.
write.csv (median_year_lm2, here::here ("output/data","yearly_average_bmi_2019to21")
