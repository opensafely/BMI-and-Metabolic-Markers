
## This script looks for predictors being in the top_decile of weight gain acceleration in patients with T2DM
## Population limited to those with T2DM



library(pacman)
library(tidyverse)
library(Hmisc)
library(here)
library(arrow)
library(purrr)
library(broom)
library(data.table)
library(janitor)
library(gtsummary)



my_data <- read_csv (here::here ("output/data", "CC_stratified_analysis_delta_change_data.csv"))

my_data <- my_data %>%
dplyr::filter(diabetes_t2 == TRUE)


my_data <- my_data  %>% 
  dplyr::mutate(eth_collapsed = case_when(
    eth_16_corrected ==   "White_British" ~ "white",
    eth_16_corrected ==  "White_Irish" ~ "white",
    eth_16_corrected ==  "Other_White"  ~ "white",
    eth_16_corrected == "White_Black_Carib" ~ "mixed",
    eth_16_corrected == "White_Black_African" ~ "mixed",
    eth_16_corrected == "White_Asian" ~ "mixed",
    eth_16_corrected == "Other_Mixed" ~ "mixed",
    eth_16_corrected == "Indian" ~ "south_asian",
    eth_16_corrected == "Pakistani" ~ "south_asian",
    eth_16_corrected == "Bangladeshi" ~ "south_asian",
    eth_16_corrected == "Other_Asian" ~ "chinese_other",
    eth_16_corrected == "Chinese" ~ "chinese_other",
    eth_16_corrected == "Caribbean" ~ "black",
    eth_16_corrected == "African" ~ "black",
    eth_16_corrected == "Other_Black" ~ "black",
    eth_16_corrected == "Other" ~ "chinese_other"
  ))  

print("check ethnicity collapsed correctly")
my_data %>%
  tabyl(eth_16_corrected, eth_collapsed)

my_data %>%
  tabyl(age_group_2) 

my_data <- my_data %>% 
  dplyr::mutate(age_collapsed = case_when(
    age_group_2 == "18-29" ~ "18-39", 
    age_group_2 ==  "30-39" ~ "18-39", 
    age_group_2 ==  "40-49" ~ "40-59", 
    age_group_2 == "50-59" ~ "40-59", 
    age_group_2 == "60-69" ~ "60- 79",
    age_group_2 ==  "70-79" ~ "60- 79",
  ))

print("check age collapsed correctly")
my_data %>%
  tabyl(age_group_2, age_collapsed)

# Collapse medication
my_data <- my_data %>% 
dplyr::mutate(diabetes_med = case_when(
  insulin_meds == 1 ~ "insulin", 
  ((insulin_meds == 0) & (oad_meds == 1)) ~ "oad", 
  ((insulin_meds == 0) & (oad_meds == 0)) ~ "lifestyle"
))



##########################
# Convert imd to a categorical variable
my_data$imd <- as.factor(my_data$imd)

# Define the list of variables for regression
regression_vars <- c("region", "hypertension", "chronic_cardiac", "learning_disability",
                     "depression", "dementia", "psychosis_schiz_bipolar", "asthma",
                     "COPD", "stroke_and_TIA", "precovid_bmi_category", "diabetes_med")

# Create an empty list to store the regression results
regression_results <- list()

# Iterate over the variables and perform regression analysis
for (var in regression_vars) {
  model <- glm(change_90th ~ age_group_2 + sex + eth_collapsed + imd + .,
               data = my_data[, c("change_90th", "age_group_2", "sex", "eth_collapsed", "imd", var)],
               family = "binomial")
  
  df <- model$df.residual
  tidy_model <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(across(where(is.numeric), round, digits = 5)) %>%
    mutate(df = df)
  
  regression_results[[var]] <- tidy_model
}

# Combine the regression results into a single dataframe
combined_results <- bind_rows(regression_results, .id = "model")



# Print or inspect the combined results
write_csv (combined_results, here::here ("output/data","change_90th_t2dm_models_2.csv"))




