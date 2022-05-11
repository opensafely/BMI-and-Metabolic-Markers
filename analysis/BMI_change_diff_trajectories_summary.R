## This script looks at the average weight change in the prepandemic periods by exposure groups
## Author: M Samuel
## Date: 4th May 2022



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
library(data.table)



BMI_trajectories <- read_feather (here::here ("output/data", "BMI_trajectories_final_demog.feather"))


## Remove infinity time change values


## Due to way BMI is extracted 141 patients with a value recorded on 1st March 2018 were counted in two time windows
## This created a time difference of 0 and therefore an infinity value with BMI change/time
## create a flag to identify when a time difference between BMI measures is recorded as '0'  Then filter these out.
BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(timechange1_check = time_change1)

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::mutate(time_change_error = case_when(
    timechange1_check == 0 ~ 1, 
    timechange1_check != 0 ~ 0
  ))


BMI_trajectories %>% 
  tabyl(time_change_error)


########### IMPORTANT 
## need to add this code to other trajectory analyses or NA will be filtered out
## Actually just want to filter out the infinity values
BMI_trajectories <- BMI_trajectories %>% 
  replace_na(list(time_change_error = 0))

BMI_trajectories <- BMI_trajectories %>% 
  dplyr::filter(time_change_error == 0)




## order the age-groups for ordered plots
BMI_trajectories <- BMI_trajectories# Replicate data
BMI_trajectories$age_group_2 <- factor(BMI_trajectories$age_group_2,      # Reordering group factor levels
                                       levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))

BMI_trajectories$age_group <- factor(BMI_trajectories$age_group,      # Reordering group factor levels
                                     levels = c("18-39", "40-65", "65-80", "80+"))


BMI_trajectories$smoking_status <- factor(BMI_trajectories$smoking_status, 
                                          levels = c('N',"S", "E", "M"))

## selected the variables for analysis


## remove redundant columns
BMI_trajectories <- BMI_trajectories  %>% 
  dplyr::select(-c(type1_diabetes,          
                   type2_diabetes,         
                   unknown_diabetes,  
                   sbp_date_measured,       
                   dbp_date_measured,       
                   diabetes_type,  
                   year, 
                   had_bmi, 
                   ends_with("_date"))) %>%
  dplyr::rename(precovid_change = yearly_bmi_change1) %>% 
  dplyr::rename(postcovid_change = yearly_bmi_change2) 







## filter out extreme BMI Change values.  Likely to be error entries.  Limits cover >95.5% of population

BMI_trajectories <- BMI_trajectories %>%
  dplyr::filter(precovid_change>-6 & precovid_change<6) %>% 
  dplyr::filter(postcovid_change>-6 & postcovid_change<6) 
  


  
## Change to data table for efficiency  


BMI_DT <- BMI_trajectories %>% 
ungroup()



BMI_DT <- BMI_DT %>% 
  dplyr::mutate(trajectory_change = postcovid_change - precovid_change)


## Categorise BMIs at different stages
BMI_DT <- BMI_DT %>%
  dplyr:: mutate (base_bmi_category = base_bmi) %>% 
  dplyr::mutate(precovid_bmi_category = precovid_bmi) %>% 
  dplyr::mutate(postcovid_bmi_category = postcovid_bmi)



## categorise patients based on BMI at base, precovid and postcovid. 
## done in base R for increased efficiency

BMI_DT$base_bmi_category[BMI_DT$base_bmi_category < 18.5] <- "underweight"
BMI_DT$base_bmi_category[BMI_DT$base_bmi_category >= 18.5 & BMI_DT$base_category <25] <- "healthy"
BMI_DT$base_bmi_category[BMI_DT$base_bmi_category >= 25 & BMI_DT$base_category <30] <- "overweight"
BMI_DT$base_bmi_category[BMI_DT$base_bmi_category >= 30 & BMI_DT$base_category <99] <- "obese"


BMI_DT$precovid_bmi_category[BMI_DT$precovid_bmi_category < 18.5] <- "underweight"
BMI_DT$precovid_bmi_category[BMI_DT$precovid_bmi_category >= 18.5 & BMI_DT$precovid_bmi_category <25] <- "healthy"
BMI_DT$precovid_bmi_category[BMI_DT$precovid_bmi_category >= 25 & BMI_DT$precovid_bmi_category <30] <- "overweight"
BMI_DT$precovid_bmi_category[BMI_DT$precovid_bmi_category >= 30 & BMI_DT$precovid_bmi_category <99] <- "obese"

BMI_DT$postcovid_bmi_category[BMI_DT$postcovid_bmi_category < 18.5] <- "underweight"
BMI_DT$postcovid_bmi_category[BMI_DT$postcovid_bmi_category >= 18.5 & BMI_DT$postcovid_bmi_category <25] <- "healthy"
BMI_DT$postcovid_bmi_category[BMI_DT$postcovid_bmi_category >= 25 & BMI_DT$postcovid_bmi_category <30] <- "overweight"
BMI_DT$postcovid_bmi_category[BMI_DT$postcovid_bmi_category >= 30 & BMI_DT$postcovid_bmi_category <99] <- "obese"


## save data set for subsequent models
BMI_DT_save <- BMI_DT


## filter out missing
BMI_DT <- BMI_DT %>%
dplyr::filter(complete_bmi_data == "complete") 

BMI_DT %>%
tabyl(age_group_2, sex)

BMI_DT_summ <- BMI_DT %>% 
  dplyr::summarise( n = n(), mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::mutate(group = "all", .before=1) %>% 
  dplyr::mutate(variable = "all", .before =1)
  


## create summaries to assess for change in mean in sd per subgroup
 #age_group_2
BMI_summ_age_group_2 <- BMI_DT %>%
  dplyr::group_by(age_group_2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = age_group_2) %>% 
  dplyr::mutate(variable = 'age_group_2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))
 


# sex
BMI_summ_sex <- BMI_DT%>%
  dplyr::group_by(sex)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = sex) %>% 
  dplyr::mutate(variable = 'sex', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# region
BMI_summ_region <- BMI_DT%>%
  dplyr::group_by(region)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = region) %>% 
  dplyr::mutate(variable = 'region', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# imd
BMI_summ_imd <- BMI_DT%>%
  dplyr::group_by(imd)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = imd) %>% 
  dplyr::mutate(variable = 'imd', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# hypertension
BMI_summ_hypertension <- BMI_DT%>%
  dplyr::group_by(hypertension)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = hypertension) %>% 
  dplyr::mutate(variable = 'hypertension', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# diabetes_t1
BMI_summ_diabetes_t1 <- BMI_DT%>%
  dplyr::group_by(diabetes_t1)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t1) %>% 
  dplyr::mutate(variable = 'diabetes_t1', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# diabetes_t2
BMI_summ_diabetes_t2 <- BMI_DT%>%
  dplyr::group_by(diabetes_t2)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = diabetes_t2) %>% 
  dplyr::mutate(variable = 'diabetes_t2', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# learning_disability
BMI_summ_learning_disability <- BMI_DT%>%
  dplyr::group_by(learning_disability)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = learning_disability) %>% 
  dplyr::mutate(variable = 'learning_disability', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# depression
BMI_summ_depression <- BMI_DT%>%
  dplyr::group_by(depression)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = depression) %>% 
  dplyr::mutate(variable = 'depression', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# psychosis_schiz_bipolar
BMI_summ_psychosis_schiz_bipolar <- BMI_DT%>%
  dplyr::group_by(psychosis_schiz_bipolar)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = psychosis_schiz_bipolar) %>% 
  dplyr::mutate(variable = 'psychosis_schiz_bipolar', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# dementia
BMI_summ_dementia <- BMI_DT%>%
  dplyr::group_by(dementia)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = dementia) %>% 
  dplyr::mutate(variable = 'dementia', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))



# asthma
BMI_summ_asthma <- BMI_DT%>%
  dplyr::group_by(asthma)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = asthma) %>% 
  dplyr::mutate(variable = 'asthma', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# COPD
BMI_summ_COPD <- BMI_DT%>%
  dplyr::group_by(COPD)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = COPD) %>% 
  dplyr::mutate(variable = 'COPD', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# stroke_and_TIA
BMI_summ_stroke_and_TIA <- BMI_DT%>%
  dplyr::group_by(stroke_and_TIA)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = stroke_and_TIA) %>% 
  dplyr::mutate(variable = 'stroke_and_TIA', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))


# all_cancer
BMI_summ_all_cancer <- BMI_DT%>%
  dplyr::group_by(all_cancer)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = all_cancer) %>% 
  dplyr::mutate(variable = 'all_cancer', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# smoking_status
BMI_summ_smoking_status <- BMI_DT%>%
  dplyr::group_by(smoking_status)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = smoking_status) %>% 
  dplyr::mutate(variable = 'smoking_status', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# ethnic_no_miss
BMI_summ_ethnic_no_miss <- BMI_DT%>%
  dplyr::group_by(ethnic_no_miss)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = ethnic_no_miss) %>% 
  dplyr::mutate(variable = 'ethnic_no_miss', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# eth_group_16
BMI_summ_eth_group_16 <- BMI_DT%>%
  dplyr::group_by(eth_group_16)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = eth_group_16) %>% 
  dplyr::mutate(variable = 'eth_group_16', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# precovid_bmi_category
BMI_summ_precovid_bmi_category <- BMI_DT%>%
  dplyr::group_by(precovid_bmi_category)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = precovid_bmi_category) %>% 
  dplyr::mutate(variable = 'precovid_bmi_category', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

# chronic_cardiac
BMI_summ_chronic_cardiac <- BMI_DT%>%
  dplyr::group_by(chronic_cardiac)%>%
  dplyr::summarise(n = n(),mean = mean(trajectory_change), sd = sd(trajectory_change)) %>% 
  dplyr::rename(group = chronic_cardiac) %>% 
  dplyr::mutate(variable = 'chronic_cardiac', .before=1) %>% 
  dplyr::mutate(group=as.factor(group))

BMI_traj_change_summary <- BMI_DT_summ %>% 
  bind_rows(BMI_summ_age_group_2) %>% 
  bind_rows(BMI_summ_sex) %>% 
  bind_rows(BMI_summ_ethnic_no_miss) %>% 
  bind_rows(BMI_summ_eth_group_16) %>% 
  bind_rows(BMI_summ_imd) %>%
  bind_rows(BMI_summ_region) %>%
  bind_rows(BMI_summ_precovid_bmi_category) %>% 
  bind_rows(BMI_summ_hypertension) %>% 
  bind_rows(BMI_summ_diabetes_t2) %>% 
  bind_rows(BMI_summ_diabetes_t2) %>% 
  bind_rows(BMI_summ_learning_disability) %>%
  bind_rows(BMI_summ_depression) %>% 
  bind_rows(BMI_summ_psychosis_schiz_bipolar) %>% 
  bind_rows(BMI_summ_chronic_cardiac) %>% 
  bind_rows(BMI_summ_COPD) %>%
  bind_rows(BMI_summ_asthma)%>%
  bind_rows(BMI_summ_dementia) %>% 
  bind_rows(BMI_summ_all_cancer) %>% 
  bind_rows(BMI_summ_stroke_and_TIA) %>% 
  bind_rows(BMI_summ_smoking_status) %>% 
  dplyr::mutate(across(where(is.numeric), round, digits=2))



traj_change_plot <- ggplot( data = BMI_DT, 
                               mapping = aes( x = trajectory_change)) + 
  labs(title = "Change in BMI Trajectory (rate of bmi change/year) before and after 1st March 2020", 
       subtitle = "Data on BMI collected through routine primary care electronic health records between March 2015 and March 2022") + 
  geom_histogram() +
  xlim (-5, 5) +
  facet_wrap(~age_group_2)


      
                  
###### SAVE OUTPUTS

## data set for further trajectory analysis
write_feather (BMI_DT_save, here::here ("output/data","BMI_trajectory_models_data.feather"))


## outputs

ggsave(
  plot = traj_change_plot,
  filename = "change_bmi_trajecotries.png", 
  path = here::here("output"),
  dpi=600, width = 30, height = 30, units = "cm"
)


write.csv (BMI_traj_change_summary, here::here ("output/data","mean_bmi_traj_change.csv"))





