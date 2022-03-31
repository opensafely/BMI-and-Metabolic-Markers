###  Demographic variables
## Link all the data sets: 


##  packages
library(broom)
library(purrr)
library(dplyr)
library(janitor)
library(tidyverse)
library(arrow)


### Read in data sets


all_2015 <- read_feather (here::here ("output/data", "input_all_2015-03-01.feather"))
all_2016 <- read_feather (here::here ("output/data", "input_all_2016-03-01.feather"))
all_2017 <- read_feather (here::here ("output/data", "input_all_2017-03-01.feather"))
all_2018 <- read_feather (here::here ("output/data", "input_all_2018-03-01.feather"))
all_2019 <- read_feather (here::here ("output/data", "input_all_2019-03-01.feather"))
all_2020 <- read_feather (here::here ("output/data", "input_all_2020-03-01.feather"))
all_2021 <- read_feather (here::here ("output/data", "input_all_2021-03-01.feather"))


## bind all together

all_data <- all_2015 %>%
  bind_rows(all_2016) %>%
  bind_rows (all_2017) %>%
  bind_rows (all_2018) %>%
  bind_rows (all_2019)  %>%
  bind_rows (all_2020) %>%
  bind_rows (all_2021)


all_data_imd <- all_data  %>%
  dplyr::select(patient_id, imd)

all_data_imd <- all_data_imd %>%
  dplyr::group_by(patient_id) %>%
  dplyr::slice_head()


write_feather (all_data_imd, here::here ("output/data","patient_imd.feather"))
