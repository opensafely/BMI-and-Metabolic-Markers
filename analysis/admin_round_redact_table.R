## This script rounds and redacts tables to ensure no counts <5 and all values rounded to 5
## 12th May
## Author: M Samuel


## Add libraries



## Add data





 ## extract numbers from strings
tabyl_test <- tabyl_test %>% 
  dplyr::mutate(F_percent = stringr::str_extract(tabyl_test$F, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(F_N = stringr::str_extract(string = tabyl_test$F,
                                         pattern = "(?<=\\().*(?=\\))")) %>% 
  dplyr::mutate(F_N = as.numeric(F_N)) %>% 
  dplyr::filter(F_N >5) %>%
  dplyr::mutate(M_percent = stringr::str_extract(tabyl_test$M, "[0-9]+[.]?[0-9]*(?=%)")) %>% 
  dplyr::mutate(M_N = stringr::str_extract(string = tabyl_test$M,
                                           pattern = "(?<=\\().*(?=\\))")) %>% 
  dplyr::select(M_percent, M_N, F_percent, F_N) %>%     ## add group and variable
  dplyr::mutate(M_N = as.numeric(M_N)) %>% 
  dplyr::filter(M_N >5)


## round
tabyl_test <- tabyl_test%>%
  dplyr::mutate(F_N = plyr::round_any(tabyl_test$F_N, 5)) %>% 
  dplyr::mutate(M_N = plyr::round_any(tabyl_test$M_N, 5)) 
  
  
