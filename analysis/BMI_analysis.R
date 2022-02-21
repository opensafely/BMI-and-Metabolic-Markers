
## Miriam Samuel 
## 14th Feb
## Generating the BMI analysis


## Questions for R:  1) Can we add flextable to the R library?


##JOBS:  1) need to replace IMD =0 with missing
# 2) re-order variable
# 3) create a loop for repeating operationg based on column position



### WHO HAD A BMI


## proportion in each year
BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%           #express as percent
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()          # format to one line per row 
  #flextable::save_as_docx(path = "tabyl.docx")   ## SAVE THE TABLE



## proportion in each year: by region
BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, region) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


## proportion in each year: by IMD
BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, imd) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


##  proportion in each year by 
# names(BMI_complete_categories)

BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, sex) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, age_group) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, region) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, imd) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, imd) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()

BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, learning_disability) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, dementia) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, depression) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, psychosis_schiz_bipolar) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, diabetes_t1) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, diabetes_t2) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, asthma) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, COPD) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, stroke_and_TIA) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, chronic_cardiac) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, hypertension) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, all_cancer) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(had_bmi, year, ethnicity) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()




### what was the BMI
## proportion in each BMI group per year


## proportion in each year
BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%           #express as percent
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()          # format to one line per row 
#flextable::save_as_docx(path = "tabyl.docx")   ## SAVE THE TABLE



## proportion in each year: by region
BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, region) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


## proportion in each year: by IMD
BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, imd) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


##  proportion in each year by 
# names(BMI_complete_categories)

BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, sex) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, age_group) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, region) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, imd) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, imd) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()

BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, learning_disability) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, dementia) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, depression) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, psychosis_schiz_bipolar) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, diabetes_t1) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, diabetes_t2) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, asthma) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, COPD) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, stroke_and_TIA) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, chronic_cardiac) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, hypertension) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()


BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, all_cancer) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



BMI_complete_categories %>%                   # choose data set
  tabyl(BMI_categories, year, ethnicity) %>%                    # proportion BMI by year
  adorn_totals() %>%                          # add total per per year
  adorn_percentages(denominator = "col") %>%   # show as a proportion in columns 
  adorn_pct_formatting(digits = 1)  %>%         #express as percent
  adorn_ns()



  
