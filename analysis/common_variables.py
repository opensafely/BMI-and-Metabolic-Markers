from cohortextractor import (
    patients,
    codelist,
    filter_codes_by_category,
    combine_codelists,
)
from codelists import *

##############  NEED TO ADD MENTAL HEALTH CODES>>> removed as run error on actions.  Need to check code.



common_variables = dict(
    
    # Sex
    sex = patients.sex(return_expectations={
        "rate": "universal",
        "category": {"ratios": {"M": 0.49, "F": 0.51}},
    }),
    
    # Age
    age_group = patients.categorised_as(
        {
            "18-39": "age >= 18 AND age < 40",
            "40-65": "age >= 40 AND age < 65",
            "65-80": "age >= 65 AND age < 80",
            "80+": "age >= 80",
            "missing": "DEFAULT",
        },
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "18-39": 0.3,
                    "40-65": 0.3,
                    "65-80": 0.2,
                    "80+": 0.1,
                    "missing": 0.1,
                }
            },
        },
        age = patients.age_as_of(
            "index_date",
        ),
    ),

   age_group_2 = patients.categorised_as(
        {
            "18-29": "age_2 >= 18 AND age < 30",
            "30-39": "age_2 >= 30 AND age < 40",
            "40-49": "age_2 >= 40 AND age < 50",
            "50-59": "age_2 >= 50 AND age < 60",
            "60-69": "age_2 >= 60 AND age < 70",
            "70-79": "age_2 >= 70 AND age < 80",
            "80+": "age_2 >= 80",
            "missing": "DEFAULT",
        },
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "18-29": 0.1,
                    "30-39": 0.2,
                    "40-49": 0.2,
                    "50-59": 0.2,
                    "60-69": 0.1,
                    "70-79": 0.1,
                    "80+": 0.05,
                    "missing": 0.05,
                }
            },
        },
    
        age_2 = patients.age_as_of(
            "index_date",
        ),
    ),

        
                       
    # Region
    region = patients.registered_practice_as_of(
        "index_date",
        returning = "nuts1_region_name",
        return_expectations = {
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.1,
                    "Yorkshire and the Humber": 0.1,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East of England": 0.1,
                    "London": 0.2,
                    "South East": 0.2, 
                }
            },       
            "incidence": 0.8}
    ),
    
    # Index of multiple deprivation
    imd = patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
            "5": """index_of_multiple_deprivation >= 32844*4/5 """,
        },
        index_of_multiple_deprivation = patients.address_as_of(
            "index_date",
            returning = "index_of_multiple_deprivation",
            round_to_nearest = 100,
        ),
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0.01,
                    "1": 0.20,
                    "2": 0.20,
                    "3": 0.20,
                    "4": 0.20,
                    "5": 0.19,
                }
            },
        },
    ), 

     # Learning disabilities
    learning_disability=patients.with_these_clinical_events(
        learning_disability_codes,
        on_or_before="index_date",
        returning="binary_flag",
        return_expectations={"incidence": 0.01, },
    ),    
        
        # Dementia
        dementia=patients.with_these_clinical_events(
            dementia_codes,
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.02, },
        ),
    
        # Depression
        depression=patients.with_these_clinical_events(
            depression_codes,
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.1, },
        ),
    
            # Psychosis
        psychosis_schiz_bipolar=patients.with_these_clinical_events(
            psychosis_schizophrenia_bipolar_affective_disease_codes,
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.01, },
        ),
    
    # Diabetes
    type1_diabetes=patients.with_these_clinical_events(
        diabetes_t1_codes,
        on_or_before="index_date",
        return_last_date_in_period=True,
        include_month=True,
    ),

    type2_diabetes=patients.with_these_clinical_events(
        diabetes_t2_codes,
        on_or_before="index_date",
        return_last_date_in_period=True,
        include_month=True,
    ),

    unknown_diabetes=patients.with_these_clinical_events(
        diabetes_unknown_codes,
        on_or_before="index_date",
        return_last_date_in_period=True,
        include_month=True,
    ),
    
    diabetes_type=patients.categorised_as(
        {
            "T1DM":
                """
                        (type1_diabetes AND NOT
                        type2_diabetes) 
                    OR
                        (((type1_diabetes AND type2_diabetes) OR 
                        (type1_diabetes AND unknown_diabetes AND NOT type2_diabetes) OR
                        (unknown_diabetes AND NOT type1_diabetes AND NOT type2_diabetes))
                        AND 
                        (insulin_lastyear_meds > 0 AND NOT
                        oad_lastyear_meds > 0))
                """,
            "T2DM":
                """
                        (type2_diabetes AND NOT
                        type1_diabetes)
                    OR
                        (((type1_diabetes AND type2_diabetes) OR 
                        (type2_diabetes AND unknown_diabetes AND NOT type1_diabetes) OR
                        (unknown_diabetes AND NOT type1_diabetes AND NOT type2_diabetes))
                        AND 
                        (oad_lastyear_meds > 0))
                """,
            "UNKNOWN_DM":
                """
                        ((unknown_diabetes AND NOT type1_diabetes AND NOT type2_diabetes) AND NOT
                        oad_lastyear_meds AND NOT
                        insulin_lastyear_meds) 
                   
                """,
            "NO_DM": "DEFAULT",
        },

        return_expectations={
            "category": {"ratios": {"T1DM": 0.03, "T2DM": 0.2, "UNKNOWN_DM": 0.02, "NO_DM": 0.75}},
            "rate" : "universal"

        },

        # Patient took antidiabetic drugs
        oad_lastyear_meds=patients.with_these_medications(
            oad_med_codes, 
            between=["index_date - 365 days", "index_date - 1 day"],
            returning="number_of_matches_in_period",
        ),
        # Patient took insulin
        insulin_lastyear_meds=patients.with_these_medications(
            insulin_med_codes,
            between=["index_date - 365 days", "index_date - 1 day"],
            returning="number_of_matches_in_period",
        ),
    ),
    
    # Indicators for diabetes type
    diabetes_t1=patients.satisfying(
        """
        diabetes_type = 'T1DM'
        """         
    ),
    
    diabetes_t2=patients.satisfying(
        """
        diabetes_type = 'T2DM'
        """         
    ),



#######################################
### BMI
#######################################

    # bmi
bmi=patients.most_recent_bmi(
    between=["index_date", "index_date + 1 year"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.7,
    }
    
    ),
    

###  Flag whether there is a BMI in the last year   
    
had_bmi = patients.satisfying(
    """
    bmi>0
    """
    ), 




#################  CHECK FOR MONTHLY BMI
####  This will only give one value.  not the average BMI over the year. 
    ##  Options - create monthly BMI variables.  ' "between index_date + 1 month"   and "index_date + 2 months"..... window for each month in question

    # bmi
bmi_march=patients.most_recent_bmi(
    between=["index_date", "index_date + 1 month"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-03-01", "latest": "2021-04-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),
    
    
 bmi_apr=patients.most_recent_bmi(
    between=["index_date + 1 month", "index_date + 2 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-04-01", "latest": "2021-05-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
  bmi_may=patients.most_recent_bmi(
    between=["index_date + 2 months", "index_date + 3 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-05-01", "latest": "2021-06-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
       
  
   bmi_june=patients.most_recent_bmi(
    between=["index_date + 3 months", "index_date + 4 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-06-01", "latest": "2021-07-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
    
   bmi_july=patients.most_recent_bmi(
    between=["index_date + 4 months", "index_date + 5 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-07-01", "latest": "2021-08-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
    
   bmi_aug=patients.most_recent_bmi(
    between=["index_date + 5 months", "index_date + 6 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-08-01", "latest": "2021-09-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
    
   
   bmi_sep=patients.most_recent_bmi(
    between=["index_date + 6 months", "index_date + 7 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-09-01", "latest": "2021-10-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
    
   bmi_oct=patients.most_recent_bmi(
    between=["index_date + 7 months", "index_date + 8 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-10-01", "latest": "2021-11-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
    
   bmi_nov=patients.most_recent_bmi(
    between=["index_date + 8 months", "index_date + 9 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-11-01", "latest": "2021-12-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
    
  
   bmi_dec=patients.most_recent_bmi(
    between=["index_date + 9 months", "index_date + 10 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2015-12-01", "latest": "2022-01-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
   
   bmi_jan=patients.most_recent_bmi(
    between=["index_date + 10 months", "index_date + 11 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2016-01-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
    
   bmi_feb=patients.most_recent_bmi(
    between=["index_date + 11 months", "index_date + 12 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM-DD",
    return_expectations={
        "date": {"earliest": "2016-02-01", "latest": "2022-03-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.2,
    }
    
    ),   
    
    


###################################################
### Systolic BP
################################################

    sbp=patients.mean_recorded_value(
        systolic_blood_pressure_codes,
        on_most_recent_day_of_measurement=True,
        include_measurement_date=True,
        between=["index_date", "index_date + 1 year"],
        date_format="YYYY-MM",
        return_expectations={
            "incidence": 0.1,
            "float": {"distribution": "normal", "mean": 110, "stddev": 20},
            "date": {"earliest": "index_date", "latest": "index_date + 1 month"},
            "rate": "uniform",
        },
    ),
    
    
    
#####################################
### Monthly HbA1c
######################################
    
    
    hba1c_march=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date", "index_date + 1 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-03-01", "latest": "2021-04-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
    hba1c_april=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date + 1 month", "index_date + 2 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-04-01", "latest": "2021-05-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
    hba1c_may=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date +  2 month", "index_date + 3 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-05-01", "latest": "2021-06-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
 
     hba1c_june=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date + 3 month", "index_date + 4 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-06-01", "latest": "2021-07-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
     hba1c_july=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date +  4 month", "index_date + 5 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-07-01", "latest": "2021-08-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
     hba1c_aug=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date +  5 month", "index_date + 6 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-08-01", "latest": "2021-09-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
      hba1c_sep=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date +  6 month", "index_date + 7 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-09-01", "latest": "2021-10-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
    
     hba1c_oct=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date +  7 month", "index_date + 8 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-10-01", "latest": "2021-11-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
    
     hba1c_nov=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date +  8 month", "index_date + 9 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-11-01", "latest": "2021-12-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
    
     hba1c_dec=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date +  9 month", "index_date + 10 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-12-01", "latest": "2022-01-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
     hba1c_jan=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date +  10 month", "index_date + 11 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2016-01-01", "latest": "2022-02-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
     hba1c_feb=patients.with_these_clinical_events(
        hba1c_new_codes, 
        find_last_match_in_period=True,
        between=["index_date +  11 month", "index_date + 12 month"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2016-02-01", "latest": "2022-03-01"},
            "float": {"distribution": "normal", "mean": 40, "stddev": 10},
            "incidence": 0.01,
        },
    
    ),
    
    
##################################
###  ADD  OTHER QOF CONDITION VARIABLES
#####################################


    
 ##### Asthma
###############################

        asthma=patients.with_these_clinical_events(
            asthma_codes,
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.1, },
    ),
    
    
######  COPD
###################################
    
        
        COPD=patients.with_these_clinical_events(
            COPD_codes,
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.05, },
     ),
 
##### Stroke and TIA
####################################
    
        stroke_and_TIA=patients.with_these_clinical_events(
            stroke_and_TIA_codes,
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.02, },
     ),

 
 #### Chronic Cardiac Disease (excluding AF and VSD)
   
        chronic_cardiac=patients.with_these_clinical_events(
            chronic_cardiac_codes,
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.10, },
        ),
    
 ####  Hypertension codes

        hypertension=patients.with_these_clinical_events(
            hypertension_codes,
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.20, },
        ),
    

    # Chronic kidney disease
    # Chronic liver disease
    # Housebound

    
######  Cancer Codes
    
    
        all_cancer=patients.with_these_clinical_events(
            all_cancer_codes,
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.05, },
        ),


 



#######################################
        cholesterol_test=patients.with_these_clinical_events(
            chol_codes,
            between=["index_date", "index_date +1 year"],
            returning="binary_flag",
            return_expectations={"incidence": 0.01, },
        ),


        dbp=patients.mean_recorded_value(
        diastolic_blood_pressure_codes,
        on_most_recent_day_of_measurement=True,
        include_measurement_date=True,
        between=["index_date", "index_date + 1 year"],
        date_format="YYYY-MM",
        return_expectations={
            "incidence": 0.1,
            "float": {"distribution": "normal", "mean": 80, "stddev": 10},
            "date": {"earliest": "index_date", "latest": "index_date + 1 month"},
            "rate": "uniform",
        },
    ),

        smoking_status=patients.categorised_as(
            {
                "S": "most_recent_smoking_code = 'S' OR smoked_last_18_months",
                "E": """
                        (most_recent_smoking_code = 'E' OR (
                        most_recent_smoking_code = 'N' AND ever_smoked
                        )
                        ) AND NOT smoked_last_18_months
                """,
                "N": "most_recent_smoking_code = 'N' AND NOT ever_smoked",
                "M": "DEFAULT",
            },
            return_expectations={
                "category": {"ratios": {"S": 0.6, "E": 0.1, "N": 0.2, "M": 0.1}}
            },
            most_recent_smoking_code=patients.with_these_clinical_events(
                clear_smoking_codes,
                find_last_match_in_period=True,
                on_or_before="index_date",
                returning="category",
            ),
            ever_smoked=patients.with_these_clinical_events(
                filter_codes_by_category(clear_smoking_codes, include=["S", "E"]),
                returning="binary_flag",
                on_or_before="index_date",
            ),
            smoked_last_18_months=patients.with_these_clinical_events(
                filter_codes_by_category(clear_smoking_codes, include=["S"]),
                between=["index_date- 548 day", "index_date"],
            ),
        ),
    
    
    
)





