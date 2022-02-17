from cohortextractor import patients
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
            "0-15": "age >= 0 AND age < 16",
            "16-24": "age >= 16 AND age < 25",
            "25-34": "age >= 25 AND age < 35",
            "35-44": "age >= 35 AND age < 45",
            "45-54": "age >= 45 AND age < 55",
            "55-64": "age >= 55 AND age < 65",
            "65-74": "age >= 65 AND age < 75",
            "75+": "age >= 75",
            "missing": "DEFAULT",
        },
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "0-15": 0.2,
                    "16-24": 0.1,
                    "25-34": 0.1,
                    "35-44": 0.15,
                    "45-54": 0.1,
                    "55-64": 0.1,
                    "65-74": 0.1,
                    "75+": 0.13,
                    "missing": 0.02,
                }
            },
        },
        age = patients.age_as_of(
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

    # Indicator for test
    took_hba1c=patients.with_these_clinical_events(
        hba1c_new_codes,
        find_last_match_in_period=True,
        between=["index_date", "last_day_of_month(index_date)"],
        returning="binary_flag",
        return_expectations={
            "incidence": 0.1,
        }
    ), 
    
    # HbA1c Test
    hba1c_mmol_per_mol=patients.with_these_clinical_events(
        hba1c_new_codes,
        find_last_match_in_period=True,
        between=["index_date", "last_day_of_month(index_date)"],
        returning="numeric_value",
        include_date_of_match=True,
        return_expectations={
            "float": {"distribution": "normal", "mean": 40.0, "stddev": 20},
            "incidence": 0.95,
        },
    ),

    prev_hba1c_mmol_per_mol=patients.with_these_clinical_events(
        hba1c_new_codes,
        find_last_match_in_period=True,
        between=["index_date - 12 months", "index_date - 1 day"],
        returning="numeric_value",
        include_date_of_match=True,
        return_expectations={
            "float": {"distribution": "normal", "mean": 40.0, "stddev": 20},
            "incidence": 0.95,
        },
    ),
    prev_hba1c_58_75 =patients.categorised_as(
        {"0": "DEFAULT", 
        "1": """(prev_hba1c_mmol_per_mol > 48) AND 
                (prev_hba1c_mmol_per_mol < 76)"""},
        return_expectations = {"rate": "universal",
                              "category": {
                                  "ratios": {
                                      "0": 0.70,
                                      "1": 0.30,
                                      }
                                  },
                              },
    ),
    prev_hba1c_gt_75 =patients.categorised_as(
        {"0": "DEFAULT", 
        "1": """(prev_hba1c_mmol_per_mol > 75)"""},
        return_expectations = {"rate": "universal",
                              "category": {
                                  "ratios": {
                                      "0": 0.70,
                                      "1": 0.30,
                                      }
                                  },
                              },
    ),
    
    # Flag elevated levels        
    hba1c_gt_48=patients.categorised_as(
        {"0": "DEFAULT", "1": """hba1c_mmol_per_mol > 48"""},
        return_expectations = {"rate": "universal",
                              "category": {
                                  "ratios": {
                                      "0": 0.70,
                                      "1": 0.30,
                                      }
                                  },
                              },
    ),
    hba1c_gt_58=patients.categorised_as(
        {"0": "DEFAULT", "1": """hba1c_mmol_per_mol > 58"""},
        return_expectations = {"rate": "universal",
                              "category": {
                                  "ratios": {
                                      "0": 0.80,
                                      "1": 0.20,
                                      }
                                  },
                              },
    ),
    hba1c_gt_64=patients.categorised_as(
        {"0": "DEFAULT", "1": """hba1c_mmol_per_mol > 64"""},
        return_expectations = {"rate": "universal",
                              "category": {
                                  "ratios": {
                                      "0": 0.90,
                                      "1": 0.10,
                                      }
                                  },
                              },
    ),
    hba1c_gt_75=patients.categorised_as(
        {"0": "DEFAULT", "1": """hba1c_mmol_per_mol > 75"""},
        return_expectations = {"rate": "universal",
                              "category": {
                                  "ratios": {
                                      "0": 0.95,
                                      "1": 0.05,
                                      }
                                  },
                              },
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
        "incidence": 0.50,
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
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),
    
    
 bmi_apr=patients.most_recent_bmi(
    between=["index_date + 1 month", "index_date + 2 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
  bmi_may=patients.most_recent_bmi(
    between=["index_date + 2 months", "index_date + 3 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
       
  
   bmi_june=patients.most_recent_bmi(
    between=["index_date + 3 months", "index_date + 4 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
    
   bmi_july=patients.most_recent_bmi(
    between=["index_date + 4 months", "index_date + 5 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
    
   bmi_aug=patients.most_recent_bmi(
    between=["index_date + 5 months", "index_date + 6 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
    
   
   bmi_sep=patients.most_recent_bmi(
    between=["index_date + 6 months", "index_date + 7 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
    
   bmi_oct=patients.most_recent_bmi(
    between=["index_date + 7 months", "index_date + 8 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
    
   bmi_nov=patients.most_recent_bmi(
    between=["index_date + 8 months", "index_date + 9 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
    
  
   bmi_dec=patients.most_recent_bmi(
    between=["index_date + 9 months", "index_date + 10 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
   
   bmi_jan=patients.most_recent_bmi(
    between=["index_date + 10 months", "index_date + 11 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
    
   bmi_feb=patients.most_recent_bmi(
    between=["index_date + 11 months", "index_date + 12 months"],
    minimum_age_at_measurement=18,
    include_measurement_date=True,
    date_format="YYYY-MM",
    return_expectations={
        "date": {"earliest": "2010-02-01", "latest": "2022-02-01"},
        "float": {"distribution": "normal", "mean": 28, "stddev": 8},
        "incidence": 0.5,
    }
    
    ),   
    
    
    
### categorising BMI
############### BUT THIS IS BASED ON MOST RECENT BMI - should we do based on average BMI from monthly readings. 
    
    bmi_base_groups = patients.categorised_as(
        {
            "1": "bmi < 18.5", 
            "2": "bmi >= 18.5 AND bmi < 25", 
            "3": "bmi >= 25 AND bmi < 27.5",
            "4": "bmi >= 27.5 AND bmi < 30",
            "5": "bmi >=30", 
            "missing": "DEFAULT", 
        }, 
        return_expectations = {
            "rate": "universal", 
            "category": {
                "ratios": {
                    "1": 0.05,
                    "2": 0.25,
                    "3": 0.2,
                    "4": 0.2,
                    "5": 0.3,
                }
            },
        },     
    ),

    
 bmi_groups = patients.categorised_as(
        {
            "underweight": "bmi < 18.5", 
            "healthy_weight": "bmi >= 18.5 AND bmi < 25", 
            "overweight": "bmi >= 25 AND bmi < 30",
            "obese": "bmi >=30", 
            "missing": "DEFAULT", 
        }, 
        return_expectations = {
            "rate": "universal", 
            "category": {
                "ratios": {
                    "underweight": 0.05, 
                    "healthy_weight": 0.25, 
                    "overweight": 0.4,
                    "obese": 0.3, 
                }
            },
        },
        
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


    
)








# ignore diabetes type for now
# For HbA1c level use codelist *opensafely/glycated-haemoglobin-hba1c-tests-numerical-value/5134e926  - this has included just IFCC measures. 


## Diabetes diagnosis:  https://github.com/opensafely/ethnicity-covid-research/issues/11  to identify Type 1 or Type 2 based on codes
# Type 1 diabetes:  opensafely/type-1-diabetes/2020-06-29
# Type 2 diabetes: opensafely/type-2-diabetes/2020-06-29
# Oral Antidiabetic drugs:  opensafely/antidiabetic-drugs/2020-07-16
# Insulin: opensafely/insulin-medication/2020-04-26
