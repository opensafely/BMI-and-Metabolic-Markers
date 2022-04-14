## cohort extractor for diabetes meds amongst diabetics

from cohortextractor import (
    StudyDefinition, 
    patients, 
    codelist, 
    codelist_from_csv,
    Measure,
)  # NOQA

from common_variables import (
    common_variables
)

study = StudyDefinition(
    index_date="2020-03-01",
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
        },
    
    ############################### Robin's code
    population=patients.satisfying(
        """
        registered_one_year AND
        (sex = "M" OR sex = "F") AND
        (age >= 18 AND age <= 90) AND
        (region != "") AND
        (diabetes_type = 'T1DM' OR diabetes_type = 'T2DM')
        
        """,
        
        registered_one_year=patients.registered_with_one_practice_between(
        "2021-03-01", "2022-03-01",
        return_expectations={"incidence": 0.9},

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
        
    ), 
  
    oad_lastyear_meds=patients.with_these_medications(
            oad_med_codes, 
            between=["index_date - 365 days", "index_date - 1 day"],
            return_expectations={"incidence": 0.50},
    ),  
    
    insulin_lastyear_meds=patients.with_these_medications(
            insulin_med_codes,
            between=["index_date - 365 days", "index_date - 1 day"],
            return_expectations={"incidence": 0.50},
        
    ),
)
from codelists import *
