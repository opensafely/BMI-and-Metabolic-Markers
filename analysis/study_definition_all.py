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
        (age >= 18 AND age <= 110) AND
        (region != "")
        """,
        
        registered_one_year=patients.registered_with_one_practice_between(
        "2019-03-01", "2020-03-01",
        return_expectations={"incidence": 0.9},

        )
    ),    
        
        
    ** common_variables
)
from codelists import *
