from cohortextractor import (
    StudyDefinition,
    patients,
    codelist,
    codelist_from_csv,
)

from codelists import ethnicity_codes, ethnicity_codes_16

study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
    },
    
    population=patients.all(),
    # ETHNICITY IN 6 CATEGORIES
    eth=patients.with_these_clinical_events(
        ethnicity_codes,
        returning="category",
        find_last_match_in_period=True,
        include_date_of_match=False,
        return_expectations={
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.75,
        },
    ),

    # fill missing ethnicity from SUS
    ethnicity_sus = patients.with_ethnicity_from_sus(
        returning="group_6",  
        use_most_frequent_code=True,
        return_expectations={
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.75,
            },
    ),
    

    ethnicity = patients.categorised_as(
            {"0": "DEFAULT",
            "1": "eth='1' OR (NOT eth AND ethnicity_sus='1')", 
            "2": "eth='2' OR (NOT eth AND ethnicity_sus='2')", 
            "3": "eth='3' OR (NOT eth AND ethnicity_sus='3')", 
            "4": "eth='4' OR (NOT eth AND ethnicity_sus='4')",  
            "5": "eth='5' OR (NOT eth AND ethnicity_sus='5')",
            }, 
            return_expectations={
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.75,
            },
    ),


    ethnicity_16=patients.categorised_as(
            {"0": "DEFAULT",
            "1": "eth16='1' OR (NOT eth16 AND ethnicity_sus16='1')", 
            "2": "eth16='2' OR (NOT eth16 AND ethnicity_sus16='2')", 
            "3": "eth16='3' OR (NOT eth16 AND ethnicity_sus16='3')", 
            "4": "eth16='4' OR (NOT eth16 AND ethnicity_sus16='4')",  
            "5": "eth16='5' OR (NOT eth16 AND ethnicity_sus16='5')",
            "6": "eth16='6' OR (NOT eth16 AND ethnicity_sus16='6')",
            "7": "eth16='7' OR (NOT eth16 AND ethnicity_sus16='7')",
            "8": "eth16='8' OR (NOT eth16 AND ethnicity_sus16='8')",
            "9": "eth16='9' OR (NOT eth16 AND ethnicity_sus16='9')",
            "10": "eth16='10' OR (NOT eth16 AND ethnicity_sus16='10')",
            "11": "eth16='11' OR (NOT eth16 AND ethnicity_sus16='11')",
            "12": "eth16='12' OR (NOT eth16 AND ethnicity_sus16='12')",
            "13": "eth16='13' OR (NOT eth16 AND ethnicity_sus16='13')",
            "14": "eth16='14' OR (NOT eth16 AND ethnicity_sus16='14')",
            "15": "eth16='15' OR (NOT eth16 AND ethnicity_sus16='15')",
            "16": "eth16='16' OR (NOT eth16 AND ethnicity_sus16='16')",
            }, 
                return_expectations={
                        "category": {
                            "ratios": {
                                "1": 0.0625,
                                "2": 0.0625,
                                "3": 0.0625,
                                "4": 0.0625,
                                "5": 0.0625,
                                "6": 0.0625,
                                "7": 0.0625,
                                "8": 0.0625,
                                "9": 0.0625,
                                "10": 0.0625,
                                "11": 0.0625,
                                "12": 0.0625,
                                "13": 0.0625,
                                "14": 0.0625,
                                "15": 0.0625,
                                "16": 0.0625,
                            }
                        },
                        "incidence": 0.75,
                    },
            eth16=patients.with_these_clinical_events(
                ethnicity_codes_16,
                returning="category",
                find_last_match_in_period=True,
                include_date_of_match=True,
                return_expectations={
                    "category": {
                        "ratios": {
                            "1": 0.0625,
                            "2": 0.0625,
                            "3": 0.0625,
                            "4": 0.0625,
                            "5": 0.0625,
                            "6": 0.0625,
                            "7": 0.0625,
                            "8": 0.0625,
                            "9": 0.0625,
                            "10": 0.0625,
                            "11": 0.0625,
                            "12": 0.0625,
                            "13": 0.0625,
                            "14": 0.0625,
                            "15": 0.0625,
                            "16": 0.0625,
                        }
                    },
                    "incidence": 0.75,
                },
            ),
            ethnicity_sus16=patients.with_ethnicity_from_sus(
                returning="group_16",  
                use_most_frequent_code=True,
                return_expectations={
                    "category": {
                        "ratios": {
                            "1": 0.0625,
                            "2": 0.0625,
                            "3": 0.0625,
                            "4": 0.0625,
                            "5": 0.0625,
                            "6": 0.0625,
                            "7": 0.0625,
                            "8": 0.0625,
                            "9": 0.0625,
                            "10": 0.0625,
                            "11": 0.0625,
                            "12": 0.0625,
                            "13": 0.0625,
                            "14": 0.0625,
                            "15": 0.0625,
                            "16": 0.0625,
                        }
                    },
                    "incidence": 0.75,
                },
            ),
    ),
)
