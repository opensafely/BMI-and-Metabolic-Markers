
import pandas as pd
import os

ethnicity_df = pd.read_feather('output/data/input_ethnicity.feather')

for file in os.listdir('output/data'):
    if file.startswith('input_all'):
        file_path = os.path.join('output/data', file)
        df = pd.read_feather(file_path)
        merged_df = df.merge(ethnicity_df, how='left', on='patient_id')        
        merged_df.to_feather(file_path)
